{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types #-}

module Main(main) where

import           Prelude                         hiding ((.))
import           Control.Category                ((.))
import           Control.Monad                   (liftM)
import           Data.Store.VtyWidgets           (MWidget, widgetDownTransaction,
                                                  makeTextEdit, makeBox, appendBoxChild, popCurChild, makeChoiceWidget)
import qualified Data.Store.Transaction          as Transaction
import           Data.Store.Transaction          (Transaction)
import           Data.Store.Property             (composeLabel)
import qualified Data.Store.Property             as Property
import qualified Data.Store.Rev.Version          as Version
import qualified Data.Store.Rev.Branch           as Branch
import           Data.Store.Rev.View             (View)
import qualified Data.Store.Rev.View             as View
import           Data.Monoid                     (Monoid(..))
import           Data.Maybe                      (fromJust)
import qualified Graphics.Vty                    as Vty
import qualified Graphics.UI.VtyWidgets.TextView as TextView
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Box      as Box
import qualified Graphics.UI.VtyWidgets.Spacer   as Spacer
import qualified Graphics.UI.VtyWidgets.Widget   as Widget
import qualified Graphics.UI.VtyWidgets.Keymap   as Keymap
import           Graphics.UI.VtyWidgets.Widget   (Widget)
import qualified Graphics.UI.VtyWidgets.Run      as Run
import qualified Data.Store.Db                   as Db
import           Editor.Filter                   (Filter)
import qualified Editor.Filter                   as Filter
import qualified Editor.Anchors                  as Anchors
import           Editor.Anchors                  (DBTag, ViewTag)
import qualified Editor.Config                   as Config

focusableTextView :: String -> Widget a
focusableTextView =
  Widget.coloredFocusableDisplay Vty.blue .
  TextView.make Vty.def_attr

makeNoneEdit :: Monad m => MWidget (Transaction ViewTag m)
makeNoneEdit = return . focusableTextView $ "[no filter]"

makeCommentFilterEdit :: Monad m =>
                         Transaction.Property ViewTag m Filter.CommentData ->
                         MWidget (Transaction ViewTag m)
makeCommentFilterEdit commentDataP = do
  childEdit <- makeFilterEdit childP
  textEdit <- makeTextEdit 1 TextEdit.defaultAttr TextEdit.editingAttr textEditP
  makeBox Box.Vertical [ textEdit, Widget.atDisplay (Spacer.indent 4) childEdit ] boxP
  where
    textEditP = Filter.commentTextEdit `composeLabel` commentDataP
    boxP      = Filter.commentBox      `composeLabel` commentDataP
    childP    = Filter.commentChild    `composeLabel` commentDataP

makeFilterEdit :: Monad m =>
                  Transaction.Property ViewTag m Filter ->
                  MWidget (Transaction ViewTag m)
makeFilterEdit filterP = do
  f <- Property.get filterP
  widget <- case f of
    Filter.None -> makeNoneEdit
    Filter.Comment commentDataIRef ->
      let commentDataP = Transaction.fromIRef commentDataIRef
      in Widget.weakerKeys (delParentKeymap $ Filter.commentChild `composeLabel` commentDataP) `liftM`
         makeCommentFilterEdit commentDataP
  return .
    Widget.weakerKeys (wrapCommentKeymap f) $
    widget
  where
    delParentKeymap = Keymap.fromGroup Config.delParentKeys "Delete comment" . delParent
    delParent childP = Property.set filterP =<< Property.get childP
    wrapCommentKeymap = Keymap.fromGroup Config.addCommentKeys "Add comment" . wrapComment
    wrapComment f = do
      firef <- Transaction.newIRef $ Filter.CommentData (TextEdit.initModel "") Box.initModel f
      Property.set filterP $ Filter.Comment firef

branchSelectorBoxModel :: Monad m => Transaction.Property DBTag m Box.Model
branchSelectorBoxModel = Anchors.dbBoxsAnchor "branchSelector"

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: Monad m => View -> MWidget (Transaction DBTag m)
makeWidgetForView view = do
  versionData <- Version.versionData =<< View.curVersion view
  widget <- widgetDownTransaction (Anchors.viewStore view) $ makeFilterEdit Anchors.filter
  return $ Widget.strongerKeys (keymaps versionData) widget
  where
    keymaps versionData = undoKeymap versionData `mappend` makeBranchKeymap
    makeBranchKeymap = Keymap.fromGroup Config.makeBranchKeys "New Branch" makeBranch
    makeBranch = do
      branch <- Branch.new =<< View.curVersion view
      textEditModelIRef <- Transaction.newIRef $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, branch)
      appendBoxChild branchSelectorBoxModel Anchors.branches viewPair
    undoKeymap versionData =
        if Version.depth versionData > 1
        then Keymap.fromGroup Config.undoKeys "Undo" .
             View.move view .
             fromJust . Version.parent $
             versionData
        else mempty

main :: IO ()
main = Db.withDb "/tmp/filteredit.db" $ runDbStore . Anchors.dbStore
  where
    runDbStore store = do
      Anchors.initDB store
      Run.widgetLoopWithOverlay 20 30 . const . makeWidget $ store
    makeWidget dbStore = widgetDownTransaction dbStore $ do
      view <- Property.get Anchors.view
      branches <- Property.get Anchors.branches
      pairs <- mapM pair branches
      (branchSelector, branch) <- makeChoiceWidget Box.Vertical pairs branchSelectorBoxModel
      View.setBranch view branch
      viewEdit <- Widget.strongerKeys quitKeymap
                  `liftM` makeWidgetForView view
      makeBox Box.Horizontal
        [viewEdit,
         Widget.simpleDisplay Spacer.makeHorizontal,
         Widget.strongerKeys (delBranchKeymap branches) branchSelector] $
        Anchors.dbBoxsAnchor "main"

    pair (textEditModelIRef, version) = do
      textEdit <- simpleTextEdit . Transaction.fromIRef $ textEditModelIRef
      return (textEdit, version)

    delBranchKeymap [_] = mempty
    delBranchKeymap _ = Keymap.fromGroup Config.delBranchKeys "Delete Branch" deleteCurBranch
    quitKeymap = Keymap.fromGroup Config.quitKeys "Quit" . fail $ "Quit"

    deleteCurBranch = do
      _ <- popCurChild branchSelectorBoxModel Anchors.branches
      return ()

    simpleTextEdit =
      makeTextEdit 1
      TextEdit.defaultAttr
      TextEdit.editingAttr
