{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types #-}

module Main(main) where

import           Prelude                               hiding ((.))
import           Control.Category                      ((.))
import           Control.Monad                         (liftM, (<=<))
import           Data.Function.Utils                   (Endo, result)
import           Data.Store.VtyWidgets                 (MWidget, widgetDownTransaction, makeTextEdit,
                                                        makeBox, appendBoxChild, popCurChild, makeChoiceWidget)
import qualified Data.Store.Transaction                as Transaction
import           Data.Store.Transaction                (Transaction)
import           Data.Store.Property                   (composeLabel)
import qualified Data.Store.Property                   as Property
import qualified Data.Store.Rev.Version                as Version
import qualified Data.Store.Rev.Branch                 as Branch
import           Data.Store.Rev.View                   (View)
import qualified Data.Store.Rev.View                   as View
import           Data.Monoid                           (Monoid(..))
import           Data.Maybe                            (fromJust)
import qualified Graphics.Vty                          as Vty
import qualified Graphics.UI.VtyWidgets.TermImage      as TermImage
import qualified Graphics.UI.VtyWidgets.TextView       as TextView
import qualified Graphics.UI.VtyWidgets.TextEdit       as TextEdit
import qualified Graphics.UI.VtyWidgets.Box            as Box
import qualified Graphics.UI.VtyWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.VtyWidgets.Spacer         as Spacer
import qualified Graphics.UI.VtyWidgets.Widget         as Widget
import qualified Graphics.UI.VtyWidgets.Keymap         as Keymap
import           Graphics.UI.VtyWidgets.Widget         (Widget)
import           Graphics.UI.VtyWidgets.Display        (Display)
import qualified Graphics.UI.VtyWidgets.Run            as Run
import qualified Data.Store.Db                         as Db
import           Editor.Filter                         (Filter)
import qualified Editor.Filter                         as Filter
import qualified Editor.Anchors                        as Anchors
import           Editor.Anchors                        (DBTag, ViewTag)
import qualified Editor.Config                         as Config

focusableTextView :: String -> Widget a
focusableTextView =
  Widget.coloredFocusableDisplay Vty.blue .
  TextView.make Vty.def_attr

makeNoneEdit :: Monad m => MWidget (Transaction ViewTag m)
makeNoneEdit = return . focusableTextView $ "[no filter]"

-- TODO: Move to datastore-vtywidgets
makeFocusDelegator :: (Monad m) =>
                      Property.Property m FocusDelegator.Model ->
                      Widget (m ()) ->
                      MWidget m
makeFocusDelegator prop child = FocusDelegator.make (Property.set prop) child `liftM`
                                Property.get prop

makeLabelFilterEdit :: Monad m =>
                         Transaction.Property ViewTag m Filter.LabelData ->
                         MWidget (Transaction ViewTag m)
makeLabelFilterEdit labelDataP = do
  childEdit <- makeFilterEdit childP
  textEdit <- simpleTextEdit textEditP
  makeFocusDelegator fdP =<< makeBox Box.Vertical [ textEdit, Widget.atDisplay (Spacer.indent 4) childEdit ] boxP
  where
    property = (`composeLabel` labelDataP)
    textEditP = property Filter.labelTextEdit
    fdP       = property Filter.labelFD
    boxP      = property Filter.labelBox
    childP    = property Filter.labelChild

horizontal :: Display a -> Display a -> Display a
horizontal left right = Box.makeView Box.Horizontal [left, right]

prefix :: String -> Endo (Display a)
prefix = horizontal . TextView.make Vty.def_attr

makeInverseFilterEdit :: Monad m =>
                         Transaction.Property ViewTag m Filter.InverseData ->
                         MWidget (Transaction ViewTag m)
makeInverseFilterEdit inverseDataP =
  makeFocusDelegator fdP .
    Widget.atDisplay (prefix "NOT ") =<<
    makeFilterEdit childP
  where
    fdP    = Filter.inverseFD    `composeLabel` inverseDataP
    childP = Filter.inverseChild `composeLabel` inverseDataP

makeDisableFilterEdit :: Monad m =>
                         Transaction.Property ViewTag m Filter ->
                         MWidget (Transaction ViewTag m)
makeDisableFilterEdit childP =
  (Widget.backgroundColorWhenFocused Config.disableFocusedBackgroundColor .
   Widget.atSizedImage (TermImage.backgroundColor Config.disableBackgroundColor) .
   Widget.atDisplay (prefix "# ") .
   (Widget.atMKeymap . const $ Just mempty) .
   (Widget.atMkImage . result . TermImage.inCursor . const $ Nothing))
  `liftM` makeFilterEdit childP

makeFilterEdit :: Monad m =>
                  Transaction.Property ViewTag m Filter ->
                  MWidget (Transaction ViewTag m)
makeFilterEdit filterP = do
  f <- Property.get filterP
  widget <- case f of
    Filter.None -> makeNoneEdit
    Filter.Label labelDataIRef ->
      let labelDataP = Transaction.fromIRef labelDataIRef
      in Widget.weakerKeys (delParentKeymap "Delete label" Config.unlabelKeys $
                            Filter.labelChild `composeLabel` labelDataP)
         `liftM`
         makeLabelFilterEdit labelDataP
    Filter.Inverse inverseDataIRef ->
      let inverseDataP = Transaction.fromIRef inverseDataIRef
      in Widget.weakerKeys (delParentKeymap "Uninverse" Config.inverseKeys $
                            Filter.inverseChild `composeLabel` inverseDataP) `liftM`
         makeInverseFilterEdit inverseDataP
    Filter.Disable childIRef ->
      let childP = Transaction.fromIRef childIRef
      in Widget.weakerKeys (delParentKeymap "Enable" Config.enableKeys childP) `liftM`
         makeDisableFilterEdit childP
  return .
    Widget.weakerKeys (
      wrapInverseKeymap `mappend`
      wrapLabelKeymap `mappend`
      disableKeymap $ f) $
    widget
  where
    delParentKeymap doc keys = Keymap.fromKeyGroups keys doc . delParent
    delParent childP = Property.set filterP =<< Property.get childP
    disableKeymap (Filter.Disable {}) = mempty
    disableKeymap f = Keymap.fromKeyGroups Config.disableKeys "Disable" $ disable f
    disable f = do
      iref <- Transaction.newIRef f
      Property.set filterP $ Filter.Disable iref
    wrapLabelKeymap = Keymap.fromKeyGroups Config.addLabelKeys "Add label" . wrapLabel
    wrapLabel =
      Property.set filterP . Filter.Label <=<
      Transaction.newIRef . Filter.LabelData (FocusDelegator.initModel True) (TextEdit.initModel "") Box.initModel
    wrapInverseKeymap (Filter.Inverse {}) = mempty
    wrapInverseKeymap f = Keymap.fromKeyGroups Config.inverseKeys "Inverse(NOT) on filter" . wrapInverse $ f
    wrapInverse =
      Property.set filterP . Filter.Inverse <=<
      Transaction.newIRef . Filter.InverseData (FocusDelegator.initModel False)

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
    makeBranchKeymap = Keymap.fromKeyGroups Config.makeBranchKeys "New Branch" makeBranch
    makeBranch = do
      branch <- Branch.new =<< View.curVersion view
      textEditModelIRef <- Transaction.newIRef $ TextEdit.initModel "New view"
      let viewPair = (textEditModelIRef, branch)
      appendBoxChild branchSelectorBoxModel Anchors.branches viewPair
    undoKeymap versionData =
        if Version.depth versionData > 1
        then Keymap.fromKeyGroups Config.undoKeys "Undo" .
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
    delBranchKeymap _ = Keymap.fromKeyGroups Config.delBranchKeys "Delete Branch" deleteCurBranch
    quitKeymap = Keymap.fromKeyGroups Config.quitKeys "Quit" . fail $ "Quit"

    deleteCurBranch = do
      _ <- popCurChild branchSelectorBoxModel Anchors.branches
      return ()

simpleTextEdit :: Monad m =>
                  Transaction.Property t m TextEdit.Model ->
                  MWidget (Transaction t m)
simpleTextEdit = makeTextEdit 1 TextEdit.defaultAttr TextEdit.editingAttr
