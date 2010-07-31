{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    filter, branches, view,
    viewBoxsAnchor, dbBoxsAnchor,
    initDB,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import           Prelude                         hiding (filter)
import           Control.Monad                   (unless)
import           Data.Binary                     (Binary)
import           Data.Store.IRef                 (IRef)
import qualified Data.Store.IRef                 as IRef
import qualified Data.Store.Transaction          as Transaction
import           Data.Store.Transaction          (Transaction, Store)
import           Data.Store.Rev.Branch           (Branch)
import           Data.Store.Rev.View             (View)
import qualified Data.Store.Rev.Branch           as Branch
import qualified Data.Store.Rev.Version          as Version
import qualified Data.Store.Rev.View             as View
import qualified Data.Store.Property             as Property
import qualified Data.Store.Db                   as Db
import           Data.Store.Db                   (Db)
import           Editor.Filter                   (Filter)
import qualified Editor.Filter                   as Filter
import qualified Graphics.UI.VtyWidgets.Box      as Box
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

filterIRef :: IRef Filter
filterIRef = IRef.anchor "root"

filter :: Monad m => Transaction.Property ViewTag m Filter
filter = Transaction.fromIRef filterIRef

boxsAnchor :: Monad m => String -> String -> Transaction.Property anyTag m Box.Model
boxsAnchor name = Transaction.containerStr . Transaction.anchorContainerDef name $ Box.initModel

viewBoxsAnchor :: Monad m => String -> Transaction.Property ViewTag m Box.Model
viewBoxsAnchor = boxsAnchor "GUI.box(v)"

dbBoxsAnchor :: Monad m => String -> Transaction.Property DBTag m Box.Model
dbBoxsAnchor = boxsAnchor "GUI.box(d)"

-- Revision control

branchesIRef :: IRef [(IRef TextEdit.Model, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef TextEdit.Model, Branch)]
branches = Transaction.fromIRef branchesIRef

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

view :: Monad m => Transaction.Property DBTag m View
view = Transaction.fromIRef viewIRef


-- init

initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef $ TextEdit.initModel "master"
      initialVersionIRef <- Version.makeInitialVersion
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    curView <- initRef viewIRef $ View.new (snd . head $ bs)
    _ <- Transaction.run (viewStore curView) . initRef filterIRef $
      return Filter.None
    return ()
