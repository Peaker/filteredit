{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Filter(
    Filter(..),
    CommentData(..), commentTextEdit, commentBox, commentChild
) where

import           Data.Binary                     (Binary(..))
import           Data.Binary.Get                 (getWord8)
import           Data.Binary.Put                 (putWord8)
import           Data.DeriveTH                   (derive)
import           Data.Derive.Binary              (makeBinary)
import           Data.Store.IRef                 (IRef)
import           Data.Record.Label               ((:->), mkLabels, label)
import qualified Graphics.UI.VtyWidgets.TextEdit as TextEdit
import qualified Graphics.UI.VtyWidgets.Box      as Box

data Filter = Comment (IRef CommentData)
            | None
  deriving (Show, Read, Eq, Ord)

data CommentData = CommentData { _commentTextEdit :: TextEdit.Model,
                                 _commentBox :: Box.Model,
                                 _commentChild :: Filter
                               }
  deriving (Show, Read, Eq, Ord)
$(mkLabels [''CommentData])
commentTextEdit :: CommentData :-> TextEdit.Model
commentBox :: CommentData :-> Box.Model
commentChild :: CommentData :-> Filter
$(derive makeBinary ''Filter)
$(derive makeBinary ''CommentData)
