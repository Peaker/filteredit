{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Editor.Filter(
    Filter(..),
    LabelData(..), labelTextEdit, labelBox, labelChild, labelFD,
    InverseData(..), inverseFD, inverseChild,
    ProcessData(..), processFD, processCompletion
) where

import           Data.Binary                           (Binary(..))
import           Data.Binary.Get                       (getWord8)
import           Data.Binary.Put                       (putWord8)
import           Data.DeriveTH                         (derive)
import           Data.Derive.Binary                    (makeBinary)
import           Data.Store.IRef                       (IRef)
import           Data.Record.Label                     ((:->), mkLabels, label)
import qualified Graphics.UI.VtyWidgets.TextEdit       as TextEdit
import qualified Graphics.UI.VtyWidgets.Completion     as Completion
import qualified Graphics.UI.VtyWidgets.Box            as Box
import qualified Graphics.UI.VtyWidgets.FocusDelegator as FocusDelegator

data Filter = Label (IRef LabelData)
            | Disable (IRef Filter)
            | Inverse (IRef InverseData)
            | Process (IRef ProcessData)
            | None
  deriving (Show, Read, Eq, Ord)

data ProcessData = ProcessData { _processFD :: FocusDelegator.Model,
                                 _processCompletion :: Completion.Model }

data LabelData = LabelData { _labelFD :: FocusDelegator.Model,
                             _labelTextEdit :: TextEdit.Model,
                             _labelBox :: Box.Model,
                             _labelChild :: Filter
                           }
  deriving (Show, Read, Eq, Ord)

data InverseData = InverseData { _inverseFD :: FocusDelegator.Model,
                                 _inverseChild :: Filter
                               }
  deriving (Show, Read, Eq, Ord)

$(mkLabels [''LabelData])
labelFD :: LabelData :-> FocusDelegator.Model
labelTextEdit :: LabelData :-> TextEdit.Model
labelBox :: LabelData :-> Box.Model
labelChild :: LabelData :-> Filter

$(mkLabels [''InverseData])
inverseFD :: InverseData :-> FocusDelegator.Model
inverseChild :: InverseData :-> Filter

$(mkLabels [''ProcessData])
processFD :: ProcessData :-> FocusDelegator.Model
processCompletion :: ProcessData :-> Completion.Model

$(derive makeBinary ''Filter)
$(derive makeBinary ''LabelData)
$(derive makeBinary ''InverseData)
$(derive makeBinary ''ProcessData)
