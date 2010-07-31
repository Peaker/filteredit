{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(quitKey, undoKey, makeBranchKey, delBranchKey)
where

import qualified Graphics.Vty as Vty

ctrl k = ([Vty.MCtrl], Vty.KASCII k)
quitKey       = ctrl 'q'
undoKey       = ctrl 'z'
makeBranchKey = ctrl 's'
delBranchKey  = ctrl 'o'
