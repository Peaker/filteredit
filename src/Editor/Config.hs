{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    quitKeys, undoKeys, makeBranchKeys, delBranchKeys,
    enableKeys, disableKeys,
    addLabelKeys, unlabelKeys,
    inverseKeys, processKeys,
    disableBackgroundColor, disableFocusedBackgroundColor)
where

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap

-- Word around Vty's misrecognition of Meta-;
altSemicolon :: Keymap.KeyGroup
altSemicolon = ("Meta-;",
                [([], Vty.KASCII '\187') -- xterm
                ,([Vty.MMeta], Vty.KASCII ';') -- rxvt
                ])

altThree = ("Alt-3",
            [([], Vty.KASCII '\179') -- xterm
            ,([Vty.MMeta], Vty.KASCII '3') -- rxvt
            ])

group mods k = Keymap.simpletonGroup (mods, k)
ascii k = Keymap.simpletonGroup ([], (Vty.KASCII k))
ctrl k = group [Vty.MCtrl] $ Vty.KASCII k

quitKeys       = [ctrl 'q']
undoKeys       = [ctrl 'z']
makeBranchKeys = [ctrl 's']
delBranchKeys  = [ctrl 'o']
addLabelKeys   = [altSemicolon, ascii ';']
unlabelKeys    = [altSemicolon, ascii ';']
disableKeys    = [altThree]
enableKeys     = [altThree]
inverseKeys    = [ctrl 'n', ascii 'n']
processKeys    = [ctrl 'p']

disableBackgroundColor = Vty.red
disableFocusedBackgroundColor = Vty.magenta
