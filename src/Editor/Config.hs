{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(quitKeys, undoKeys, makeBranchKeys, delBranchKeys,
                     addCommentKeys, delCommentKeys)
where

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.Keymap as Keymap

-- Word around Vty's misrecognition of Meta-;
altSemicolon :: Keymap.KeyGroup
altSemicolon = ("Meta-;",
                [([], Vty.KASCII '\187') -- xterm
                ,([Vty.MMeta], Vty.KASCII ';') -- rxvt
                ])

-- ascii k = Keymap.simpletonGroup ([], (Vty.KASCII k))
ctrl k = Keymap.simpletonGroup ([Vty.MCtrl], Vty.KASCII k)

quitKeys       = ctrl 'q'
undoKeys       = ctrl 'z'
makeBranchKeys = ctrl 's'
delBranchKeys  = ctrl 'o'
addCommentKeys = altSemicolon
delCommentKeys = ctrl 'o'
