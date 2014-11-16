import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
	, borderWidth = 1
	, focusedBorderColor = "#d33682"
	, normalBorderColor = "#eee8d5"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock") --mod4mask is the windows key
    , ((0, xK_Print), spawn "gnome-screenshot")
    ]
