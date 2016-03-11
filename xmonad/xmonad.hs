import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , logHook = dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppCurrent = xmobarColor "#6A9FB5" "" . wrap "[" "]"
        , ppHidden = xmobarColor "#505050" ""
        , ppHiddenNoWindows = xmobarColor "#AAAAAA" ""
        , ppUrgent = xmobarColor "#CCCCCC" ""
        , ppTitle = xmobarColor "#505050" "" . shorten 7
        }
    , layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig
	, borderWidth = 1
	, focusedBorderColor = "#AC4142"
	, normalBorderColor = "#eee8d5"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock") --mod4mask is the windows key
    , ((mod1Mask .|. shiftMask, xK_e), spawnHere "emacsclient --no-wait --create-frame --alternate-editor=''") -- %! Launch emacs
    , ((0, xK_Print), spawn "gnome-screenshot")
    ]

