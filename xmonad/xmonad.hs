import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myLayout = ( smartBorders $ avoidStruts  (resizableTile ||| Mirror resizableTile |||  Full ))
    where
    resizableTile = ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100

main = do
xmproc <- spawnPipe "xmobar ~/.xmobarrc"
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
    , layoutHook = myLayout
        , borderWidth = 2
        , focusedBorderColor = "#AC4142"
        , normalBorderColor = "#eee8d5"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock") --mod4mask is the windows key
    , ((mod1Mask .|. shiftMask, xK_e), spawnHere "emacsclient --no-wait --create-frame --alternate-editor='' --eval '(switch-to-buffer nil)'") -- %! Launch emacs
    , ((mod1Mask .|. shiftMask, xK_f), spawnHere "firefox") -- %! Launch Firefox
    , ((mod1Mask .|. shiftMask, xK_r), spawnHere "nautilus -w") -- %! Launch Nautilus
    , ((mod1Mask,               xK_z), sendMessage MirrorShrink)
    , ((mod1Mask,               xK_a), sendMessage MirrorExpand)
    , ((0, xK_Print), spawn "gnome-screenshot")
    ]

