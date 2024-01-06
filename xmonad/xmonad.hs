import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.ManageDocks (ToggleStruts(..),avoidStruts,docks,manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Layout.NoBorders
import XMonad.Util.Dmenu
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.StackSet as W
import System.Exit
import System.IO
import Control.Monad

-- scratchPads
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "1password" "1password" (className =? "1Password") doCenterFloat
  , NS "gnome-clocks" "gnome-clocks" (className =? "Org.gnome.clocks") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  , NS "gnome-control-center" "gnome-control-center network" (className =? "Gnome-control-center") doCenterFloat
  , NS "terminal" "gnome-terminal --class Gnome-terminal-scratch" (className =? "Gnome-terminal-scratch") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  , NS "qalculate" "qalculate" (className =? "Qalculate-gtk") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  , NS "nextcloud" "nextcloud-desktop-client.nextcloud" (className =? "Nextcloud") doCenterFloat
  ]

myLayout = ( smartBorders $ avoidStruts  (resizableTile ||| threeColMid ||| mirrorResizableTile ||| Full ))
    where
    resizableTile = ResizableTall 1 (3/100) (1/2) []
    mirrorResizableTile = Mirror resizableTile
    threeColMid = ThreeColMid 1 (3/100) (1/2)

myManageHooks = composeAll
  [ className =? "Gnome-calculator" --> doFloat
  , className =? "Gnome-system-monitor" --> doCenterFloat
  , className =? "kruler" --> doFloat
  , isFullscreen --> doFullFloat
  ]

main = do
xmproc <- spawnPipe "xmobar ~/.xmobarrc"
xmonad $ docks gnomeConfig
    { manageHook = myManageHooks <+> namedScratchpadManageHook scratchpads <+> manageDocks <+> manageHook defaultConfig
    , terminal = "gnome-terminal"
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
        , focusedBorderColor = "#E7766B"
        , normalBorderColor = "#E7E8EB"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock") --mod4mask is the windows key
    , ((mod1Mask .|. shiftMask, xK_e), spawnHere "emacsclient --no-wait --create-frame --alternate-editor='' --eval '(switch-to-buffer nil)'") -- %! Launch emacs
    , ((mod1Mask .|. shiftMask, xK_f), spawnHere "firefox") -- %! Launch Firefox
    , ((mod1Mask .|. shiftMask, xK_g), spawnHere "google-chrome") -- %! Launch Chrome
    , ((mod1Mask .|. shiftMask, xK_b), spawnHere "brave-browser") -- %! Launch Brave
    , ((mod1Mask .|. shiftMask, xK_r), spawnHere "nautilus -w") -- %! Launch Nautilus
    , ((mod1Mask .|. shiftMask, xK_p), namedScratchpadAction scratchpads "1password")
    , ((mod1Mask .|. shiftMask, xK_d), namedScratchpadAction scratchpads "qalculate")
    , ((mod1Mask .|. shiftMask, xK_t), spawnHere "thunderbird") -- %! Launch Thunderbird
    , ((mod1Mask .|. shiftMask, xK_w), namedScratchpadAction scratchpads "gnome-clocks")
    , ((mod1Mask .|. shiftMask, xK_u), namedScratchpadAction scratchpads "gnome-control-center")
    , ((mod1Mask .|. shiftMask, xK_i), spawnHere "gnome-system-monitor")
    , ((mod1Mask .|. shiftMask, xK_n), namedScratchpadAction scratchpads "nextcloud")
    , ((mod1Mask .|. shiftMask, xK_m), namedScratchpadAction scratchpads "terminal")
    , ((mod1Mask .|. shiftMask, xK_q), spawn "gnome-session-quit")
    , ((mod1Mask,               xK_p), spawn "dmenu_run -fn 'Input Bold-8' -nf 'white' -nb '#252525' -sf 'white' -sb '#DB2D20'")
    , ((mod1Mask,               xK_z), sendMessage MirrorShrink)
    , ((mod1Mask,               xK_a), sendMessage MirrorExpand)
    , ((0, xK_Print), spawn "gnome-screenshot")
    ]

