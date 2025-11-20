import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.Volume
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (ToggleStruts(..),avoidStruts,docks,manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Dwindle as D
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import System.Exit
import System.IO

-- scratchPads
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "gnome-clocks" "gnome-clocks" (className =? "org.gnome.clocks") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  , NS "gnome-control-center" "gnome-control-center network" (className =? "gnome-control-center") doCenterFloat
  , NS "gnome-system-monitor" "gnome-system-monitor" (className =? "gnome-system-monitor") (customFloating $ W.RationalRect (1/4) 0 (1/2) (4/5))
  , NS "localsend" "localsend" (className =? "Localsend_app") nonFloating
  , NS "keepassxc" "keepassxc" (className =? "KeePassXC") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  , NS "nextcloud" "nextcloud-desktop-client" (className =? "Nextcloud") doCenterFloat
  , NS "gnome-calculator" "gnome-calculator" (className =? "gnome-calculator") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  , NS "terminal" "gnome-terminal --class Gnome-terminal-scratch" (className =? "Gnome-terminal-scratch") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  ]

myLayout = ( smartBorders $ avoidStruts  (resizableTile ||| dwindleTile ||| threeColMid ||| mirrorResizableTile ||| Full ))
    where
    resizableTile = ResizableTall 1 (3/100) (1/2) []
    dwindleTile = D.Dwindle D.R D.CW 1.5 1.1
    mirrorResizableTile = Mirror resizableTile
    threeColMid = ThreeColMid 1 (3/100) (1/2)

myManageHooks = composeAll
  [ className =? "Gnome-calculator" --> doFloat
  , className =? "Gpick" --> (customFloating $ W.RationalRect (3/8) (3/8) (1/4) (1/4))
  , className =? "Screenruler.rb" --> doFloat
  , isDialog --> doCenterFloat
  , isFullscreen --> doFullFloat
  ]

myXmobarPP :: PP
myXmobarPP = def
-- TODO customize config

main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . pagerHints
       $ docks gnomeConfig
    { manageHook = myManageHooks <+> namedScratchpadManageHook scratchpads <+> manageDocks
    , terminal = "gnome-terminal"
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
    , ((mod1Mask .|. shiftMask, xK_p), namedScratchpadAction scratchpads "keepassxc")
    , ((mod1Mask .|. shiftMask, xK_d), namedScratchpadAction scratchpads "gnome-calculator")
    , ((mod1Mask .|. shiftMask, xK_t), spawnHere "thunderbird") -- %! Launch Thunderbird
    , ((mod1Mask .|. shiftMask, xK_w), namedScratchpadAction scratchpads "gnome-clocks")
    , ((mod1Mask .|. shiftMask, xK_u), namedScratchpadAction scratchpads "gnome-control-center")
    , ((mod1Mask .|. shiftMask, xK_i), namedScratchpadAction scratchpads "gnome-system-monitor")
    , ((mod1Mask .|. shiftMask, xK_l), namedScratchpadAction scratchpads "localsend")
    , ((mod1Mask .|. shiftMask, xK_n), namedScratchpadAction scratchpads "nextcloud")
    , ((mod1Mask .|. shiftMask, xK_m), namedScratchpadAction scratchpads "terminal")
    , ((mod1Mask .|. shiftMask, xK_q), spawn "gnome-session-quit")
    , ((mod1Mask,               xK_p), spawn "rofi -show combi")
    , ((mod1Mask,               xK_z), sendMessage MirrorShrink)
    , ((mod1Mask,               xK_a), sendMessage MirrorExpand)
    , ((0, xK_F11), lowerVolume 5 >> return ())
    , ((0, xK_F12), raiseVolume 5 >> return ())
    , ((0, xK_Print), spawn "gnome-screenshot")
    ]

