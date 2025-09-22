module Main where

import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Workspaces

main :: IO ()
main = simpleTaffybar defaultSimpleTaffyConfig
  { startWidgets = [workspacesNew defaultWorkspacesConfig]
  , endWidgets = [textClockNewWith defaultClockConfig]
  , barHeight    = ScreenRatio (1 / 40)  -- smaller than default 1/27
  }
