module Main where

import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Workspaces

main :: IO ()
main = simpleTaffybar defaultSimpleTaffyConfig
  { startWidgets = [workspacesNew defaultWorkspacesConfig]
  , endWidgets = [textClockNewWith defaultClockConfig]
  }
