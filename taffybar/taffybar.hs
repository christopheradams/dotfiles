module Main where

import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Workspaces

myClockConfig :: ClockConfig
myClockConfig = defaultClockConfig
  { clockFormatString = "%a %b %d %Y %H:%M" } -- 24 hour clock

main :: IO ()
main = simpleTaffybar defaultSimpleTaffyConfig
  { startWidgets = [workspacesNew defaultWorkspacesConfig]
  , endWidgets = [textClockNewWith myClockConfig]
  , barHeight    = ScreenRatio (1 / 40)  -- smaller than default 1/27
  }
