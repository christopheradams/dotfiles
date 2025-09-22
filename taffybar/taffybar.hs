module Main where

import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Text.CPUMonitor
import System.Taffybar.Widget.Text.MemoryMonitor
import System.Taffybar.Widget.Text.NetworkMonitor
import System.Taffybar.Widget.Workspaces

myClockConfig :: ClockConfig
myClockConfig = defaultClockConfig
  { clockFormatString = "%a %b %d %Y %H:%M %Z" } -- 24 hour clock

main :: IO ()
main = simpleTaffybar defaultSimpleTaffyConfig
       { startWidgets = [workspacesNew defaultWorkspacesConfig
                        , textCpuMonitorNew "| $total$%" 1.0
                        , textMemoryMonitorNew "| $used$" 1.0
                        , networkMonitorNew "| ▼ $inAuto$ ▲ $outAuto$" Nothing
                        ]
  , endWidgets = [ textClockNewWith myClockConfig]
  , barHeight    = ScreenRatio (1 / 40)  -- smaller than default 1/27
  }
