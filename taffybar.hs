{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Taffybar
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.Text.NetworkMonitor
import System.Taffybar.Widget.Text.MemoryMonitor


transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = transparent
  }

netCfg = myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just "net"
  }

memCfg = myGraphConfig
  { graphDataColors = [taffyBlue]
  , graphLabel = Just "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [green1, green2]
  , graphLabel = Just "cpu"
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 0
        , maxIcons = Just 1
        , widgetGap = 0
        , underlineHeight = 3
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      -- mem = textMemoryMonitorNew "mem:$used$" 1.0
      -- net = networkMonitorNew "eno2" Nothing
      mem = pollingGraphNew memCfg 1 memCallback
      net = networkGraphNew netCfg Nothing
      clock = textClockNew Nothing "%a %b %_d %r" 1
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
        -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
        -- for a better way to set up the sni tray
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
            workspaces : map (>>= buildContentsBox) [ layout ]
        , endWidgets = map (>>= buildContentsBox)
          [ clock
          -- , batteryIconNew
          , cpu
          , mem
          , net
          , tray
          -- , mpris2New
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = 22
        , widgetSpacing = 0
        , monitorsAction = usePrimaryMonitor
        }
  dyreTaffybar $ withLogServer $ withToggleServer $ toTaffyConfig myConfig
  -- dyreTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
  --   toTaffyConfig myConfig
--
-- main =
--   defaultTaffybar
--     defaultTaffybarConfig
--       { startWidgets = [pager, note]
--       , endWidgets = [clock, tray, batt, mem, cpu, net]
--       }
--   where
--     clock = textClockNew Nothing
--       "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
--     pager = taffyPagerNew defaultPagerConfig
--     note = notifyAreaNew defaultNotificationConfig
--     batt = batteryBarNew defaultBatteryConfig 5.0
--     mem = textMemoryMonitorNew "mem:$used$" 1.0
--     cpu = textCpuMonitorNew "cpu:$total$" 0.5
--     net = netMonitorNew 1.5 "eth0"
--     tray = systrayNew

