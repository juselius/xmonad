import System.Taffybar

import System.Taffybar.FreedesktopNotifications
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Battery
import System.Taffybar.NetMonitor
import System.Taffybar.Text.CPUMonitor
import System.Taffybar.Text.MemoryMonitor

main =
  defaultTaffybar
    defaultTaffybarConfig
      { startWidgets = [pager, note]
      , endWidgets = [clock, tray, batt, mem, cpu, net]
      }
  where
    clock = textClockNew Nothing
      "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
    pager = taffyPagerNew defaultPagerConfig
    note = notifyAreaNew defaultNotificationConfig
    batt = batteryBarNew defaultBatteryConfig 5.0
    mem = textMemoryMonitorNew "mem:$used$" 1.0
    cpu = textCpuMonitorNew "cpu:$total$" 0.5
    net = netMonitorNew 1.5 "eth0"
    tray = systrayNew
