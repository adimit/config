{-# LANGUAGE OverloadedStrings #-}

import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar (startTaffybar)

main :: IO ()
main =
  let
    myWorkspacesConfig = defaultWorkspacesConfig { minIcons = 1, widgetGap = 0, showWorkspaceFn = hideEmpty }
    workspaces = workspacesNew myWorkspacesConfig
    myConfig = toTaffyConfig $ defaultSimpleTaffyConfig
       { startWidgets = [ workspaces ]
       , centerWidgets = [ textClockNewWith defaultClockConfig { clockFormatString = "<b>%R</b>  %e %b <small>W %V</small>" } ]
       , cssPath = Just "/home/aleks/config/xmonad/taffybar.css"
       , endWidgets = [ sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt, textBatteryNew "$percentage$ $time$", batteryIconNew ]
       , barHeight = 32 }
  in startTaffybar myConfig
