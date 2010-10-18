import Data.Ratio ((%))

import XMonad
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome
import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Grid
import XMonad.Layout.Accordion
import XMonad.Layout.AutoMaster
import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace

import qualified Data.Map as M
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.StackSet as W

-- | Default Font
monospace :: String
monospace = "xft:Monospace:size=8"

myTheme = defaultTheme { fontName = monospace }

-- | Browser executable as found in $PATH
browser :: String
browser = "chromium-browser"

layouts = layoutHints
        . smartBorders
        . mkToggle(NOBORDERS ?? FULL ?? EOT)
        . mkToggle(single MIRROR)
        . desktopLayoutModifiers
        $ tiled
    where tiled = XMonad.Tall nm delta ratio
          nm    = 1
          ratio = 1/2
          delta = 3/100


layouts'= ( workspaceDir "~"
          . smartBorders
          . mkToggle(NOBORDERS ?? FULL ?? EOT)
          . mkToggle(single MIRROR)
          . dwmStyle shrinkText myTheme
          . desktopLayoutModifiers
          $ layoutHints (autoMaster 1 (3/100) Grid) ||| layoutHints Accordion ||| layoutHints tiled
          ) where tiled   = XMonad.Tall nmaster delta ratio
                  nmaster = 1
                  ratio   = 1/2
                  delta   = 3/100
                  magnify = Mag.magnifiercz (12%10)

promptConfig = defaultXPConfig
        { position          = Bottom
        , font              = "xft:Dejavu Sans Mono:size=8"
        , promptBorderWidth = 0 }

myKeys sp conf@(XConfig { modMask = mask, workspaces = ws }) = M.fromList $
            [ ((mask,               xK_a         ), withFocused $ windows . W.sink)
            , ((mask .|. shiftMask, xK_Return    ), dwmpromote)
            , ((mask,               xK_BackSpace ), shellPromptHere sp promptConfig)
            , ((mask,               xK_Return    ), spawnHere sp $ XMonad.terminal conf)
            , ((mask,               xK_grave     ), toggleWS) ]
            ++ -- GridSelect
            [ ((mask              , xK_g         ), (goToSelected defaultGSConfig)) ]
            ++ -- MultiToggle
            [ ((mask .|. shiftMask, xK_f          ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m          ), sendMessage $ Toggle MIRROR) ]

workspaceKeys = [ xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0 ]

main = do sp <- mkSpawner
          xmonad $ gnomeConfig
             { terminal   = "urxvt"
             , layoutHook = layouts
             , modMask    = mod4Mask
             , keys       = \c -> myKeys sp c `M.union` keys gnomeConfig c
             , manageHook = composeAll [manageHook gnomeConfig, manageSpawn sp]
             , logHook    = fadeInactiveLogHook 0.8
             , normalBorderColor  = "#000000"
             , focusedBorderColor = "#8899ff"
             }

