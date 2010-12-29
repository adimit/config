import XMonad
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome
import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Reflect
import XMonad.Hooks.DynamicLog

import qualified Data.Map as M
import qualified XMonad.StackSet as W

layouts = layoutHints
        . mkToggle(NOBORDERS ?? FULL ?? EOT)
        . mkToggle(single MIRROR)
        . desktopLayoutModifiers
        $ tiled ||| reflectHoriz (simpleTabbed *|* Full)
    where tiled = XMonad.Tall 1 (3/100) (1/2)

promptConfig = defaultXPConfig
        { position          = Top
        , font              = "xft:Droid Sans Mono:size=8"
        , promptBorderWidth = 0 }

myKeys conf@(XConfig { modMask = mask, workspaces = ws }) = M.fromList $
            [ ((mask,               xK_a         ), withFocused $ windows . W.sink)
            , ((mask .|. shiftMask, xK_Return    ), dwmpromote)
            , ((mask,               xK_BackSpace ), shellPromptHere promptConfig)
            , ((mask,               xK_Return    ), spawnHere $ XMonad.terminal conf)
            , ((mask,               xK_grave     ), toggleWS) ]
            ++ -- GridSelect
            [ ((mask              , xK_g         ), (goToSelected defaultGSConfig)) ]
            ++ -- MultiToggle
            [ ((mask .|. shiftMask, xK_f          ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m          ), sendMessage $ Toggle MIRROR) ]

workspaceKeys = [ xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0 ]

myConfig = gnomeConfig { terminal   = "urxvt"
                       , layoutHook = smartBorders layouts
                       , modMask    = mod4Mask
                       , keys       = \c -> myKeys c `M.union` keys gnomeConfig c
                       , manageHook = composeAll [ manageHook gnomeConfig
                                                 , manageSpawn
                                                 , isFullscreen --> doFullFloat ]
                       , normalBorderColor  = "#000000"
                       , focusedBorderColor = "#8899ff" }

main = xmonad =<< xmobar myConfig
