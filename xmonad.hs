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

import XMonad.Actions.Warp
import XMonad.Layout.PerWorkspace

import System.Info (os)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

layouts = smartBorders
        . desktopLayoutModifiers
        . mkToggle(NOBORDERS ?? FULL ?? EOT)
        . mkToggle(single MIRROR)
        . layoutHintsToCenter
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
            [ ((mask              , xK_g         ), warpToCentre >> goToSelected  myGSConfig)
            , ((mask .|. shiftMask, xK_g         ), spawnSelected myGSConfig myGSApps) ]
            ++ -- MultiToggle
            [ ((mask .|. shiftMask, xK_f          ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m          ), sendMessage $ Toggle MIRROR) ]

workspaceKeys = [ xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0 ]

warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig { gs_cellheight = 35
                             , gs_cellwidth  = 100
                             , gs_navigate = gsn }
    where gsn                 = M.unions [ reset, fpsKeys ]
          addPair (a,b) (x,y) = (a+x,b+y)
          fpsKeys = M.map addPair $ M.fromList [ ((0,xK_e), (0,1))
                                               , ((0,xK_o), (-1,0))
                                               , ((0,xK_u), (1,0))
                                               , ((0,xK_period), (0,-1)) ]
          reset = M.singleton (0,xK_space) (const (0,0))

myGSApps = ["opera", "miro", "eclipse", "gmpc", "gvim"]

detectModMask = if os == "darwin" then mod1Mask else mod4Mask

myConfig = gnomeConfig { terminal   = "urxvt"
                       , layoutHook = layouts
                       , modMask    = detectModMask
                       , keys       = \c -> myKeys c `M.union` keys gnomeConfig c
                       , manageHook = composeAll [ manageHook gnomeConfig
                                                 , manageSpawn
                                                 , isFullscreen --> doFullFloat ]
                       , normalBorderColor  = "#202020"
                       , focusedBorderColor = "#cae682" }

main = xmonad =<< xmobar myConfig
