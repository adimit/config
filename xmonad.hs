{-# LANGUAGE TypeSynonymInstances, ExtendedDefaultRules, Rank2Types #-}
import XMonad

import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome

import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces

import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect

import XMonad.Prompt
import XMonad.Prompt.Workspace

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

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

promptConfig = defaultXPConfig { position          = Top
                               , font              = myFont
                               , bgColor           = myHLBG
                               , fgColor           = myFG
                               , fgHLight          = myBG
                               , bgHLight          = myHL
                               , promptBorderWidth = 0 }

myFont     = "xft:Droid Sans Mono:size=8"
myBG       = "#202020"
myFG       = "#EEEEEE"
myHL       = "#cae682"
myHLBG     = "#363946"
myTerminal = "urxvt"

spawnShellIn dir = spawn $ myTerminal ++ " -cd \"" ++ escape dir ++ "\" || "
                        ++ "FAILED_CHDIR='"++escape dir++"' " ++myTerminal
                        -- ++ myTerminal
    where escape ('\'':xs) = "\\\"" ++ escape xs
          escape    (x:xs) = x:escape xs
          escape        [] = []

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig { gs_cellheight  = 25
                             , gs_cellwidth   = 100
                             -- , gs_navigate    = M.unions [ reset, fpsKeys ]
                             , gs_font        = myFont
                             , gs_cellpadding = 4 }
    where (a,b) <+> (x,y) = (a+x,b+y)
          reset   = M.singleton (0,xK_space) (const (0,0))
          fpsKeys = M.map (<+>) $ M.fromList
                                [ ((0,xK_e),      (0,1))
                                , ((0,xK_o),      (-1,0))
                                , ((0,xK_u),      (1,0))
                                , ((0,xK_period), (0,-1)) ]

myKeys conf@(XConfig { modMask = mask, workspaces = ws }) = M.fromList $
            [ ((mask,               xK_a        ), withFocused $ windows . W.sink)
            , ((mask .|. shiftMask, xK_Return   ), dwmpromote)
            , ((mask              , xK_Return   ), spawnShellIn "~")
            , ((mask,               xK_BackSpace), shellPromptHere promptConfig)
            , ((mask,               xK_grave    ), toggleWS) ]
            ++ -- GridSelect
            [ ((mask              , xK_g        ), goToSelected myGSConfig) ]
            ++ -- Dynamic Workspaces
            [ ((mask              , xK_t        ), selectWorkspace promptConfig)
            , ((mask .|. shiftMask, xK_BackSpace), removeWorkspace) ]
            ++ -- MultiToggle
            [ ((mask .|. shiftMask, xK_f        ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m        ), sendMessage $ Toggle MIRROR) ]
            ++ -- Grid for Workspaces
            [ ((mask              , xK_d        ), runSelectedAction myGSConfig (tgr W.greedyView))
            , ((mask .|. shiftMask, xK_d        ), runSelectedAction myGSConfig (tgr W.shift)) ]
            where prompt = workspacePrompt promptConfig
                  tgr f  = map (\t -> (t, windows $ f t)) myWS

myWS :: [String]
myWS = map show [1..9]

myConfig = gnomeConfig { terminal   = myTerminal
                       , layoutHook = layouts
                       , modMask    = if os == "darwin" then mod1Mask else mod4Mask
                       , keys       = \c -> myKeys c `M.union` keys gnomeConfig c
                       , workspaces = myWS
                       , manageHook = composeAll [ manageHook gnomeConfig
                                                 , manageSpawn
                                                 , className =? "Wine" --> doFloat
                                                 , isFullscreen --> doFullFloat ]
                       , normalBorderColor  = myBG
                       , focusedBorderColor = myHL
                       , startupHook = setWMName "LG3D" }

main = xmonad =<< xmobar myConfig
