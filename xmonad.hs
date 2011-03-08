{-# LANGUAGE TypeSynonymInstances, ExtendedDefaultRules, Rank2Types #-}
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
import XMonad.Actions.TopicSpace
import XMonad.Hooks.SetWMName

import XMonad.Layout.PerWorkspace
import XMonad.Prompt.Workspace

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
myGSApps   = ["opera", "miro", "eclipse", "gmpc", "gvim"]

spawnShell       = currentTopicDir myTopicConfig >>= spawnShellIn
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
            , ((mask,               xK_BackSpace), shellPromptHere promptConfig)
            , ((mask,               xK_grave    ), toggleWS) ]
            ++ -- GridSelect
            [ ((mask              , xK_g        ), goToSelected myGSConfig)
            , ((mask .|. shiftMask, xK_g        ), spawnSelected myGSConfig myGSApps) ]
            ++ -- MultiToggle
            [ ((mask .|. shiftMask, xK_f        ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m        ), sendMessage $ Toggle MIRROR) ]
            ++ -- TopicSpace
            [ ((mask              , xK_t        ), prompt (switchTopic myTopicConfig))
            , ((mask .|. shiftMask, xK_t        ), prompt $ windows . W.shift)
            , ((mask              , xK_Return   ), spawnShell) ]
            ++ -- Topic + Grid
            [ ((mask              , xK_d        ), runSelectedAction myGSConfig (tgr W.greedyView))
            , ((mask .|. shiftMask, xK_d        ), runSelectedAction myGSConfig (tgr W.shift)) ]
            where prompt = workspacePrompt promptConfig
                  tgr f  = map (\t -> (t, windows $ f t)) myTopics


myTopics :: [Topic]
myTopics = [ "dash", "web", "comm", "werti", "eclipse", "logs", "test", "admin", "music"
           , "podc", "read", "yi", "vid", "play", "xmonad", "documents", "torrent", "conf"
           , "src", "picasa", "srv" ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic       = "dash"
    , maxTopicHistory    = 10
    , topicActions = M.fromList [ ("conf"    , spawnShell >> spawnHere ed)
                                , ("read"    , (spawn $ ed ++ " " ++ doc ++ "lib/pdf.bib") >> spawnShell)
                                , ("dash"    , (spawn $ ed ++ " ~/doc/org/main.org") >> spawnShell)
                                , ("werti"   , spawn ed >> spawnShell) ]
    , topicDirs    = M.fromList [ ("conf"    , "$HOME/config")
                                , ("doc"     , doc)
                                , ("read"    , doc ++ "lib/pdf")
                                , ("eclipse" , src ++ "workspace")
                                , ("yi"      , src ++ "yi")
                                , ("werti"   , src ++ "werti")
                                , ("xmonad"  , "$HOME/.xmonad")
                                , ("srv"     , "$HOME/local/apache-tomcat-6.0.30")
                                , ("src"     , src) ] }
    where (src,doc,ed,web) = ("${HOME}/src/","${HOME}/doc/","gvim","opera")

myConfig = gnomeConfig { terminal   = myTerminal
                       , layoutHook = layouts
                       , modMask    = if os == "darwin" then mod1Mask else mod4Mask
                       , keys       = \c -> myKeys c `M.union` keys gnomeConfig c
                       , workspaces = myTopics
                       , manageHook = composeAll [ manageHook gnomeConfig
                                                 , manageSpawn
                                                 , isFullscreen --> doFullFloat ]
                       , normalBorderColor  = myBG
                       , focusedBorderColor = myHL
                       , startupHook = setWMName "LG3D" }

main = xmonad =<< xmobar myConfig
