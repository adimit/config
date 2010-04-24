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

import XMonad.Actions.TopicSpace
import qualified Data.Map as M
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.StackSet as W

-- | Default Font
monaco :: String
monaco = "xft:Monaco:size=8"

myTheme = defaultTheme { fontName = monaco }

-- | Browser executable as found in $PATH
browser :: String
browser = "chromium-browser"

mua :: String
mua = "evolution"

musicplayer :: String
musicplayer = "rhythmbox-client"

-- | Topic configuration
myTopics :: [Topic]
myTopics = [ "dash", "mail", "web", "src", "osem", "ord", "admin", "read", "music"
           , "vid", "wow", "w", "u" ]

-- Topic helpers
--
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvt -cd " ++ dir

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt promptConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt promptConfig $ windows . W.shift


myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("dash" , "~/Desktop")
        , ("mail" , "~/Downloads")
        , ("web"  , "~/Downloads")
        , ("read" , "~/Documents/lit")
        , ("src"  , "~/src")
        , ("osem" , "~/Documents/w/osem")
        , ("admin", "~/Documents/w/srv")
        , ("ord"  , "~/Documents/w/ord")
        , ("w"    , "~/Documents/w")
        , ("u"    , "~/Documents/u/")
        ]
    , defaultTopicAction = const $ spawnShell >*> 2
    , defaultTopic = "dash"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("mail" , spawn mua)
        , ("web"  , spawn browser)
        , ("music", spawn musicplayer)
        , ("read" , spawn "mendeleydesktop")
        , ("src"  , spawn "gvim" >> spawnShell >*> 2)
        , ("osem" , spawn "gvim" >> spawnShell >*> 2)
        , ("ord"  , spawn "gvim" >> spawnShell >*> 2)
        , ("vid"  , spawn "vlc")
        --, ("u"    , spawnShell >*> 3)
        -- , ("w"    , spawnShell >*> 3)
        ]
    }

layouts = ( workspaceDir "~"
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
        , font              = monaco
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
            [ ((mask .|. shiftMask, xK_f        ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m        ), sendMessage $ Toggle MIRROR)
            ]
            ++ -- Topic Space
            [ ((mask              , xK_k  ), promptedGoto)
            , ((mask .|. shiftMask, xK_k  ), promptedShift)
            , ((mask              , xK_a  ), currentTopicAction myTopicConfig)
            ]
            ++ -- Topic Space numbers
            [ ((mask, k), switchNthLastFocused myTopicConfig i)
            | (i,k) <- zip [1..] workspaceKeys]

workspaceKeys = [ xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0 ]

main = do sp <- mkSpawner
          checkTopicConfig myTopics myTopicConfig
          xmonad $ gnomeConfig
             { terminal   = "urxvt"
             , layoutHook = layouts
             , modMask    = mod4Mask
             , keys       = \c -> myKeys sp c `M.union` keys gnomeConfig c
             , manageHook = composeAll [manageHook gnomeConfig, manageSpawn sp]
             , logHook    = fadeInactiveLogHook 0.8
             , normalBorderColor  = "#000000"
             , focusedBorderColor = "#8899ff"
             , workspaces = myTopics
             }
