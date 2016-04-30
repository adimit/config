{-# LANGUAGE TypeSynonymInstances, ExtendedDefaultRules, Rank2Types #-}
import XMonad

import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome

import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D

import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile

import XMonad.Prompt

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import Data.List (isInfixOf,isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Util.Run

import Control.Applicative (liftA2)
import Control.Concurrent
import Data.Time.Clock
import System.IO (hFlush,Handle)
import Control.Monad (liftM)
import Data.Time.Format
import Data.Time.LocalTime

import XMonad.Layout.BinarySpacePartition

-- import qualified XMonad.Actions.Drag as Drag

statusUpdate :: Handle -> TimeZone -> IO ()
statusUpdate h tz = do
    t <- liftM (utcToLocalTime tz) getCurrentTime
    let date = formatTime defaultTimeLocale "%b %d" t
        hour = formatTime defaultTimeLocale "%R" t
        str = wrap (fg hilight) fg' hour ++ " | " ++ wrap (fg offlight) fg' date
    hPutStr h str
    hPutStr h "\n" >> hFlush h
    threadDelay 1000000

layouts = smartBorders
        . desktopLayoutModifiers
        . mkToggle(NOBORDERS ?? FULL ?? EOT)
        . mkToggle(single MIRROR)
        $ emptyBSP ||| tabbed shrinkText myTabbedTheme ||| layoutHintsToCenter resizableTiled
    where resizableTiled = smartSpacing 5 $ ResizableTall 1 (3/100) (1/2) []
          myTabbedTheme = def { fontName = "xft:Dejavu Sans Mono:size=8"
                              , decoHeight = 15 }

promptConfig :: XPConfig
promptConfig = def { position          = Top
                   , font              = myFont
                   , bgColor           = myHLBG
                   , fgColor           = myFG
                   , fgHLight          = myBG
                   , bgHLight          = myHL
                   , promptKeymap      = defaultXPKeymap' wordSep
                   , promptBorderWidth = 0 }

wordSep :: Char -> Bool
wordSep c = isSpace c || c == '/'

myFont, myBG, myFG, myHL, myHLBG, myTerminal :: String
myFont     = "xft:Droid Sans Mono:size=8"
myBG       = "#202020"
myFG       = "#EEEEEE"
myHL       = "#cae682"
myHLBG     = "#363946"
myTerminal = "/usr/bin/urxvt"

newTmuxIn :: (MonadIO m) => String -> m ()
newTmuxIn dir = spawn $ "cd " ++ dir ++ ";" ++ myTerminal ++ " -e tmux"

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = def { gs_cellheight  = 25
                 , gs_cellwidth   = 100
              -- , gs_navigate    = M.unions [ reset, fpsKeys ]
                 , gs_font        = myFont
                 , gs_cellpadding = 4 }

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig { modMask = mask } = M.fromList $
            [ ((mask,                 xK_a        ), withFocused $ windows . W.sink)
            , ((mask .|. shiftMask,   xK_Return   ), dwmpromote)
            , ((mask,                 xK_Return   ), spawn myTerminal)
            , ((mask .|. controlMask, xK_Return   ), newTmuxIn "$HOME")
            , ((mask,                 xK_BackSpace), shellPromptHere promptConfig)
            , ((mask,                 xK_grave    ), toggleWS) ]
            ++ -- GridSelect
            [ ((mask              , xK_g        ), goToSelected myGSConfig) ]
            ++ -- Dynamic Workspaces
            [ ((mask              , xK_d        ), selectWorkspace promptConfig)
            , ((mask .|. shiftMask, xK_d        ), withWorkspace promptConfig (windows . W.shift))
            , ((mask .|. shiftMask, xK_BackSpace), removeWorkspace) ]
            ++ -- ResizableTile
            [ ((mask              , xK_v        ), sendMessage MirrorShrink)
            , ((mask              , xK_z        ), sendMessage MirrorExpand) ]
            ++ -- MultiToggle
            [ ((mask .|. shiftMask, xK_f        ), sendMessage $ Toggle FULL)
            , ((mask .|. shiftMask, xK_m        ), sendMessage $ Toggle MIRROR) ]
            ++ -- Navigation2D
            [ ((mask              , xK_h        ), windowGo L True)
            , ((mask              , xK_s        ), windowGo R True)
            , ((mask              , xK_n        ), windowGo U True)
            , ((mask              , xK_t        ), windowGo D True) ]
            ++ -- Resize with BinarySpacePartition
            [ ((mask .|. shiftMask, xK_s        ), MoveSplit R `bsplitOr` Expand)
            , ((mask .|. shiftMask, xK_h        ), MoveSplit L `bsplitOr` Shrink)
            , ((mask .|. shiftMask, xK_n        ), MoveSplit U `bsplitOr` MirrorShrink)
            , ((mask .|. shiftMask, xK_t        ), MoveSplit D `bsplitOr` MirrorExpand)
            , ((mask .|. shiftMask, xK_r        ), sendMessage Swap)
            , ((mask              , xK_r        ), sendMessage Rotate) ]
--            ++ -- Drag and Drop
--            [ ((mask              , xK_w        ), Drag.dragOrDrop Drag.Swap)
--            , ((mask .|. shiftMask, xK_w        ), Drag.dragOrDrop Drag.Drop) ]
            ++ -- MPD control
            [ ((mask,               xK_F11      ), spawn "mpc toggle")
            , ((mask .|. shiftMask, xK_F11      ), spawn "mpc crop")
            , ((mask,               xK_F10      ), spawn "mpc prev")
            , ((mask,               xK_F12      ), spawn "mpc next") ]

-- If we're currently in the BinarySpacePartition layout, send the left message,
-- else send the right message.
bsplitOr :: (Message a, Message b) => a -> b -> X ()
bsplitOr m1 m2 = do
  currentLayout <- liftM (description . getCurrentLayout)  get
  if "BSP" `isPrefixOf` currentLayout
     then sendMessage m1 else sendMessage m2

getCurrentLayout :: XState -> Layout Window
getCurrentLayout = W.layout . W.workspace . W.current . windowset

myWS :: [String]
myWS = map show [1..9]

myConfig = ewmh $ xfceConfig
   { terminal   = myTerminal
   , layoutHook = layouts
   , modMask    = mod4Mask
   , keys       = \c -> myKeys c `M.union` keys gnomeConfig c
   , workspaces = myWS
   , manageHook = composeAll [ manageHook gnomeConfig
                             , manageSpawn
                             , className =? "Conky" --> doIgnore
                             , className =? "Wine" --> doFloat
                             , isFullscreen --> doFullFloat ]
   , normalBorderColor  = myBG
   , focusedBorderColor = myHL
   , borderWidth = 2
   , handleEventHook = fullscreenEventHook
   , startupHook = ewmhDesktopsStartup }

myStatusBar d w x = unwords
    [ "/home/aleks/local/dzen/bin/dzen2"
    , "-xs", "1" -- xinerama screen 1
    , "-fn", "\"Ubuntu Mono for Powerline:size=10\""
    , "-ta", d
    , "-sa", d
    , "-fg", "'" ++ fontcol ++ "'"
    , "-bg", "'" ++ black ++ "'"
    , "-w", w
    , "-x", x
    , "-dock"
    ]

{- Powerline font escape codes:
 - solid left \11138
 - solid right \11136
 - hollow left \11139
 - hollow right \11137
 -}

icon x   = "^i(/home/aleks/.xmonad/icons/"++x++".xbm)"
fg c     = "^fg("++c++")"
fg'      = "^fg()"
bg c     = "^bg("++c++")"
bg'      = "^bg()"
fontcol  = "#A0A0A0"
shadecol = "#505050"
hilight  = "#afd700"
offlight = "#ffaf00"
black    = "#101010"

myDzenPP_ h = def
    { ppCurrent = wrap (fg hilight ++ icon "tableft" ++ fg black ++ bg hilight)
                       (bg' ++ fg hilight ++ icon "tabright" ++ fg')
    , ppVisible = wrap (fg offlight ++ icon "tableft"++ fg black ++ bg offlight)
                       (bg' ++ fg offlight ++ icon "tabright"++ fg')
    , ppHidden  = wrap (fg shadecol ++ icon "tableft"++ fg black ++ bg shadecol)
                       (bg' ++ fg shadecol ++ icon "tabright"++ fg')
    , ppSep = " "
    , ppWsSep = ""
    , ppTitle = wrap (' ' : fg hilight) (' ' : fg' ++ bg')
    , ppLayout = wrap (icon "tableft" ++ fg black ++ bg fontcol)
                      (bg' ++ fg' ++ icon "tabright") . iconMap
    , ppOutput = hPutStrLn h }

iconMap :: String -> String
iconMap l = case mapMaybe (\(s,i) -> if s l then Just i else Nothing) icons of
                [] -> l
                (x:_) -> x
        where icons :: [(String -> Bool,String)]
              icons = [ (isInfixOf "Mirror" &*& isInfixOf "ResizableTall", icon "mirr-res-tall")
                      , (isInfixOf "ResizableTall", icon "res-tall")
                      , (isInfixOf "Full", icon "full")
                      , (isInfixOf "Tabbed Simplest", icon "full-tabbed") ]

(&*&) = liftA2 (&&)

main :: IO ()
main =
    -- dzenPipe <- spawnPipe $ myStatusBar "l" "1080" "0"
    xmonad . withNavigation2DConfig def $
        myConfig { logHook = ewmhDesktopsLogHook }
