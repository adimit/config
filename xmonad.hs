{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
-- 
-- XMonad.hs
-- 
-- Author    : Aleksandar Dimitrov <aleks.dimitrov@gmail.com>
-- Created   : Fri Nov 9 2007
-- Rewritten : Sun Jan 18 2009

import Data.Ratio ((%))
import System.IO

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Config (defaultConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Monitor
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M
import qualified XMonad.Actions.DynamicWorkspaces as DWS
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.StackSet as W

-- | Miscellaneous Definitions
ctrlMask :: KeyMask
ctrlMask = controlMask

-- | Always print a certain number of characters, adding '...' when it's too long and spaces
--   to pad it when it's too short
staticString :: Int -> String -> String
staticString l s | length s <=  l = s ++ replicate (l - length s) ' '
                 | otherwise     = (take (l - length end) s) ++ end
                 where end = "..."

-- | Workspaces
myWorkspaces :: [String]
myWorkspaces = [ "code", "code'", "comm", "web",  "doc" 
             , "rec", "work", "write", "read", "music", "misc" ]

-- | Workspace Color Map
wsCols :: M.Map WorkspaceId [Char]
wsCols = M.fromList $
      zip myWorkspaces [ "#aaccee" , "#eebbaa" , "#aa99ee" , "#bb99aa" , "#bbdd99"
      , "#edcd88" , "#ee8844" , "grey90"  , "#ee9988" , "#aaeebb" , "#888888" ]

-- | Layout 
myLayout = workspaceDir "~" $ ewmhDesktopsLayout $ 
         avoidStruts ( id
                     . smartBorders
                     . mkToggle(NOBORDERS ?? FULL ?? EOT)
                     . mkToggle(single MIRROR)
                     . ModifiedLayout clock
                     $ tiled ||| magnify tiled
                     ) where tiled   = XMonad.Tall nmaster delta ratio
                             nmaster = 1
                             ratio   = 1/2
                             delta   = 3/100
                             magnify = Mag.magnifiercz (12%10)

-- | Colors
backgroundColor      = "#0a0c0f"
highlightColor       = "#b09a90"

myBitmapsDir         = "/home/adimit/etc/dzen2"

-- | Search Engines.
lastfm     = searchEngine "last.fm"        "http://www.last.fm/music/"
piratebay  = searchEngine "The Pirate Bay" "http://thepiratebay.org/s/?audio=on&page=0&orderby=7&q="
piratebay' = searchEngine "The Pirate Bay" "http://thepiratebay.org/s/?&page=0&orderby=7&q="
leo        = searchEngine "dict.leo.org"   "http://dict.leo.org/ende?lp=ende&lang=en&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&relink=on&search="

-- | Keymaps for Search Engines
searchEngineMap method = M.fromList $
        [ ((0, xK_g), method google)
        , ((0, xK_w), method wikipedia)
        , ((0, xK_s), method scholar)
        , ((0, xK_y), method youtube)
        , ((0, xK_m), method maps)
        , ((0, xK_p), method piratebay)
        , ((0, xK_l), method lastfm)
        , ((0, xK_d), method leo)
        , ((shiftMask, xK_p), method piratebay')
        ]

-- modmasks
msModMask = mod4Mask -- use windows key

-- | Monitor Layout:
clock = monitor
        { prop = ClassName "Cairo-clock" `And` Title "MacSlow's Cairo-Clock"
        , rect = Rectangle (1400-155) (10) 150 150
        , persistent = True
        , opacity = 0x77777777
        , visible = True -- Hide on start if false
        , name = "clock"
        }

myManageHook = composeAll 
    [ className =? "MPlayer"        --> doFloat
    , className =? "Navigator"      --> doF (W.shift "web")
    , className =? "Eclipse"        --> doF (W.shift "code'")
    , className =? "Opera"          --> doF (W.shift "doc")
    , className =? "Gimp"           --> doFloat
    , className =? "Gimp"           --> doF (W.shift "gimp")
    -- , className =? "Opera"          --> doF (\w -> setOpacity w 50)
    , className =? "Conky"          --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
    <+> manageDocks <+> manageMonitor clock

-- LoogHook: Fade hook
fadeHook :: X ()
fadeHook = fadeInactiveLogHook 0xdddddddd

-- | Default Font
monaco :: String
monaco = "xft:Monaco:size=8"

-- | Prompt Configuration
promptConfig = defaultXPConfig
               { position            = Bottom
               , font                = monaco
               , bgColor             = backgroundColor
               , fgColor             = "white"
               , bgHLight            = highlightColor 
               , fgHLight            = "black"
               , historyFilter       = deleteConsecutive
               , promptBorderWidth   = 0
               }

-- | Multimedia Keys
xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioMute        = 0x1008ff12

-- | Custom Key Bindings
myKeys sp conf@(XConfig {modMask = mmsk, workspaces = ws}) = M.fromList $
        customKeys
        ++ zip (zip (repeat (mmsk)) wsKeys) (map (withNthWorkspace W.greedyView) [0..])
        ++ zip (zip (repeat (mmsk .|. shiftMask)) wsKeys) (map (withNthWorkspace W.shift) [0..])
        where customKeys =  
               [ ((mmsk,               xK_Return   ), spawn $ XMonad.terminal conf) 
               , ((mmsk,               xK_BackSpace), shellPrompt promptConfig)
               , ((mmsk .|. shiftMask, xK_Return   ), dwmpromote)
               , ((mmsk,               xK_a        ), withFocused $ windows . W.sink)
               , ((mmsk,               xK_b        ), sendMessage ToggleStruts)
               , ((mmsk,               xK_grave    ), toggleWS)
               , ((mmsk,               xK_d        ), changeDir promptConfig)
               , ((mmsk .|. shiftMask, xK_b        ), warpToWindow (1/7) (1/7))
               , ((mmsk,               xK_m        ), broadcastMessage ToggleMonitor >> refresh)
               , ((mmsk,               xK_c        ), submap . M.fromList $ mpcControls "localhost")
               , ((mmsk .|. shiftMask, xK_c        ), submap . M.fromList $ mpcControls "kumar")
               ]
               ++ -- Find windows
               [ ((mmsk              , xK_slash    ), windowPromptGoto  promptConfig)
               , ((mmsk .|. shiftMask, xK_slash    ), windowPromptBring promptConfig)  
               ]
               ++ -- Search Engines
               [ ((mmsk              , xK_s        ), submap $ searchEngineMap $ 
                                                               promptSearch promptConfig)
               , ((mmsk .|. shiftMask, xK_s        ), submap $ searchEngineMap $ selectSearch)
               ]
               ++ -- Volume Control
               [ ((0,       xK_XF86AudioLowerVolume), spawn "amixer -q sset Master '3%-'")
               , ((0,       xK_XF86AudioRaiseVolume), spawn "amixer -q sset Master '3%+'")
               , ((0,       xK_XF86AudioMute       ), spawn "amixer -q sset Master togglemute")
               ]
               ++ -- Dynamic Workspaces
               [ ((mmsk .|. shiftMask, xK_BackSpace), DWS.removeWorkspace)
               , ((mmsk,               xK_t        ), DWS.selectWorkspace promptConfig)
               , ((mmsk .|. shiftMask, xK_t        ), withWorkspace promptConfig (windows . W.shift))
               , ((mmsk .|. ctrlMask , xK_t        ), withWorkspace promptConfig (windows . copy))
               , ((mmsk .|. shiftMask, xK_r        ), DWS.renameWorkspace promptConfig)
               ]
               ++ -- MultiToggle
               [ ((mmsk .|. shiftMask, xK_f        ), sendMessage $ Toggle FULL)
               , ((mmsk .|. shiftMask, xK_m        ), sendMessage $ Toggle MIRROR)
               ]
               ++ -- GridSelect
               [ ((mmsk              , xK_g        ), (gridselect defaultGSConfig) >>= (\w -> 
                                                    case w of
                                                         Just win  -> focus win >> windows W.shiftMaster 
                                                         Nothing   -> return()))
               ]

mpcControls host = [ ((0, xK_n    ), spawn $ "MPD_HOST=" ++ host ++ " mpc next")
                   , ((0, xK_p    ), spawn $ "MPD_HOST=" ++ host ++ " mpc prev")
                   , ((0, xK_r    ), spawn $ "MPD_HOST=" ++ host ++ " mpc repeat")
                   , ((0, xK_space), spawn $ "MPD_HOST=" ++ host ++ " mpc toggle")
                   , ((0, xK_c    ), spawn $ "MPD_HOST=" ++ host ++ " mpc crop")
                   , ((0, xK_s    ), spawn $ "MPD_HOST=" ++ host ++ " mpc stop")
                   ]

wsKeys = wsKeysDvorak

wsKeysDvorak :: [KeySym]
wsKeysDvorak = [1..9] ++ [xK_apostrophe]
               
-- | Key Bindings: Switch Workspaces
wsKeysDVP :: [KeySym]
wsKeysDVP = [ xK_ampersand
         , xK_bracketleft
         , xK_braceleft
         , xK_braceright
         , xK_parenleft
         , xK_equal
         , xK_asterisk
         , xK_parenright
         , xK_plus
         , xK_bracketright
         , xK_semicolon
         ]

-- | Custom Mouse Bindings
myMouseBindings (XConfig {XMonad.modMask = mmsk}) = M.fromList $
                [ ((mmsk, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((mmsk, button2), (\w -> focus w >> windows W.swapMaster))
                , ((mmsk, button3), (\w -> focus w >> mouseResizeWindow w))
                , ((mmsk .|. shiftMask, button1), (\w -> setOpacity w 0x77777777))
                ]

getWSCol :: WorkspaceId -> [Char] -> [Char]
getWSCol wsName dftCol = 
        case (M.lookup wsName wsCols) of
                Nothing -> dftCol
                Just color -> color

-- | Custom Log Pretty Printer
myPP = defaultPP
         { ppCurrent  = (\wsName -> xmobarColor "black" (getWSCol wsName highlightColor) (wrap " " " " wsName))
         , ppHidden   = (\wsName -> xmobarColor (getWSCol wsName highlightColor) [] wsName)
         , ppLayout   = (\wsName -> case wsName of
                        --"Tall"                  -> " ^i(" ++ myBitmapsDir ++ "/tall.xbm) "
                        --"Mirror Tall"           -> " ^i(" ++ myBitmapsDir ++ "/mirrortall.xbm) "
                        --"Full"                  -> " ^i(" ++ myBitmapsDir ++ "/full.xbm) "
                        --"Grid True"             -> " ^i(" ++ myBitmapsDir ++ "/grid.xbm) "
                        --"Dishes 2 (1%6)"        -> " ^i(" ++ myBitmapsDir ++ "/dishes.xbm) "
                        --"Mirror Dishes 2 (1%6)" -> " ^i(" ++ myBitmapsDir ++ "/dishes_mirrored.xbm) "
                        --"Tabbed Simplest"       -> " ^i(" ++ myBitmapsDir ++ "/tabbed.xbm) "
                        --"Magnifier Tall"        -> " ^i(" ++ myBitmapsDir ++ "/magnifier.xbm) "
                        --"Mirror Magnifier Tall" -> " ^i(" ++ myBitmapsDir ++ "/mirrormagnifier.xbm) "
                        _                       -> wsName)
        , ppSep     = " "
        , ppTitle   = xmobarColor "black" highlightColor . wrap " " " ". staticString 100
        -- , ppExtras  = logLoad : L.date ("^pa(1250)^bg() %a, %b %d ^fg(white)%H:%M^fg()") : []
        }

-- | Log Hook
myLogHook = \bar ->
            do fadeHook
               ewmhDesktopsLogHook
               setWMName "LG3D"
               dynamicLogWithPP myPP { ppOutput = hPutStrLn bar }
               updatePointer (Relative 1 1)

main :: IO ()
main = do sp <- mkSpawner
          bar <- spawnPipe "xmobar"
          xmonad $ defaultConfig
                   { terminal           = "urxvtc"
                   , workspaces         = myWorkspaces
                   , logHook            = myLogHook bar
                   , normalBorderColor  = backgroundColor
                   , focusedBorderColor = highlightColor
                   , mouseBindings      = myMouseBindings
                   , keys               = \c -> myKeys sp c `M.union` keys defaultConfig c
                   , layoutHook         = myLayout
                   , manageHook         = myManageHook }
