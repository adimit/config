{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Config.Gnome
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks

import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.MultiColumns
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.MouseResizableTile
import System.Taffybar.Support.PagerHints (pagerHints)

blue, black, white :: String
blue = "#3388BB"
black = "#000000"
white = "#FFFFFF"

promptConfig :: XPConfig
promptConfig = def { position          = Top
                   , font              = "xft:Fira Code"
                   , alwaysHighlight   = True
                   , height            = 30
                   , bgColor           = black
                   , borderColor       = blue
                   , fgColor           = white
                   , bgHLight          = blue
                   , fgHLight          = black
                   , promptBorderWidth = 2 }

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig { modMask = mask } = M.fromList $
  -- window movement
  [ ((mask                , xK_h           ), windowGo L False)
  , ((mask                , xK_s           ), windowGo R False)
  , ((mask                , xK_n           ), windowGo U False)
  , ((mask                , xK_t           ), windowGo D False) ]
  ++ -- general keys
  [ ((mask                , xK_Return      ), spawn "kitty")
  , ((mask .|. shiftMask  , xK_Return      ), windows W.swapMaster)
  , ((mask                , xK_l           ), spawn "gnome-screensaver-command -l")
  , ((mask                , xK_a           ), withFocused $ windows . W.sink) -- %! Push window back into tiling
  , ((mask                , xK_BackSpace   ), shellPromptHere promptConfig)
  , ((mask                , xK_grave       ), toggleWS) ]
  ++ -- Dynamic Workspac  es
  [ ((mask                , xK_d           ), selectWorkspace promptConfig)
  , ((mask .|. shiftMask  , xK_d           ), withWorkspace promptConfig (windows . W.shift))
  , ((mask .|. shiftMask  , xK_BackSpace   ), removeWorkspace) ]
  ++ -- Screen control
  [ ((mask                , xK_bracketleft ), screenGo L False)
  , ((mask                , xK_bracketright), screenGo R False)
  , ((mask .|. controlMask, xK_bracketleft ), screenSwap L False)
  , ((mask .|. controlMask, xK_bracketright), screenSwap R False)
  , ((mask .|. shiftMask  , xK_bracketleft ), windowToScreen L False)
  , ((mask .|. shiftMask  , xK_bracketright), windowToScreen R False) ]
  ++ -- Grid select
  [ ((mask                , xK_g           ), goToSelected def)]
  ++ -- Resize
  [ ((mask .|. shiftMask  , xK_h           ), sendMessage Shrink)
  , ((mask .|. shiftMask  , xK_s           ), sendMessage Expand)
  , ((mask                , xK_u           ), sendMessage ShrinkSlave)
  , ((mask                , xK_i           ), sendMessage ExpandSlave) ]
  ++ -- Multimedia
  [ ((0,       xF86XK_AudioRaiseVolume), spawn $ volumeControl "5%+")
  , ((0,       xF86XK_AudioLowerVolume), spawn $ volumeControl "5%-")
  , ((0,       xF86XK_AudioMute       ), spawn $ volumeControl "toggle")
  , ((0,       xF86XK_AudioPlay       ), spawn $ gdbusLollypop "PlayPause")
  , ((0,       xF86XK_AudioPause      ), spawn $ gdbusLollypop "Pause")
  , ((0,       xF86XK_AudioNext       ), spawn $ gdbusLollypop "Next")
  , ((0,       xF86XK_AudioPrev       ), spawn $ gdbusLollypop "Prev") ]
  where
    gdbusLollypop c = "gdbus call --session --dest org.mpris.MediaPlayer2.Lollypop --object-path /org/mpris/MediaPlayer2 --method org.mpris.MediaPlayer2.Player." ++ c
    volumeControl v = "notify-send -t 400 \"Sound Level\" \"🔊 \"$(amixer -D pulse sset Master "
      ++ v ++ " | perl -wnE 'say /\\[(\\d?\\d?\\d%)\\]/' | tail -n 1)"

main :: IO()
main = xmonad $ docks $ ewmh $ pagerHints $ withNavigation2DConfig navconf $ gnomeConfig
    { terminal = "kitty"
    , layoutHook = layout
    , modMask = mod4Mask
    , normalBorderColor = black
    , focusedBorderColor = blue
    , keys = \c -> myKeys c `M.union` keys gnomeConfig c }
  where
    navconf = def { defaultTiledNavigation = centerNavigation , screenNavigation = centerNavigation }
    layout = avoidStrutsOn [D] $
          let gap = 8 in gaps [(U, gap), (D, gap), (L, gap), (R, gap)] mouseResizableTile
          ||| multiCol [2, 1, 0] 0 0.5 0.3
          ||| noBorders Full
