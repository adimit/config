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

import XMonad.Layout.MultiColumns
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.MouseResizableTile
import System.Taffybar.Support.PagerHints (pagerHints)

promptConfig :: XPConfig
promptConfig = def { position          = Top
                   --, font              = myFont
                   --, bgColor           = myHLBG
                   --, fgColor           = myFG
                   --, fgHLight          = myBG
                   --, bgHLight          = myHL
                   --, promptKeymap      = defaultXPKeymap' wordSep
                   , promptBorderWidth = 0 }

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

main :: IO()
main = xmonad $ docks $ ewmh $ pagerHints
  $ withNavigation2DConfig def
    { defaultTiledNavigation = centerNavigation
    , screenNavigation = centerNavigation }
  $ gnomeConfig
    { terminal = "kitty"
    , layoutHook = avoidStrutsOn [D] $
      multiCol [2, 1, 0] 0 0.5 0.3
      ||| let gap = 8 in gaps [(U, gap), (D, gap), (L, gap), (R, gap)] mouseResizableTile
      ||| noBorders Full
    , modMask = mod4Mask
    , keys = \c -> myKeys c `M.union` keys gnomeConfig c }
