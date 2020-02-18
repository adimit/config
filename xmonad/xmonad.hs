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
  , ((mask                , xK_l           ), spawn "gnome-screensaver-command -l")
  , ((mask .|. shiftMask  , xK_q           ), spawn "gnome-session-save --gui --logout-dialog")
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

main :: IO()
main = xmonad
  $ withNavigation2DConfig def
  $ gnomeConfig
    { terminal = "kitty"
    , modMask = mod4Mask
    , keys = \c -> myKeys c `M.union` keys gnomeConfig c }
