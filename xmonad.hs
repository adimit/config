-- xmonad.hs

-- Aleksandar Dimitrov
--
-- Fri Nov  9 10:09:27 CET 2007

import XMonad
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog  (PP(..), dynamicLogWithPP, wrap, dzenColor, defaultPP, shorten, dzenEscape)
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Window
import XMonad.Layout.Dishes
import XMonad.Layout.Tabbed
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FlexibleResize as Flexi
import XMonad.Actions.Submap
import XMonad.Actions.GridSelect
import XMonad.Util.Run
import XMonad.Layout.Combo
import XMonad.Layout.Dishes
import XMonad.Layout.Grid
import XMonad.Layout.HintedGrid as HG
import XMonad.Layout.HintedTile as HT
import XMonad.Layout.Square
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.WindowArranger
import XMonad.Prompt.Shell
import XMonad.Actions.Search
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Hooks.SetWMName
import XMonad.Util.Loggers as L
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Monitor
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.Maybe as DM
import System.Exit
import XMonad.Actions.CopyWindow as CP
import Data.Bits ((.|.))
import XMonad.Util.Run
import System.IO

---- Constants
-- Terminal
myTerminal = "urxvtc"

wsCode = "code"
wsWeb  = "web"
wsCode'= "code'"
wsMail = "comm"
wsDoc  = "doc" 
wsWork = "work" 
wsWrite= "write" 
wsTest = "test" 
wsMisc = "misc"
wsLog  = "logs"
wsRead = "read"

-- Workspace colors map
wsCols :: M.Map WorkspaceId [Char]
wsCols = M.fromList $
	[ (wsCode,  "#aaccee")
	, (wsWeb,   "#eebbaa")
	, (wsCode', "#aa99ee")
	, (wsMail,  "#bb99aa")
	, (wsDoc,   "#bbdd99")
	, (wsWork,  "#edcd88")
	, (wsTest,  "#ee8844")
	, (wsMisc,  "grey90")
	, (wsLog,   "#ee9988")
	, (wsWrite, "#aaeebb")
	, (wsRead,  "#888888") ]

-- Workspaces, as xmonad knows them
myWorkspaces = [  wsCode
		, wsCode'
		, wsMail
		, wsWeb
		, wsDoc
		, wsTest
		, wsWrite
		, wsRead
		, wsWork
		, wsLog
		, wsMisc ]

-- Graphical setup
myBgColor            = "#0a0c0f"
myFgColor            = "#aacccc"
myBgColor'           = "'" ++ myBgColor ++ "'" -- quote-escaped for dzen2
myFgColor'           = "'" ++ myFgColor ++ "'" -- quote-escaped for dzen2
myFont               = "'-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*'"
myNormalBorderColor  = myBgColor
myFocusedBorderColor = myFgColor
myBitmapsDir         = "/home/adimit/etc/dzen2"
myHighlightFG        = "#eeeeee"
myHighlightBG        = "#6080B5"

-- Borders
-- Default Gaps
-- Fields are: top, bottom, left, right.
myDefaultGaps = [(14,14,0,0)]

dzen2 = "dzen2 -ta l -bg " ++ myBgColor'
                   ++ " -fg " ++ myFgColor'
		   ++ " -fn " ++ myFont
		   ++ " -w "  ++ "1200"
		   ++ " -ta l"

dzen2' = "/home/adimit/bin/status.sh | HLBAK='"++ myHighlightBG ++"' dzen2 -ta l -bg " ++ myBgColor'
                   ++ " -fg " ++ myFgColor'
		   ++ " -fn " ++ myFont
		   ++ " -w "  ++ "205"
		   ++ " -x "  ++ "1195"
		   ++ " -ta r -l 3 -u"

-- Search Engines.
lastfm    = searchEngine "last.fm"        "http://www.last.fm/music/"
piratebay = searchEngine "The Pirate Bay" "http://thepiratebay.org/s/?audio=on&page=0&orderby=7&q="
piratebay'= searchEngine "The Pirate Bay" "http://thepiratebay.org/s/?&page=0&orderby=7&q="
leo       = searchEngine "dict.leo.org"   "http://dict.leo.org/ende?lp=ende&lang=en&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&relink=on&search="

-- Configs
myTabbedConf =
    defaultTheme { activeColor 	= "#A0A5A0"
		 , inactiveColor 	= "#202A24"
		 , urgentColor 	= "#FF8800"
		 , activeBorderColor   = "#FFFFFF"
		 , inactiveBorderColor = "#BBBBBB"
		 , urgentBorderColor   = "##00FF00"
		 , activeTextColor     = "#FFFFFF"
		 , inactiveTextColor   = "#BFBFBF"
		 , urgentTextColor     = "#FF0000"
		 , fontName            = myFont
                 }

myXPConfig = defaultXPConfig
	{ font		= myFont
	, bgColor	= myBgColor
	, fgColor	= myHighlightFG
	, fgHLight	= myHighlightBG
	, bgHLight	= myHighlightFG
	, borderColor	= myFocusedBorderColor
	, position	= Bottom
        }

getWSCol :: WorkspaceId -> [Char] -> [Char]
getWSCol wsName dftCol = 
	case (M.lookup wsName wsCols) of
		Nothing -> dftCol
		Just color -> color

myPP h = defaultPP
	{ ppCurrent  = (\wsName -> dzenColor "black" (getWSCol wsName myHighlightBG) (wrap " " " " wsName))
	, ppHidden   = (\wsName -> dzenColor ( getWSCol wsName myFocusedBorderColor) "" wsName)
	, ppLayout   = (\wsName -> "^bg()^fg()" ++ case wsName of
			"Tall"                  -> " ^i(" ++ myBitmapsDir ++ "/tall.xbm) "
			"Mirror Tall"           -> " ^i(" ++ myBitmapsDir ++ "/mirrortall.xbm) "
			"Full"                  -> " ^i(" ++ myBitmapsDir ++ "/full.xbm) "
			"Grid True"             -> " ^i(" ++ myBitmapsDir ++ "/grid.xbm) "
			"Dishes 2 (1%6)"        -> " ^i(" ++ myBitmapsDir ++ "/dishes.xbm) "
			"Mirror Dishes 2 (1%6)" -> " ^i(" ++ myBitmapsDir ++ "/dishes_mirrored.xbm) "
			"Tabbed Simplest"       -> " ^i(" ++ myBitmapsDir ++ "/tabbed.xbm) "
			otherwise               -> wsName)
	, ppSep     = " "
	, ppTitle   = dzenColor myHighlightFG myHighlightBG . wrap " " "^bg(black)" . staticString 100
	, ppOutput  = hPutStrLn h
	-- , ppExtras  = logLoad : L.date ("^pa(1250)^bg() %a, %b %d ^fg(white)%H:%M^fg()") : []
	}

-- Keymaps
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

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch a shell prompt
    , ((modMask,               xK_p     ), shellPrompt myXPConfig)
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_Return), dwmpromote)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_a    ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart ("xmonad") True)
    -- Switch workspaces quickly
    , ((modMask .|. controlMask, xK_Right), nextWS)
    , ((modMask .|. controlMask, xK_Left),  prevWS)
    , ((modMask,               xK_dollar),     toggleWS)

    -- Multimedia keys (don't forget xmodmap)
    , ((0                    , xK_XF86AudioRaiseVolume), unsafeSpawn "amixer -q sset Master 3%+")
    , ((0                    , xK_XF86AudioLowerVolume), unsafeSpawn "amixer -q sset Master 3%-")
    , ((0                    , xK_XF86AudioMute), unsafeSpawn "amixer -q sset Master toggle")

    -- remote multimedia keys
    , ((modMask .|. shiftMask,          xK_Left   ), unsafeSpawn "MPD_HOST=\"payak\" mpc volume -1")
    , ((modMask .|. shiftMask,          xK_Right  ), unsafeSpawn "MPD_HOST=\"payak\" mpc volume +1")
    , ((modMask .|. shiftMask,          xK_Escape ), unsafeSpawn "MPD_HOST=\"payak\" mpc volume 0")

    , ((modMask,               xK_BackSpace),     shellPrompt myXPConfig)

    -- mpc control
    , ((modMask , xK_c), submap . M.fromList $
		    [ ((0, xK_n),     spawn "mpc next")
		    , ((0, xK_p),     spawn "mpc prev")
		    , ((0, xK_r),     spawn "mpc repeat")
		    , ((0, xK_space), spawn "mpc toggle")
		    , ((0, xK_c),     spawn "mpc crop")
		    ])

    -- remote mpc control
    , ((modMask , xK_r), submap . M.fromList $
		    [ ((0, xK_n),     spawn "MPD_HOST=\"payak\" mpc next")
		    , ((0, xK_p),     spawn "MPD_HOST=\"payak\" mpc prev")
		    , ((0, xK_r),     spawn "MPD_HOST=\"payak\" mpc repeat")
		    , ((0, xK_space), spawn "MPD_HOST=\"payak\" mpc toggle")
		    , ((0, xK_c),     spawn "MPD_HOST=\"payak\" mpc crop")
		    ])

    -- Prompt.AppendFile
    , ((modMask, xK_F3), appendFilePrompt myXPConfig "/home/adimit/TODO")

    -- Prompt.Man
    , ((modMask, xK_F1), manPrompt myXPConfig)

    -- Prompt.Ssh
    , ((modMask, xK_F2), sshPrompt myXPConfig) 

    -- Goto Windows - and bring them!
    , ((modMask , xK_slash     ), windowPromptGoto  myXPConfig)
    , ((modMask .|. shiftMask, xK_slash     ), windowPromptBring myXPConfig)  

    -- Search Engines.
    , ((modMask, xK_s), submap $ searchEngineMap $ promptSearch myXPConfig)
    , ((modMask .|. shiftMask, xK_s), submap $ searchEngineMap $ selectSearch)

    -- Switch current Workspace's working dir
    , ((modMask, xK_d ), changeDir myXPConfig)

    -- Banish the mouse pointer
    , ((modMask .|. shiftMask, xK_b), warpToWindow (1/7) (1/7))

    -- MultiToggle
    , ((modMask .|. shiftMask, xK_f), sendMessage $ Toggle FULL)
    , ((modMask .|. shiftMask, xK_m), sendMessage $ Toggle MIRROR)

    -- Monitor overlay (toggle widget layer)
    , ((modMask, xK_m     ), sendMessage ToggleMonitor)

    -- GridSelect
    , ((modMask, xK_g     ), (gridselect defaultGSConfig) >>= (\w -> case w of
                                Just w -> focus w >> windows W.shiftMaster 
                                Nothing -> return()))
    ]

    ++

    -- dynamic workspaces
    [ ((modMask .|. shiftMask, xK_BackSpace ), removeWorkspace)
    , ((modMask, xK_t                       ), selectWorkspace myXPConfig)
    , ((modMask .|. shiftMask  , xK_t       ), withWorkspace myXPConfig (windows . W.shift))
    , ((modMask .|. controlMask, xK_t       ), withWorkspace myXPConfig (windows . copy))
    --, ((modMask .|. shiftMask, xK_r       ), renameWorkspace myXPConfig)
    ]

    ++
    zip (zip (repeat (modMask)) wsKeyList) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modMask .|. shiftMask)) wsKeyList) (map (withNthWorkspace W.shift) [0..])
    ++

    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_v, xK_z] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- modified for progdvor
wsKeyList = [xK_ampersand
		, xK_bracketleft
		, xK_braceleft
		, xK_braceright
		, xK_parenleft
		, xK_equal
		, xK_asterisk
		, xK_parenright
		, xK_plus
		, xK_bracketright
		, xK_semicolon]

xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioMute        = 0x1008ff12
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> Flexi.mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
--
-- tabbed layout config:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
	workspaceDir "~"
        $ ewmhDesktopsLayout
	$ avoidStruts(

		    id
		    . smartBorders
		    . mkToggle(NOBORDERS ?? FULL ?? EOT)
		    . mkToggle(single MIRROR)
		    . addPersistentMonitor (ClassName "Cairo-clock" `And` Title "MacSlow's Cairo-Clock") (Rectangle (1400-150) (100) 150 150)
		    -- $ hintedTile HT.Tall
		    $ tiled
		    -- ||| Dishes 2 (1/6)
		    -- ||| tabbed shrinkText myTabbedConf
		   )
  where
     -- default tiling algorithm partitions the screen into two panes
     -- hintedTile   = HintedTile nmaster delta ratio TopLeft

     tiled = XMonad.Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll 
    [ className =? "MPlayer"        --> doFloat
    , className =? "Navigator"      --> doF (W.shift wsWeb)
    , className =? "Eclipse"        --> doF (W.shift wsCode')
    , className =? "Opera"  	    --> doF (W.shift wsDoc)
    , className =? "Gimp"           --> doFloat
    , className =? "Gimp"           --> doF (W.shift "gimp")
    , className =? "Cairo-clock"    --> (ask >>= \w -> liftX (hide w) >> doF (W.delete w))
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
    <+> manageDocks
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
-- myLogHook = dynamicLogXinerama

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. Modified
--


main = do 
	din <- spawnPipe dzen2
	spawnPipe dzen2'
	xmonad $ defaultConfig {
          terminal           = myTerminal
	, modMask            = msModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
	, startupHook        = setWMName "LG3D"
        , layoutHook         = myLayout
        , manageHook         = myManageHook
	, logHook            = (dynamicLogWithPP $ myPP din) >> myLogHook
	}

myLogHook :: X ()
myLogHook = fadeHook >> ewmhDesktopsLogHook

-- LoogHook: Fade hook
fadeHook :: X ()
fadeHook = fadeInactiveLogHook 0xdddddddd

-- Custom loggers
logLoad :: L.Logger
logLoad = L.logCmd "cut -d \\  -f 1 < /proc/loadavg"

-- | Always print a certain number of characters, adding ... when it's too long and spaces
--   to pad it when it's too short
staticString :: Int -> String -> String
staticString l s | length s <=  l = s ++ replicate (l - length s) ' '
		 | otherwise     = (take (l - length end) s) ++ end
	where
		end = "..."
