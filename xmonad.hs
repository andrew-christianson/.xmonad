-- Stolen shamelessly from Joey Hess, with minor modifications
-- My second haskell program: A window manager.
--
-- With a little help from my personal gods, the xmonad dev team:
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Config.Xfce
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import XMonad.Layout.FixedColumn
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Dishes
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LimitWindows
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
import XMonad.Util.Themes
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.RotSlaves
import XMonad.Actions.PerWorkspaceKeys
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad
import Data.Ratio ((%))
import Data.Maybe
import Data.List
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myMod = mod4Mask -- windows key
myTerminal = "st"
myTerminalFloat = "st"
myWorkSpaces = ["logs", "main", "web", "chat", "misc", "book", "terminals"] ++ map show [8..16]


myTheme = defaultTheme
	{ activeColor         = blue
	, inactiveColor       = grey
	, activeBorderColor   = blue
	, inactiveBorderColor = grey
	, activeTextColor     = "white"
	, inactiveTextColor   = "black"
	, decoHeight          = 12
	}
	where
		blue = "#4a708b" -- same color used by pager
		grey = "#cccccc"

myXPConfig = defaultXPConfig
	{ fgColor  = "white"
	, bgColor  = "black"
	, promptBorderWidth = 0
	, position = Bottom
	, height   = 25
	}

myLayout = toggleLayouts Full perWS
	where
		mySpacing = 10
		-- Per workspace layout selection.
		perWS =
			onWorkspace "logs" (noTitles $ myLogs dishFirst) $
			onWorkspace "web"  (noTitles $ (mySplit ||| myWide)) $
			onWorkspace "chat" (noTitles $ myChat gridFirst) $
			onWorkspace "terminals" (withTitles $ gridFirst) $
			onWorkspace "book" (noTitles $ myBook) $
			                   (noTitles $ codeFirst)


		-- Modifies a layout to be desktop friendly with title bars
		-- and avoid the panel.
		withTitles l = noFrillsDeco shrinkText myTheme $ desktopLayoutModifiers l
		--withTitles l = desktopLayoutModifiers l

		-- Modifies a layout to be desktop friendly, but with no title bars
		-- and avoid the panel.
		noTitles l = desktopLayoutModifiers l

		-- Each of these allows toggling through a set of layouts
		-- in the same logical order, but from a different starting
		-- point.
		codeFirst = myCode ||| myWide ||| mySpiral ||| myGrid ||| myDish
		dishFirst = myDish ||| myCode ||| myWide ||| mySpiral ||| myGrid
		gridFirst = myGrid ||| myColumns ||| myColumnsCentered ||| myDish ||| myCode ||| myWide ||| mySpiral

		-- This is a tall-like layout.
		-- The master window is fixed at 80 columns wide, making
		-- this good for coding. Limited to 4 visible windows at
		-- a time to ensure all are a good size.
		myCode = smartSpacing mySpacing $ limitWindows 4 $
			FixedColumn 1 1 80 10

		-- Stack with one large master window.
		-- It's easy to overflow a stack to the point that windows
		-- are too small, so only show first 5.
		myDish = smartSpacing (mySpacing - 5) $ limitWindows 5 $ Dishes nmaster ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- proportion of screen occupied by other panes
				ratio = 1/5

		-- Wide layout with subwindows at the bottom.
		myWide = smartSpacing mySpacing $ Mirror $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 80/100

		-- Split screen, optimized for web browsing.
		mySplit = smartSpacing mySpacing $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 60/100

		-- Standard grid.
		myGrid = smartSpacing mySpacing $ gaps [(U,75), (R,100),(D,100),(L,100)] $ Grid

		-- Determined experimentally
		mySpiral = smartSpacing mySpacing $ spiral (6/7)

		-- The chat workspace has a roster on the right.
		myChat base = smartSpacing mySpacing $ mirror base $ withIM size roster
			where
				-- Ratio of screen roster will occupy
				size = 1%5
				-- Match roster window
				roster = Title "Buddy List"

		-- The logs workspace has space for procmeter.
		myLogs base = smartSpacing mySpacing $ mirror base $ withIM procmeterSize procmeter
			where
				-- Ratio of screen procmeter will occupy
				procmeterSize = 1%7
				-- Match procmeter
				procmeter = ClassName "ProcMeter3"

		-- For reading books, I typically want borders on
		-- the margin of the screen.
		myBook = smartSpacing mySpacing $ ThreeColMid nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 2/3
                myColumns = smartSpacing mySpacing $ multiCol [1] 4 0.03 0.5
		myColumnsCentered = smartSpacing mySpacing $ gaps [(U,75), (R,100),(D,100),(L,100)] $ multiCol [1] 4 0.03 0.5
		-- Applies a layout mirrored.
		mirror base a = reflectHoriz $ a $ reflectHoriz base

myKeys =
	[ ((myMod, xK_x), spawn myTerminal)
        , ((myMod .|. shiftMask, xK_x), spawn myTerminalFloat)
	, ((myMod, xK_c), kill)
	, ((myMod, xK_Left), prevWS)
	, ((myMod, xK_Right), nextWS)
	, ((myMod, xK_a), toggleWS)
	, ((myMod, xK_Up), prevWS)
	, ((myMod, xK_Down), nextWS)
	, ((myMod, xK_z), shellPrompt myXPConfig)
	, ((myMod, xK_Tab), bindOn [("chat", rotSlavesDown), ("", rotAllDown)])
	, ((myMod .|. shiftMask, xK_Tab), bindOn [("chat", rotSlavesUp), ("", rotAllUp)])
	, ((myMod, xK_d), sendMessage $ NextLayout)
	, ((myMod, xK_space), sendMessage $ ToggleLayout)
	-- , ((myMod, xK_p), spawn "password gui")
	]

myManageHook = composeAll
	-- comes first to partially override default gimp floating behavior
	[ gimp "toolbox" --> nofloat
	, gimp "image-window" --> nofloat
	, manageHook xfceConfig
	, doF avoidMaster
        , title =? "urxvtf"   --> doFloat
        , resource =? "urxvtf"   --> doFloat
        , className =? "urxvtf"   --> doFloat
	, resource =? "floatterm" --> doFloat
	, className =? "mplayer2" --> doFloat
	-- workaround for <http://code.google.com/p/xmonad/issues/detail?id=228>
	, composeOne [ isFullscreen -?> doFullFloat ]
	-- display notifications on all workspaces,
	-- and avoid focus stealing
	, className =? "Xfce4-notifyd" --> doF W.focusDown <+> doF copyToAll
	]
	where
		gimp win = className =? "Gimp" <&&> fmap (win `isSuffixOf`) role
		role = stringProperty "WM_WINDOW_ROLE"
		nofloat = ask >>= doF . W.sink

-- Modified to only operate on floating windows, since I seem to do this by
-- accident to non-floating.
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
	-- mod-button1, Move by dragging
	[ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
	-- mod-button2, Raise the window to the top of the stack
	--, ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
	-- mod-button3, Resize by dragging
	, ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
	]
	where
		ifFloating w f = withWindowSet $ \ws ->
			when (M.member w $ W.floating ws) (f w)
myLogHook xmp = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmp
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
myConfig xmp = defaultConfig
	{ manageHook = myManageHook
	, layoutHook = myLayout
	, modMask = myMod
	, logHook = myLogHook xmp
	, workspaces = myWorkSpaces
	, mouseBindings = myMouseBindings
	, terminal = myTerminal
	, borderWidth = 1
	, normalBorderColor  = inactiveBorderColor myTheme
	, focusedBorderColor = activeBorderColor myTheme
	--, startupHook = adjustEventInput
	--, handleEventHook = focusOnMouseMove
	} `additionalKeys` myKeys

main = do
  xmproc <- spawnPipe "xmobar /home/andrew/.xmonad/.xmobarrc"
  xmonad (myConfig xmproc)

-- Avoid the master window, but otherwise manage new windows normally.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c ->
	case c of
		W.Stack t [] (r:rs) -> W.Stack t [r] rs
		_ -> c


-- main = do
--     xmproc <- spawnPipe "xmobar /home/andrew/.xmonad/.xmobarrc"
--     xmonad $ defaultConfig
--         {terminal = "urxvt",
--          borderWidth  = 2
--         , manageHook = manageDocks <+> manageHook defaultConfig
--         , layoutHook = avoidStruts  $  layoutHook defaultConfig
--         , logHook = dynamicLogWithPP xmobarPP
--                        { ppOutput = hPutStrLn xmproc
--                        , ppTitle = xmobarColor "green" "" . shorten 50
--                        }
--        , modMask = mod4Mask     -- Rebind Mod to the Windows key
--        } `additionalKeys`
--        [
--          -- ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
--        -- , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
--        -- , ((0, xK_Print), spawn "scrot")
--        ]
