-- XMonad modules
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

import XMonad.Prompt.Shell
import XMonad.Prompt

-- Keybindings , greedyview 'n stuff
import XMonad.Util.EZConfig

-- spawnPipe and hPutStrLn
import XMonad.Util.Run

-- urgent notifications
import XMonad.Hooks.UrgencyHook

-- Custom Rules
import Workspaces
import LayoutRules
import ApplicationRules
import Tools 
 
-- Main --
main = do
        xmproc <- spawnPipe "xmobar"  -- start xmobar
    	xmonad 	$ withUrgencyHook NoUrgencyHook $ defaultConfig
        	{ manageHook = myManageHook
        	, layoutHook = LayoutRules.layoutRules
		, logHook = dynamicLogWithPP $ myPP xmproc 
		, modMask = mod4Mask
		, keys = myKeys
		, terminal = "urxvt"
		, borderWidth = 1 
		, normalBorderColor = "#333333"
		, focusedBorderColor = "#FF0000" 
		, workspaces = Workspaces.myWorkspaces
                , focusFollowsMouse = False
		}

-- ManageHook
-- automaticly switching app to workspace
myManageHook :: ManageHook
myManageHook = composeAll [
	  manageHook defaultConfig
	, manageDocks
	, manageWorkspaces
	, composeOne [ isFullscreen -?> doFullFloat ] -- manage fullscreeen windows
        ] 
manageWorkspaces = composeAll . concat $ ApplicationRules.rules

-- XMobar Style
myPP output = defaultPP { 
	  ppCurrent = xmobarColor "#7B79B1" "#0F141F" . wrap "[" "]"
	, ppVisible = wrap "(" ")"
	--, ppHidden = noScratchPad
	, ppHiddenNoWindows = const ""
	, ppSep    = " -> " 
	, ppTitle  = xmobarColor "#7B79B1" "" . shorten 50 
	, ppUrgent = xmobarColor "#2BA624" "0FA3A3"
	, ppWsSep  = " "
	, ppLayout = const ""
	-- receives three formatted strings: workspace, layout, current window title
	, ppOrder  = \(ws:_:t:_) -> [ws,t] -- ignore layout
	, ppOutput = hPutStrLn output 
}

-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c ), kill)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)

    -- rotate screen
    , ((modMask	, xK_r ), SM.submap . M.fromList $
        [ ((0, xK_l), spawn "xrandr --output LVDS1 --rotate left" )
        , ((0, xK_n), spawn "xrandr --output LVDS1 --rotate normal" )
        , ((0, xK_r), spawn "xrandr --output LVDS1 --rotate right")
        ] 
      )

	-- multimedia keys
	-- XF86AudioLowerVolume
	, ((0, 0x1008ff11), spawn "amixer -q set Master 2dB-")
	-- XF86AudioRaiseVolume
	, ((0, 0x1008ff13), spawn "amixer -q set Master 2dB+")
	-- XF86AudioMute
	, ((0, 0x1008ff12), spawn "amixer -q set Master toggle")

	-- screenshot
	, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s -e 'mv $f ~/images/screenshots/'")
	, ((0, xK_Print), spawn "scrot -e 'mv $f ~/images/screenshots/'")

    -- launcher
    , ((modMask , xK_space), shellPrompt myXPConfig)
    , ((modMask , xK_s ), SM.submap $ Tools.searchEngineMap $ S.promptSearchBrowser myXPConfig "Google-chrome")

    -- layout
    , ((modMask, xK_n ), sendMessage NextLayout)

    --focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )

    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    -- , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    -- , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- some nice colors for the prompt
myXPConfig = defaultXPConfig
    {
    font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
    ,fgColor = "#00FFFF"
    , bgColor = "#000000"
    , bgHLight    = "#000000"
    , fgHLight    = "#FF0000"
    , position = Top
    }
