-- XMonad modules
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- Keybindings , greedyview 'n stuff
import XMonad.Util.EZConfig

-- spawnPipe and hPutStrLn
import XMonad.Util.Run

-- urgent notifications
import XMonad.Hooks.UrgencyHook

-- Custom Rules
import Workspaces
import LayoutRules
import ApplictionRules
import Tools 
 
-- Main --
main = do
        xmproc <- spawnPipe "xmobar"  -- start xmobar
    	xmonad 	$ withUrgencyHook NoUrgencyHook $ defaultConfig
        	{ manageHook = myManageHook
        	, layoutHook = LayoutRules.layoutRules
		, logHook = myLogHook

		, modMask = mod4mask
		, keys = myKeys
		, terminal = "urxvt"

		, borderWidth = 1 
		, normalBorderColor = "#333333"
		, focusedBorderColor = "#FF0000" 

		, workspaces = Workspaces.myWorkspaces
                , focusFollowsMouse = False
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
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
