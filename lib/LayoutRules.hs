module LayoutRules
where

-- Xmonad modules
import Xmoand
import XMonad.Hooks.ManageDocks
-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier


layoutRules = 
	onWorkspace "1:web" webLayout $
	onWorkspace "3:chat" imLayout $
	onWorkspace "4:pdf" tabLayout $
	standardLayouts
	where
	
	standardLayouts = avoidStruts $ titled ||| reflectTiled ||| Mirror tiled ||| Grid ||| Full

	-- Layouts
	tiled        = smartBorders (ResizableTall 1 (2/100) (1/2) [])
	reflectTiled = (reflectHoriz tiled)
	full         = noBorders Full
	tabLayout    = (tabbed shrinkText myTheme)

	-- IM Layout
	imLayout = avoidStructs $ smartBorders $ reflectHoriz $ withIM pidginRatio PidginRoster (tiled ||| reflectTiled ||| Grid) where
		pidginRatio = (1%4)
		pidginRoster = (ClassName = "Pidgin") `And`(Role="buddy_list")
	-- web Layout
	 -- webLayout = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full
	webLayout = magnifiercz' 1.4 $ Tall nmaster delta ratio where
		-- The default number of windows in the master pane
		nmaster = 1
		-- Percent of screen to increment by when resizing p    anes
		delta   = 3/100
		-- Default proportion of screen occupied by master p    ane
		ratio   = 60/100

	-- Gimp Layout
	gimpLayout = avoidStruts $ smartBorders $ withIM(0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
	-- full
	fullL = avoidStruts $ full	
