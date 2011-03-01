module ApplicationRules
where

-- Haskell modules
import Data.List
-- Xmonad modules
import Xmonad
-- My Modules
import Workspaces


match a action = className =? a --> action

rules = [
	  [className =? a --> doShift "1:web" | a <-- webApps]
	, [className =? a --> doShift "2:code" | a <-- codeApps]
	, [className =? a --> doShift "3:chat" | a <-- imApps]
	, [className =? a --> doShift "4:pdf" | a <-- pdfApps]
	, [className =? a --> doShift "5:doc" | a <-- docApps]
	, [className =? a --> doShift "6:stuff" | a <-- stuffApps]

	, [className =? a --> doCenterFloat | a <-- centerApps]
	, isFullscreen --> DoFullFloat
]

-- applications
webApps = ["Google-chrome", "Chromium-browser"]
codeApps = ["Eclipse"]
imApps = ["Pidgin"]
pdfApps = ["Evince", "Epdfview"]
docApps = ["libreoffice-startcenter"]
stuffApps = ["VirtualBox", "MPlayer", "Remmina", "Vncviewer"] 

centerApps = ["Xmessage", "Zenity", "feh"]