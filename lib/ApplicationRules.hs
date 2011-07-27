module ApplicationRules
where

-- Haskell modules
import Data.List
-- Xmonad modules
import XMonad
import XMonad.Hooks.ManageHelpers (doCenterFloat, isFullscreen, doFullFloat)
-- My Modules
import Workspaces


match a action = className =? a --> action

rules = [
	  [className =? a --> doShift "1:web" | a <- webApps]
	, [className =? a --> doShift "2:code" | a <- codeApps]
	, [className =? a --> doShift "3:chat" | a <- imApps]
	, [className =? a --> doShift "4:pdf" | a <- pdfApps]
	, [className =? a --> doShift "5:doc" | a <- docApps]
	, [className =? a --> doShift "6:stuff" | a <- stuffApps]

	, [className =? a --> doCenterFloat | a <- centerApps]
	--, isFullscreen --> doFullFloat
        ]

-- applications
webApps = ["firefox", "Google-chrome", "Chromium-browser"]
codeApps = ["Eclipse"]
imApps = ["Pidgin"]
pdfApps = ["Evince", "Epdfview"]
docApps = ["texmaker", "libreoffice-startcenter"]
stuffApps = ["vlc", "Vncviewer"] 

centerApps = ["nm-applet", "Xmessage", "Zenity", "feh"]
