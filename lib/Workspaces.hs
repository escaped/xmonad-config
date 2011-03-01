module Workspaces
where

import Xmonad

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:web", "2:code", "3:chat", "4:pdf", "5:doc", "6:stuff"]

-- Swtich to the Webworkspace
viewWeb = windows (W.greedyView "1:web")

