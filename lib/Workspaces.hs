module Workspaces
where

import XMonad

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:web", "2:code", "3:term", "4:chat", "5:doc", "6:stuff"]

-- Swtich to the Webworkspace
-- viewWeb = windows (W.greedyView "1:web")

