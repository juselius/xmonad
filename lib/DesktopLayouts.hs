module DesktopLayouts where
import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageDocks

desktopLayouts =
    onWorkspace "1"  mailLayout $
    onWorkspace "2"  webLayout $
    onWorkspaces (map show [3..9]) defLayout $
    -- onWorkspace "9" fullLayout $
    smartBorders (layoutHook defaultConfig)
    where
        defLayout = desktopLayoutModifiers $
            smartBorders $ ResizableTall 1 (5/100) 0.5 [] ||| Full
        webLayout  = desktopLayoutModifiers $
            smartBorders $ Full ||| Tall 1 (3/100) 0.65
        fullLayout = desktopLayoutModifiers $
            noBorders $ Full ||| Mirror (Tall 1 (3/100) 0.8)
        mailLayout = desktopLayoutModifiers $
            smartBorders $ Full ||| Tall 1 (3/100) 0.6
         --threeCols = desktopLayoutModifiers $ smartBorders $
        --         ThreeCol 1 (3/100) (1/3) ||| Full ||| Tall 1 (2/100) 0.7
        -- gimpLayout  = avoidStruts $ withIM 0.11 (Role "gimp-toolbox") $
        --     reflectHoriz $ withIM 0.15 (Role "gimp-dock") Full

