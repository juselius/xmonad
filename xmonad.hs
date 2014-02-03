import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers
import XMonad.Config.Gnome
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W

myManageHook = composeAll [
    isFullscreen --> doFullFloat
    , (className =? "Thunderbird") --> doShift "1"
    , (className =? "Thunderbird" <&&>
        appName =? "Msgcompose") -->  doF W.swapDown
    , (className =? "Google-chrome") --> doShift "2"
    , (className =? "Spotify") --> doShift "2"
    , (className =? "VirtualBox") --> doShift "9" <+> doFloat
    , (className =? "Vlc") --> doFloat
    , (className =? "Eog") --> doFloat
    , (className =? "Mplayer2") --> doCenterFloat
    , (className =? "Gnome-panel" <&&>
        title =? "Run Application") --> doCenterFloat
--     , (className =? "Diffuse") --> doFloat
--     , (className =? "Pidgin" <&&>
--          (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
--     , (className =? "Gcr-prompter") --> doCenterFloat
--     , (className =? "Xfce4-notifyd" -->  doIgnore)
    ]

myLayout =
    onWorkspaces ["1"] ( desktopLayoutModifiers $
        smartBorders $ Mirror (Tall 1 (5/100) 0.55) ||| Full) $
    onWorkspaces ["2"] ( desktopLayoutModifiers $
        smartBorders $ Full ||| Tall 1 (3/100) 0.65) $
    onWorkspaces ["3", "4", "5", "6"] ( desktopLayoutModifiers $
        smartBorders $ Tall 1 (3/100) 0.5 ||| Full) $
    smartBorders (layoutHook gnomeConfig)

startup :: X ()
startup = do
    spawn "xsetroot -solid '#222222'"
--    xloadimage -onroot -fullscreen <path.to.image>

main = xmonad $ gnomeConfig
    { modMask            = mod4Mask
    , layoutHook         = myLayout
    , manageHook         = myManageHook <+> manageHook gnomeConfig
    , startupHook        = startup
    , borderWidth        = 3
    , normalBorderColor  = "#444444"
    , focusedBorderColor = "#0066aa"
    , workspaces = map show [1..9]
}

-- noFrills = desktopLayoutModifiers $ noFrillsDeco shrinkText defaultTheme
--     (layoutHook defaultConfig)

-- myLayout = onWorkspaces ["1", "2"] (desktopLayoutModifiers $ smartBorders
-- (layoutHook gnomeConfig)) noFrills
