import XMonad
import XMonad.Config.Gnome
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run (safeSpawn)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Environment (getEnvironment)

main = do
    gnomeRegister -- hack for newer gnome: must register early
    xmonad $ gnomeConfig
        { modMask            = mod4Mask
        , layoutHook         = myLayout
        , manageHook         = myManageHook <+> manageHook gnomeConfig
        --, terminal           = "urxvt"
        , startupHook        = startup
        , keys               = myKeys <+> keys gnomeConfig
        , borderWidth        = 3
        , normalBorderColor  = "#444444"
        , focusedBorderColor = "#0066aa"
        , workspaces = map show [1..9]
        }

myManageHook = composeAll [
    isFullscreen --> doFullFloat
    , (className =? "Thunderbird") --> doShift "1"
    , (className =? "Google-chrome") --> doShift "2"
    , (className =? "Spotify") --> doShift "2"
    , (className =? "Remmina") --> doShift "9"
    , (className =? "VirtualBox") --> doShift "9" <+> doFloat
    , (className =? "Vlc") --> doFloat
    , (className =? "Mplayer2") --> doCenterFloat
    , (className =? "Gnome-panel" <&&>
        title =? "Run Application") --> doCenterFloat
    , (className =? "sun-applet-PluginMain" <&&>
        appName =? "sun-awt-X11-XFramePeer") --> doFloat
    , (currentWs =? "1") --> insertPosition Below Newer
    , (currentWs =? "2") --> insertPosition Below Newer
        {-appName =? "Msgcompose") -->  doF W.swapDown-}
    {-, (className =? "Eog") --> doFloat-}
--     , (className =? "Diffuse") --> doFloat
--     , (className =? "Pidgin" <&&>
--          (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
--     , (className =? "Gcr-prompter") --> doCenterFloat
--     , (className =? "Xfce4-notifyd" -->  doIgnore)
    ]

myLayout =
    onWorkspace "1" (desktopLayoutModifiers $
        smartBorders $ Tall 1 (3/100) 0.65 ||| Full) $
    onWorkspace "2" (desktopLayoutModifiers $
        smartBorders $ Full ||| Tall 1 (3/100) 0.65) $
    onWorkspaces  (map show [3..3]) (desktopLayoutModifiers $
        smartBorders $
            ThreeCol 1 (3/100) (1/3) ||| Full ||| Tall 1 (3/100) 0.65) $
    onWorkspaces (map show [4..9]) (desktopLayoutModifiers $
        smartBorders $ Tall 1 (3/100) 0.5 ||| Full) $
    smartBorders (layoutHook gnomeConfig)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList []
    {-[ ((modm, xK_F12), xmonadPrompt defaultXPConfig)-}
    {-, ((modm, xK_F3 ), shellPrompt  defaultXPConfig)-}
    {-]-}

startup :: X ()
startup = do
    spawn "xsetroot -solid '#888888'"
--    xloadimage -onroot -fullscreen <path.to.image>

