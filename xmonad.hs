import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.Util.EZConfig as EZ
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.IO (hPutStrLn)
import System.Environment (getEnvironment, getEnv)

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh defaultConfig
        { modMask            = mod4Mask
        , layoutHook         = myLayout
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        , manageHook         =
                myManageHook
            <+> manageDocks
            <+> manageHook defaultConfig
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , terminal           = "xfce4-terminal"
        , keys               = myKeys <+> mmKeys <+> keys defaultConfig
        , borderWidth        = 1
        , normalBorderColor  = "gray"
        , focusedBorderColor = "crimson"
        , focusFollowsMouse  = False
        , workspaces = map show [1..9]
        , startupHook        =
            gnomeRegister2 >> startup >> startupHook defaultConfig
        }

myManageHook = composeAll . concat $
    [ [isFullscreen --> myDoFullFloat]
    , [className =? c --> doIgnore      | c <- myIgnores]
    , [className =? c --> doCenterFloat | c <- myFloats]
    , [className =? c --> doShift "1"   | c <- onWs1]
    , [className =? c --> doShift "2"   | c <- onWs2]
    , [className =? c --> doShift "7"   | c <- onWs7]
    , [className =? c --> doShift "8"   | c <- onWs8]
    , [className =? c --> doShift "9"   | c <- onWs9]
    , [appName   =? n --> doCenterFloat | n <- myNames]
    , [citrixReceiver --> doFloat]
    , [currentWs =? n --> insertPosition Below Newer | n <- ["1", "2"]]
    ] where
        -- workspaces
        onWs1   = myMail
        onWs2   = myWeb ++ myMusic
        onWs7   = myChat
        onWs8   = myGimp
        onWs9   = myVm

        -- classnames
        myMail   = ["Thunderbird", "Evolution"]
        myWeb    = ["Firefox", "Google-chrome", "Chromium", "Chromium-browser"]
        myMovie  = ["mplayer2", "Vlc"]
        myMusic  = ["Rhythmbox", "Spotify"]
        myChat   = ["Pidgin", "Buddy List", "Skype"]
        myGimp   = ["Gimp"]
        myVm     = ["VirtualBox", "Remmina"]
        myFloats = myMovie ++
            [ "Xmessage"
            , "XFontSel"
            , "Do"
            , "Downloads"
            , "Nm-connection-editor"
            , "Launchbox"
            --, "VirtualBox"
            --, "Remmina"
            ]

        -- resources
        myIgnores = ["desktop", "desktop_window", "notify-osd", "stalonetray"]

        -- names
        myNames = ["bashrun", "Google Chrome Options", "Chromium Options"]

        -- special apps
        citrixReceiver = className =? "sun-applet-PluginMain" <&&>
            appName =? "sun-awt-X11-XFramePeer"

        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

myLayout =
    onWorkspace "1"  mailLayout $
    onWorkspace "2"  webLayout $
    onWorkspaces (map show [3..6]) defLayout $
    onWorkspace "7" threeCols $
    onWorkspace "8" gimpLayout $
    onWorkspace "9" fullLayout $
    smartBorders (layoutHook defaultConfig)
    where
        defLayout = desktopLayoutModifiers $
            smartBorders $ Tall 1 (3/100) 0.5 ||| Full
        mailLayout = desktopLayoutModifiers $
            smartBorders $ Tall 1 (3/100) 0.65 ||| Full
        webLayout  = desktopLayoutModifiers $
            smartBorders $ Full ||| Tall 1 (3/100) 0.65
        threeCols = desktopLayoutModifiers $ smartBorders $
                ThreeCol 1 (3/100) (1/3) ||| Full ||| Tall 1 (2/100) 0.7
        fullLayout = desktopLayoutModifiers $
            noBorders $ Full ||| Mirror (Tall 1 (3/100) 0.8)
        gimpLayout  = avoidStruts $ withIM 0.11 (Role "gimp-toolbox") $
            reflectHoriz $ withIM 0.15 (Role "gimp-dock") Full

myKeys (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm,               xK_p),
        spawn "dmenu_run -fn -*-fixed-*-*-*-*-15-*-*-*-*-*-iso8859-1")
    , ((modm .|. shiftMask, xK_p), spawn "/opt/bin/launchbox.py")
    , ((modm .|. shiftMask, xK_o), spawn "gnome-do")
    , ((modm .|. shiftMask, xK_n), spawn "nautilus --no-desktop --browser")
    , ((modm .|. shiftMask, xK_s), spawn "gnome-control-center")
    , ((modm              , xK_Print), spawn "gnome-screenshot -a")
    , ((modm .|. shiftMask, xK_Print), spawn "gnome-screenshot -w -B")
    , ((modm .|. controlMask, xK_Print), spawn "gnome-screenshot")
    , ((modm .|. shiftMask, xK_q), spawn "gnome-session-quit")
    --, ((modm              , xK_Print), spawn "scrot -e 'mv $f ~/Downloads'")
    --, ((modm .|. shiftMask, xK_Print), spawn "scrot -u -e 'mv $f ~/Downloads'")
    --, ((modm .|. controlMask, xK_Print), spawn "scrot -s -e 'mv $f ~/Downloads'")
    ]

mmKeys = flip EZ.mkKeymap [
      ("<XF86AudioMute>"
        , spawn "amixer -q -D pulse sset Master toggle")
    , ("<XF86AudioRaiseVolume>"
        , spawn "amixer -q -D pulse sset Master 6000+ unmute")
    , ("<XF86AudioLowerVolume>"
        , spawn "amixer -q -D pulse sset Master 6000- unmute")
    , ("<XF86AudioPlay>"
        , spawn $ unwords
        [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
        , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
        ])
    , ("<XF86AudioNext>"
        , spawn $ unwords
        [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
        , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
        ])
    , ("<XF86AudioPrevious>"
        , spawn $ unwords
        [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
        , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
        ])
    ]

startup :: X ()
startup = do
    user <- liftIO $ getEnv "USER"
    --spawn "xsetroot -solid #888888"
    --spawn "xloadimage -onroot -fullscreen <path.to.image>"
    spawn $ unwords -- restart trayer on M-q
        [ "killall -u " ++ user ++ " trayer;"
        , "exec trayer --edge top --align right --SetDockType true"
        , "--SetPartialStrut true --expand true --width 15 --transparent true"
        , "--tint 0x000000 --height 20 --distancefrom right --distance 750"
        ]
    return ()

gnomeRegister2 :: MonadIO m => m ()
gnomeRegister2 = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.gnome.SessionManager"
            ,"/org/gnome/SessionManager"
            ,"org.gnome.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:" ++ sessionId]
