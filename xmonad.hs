import qualified Data.Map as M
import DesktopLayouts
import System.Environment (getEnvironment, getEnv)
import System.IO (hPutStrLn)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run (spawnPipe, safeSpawn)
import System.Taffybar.Support.PagerHints (pagerHints)
-- import Data.String.Conversions
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZ
import qualified DBus as D
import qualified DBus.Client as D


main = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
      [ D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue ]
    xmproc <- spawnPipe "xmobar"
    xmonad $
        docks $
        ewmh $
        pagerHints
            defaultConfig
            { modMask = mod4Mask
            , layoutHook = desktopLayouts
            , manageHook =
                  myManageHook <+> manageDocks <+> manageHook defaultConfig
            , handleEventHook = docksEventHook <+> fullscreenEventHook
            -- , logHook = dynamicLogWithPP (myLogHook dbus)
            , logHook = dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "green" "" . shorten 50
              }
            , terminal = "termite"
            , keys = myKeys <+> keys defaultConfig
            , borderWidth = 1
            , normalBorderColor = "gray"
            , focusedBorderColor = "crimson"
            , focusFollowsMouse = True
            , workspaces = map show [1 .. 9]
            , startupHook =
                     gnomeRegister2
                  >> startup
                  >> startupHook defaultConfig
            }

myManageHook =
    composeAll . concat $
    [ [isFullscreen --> myDoFullFloat]
    , [className =? c --> doIgnore | c <- myIgnores]
    , [className =? c --> doCenterFloat | c <- myFloats]
    , [className =? c --> doShift "1" | c <- onWs1]
    , [className =? c --> doShift "2" | c <- onWs2]
    -- , [className =? c --> doShift "7" | c <- onWs7]
    , [className =? c --> doShift "8" | c <- onWs8]
    , [className =? c --> doShift "9" | c <- onWs9]
    , [appName =? n --> doCenterFloat | n <- myNames]
    , [citrixReceiver --> doFloat]
    , [currentWs =? n --> insertPosition Below Newer | n <- ["1", "2"]]
    ]
    -- workspaces
  where
    onWs1 = myMail
    onWs2 = myWeb ++ myMusic
    onWs7 = myChat
    onWs8 = myGimp
    onWs9 = myVm
    -- classnames
    myMail = ["wavebox", "Evolution"]
    myWeb = ["Firefox", "Google-chrome", "Chromium", "Chromium-browser"]
    myMovie = ["mplayer2", "Vlc"]
    myMusic = ["Rhythmbox", "Spotify"]
    myChat = ["Pidgin", "Buddy List", "Skype"]
    myGimp = ["Gimp"]
    myVm = ["VirtualBox", "Remmina"]
    myFloats =
        myMovie ++
        [ "Xmessage"
        , "XFontSel"
        , "Do"
        , "Downloads"
        , "Nm-connection-editor"
        , "Launchbox"
        , "Pinentry"
        , "Gcr-prompter"
        ]
    --, "VirtualBox"
    --, "Remmina"
    -- resources
    myIgnores = ["desktop", "desktop_window", "notify-osd", "stalonetray"]
    -- names
    myNames = ["bashrun", "Google Chrome Options", "Chromium Options"]
    -- special apps
    citrixReceiver =
        className =? "sun-applet-PluginMain" <&&> appName =?
        "sun-awt-X11-XFramePeer"
    -- a trick for fullscreen but stil allow focusing of other WSs
    myDoFullFloat :: ManageHook
    myDoFullFloat = doF W.focusDown <+> doFullFloat

myKeys =
    flip
        EZ.mkKeymap
        [ ("M-p", spawn dmenu)
        , ("S-M-n", spawn "nautilus --no-desktop --browser")
        -- , ("S-M-q", spawn "gnome-session-quit --force")
        , ("<XF86AudioMute>", spawn "amixer -q -D pulse sset Master toggle")
        , ( "<XF86AudioRaiseVolume>"
          , spawn "amixer -q -D pulse sset Master 6000+ unmute")
        , ( "<XF86AudioLowerVolume>"
          , spawn "amixer -q -D pulse sset Master 6000- unmute")
        , ( "<XF86AudioPlay>"
          , spawn $
            unwords
                [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
                , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
                ])
        , ( "<XF86AudioNext>"
          , spawn $
            unwords
                [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
                , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
                ])
        , ( "<XF86AudioPrevious>"
          , spawn $
            unwords
                [ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify"
                , "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
                ])
        , ("S-M-s", spawn "flameshot gui")
        -- , ("M-S-<Print>", screenshot "-s")
        -- , ("M-C-<Print>", screenshot "-u")
        ]

startup :: X ()
startup = return ()
-- startup = do
  -- user <- liftIO $ getEnv "USER"
  -- spawn $
  --   unwords -- restart trayer on M-q
  --     [ "killall -u " ++ user ++ " trayer;"
  --     , "exec trayer --edge top --align right --SetDockType true"
  --     , "--SetPartialStrut true --expand true --width 15 --transparent true"
  --     , "--alpha 0 --tint 0x000000 --height 20 --distancefrom right --distance 750"
  --     ]
    --spawn "xsetroot -solid #888888"
    --spawn "xloadimage -onroot -fullscreen <path.to.image>"

gnomeRegister2
    :: MonadIO m
    => m ()
gnomeRegister2 =
    io $ do
        x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
        whenJust x $ \sessionId ->
            safeSpawn
                "dbus-send"
                [ "--session"
                , "--print-reply=literal"
                , "--dest=org.gnome.SessionManager"
                , "/org/gnome/SessionManager"
                , "org.gnome.SessionManager.RegisterClient"
                , "string:xmonad"
                , "string:" ++ sessionId
                ]

dbusLogHook :: D.Client -> PP
dbusLogHook dbus = def { ppOutput = dbusOutput dbus }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

dmenu =
    unwords
        [ "exec `yeganesh -x --"
        , " -fn 'DejaVu Sans Mono-11'"
        , "-s 0 -nb white -nf black -h 24`"
        ]
