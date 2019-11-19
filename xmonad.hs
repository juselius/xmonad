import qualified Data.Map as M
import System.Environment (getEnvironment, getEnv)
import System.IO (hPutStrLn)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook (UrgencyHook, urgencyHook, withUrgencyHook)
import XMonad.Util.Run (spawnPipe, safeSpawn)
-- import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad.Layout.ResizableTile
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZ
import qualified DBus as DBus
import qualified DBus.Client as DBus
import qualified XMonad.StackSet as XStack
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import           XMonad.Util.NamedWindows         (getName)
import           Codec.Binary.UTF8.String         (decodeString)

main = do
    dbus <- createDBusClient
    -- xmproc <- spawnPipe "xmobar"
    xmonad $
        docks $
        ewmh $
        -- pagerHints
           defaultConfig
            { modMask = mod4Mask
            , layoutHook = desktopLayouts
            , manageHook =
                  myManageHook <+> manageDocks <+> manageHook defaultConfig
            , handleEventHook = docksEventHook <+> fullscreenEventHook
            , logHook = dynamicLogWithPP $ myLogHook dbus
            -- , logHook = polybarWorkspacesLogHook dbus
            -- , logHook = xdynamicLogWithPP mobarPP
            --   { ppOutput = hPutStrLn xmproc
            --   , ppTitle = xmobarColor "green" "" . shorten 50
            --   }
            -- , urgencyHook = LibNotifyUrgencyHook
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
desktopLayouts =
    onWorkspace "1"  mailLayout $
    onWorkspace "2"  webLayout $
    onWorkspaces (map show [3..9]) defLayout $
    -- onWorkspace "9" expLayout $
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
        expLayout =
          desktopLayoutModifiers $
          smartBorders $
          avoidStruts $
            ThreeColMid 1 (3/100) (3/7)
            ||| ResizableTall 1 (3/100) (1/2) []
            ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
            ||| noBorders Full
            ||| Circle
            ||| Grid


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
        , ("M-S-h", sendMessage MirrorExpand)
        , ("M-S-l", sendMessage MirrorShrink)
        ]

green     = "#78ee26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#cabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"
pur2      = "#5b51c9"
blue2     = "#2266d0"

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- XStack.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

myLogHook :: DBus.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap (" [ %{F" ++ yellow ++ "}") "%{F-} ] "
    , ppVisible = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle =  wrap ("%{F" ++ green ++ "}") "%{F-}" . myAddSpaces 25
    }

createDBusClient :: IO DBus.Client
createDBusClient = do
  client <- DBus.connectSession
  DBus.requestName client (DBus.busName_ "user.xmonad.log") dBusParams
  return client
  where
    dBusParams = [
      DBus.nameAllowReplacement,
      DBus.nameReplaceExisting,
      DBus.nameDoNotQueue]
--
-- Emit a DBus signal on log updates
dbusOutput :: DBus.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (DBus.signal objectPath interfaceName memberName) {
            DBus.signalBody = [DBus.toVariant $ UTF8.decodeString str]
        }
    DBus.emit dbus signal
  where
    objectPath = DBus.objectPath_ "/org/xmonad/Log"
    interfaceName = DBus.interfaceName_ "org.xmonad.Log"
    memberName = DBus.memberName_ "Update"

startup :: X ()
startup = return ()

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

dmenu = --"exec dmenu_run"
    unwords
        [ "exec `yeganesh -x --"
        , " -fn 'DejaVu Sans Mono-11'"
        , "-nb white -nf black`"
        ]
