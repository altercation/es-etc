-- xmonad config file
-- es@ethanschoonover.com

-- coding style guidelines: http://goo.gl/MW05v
-- sources of inspiration and code:
-- https://github.com/thomasf/dotfiles-thomasf-xmonad
-- https://github.com/league/dot-files/blob/master/xmonad.hs
-- https://github.com/pbrisbin/xmonad-config
-- https://github.com/mntnoe/mntnoe-dotfiles
-- https://github.com/vdemeester/xmonad-config
-- anything xmonad related by Brent Yorgey http://byorgey.wordpress.com
-- inc. http://goo.gl/WNXm5 & http://hpaste.org/52657
-- what a modularized xmonad.hs: http://goo.gl/8yu6N
-- https://github.com/mortenbp/config/blob/master/xmonad.hs
-- lots of others that i've forgotten to record here
-- https://github.com/bogner/dotxmonad/blob/master/xmonad.hs - interesting float management
-- serious submaps: http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Regalia%27s_xmonad.hs
-- sersiously modular: https://bitbucket.org/anekos/xmonad-conf/src/

-- following has a solution for indicating that you are in a submap entry mode
-- read again: https://bitbucket.org/00Davo/dotfiles/src/89908e98731ca0e029ddf5ab3cfd50e52b81b2ea/xmonad/xmonad.hs?at=default

-- TODO: Modularize
-- TODO: search engines (see by's example here http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs for greek dict search example)

-- TODO: review the ewmh stuff again http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8#EwmhDesktops_0.9_config_updates

-- TODO: review this http://zuttobenkyou.wordpress.com/2011/08/24/xorg-using-the-us-international-altgr-intl-variant-keyboard-layout/ for keyboard mapping switching ideas

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------
-- `! sort -k1.18` in vim for a quick sort of these, natch

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
import           System.Directory -- killprompt test
import           System.Exit(ExitCode(ExitSuccess), exitWith)
import           System.IO
import qualified System.IO.UTF8
import           Text.EditDistance
import           XMonad hiding ((|||))
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleSelectedLayouts
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.DynamicWorkspaces as DW
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.Submap
import           XMonad.Actions.TopicSpace
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer -- For fuzzy spawn
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WithAll
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ToggleHook ( runLogHook )
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Decoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Drawer
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.ImageButtonDecoration 
import           XMonad.Layout.LayoutCombinators ((|||))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.SideSpacing
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Simplest
import           XMonad.Layout.TabBarDecoration
import           XMonad.Layout.TabbedWindowSwitcherDecoration
import           XMonad.ManageHook
import           XMonad.Prompt
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Image
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.WindowProperties

import           XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import           XMonad.Actions.SpawnOn

-- import        XMonad.Util.NamedActionsLocal
-- import        XMonad.Util.Statusbars
-- import        XMonad.Actions.Navigation2D
-- import        XMonad.Prompt.FuzzyWindow
-- import        XMonad.Prompt.Man
-- import        XMonad.Prompt.RunOrRaise
-- import        XMonad.Prompt.Ssh
-- import        XMonad.Prompt.AppendFile
-- import        Text.Regex.Posix ((=~))

import XMonad.Hooks.SetWMName

-- TODO: implement a preconfigured grid select for tablet mode
-- implement normal gridselect

-- Note on fullscreen related modules
-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages & require
-- X.L.Fullscreen, so we import both and reference X.H.EwmhDesktops as E

-- Notes on window properties:
-- Where XMonad.Util.WindowProperties is used, the following properties
-- and operators are used:
--
-- Title String	 
-- ClassName String	 
-- Resource String	 
-- Role String	
-- WM_WINDOW_ROLE property
-- Machine String	
-- WM_CLIENT_MACHINE property
-- And Property Property	 
-- Or Property Property	 
-- Not Property	 
-- Const Bool


------------------------------------------------------------------------
-- Keyboard configuration:
------------------------------------------------------------------------

confModMask = mod1Mask  -- alt
an n = addName n        -- simply for concision
bkSP = "<Backspace>"    -- ibid

-- MAIN APPS KEYS ------------------------------------------------------

keysMainApps conf =
  (subtitle "MAIN APPS":) $ mkNamedKeymap conf $
  [("M-<Return>",    an "New terminal"           $ newTerminal)
  ,("M-S-<Return>",  an "Next open terminal"     $ nextTerminal)
  ,("M-\\",          an "New browser"            $ newBrowser)
  ,("M-S-\\",        an "Next open browser"      $ nextBrowser)
  ,("M-]",           an "New firefox"            $ newFirefox)
  ,("M-S-]",         an "Next open firefox"      $ nextFirefox)
  ,("M-[",           an "New minimal browser"    $ newMinimal)
  ,("M-S-[",         an "Next open minimal browser" $ nextMinimal)]

-- SECONDARY APPS KEYS -------------------------------------------------

keysSecondaryApps conf =
  (subtitle "SECONDARY APPS":) $ mkNamedKeymap conf $
  [("M4-m",          an "Show mail"              $ showMail)
  ,("M4-y",          an "Show web music player"  $ showWebMusic)
  ,("M4-p",          an "Show web contacts"      $ showWebContacts)
  ,("M4-g c",        an "Show calendar"          $ showWebCalendar)
  ,("M4-t",          an "Show tasks"             $ showWebTasks)
  ,("M4-d",          an "Show Google drive"      $ showWebDrive)
  ,("M4-i",          an "Show Chat Drawer"       $ showChatDrawer)
  ,("M4-a",          an "Anki Flash Cards"       $ flashCards)
  ,("M4-z",          an "Zim wiki"               $ zimWiki)
  ,("M4-v",          an "New Vim"                $ newVim)
  ,("M4-S-v",        an "Next Vim"               $ nextVim)]

-- SCRATCHPAD KEYS -----------------------------------------------------

keysScratchpads conf =
  (subtitle "SCRATCHPADS":) $ mkNamedKeymap conf $
  [("M4-f",          an "Filemanager"            $ togSP "filemanager")
  ,("M4-n",          an "Persistent Notes"       $ togSP "notepad")
  ,("M4-x",          an "Audio Mixer"            $ togSP "mixer")
  ,("M4-h",          an "Process monitor"        $ togSP "htop")
  ,("M4-c",          an "Calendar - week"        $ togSP "calweek")
  ,("M4-S-c",        an "Calendar - month"       $ togSP "calmonth")
  ,("M4-v",          an "Virtual Box Manager"    $ spawn "VirtualBox")
  ,("M4-w",          an "Wifi connection menu"   $ togSP "wifi")]

-- PROMPT KEYS ---------------------------------------------------------

keysPrompts conf =
  (subtitle "PROMPTS":) $ mkNamedKeymap conf $
  [(smK "w",         an "Window prompt"          $ promptGotoWin)
  ,(smK "S-w",       an "Workspace prompt"       $ wsPrompt)
  ,(smK "C-w",       an "Add workspace prompt"   $ inPrompt)
  ,(smK bkSP,        an "Kill prompt"            $ killAllPrompt)
--,(smK "z",         an "Display Modes"          $ displayPrompt)
--,(smK "z",         an "Fuzzy window test"      $ fuzzyWindow)
  ] where
    smK k = "M4-<Space> " ++ k
    wsPrompt = workspacePrompt myPromptConfig (windows . W.greedyView)
    inPrompt = inputPrompt defaultXPConfig "add hidden" ?+
               addHiddenWorkspace

-- KILL/QUIT KEYS ------------------------------------------------------

keysKillQuit conf =
  (subtitle "KILL & QUIT":) $ mkNamedKeymap conf $
  [("M-" ++ bkSP,    an "Close focused"          $ kill1)
  ,("M-z",           an "Close focused"          $ unspawn "xmobar")
  ,("M-S-" ++ bkSP,  an "Close all"              $ killAll)
  ,("M-q",           an "Restart XMonad"         $ restartXMonad)
  ,("M-C-q",         an "Rebuild XMonad"         $ rebuildXMonad)
  ,("M-S-q",         an "Quit XMonad"            $ quitXMonad)]

-- WINDOW KEYS ---------------------------------------------------------

keysActionsWindows conf =
  (subtitle "WINDOW ACTIONS":) $ mkNamedKeymap conf $
  [("M-j",           an "Go to next window"      $ goNextWindow)
  ,("M-k",           an "To to prev window"      $ goPrevWindow)
  ,("M-S-j",         an "Move window to prev"    $ moveNextWindow)
  ,("M-S-k",         an "Move window to next"    $ movePrevWindow)
  ,("M-C-j",         an "Rotate to next window"  $ rotNextWindow)
  ,("M-C-k",         an "Rotate to prev window"  $ rotPrevWindow)
  ,("M-m",           an "Move window to master"  $ moveToMaster)
  ,("M-S-m",         an "Swap window w/ master"  $ swapMaster)
  ,("M-o",           an "Back to last window"    $ goToHistory)
  ,("M-t",           an "Tile this window"       $ moveToTiled)
  ,("M-S-t",         an "Tile all windows "      $ moveAllToTiled)
  ,("M-u",           an "Focus urgent winow"     $ focusUrgent)
  ,("M-C-u",         an "Clear urgent status"    $ clearUrgents)
  ,("M-p",           an "Pin/unpin window"       $ togglePin)]
  where
    goNextWindow = windows W.focusDown
    goPrevWindow = windows W.focusUp
    rotNextWindow = rotAllDown
    rotPrevWindow = rotAllUp
    moveNextWindow = windows W.swapDown
    movePrevWindow = windows W.swapUp
    -- goToMaster
    moveToMaster = windows W.focusMaster
    goToHistory = nextMatch History (return True)
    swapMaster = windows W.swapMaster
    moveToTiled = withFocused (windows . W.sink)
    moveAllToTiled = sinkAll
    togglePin = do
        ws <- wsContainingCopies
        if (null ws)
            then windows copyToAll
        else killAllOtherCopies

-- WORKSPACE KEYS ------------------------------------------------------
-- X.A.CycleWS is doing the heavy lifting here http://goo.gl/jthn4

keysActionsWS conf =
  (subtitle "WORKSPACE ACTIONS":) $ mkNamedKeymap conf $
  [("M-l",           an "Next workspace"           $ goNextWS)
  ,("M-h",           an "Previous workspace"       $ goPrevWS)
  ,("M-S-l",         an "Move to next workspace"   $ moveToNextWS)
  ,("M-S-h",         an "Move to prev workspace"   $ moveToPrevWS)
  ,("M-C-l",         an "Send to next workspace"   $ shiftToNext)
  ,("M-C-h",         an "Send to prev workspace"   $ shiftToPrev)
  ,("M-S-o",         an "Jump to prev workspace"   $ toggleWS)
  ,("M-S-i", an "Move to workspace prompt" $ goToWSPrompt)]
  -- ,("M4-M1-w",    an "test"
  -- $ moveTo Next (WSTagGroup '.'))
  -- $ moveTo Next (WSIs $ return (('w' `elem`) . W.tag)))
  -- TODO: chrome
  -- , ((modm .|. controlMask, xK_t), 
  -- $ nextMatch History (className =? "XTerm"))
  where
      goNextWS       = moveTo Next NonEmptyWS
      goPrevWS       = moveTo Prev NonEmptyWS
      moveToNextWS   = shiftToNext >> nextWS
      moveToPrevWS   = shiftToPrev >> prevWS
      goToWSPrompt   = workspacePrompt myPromptConfig $ \w ->
                       do s <- gets windowset
                          if W.tagMember w s
                            then windows $ W.view w
                            else DW.addWorkspace w
--    moveToWSPrompt = workspacePrompt myPromptConfig $ \w ->
--                     do s <- gets windowset
--                        if W.tagMember w s
--                          then withFocused $ W.shift w
--                          else DW.addWorkspace w >> (withFocused $ W.shift w)
--    curWorkspace :: X String
--    curWorkspace = withWindowSet (return . W.currentTag)
--    flashWS = (curWorkspace >>= \d->spawn $"flash "++d)

keysActionsWSMap :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keysActionsWSMap conf@(XConfig {XMonad.modMask = modm}) =
    [((m .|. modm, k), addName (n ++ i) $ (windows $ f i) >> moveCursor)
    | (f, m, n) <- [(W.view, 0, "Switch to workspace ")
    , (W.shift, shiftMask, "Move client to workspace ")]
    , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    where
        moveCursor = updatePointer (Relative 0.99 0.99)

-- LAYOUT KEYS ---------------------------------------------------------

-- TODO: decide if I'm going to use different cycleThroughLayouts
-- or limit layouts on a perworkspace basis
keysActionsLayouts conf =
  (subtitle "LAYOUTS":) $ mkNamedKeymap conf $
  [("M-<Space>",     an "Next layout"            $ cycleAllLayouts)
  ,("M-S-<Space>",   an "Reset layout"           $ resetLayout conf)
  ,("M-C-<Space>",   an "Refresh layout"         $ refresh)
  ,("M-f",           an "Full screen"            $ fullScreen)
  ,("M-b",           an "Toggle status bar"      $ toggleStruts)
  ,("M-S-9",         an "Shrink the master area" $ shrinkMaster)
  ,("M-S-0",         an "Expand the master area" $ expandMaster)
  ,("M-,",           an "+ windows in master"    $ incMaster)
  ,("M-.",           an "- windows in master"    $ decMaster)]
  where
      resetLayout c  = setLayout $ XMonad.layoutHook c
      toggleStruts   = sendMessage ToggleStruts
      shrinkMaster   = sendMessage Shrink
      expandMaster   = sendMessage Expand
      incMaster      = sendMessage (IncMasterN 1)
      decMaster      = sendMessage (IncMasterN (-1))
--    -- use the following in a key binding such as:
--    --  addName "Switch to next layout" $ sendMessage NextLayout 
--    --  >> (curLayout >>= \d->flash d)
--    curLayout :: X String
--    curLayout = gets windowset >>= return
--                . description
--                . W.layout
--                . W.workspace
--                . W.current

-- SCREEN KEYS ---------------------------------------------------------

keysActionsScreens conf =
  (subtitle "SCREEN ACTIONS":) $ mkNamedKeymap conf $
  [("M-n",           an "Next screen"            $ goNextScreen)
  -- TODO: back to p or resolve collision with pinning
  ,("M-8",           an "Previous screen"        $ goPrevScreen)
  ,("M-S-n",         an "Move to next screen"    $ moveNextScreen)
  ,("M-S-p",         an "Move to prev screen"    $ movePrevScreen)
  ,("M-C-n",         an "Swap screen w/next"     $ chngNextScreen)
  ,("M-C-p",         an "Swap screen w/prev"     $ chngPrevScreen)]
  where
      moveCursor     = updatePointer (Relative 0.99 0.99)
      goNextScreen   = nextScreen >> moveCursor
      goPrevScreen   = prevScreen >> moveCursor
      moveNextScreen = shiftNextScreen >> nextScreen >> moveCursor
      movePrevScreen = shiftPrevScreen >> prevScreen >> moveCursor
      chngNextScreen = swapNextScreen >> nextScreen >> moveCursor
      chngPrevScreen = swapPrevScreen >> nextScreen >> moveCursor

keysActionsScreensMap :: XConfig Layout -> 
                         [((KeyMask, KeySym), NamedAction)]
keysActionsScreensMap conf@(XConfig {XMonad.modMask = modm}) =
  [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc
      >>= flip whenJust (windows . f))
      | (f, m, n) <- [(W.view, 0, "Switch to screen number "),
      (W.shift, shiftMask, "Move client to screen number ")]
      , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

-- SYSTEM KEYS ---------------------------------------------------------

keysSystem conf =
  (subtitle "SYSTEM KEYS":) $ mkNamedKeymap conf $
  [(btnSleep,        an "System sleep"           $ syst "sleep")
  ,(btnPwr,          an "System power off"       $ syst "off")
  ,("S-" ++ btnPwr,  an "System reboot"          $ syst "reboot")
  ,(btnLockScreen,   an "Lock screen"            $ disp "lock")
  ,(btnDisp,         an "Cycle display mode"     $ disp "toggle")
  ,("S-" ++ btnDisp, an "Mirror display mode"    $ disp "mirror")
  ,("C-" ++ btnDisp, an "Span display mode"      $ disp "span")
  ,(btnBluetooth,    an "Bluetooth toggle"       $ togBT)
  ,(btnTrackpad,     an "Trackpad toggle"        $ togTrackpad)
  ,(prtSc,           an "Screendraw start/stop"  $ draw "")
  ,("S-" ++ prtSc,   an "Screendraw force stop"  $ draw "finish")
  ,("M-" ++ prtSc,   an "Screendraw cancel"      $ draw "cancel")
  ,(btnRotate,       an "Screendraw clear"       $ draw "clear")]
  where
      -- note that my 'display' script will restart xmonad
      -- if required. could also do that here to make that
      -- script more window manager independent.
      disp c         = spawn $ "displays " ++ c
      syst c         = spawn $ "system " ++ c
      togBT          = spawn $ "wireless bluetooth toggle"
      togTrackpad    = spawn $ "trackpad toggle"
      draw c         = spawn $ "screendraw " ++ c
      btnDisp        = "<XF86Display>"
      btnLockScreen  = "<XF86ScreenSaver>"
      btnTrackpad    = "<XF86TouchpadToggle>"
      btnSleep       = "<XF86Sleep>"
      btnPwr         = "<XF86PowerOff>"
      btnBluetooth   = "<XF86Launch1>"
      btnRotate      = "<XF86RotateWindows>"
      prtSc          = "<Print>"

keysSystemCodes :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keysSystemCodes conf@(XConfig {XMonad.modMask = modm}) =
    [ ((0, btnBatt),                 addName "Toggle min/max pwr modes"  
    $ spawn "power toggle")
    , ((0 .|. shiftMask, btnBatt),   addName "Toggle miv/mov pwr modes"  
    $ spawn "power toggle pinned")
    , separator
    , ((0 .|. controlMask, btnBatt), addName "Auto power modes"          
    $ spawn "power toggle pinned")]
    where
        btnBatt      = 0x1008ff93
        btnSuspend   = 0x1008ffa7

-- MEDIA KEYS ----------------------------------------------------------

keysMedia conf =
  (subtitle "MEDIA KEYS":) $ mkNamedKeymap conf $
  -- c.f. key names at http://goo.gl/KqOA6
  [ (volUpK,         an "Volume up by 1"         $ volUp "1")
  , (volDnK,         an "Volume down by 1"       $ volDn "1")
  , ("S-" ++ volUpK, an "Volume up by 10"        $ volUp "10")
  , ("S-" ++ volDnK, an "Volume down by 10"      $ volDn "10")
  , ("C-" ++ volUpK, an "Volume at 50%"          $ volMax)
  , ("C-" ++ volDnK, an "Volume at maximum"      $ volMid)
  , (volMtK,         an "Volume mute toggle"     $ volTog)]
  where
        volUpK       = "<XF86AudioRaiseVolume>"
        volDnK       = "<XF86AudioLowerVolume>"
        volMtK       = "<XF86AudioMute>"
        volUp i      = spawn $ "volume up "   ++ i
        volDn i      = spawn $ "volume down " ++ i
        volMax       = spawn $ "volume max"
        volMid       = spawn $ "volume mid"
        volTog       = spawn $ "volume toggle"

myKeys conf =
       keysMainApps conf
   ++  keysSecondaryApps conf
   ++  keysScratchpads conf
   ++  keysPrompts conf
   ++  keysKillQuit conf
   ++  keysActionsWindows conf
   ++  keysActionsWS conf
  ^++^ keysActionsWSMap conf
   ++  keysActionsLayouts conf
   ++  keysActionsScreens conf
   ++  keysActionsScreensMap conf
   ++  keysSystem conf
   ++  keysSystemCodes conf
   ++  keysMedia conf


------------------------------------------------------------------------
-- Applications
------------------------------------------------------------------------

myShell              = "$SHELL"
confTerminal         = terminalCmd

terminalCmd          = "urxvtc"
terminalClass        = "URxvt"

initTerminal :: X ()
initTerminal         = spawn "pgrep urxvtd || urxvtd -f -o -q"

newTerminal :: X ()
newTerminal          = spawn terminalCmd

newNamedTerminal :: String -> X ()
newNamedTerminal n   = spawn $ terminalCmd ++ " -name " ++ show n

nextTerminal         = raiseNextMaybe
                       newTerminal (className =? "URxvt")

showNotesDrawer      = raiseNextMaybe
                       (newNamedTerminal "notes")
                       (resource =? "notes")

showChatDrawer       = raiseNextMaybe
                       (newNamedTerminal "drawer")
                       (resource =? "drawer")

newVim :: X ()
newVim               = runInTerm "" "vim"
nextVim              = raiseNextMaybe
                       (runInTerm "" "vim") (title =? "vim")

flashCards           = raiseNextMaybe
                       (spawn $ "anki") (className =? "Anki")

zimWiki              = raiseNextMaybe
                       (spawn $ "zim") (className =? "Zim")

-- Open file in vim, relative to topic's directory
-- from: http://goo.gl/8Rfkv
-- vim :: String -> X ()
-- vim file = do
--     dir <- currentTopicDir myTopicConfig
--     let opts = "-name vim -cd $HOME/" ++ dir
--     runInTerm opts $ "vim " ++ file

showMail             = raiseMaybe 
                       (runInTerm "" "mutt") (title =? "mutt")

chromeClass          = "Chromium"
chromeBase           = "chromium --memory-model=low"
                     ++ "     --enable-smooth-scrolling"
                     ++ "     --enable-sync-extensions"
                     ++ "     --enable-webgl"
                     ++ "     --ignore-gpu-blacklist"
                     ++ "     --enable-print-preview"
chrome               = chromeBase
                     ++ " --class=Chromium --name=chromium"

newBrowser :: X ()
newBrowser           = spawnHere $ chrome
nextBrowser          = raiseNextMaybe
                       (spawnHere $ chrome) (className =? chromeClass)

newFirefox :: X ()
newFirefox           = spawnHere "firefox"
nextFirefox          = raiseNextMaybe
                       (spawnHere "firefox") (className =? "Firefox")

newMinimal :: X ()
newMinimal           = spawnHere "luakit"
nextMinimal          = raiseNextMaybe
                       (spawnHere "luakit") (className =? "luakit")

showWebApp :: String -> String -> X ()
showWebApp u m       = raiseNextMaybe
                       (spawn $ (appBaseCmdURL $ u) ) 
                       (className =? "ChromiumAppMode" 
                       <&&>
                       (fmap
                       (\t -> isPrefixOf m t || isSuffixOf m t)
                       title)) 

newWebApp :: String -> X ()
newWebApp u          = spawn $ (appBaseCmdURL $ u) 

appBaseCmdURL :: String -> String
appBaseCmdURL u      = chromeBase
                     ++ " --class=ChromiumAppMode"
                     ++ " --name==chromiumappmode"
                     ++ " --user-data-dir="
                     ++ "/home/es/.config/chromium-app-mode"
                     ++ " --app=" ++ u

showWebMail          = showWebApp "https://mail.google.com"
                       "Ethan Schoonover Mail"

showWebCalendar      = showWebApp "https://calendar.google.com"
                       "Ethan Schoonover - Calendar"

showWebContacts      = showWebApp "https://www.google.com/contacts"
                       "Google Contacts"

showWebDrive         = showWebApp "https://drive.google.com"
                       "Google Drive"

newWebDrive          = newWebApp  "https://drive.google.com" 

showWebNews          = showWebApp "https://reader.google.com"
                       "Google Reader"

showWebTasks         = showWebApp "https://astrid.com"
                       "Astrid"

-- showWebMusic         = showWebApp "https://play.google.com/music/listen"
--                        "Home - My Music"

showWebMusic         = raiseNextMaybe
                       (spawn "spotify &") 
                       (className =? "Spotify")

showWebVault         = showWebApp
                       ("chrome-extension://"
                       ++ "hdokiejnpimakedhajhdlcegeplioahd"
                       ++ "/homelocal2.html")
                       "My LastPass Vault"

-- unspawn :: String -> X ()
unspawn p            = spawn $ "for pid in `pgrep " 
                     ++ p ++ "`; do kill -9 $pid; echo $pid " 
                     ++ p ++ " killed >> ~/tmp/xmkilltest; done"

-- startCoreApps :: X ()
startCoreApps        = do
                       showWebNews
                       showWebDrive
                       showWebContacts
                       showWebTasks
                       showWebCalendar
                       showMail
                       showWebVault

-- initSystemTray       :: X ()
-- initSystemTray       = spawn   "stalonetray --background '#073642' --icon-size 18 --slot-size 20 --sticky --window-strut auto --window-type toolbar --geometry 6x1-1+0 --icon-gravity NE --kludges fix_window_pos,force_icons_size --dockapp-mode simple --window-layer bottom &"
-- killSystemTray       :: X ()
-- killSystemTray       = unspawn "stalonetray"

initSystemTray       :: X ()
initSystemTray       = spawn   "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --widthtype percent --width 6 --transparent true --tint 0x073642 --alpha 0 --margin 0 --padding 0 --heighttype pixel --height 20 &"
killSystemTray       :: X ()
killSystemTray       = unspawn "trayer"

initCompositor    :: X ()
initCompositor       = spawn   "compton -f -D 6 -m 0.95 --vsync drm --unredir-if-possible --detect-transient --detect-client-leader --use-ewmh-active-win --paint-on-overlay &"
-- initCompositor       = spawn   "xcompmgr -f -D 6 &"
-- initCompositor       = spawn   "cairo-compmgr &"
-- initCompositor       = return ()
killCompositor    :: X ()
killCompositor       = unspawn "compton"
-- killCompositor       = unspawn "xcompmgr"
-- killCompositor       = unspawn "cairo-compmgr"
-- killCompositor       = return ()

initNotifier      :: X ()
initNotifier         = spawn   "dunst &"
killNotifier      :: X ()
killNotifier         = unspawn "dunst"

initStatusBar        = spawn   "xmobar"
killStatusBar        = unspawn "xmobar"

initAudioTray     :: X ()
initAudioTray        = spawn   "pgrep pasystray || pasystray &"
killAudioTray     :: X ()
killAudioTray        = unspawn "pasystray"

initFileManager :: X ()
initFileManager      = spawn "pgrep spacefm || spacefm -d"

flash :: String -> X ()
flash s = spawn $ "flash "++ s

notify :: String -> X ()
notify s = spawn $ "notify "++ s

alert :: String -> X ()
alert s = spawn $ "alert "++ s

warn :: String -> X ()
warn s = spawn $ "warn "++ s


------------------------------------------------------------------------
-- Startup & Restarting
------------------------------------------------------------------------

rebuildXMonad' :: X ()
rebuildXMonad' = spawn $ "xmonad --recompile && xmonad --restart"
               ++ " || warn 'XMonad recompile failed'"

restartXMonad' :: X ()
restartXMonad' = spawn $ "xmonad --restart"
               ++ " && notify 'XMonad restarted' || warn 'XMonad restart failed'"

confStartupHook = do
    E.ewmhDesktopsStartup
    initSystemTray
    initCompositor
    initNotifier
    initTerminal
    -- initAudioTray
    initFileManager
    spawn "bloop up"
    -- notify "XMonad started"

rebuildXMonad :: X ()
rebuildXMonad = do
    flash "rebuilding XMonad"
    killStatusBar
    killSystemTray
    killCompositor
    killNotifier
    killAudioTray
    rebuildXMonad'

restartXMonad :: X ()
restartXMonad = do
    killStatusBar
    killSystemTray
    killCompositor
    killNotifier
    killAudioTray
    restartXMonad'

quitXMonad = do
    io (exitWith ExitSuccess)


------------------------------------------------------------------------
-- Scratch Pads
------------------------------------------------------------------------

togSP sp = namedScratchpadAction myScratchpads sp

spTerminal :: String -> String -> String -> String
spTerminal f a c    = confTerminal
                    ++ " -fn " ++ show f ++ " -fb " ++ show f
                    ++ " -fi " ++ show f ++ " -fbi " ++ show f
                    ++ " +sb " ++ " -b 15 " ++ a ++ " -e " ++ c

myScratchpads =
    [ NS "htop"
      (spTerminal myFont "" "htop")
      (title =? "htop") centWin

    , NS "wifi"
      (spTerminal myFont "-name wifi" "wifi")
      (resource =? "wifi") centSquare

    , NS "filemanager"
      "spacefm"
      (className =? "Spacefm") nonFloating

    , NS "notepad"
      (spTerminal myFont "-name notepad" "vim")
      (resource =? "notepad") centSquare

    , NS "mixer"
      "pavucontrol"
      (className =? "Pavucontrol") centWinMax
      -- (spTerminal myFontBig "" "alsamixer")
      -- (title =? "alsamixer") centWinBig

    , NS "calweek"
      (spTerminal myFont
      ("-name calweek -cr " ++ show base03) "gcal view week")
      (resource =? "calweek") centWinThin

    , NS "calmonth"
      (spTerminal myFontSmall
      ("-name calmonth -cr " ++ show base03) "gcal view month")
      (resource =? "calmonth") centWinMax

    ] where

        -- order of ratios: left-margin top-margin width height

        centWin     = (customFloating 
                      $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

        centWinBig  = (customFloating 
                      $ W.RationalRect (1/8) (1/8) (3/4) (3/4))

        centWinMax  = (customFloating 
                      $ W.RationalRect (2/15) (1/20) (11/15) (9/10))

        centWinThin = (customFloating 
                      $ W.RationalRect (1/30) (1/4) (28/30) (1/2))

        centSquare  = (customFloating 
                      $ W.RationalRect (1/3) (1/4) (1/3) (1/2))

        lowerThird  = (customFloating 
                      $ W.RationalRect (0) (2/3) (1) (1/3))


------------------------------------------------------------------------
-- Prompts
------------------------------------------------------------------------

promptGotoWin = windowPromptGoto myPromptConfig
    { autoComplete = Just 500000
    , searchPredicate = isInfixOf . (map toLower)
    }

displayPrompt = inputPromptWithCompl promptConfig "Display Mode"
    (mkComplFunFromList compList) ?+ \d -> spawn $ "displays " ++ d
        where
            promptConfig = myPromptConfig
                { autoComplete = Nothing
                , showCompletionOnTab = True
                }
            compList =
                ["internal","external","span","mirror"
                ,"span internal","mirror internal"]

myGoogleCLPrompt = googleCLPrompt myPromptConfig googleCLServices
googleCLServices = ["calendar","docs","picasa"
                   ,"blogger","youtube","contacts"]
googleCLPrompt :: XPConfig -> [String] -> X ()
googleCLPrompt c serviceList =
    inputPromptWithCompl c "google service" 
        (mkComplFunFromList serviceList) ?+ \googleCLService ->
    inputPrompt c "command" ?+ \googleCLCommand ->
    inputPrompt c "content" ?+ \googleCLContent ->
    runProcessWithInput "google" 
        [googleCLService, googleCLCommand] googleCLContent 
        >> return ()


killAllPrompt = inputPromptWithCompl myPromptConfig 
                "kill process" runningProcessesCompl ?+ killAllProc
killAllProc procName = spawn ("killall " ++ procName)
runningProcessesCompl str = runningProcesses >>= 
    (\procs -> return $ filter (\proc -> str `isPrefixOf` proc) procs)
runningProcesses = getDirectoryContents "/proc" >>= 
    (\dir -> return $ map (\pid -> "/proc/" ++ pid ++ "/comm") $ 
    filter (\dir -> all isDigit dir) $ dir) >>= 
    (\filenames -> sequence $ 
    map (\filename -> openFile filename ReadMode >>= 
    hGetContents) filenames) >>= 
    (\procs -> return $ sort $ nub $ 
    map (\proc -> init proc) procs)


-- Fuzzy Spawn Test

data FuzzySpawn = FuzzySpawn deriving (Read, Show)
instance XPrompt FuzzySpawn where showXPrompt _ = "Prompt Test: "

fuzzySpawn = do
    cmds <- io getCommands
    let compl s
          | null s = []
          | otherwise = let weight c = eDist defaultEditCosts s c
            in map snd $ take 10 $ sort $ map (\c -> (weight c,c)) cmds
    mkXPrompt FuzzySpawn myPromptConfig (return . compl) spawn
    where
        eDist = levenshteinDistance
        -- eDist = restrictedDamerauLevenshteinDistance 


------------------------------------------------------------------------
-- -- Doc prompt
-- data Doc = Doc
-- 
-- instance XPrompt Doc where
--     showXPrompt Doc = "okular "
-- 
-- docPrompt :: XPConfig -> X ()
-- docPrompt c = do
--   files <- liftIO $ flatDir "/home/scvalex/" 2
--   let files' = filter ((==".pdf") . takeExtension . fst) files
--       cmds = map (\(f, fp) -> (f, spawn $ "okular " ++ fp)) files'
--   docPromptC cmds c
--     where
--       flatDir :: FilePath -> Int -> IO [(FilePath, FilePath)]
--       flatDir _    0     = return []
--       flatDir base depth = do
--         ok <- return . isDirectory =<< getFileStatus base
--         if ok
--            then do
--              fs <- getDirectoryContents base
--              fs' <- mapM (flip flatDir (depth - 1)) (map (base </>) fs)
--              return $ (concat fs') ++ (map (\f -> (f, base </> f)) fs)
--            else do
--              return []
-- 
-- docPromptC :: [(String, X ())] -> XPConfig -> X ()
-- docPromptC commands c =
--     mkXPrompt Doc c (mkComplFunFromList' (map fst commands)) $
--         fromMaybe (return ()) . (`lookup` commands)
------------------------------------------------------------------------


------------------------------------------------------------------------
-- Manage windows
------------------------------------------------------------------------
-- use xprop to id windows

-- cf http://goo.gl/Ql7PG for a clean formatting example
-- cf http://dotfiles.org/~jbromley/.xmonad/xmonad.hs
-- cf http://goo.gl/gBuwu

confManageHook =   manageDocks
               <+> namedScratchpadManageHook myScratchpads
               -- <+> manageGimp
               <+> manageGeneral

-- cf anekos' setup http://goo.gl/xjJ1l
-- TODO: read every module that anekos has developed for their config
-- they are all pretty much mind blowing and along the lines I'm
-- thinking of

manageGeneral :: ManageHook
manageGeneral =
    composeAll . concat $
    [ [ matchCRT cr t --> doFloat     | (cr, t) <- appsFloatByCRT  ]
    , [ matchCR  cr   --> doFloat     | cr      <- appsFloatByCR   ]
    , [ matchC   c    --> doFloat     | c       <- appsFloatByC    ]
    , [ matchR   c    --> doFloat     | c       <- appsFloatByR    ]
    , [ matchT   t    --> doFloat     | t       <- appsFloatByT    ]
    , [ matchC   c    --> doIgnore    | c       <- appsIgnoreByCR  ]
    , [ isFullscreen  --> doFullFloat                              ]
    -- , [ manageDocks ]
    ] where
    matchCRT s t = (className =? s <||> resource =? s) <&&> title =? t
    matchCR  s   =  className =? s <||> resource =? s
    matchC   s   =  className =? s
    matchR   s   =  resource =? s
    matchT   s   =  title =? s
    appsFloatByCRT   = [("thunar","File Operation Progress")]
    appsFloatByCR    = ["MPlayer","Gimp","Skype","Gloobus-preview"]
    appsFloatByC     = []
    appsFloatByR     = []
    appsFloatByT     = []
    appsIgnoreByCR   = ["Onboard","desktop_window","stalonetray"]
    appsForWS1       = []
    appsDrawerByT    = []
    appsDrawersByCR  = []

-- manageGimp :: ManageHook -- from: http://goo.gl/xjJ1l
-- manageGimp =
--     className =? "Gimp" --> (doNamedShift "gimp" <+> doJumpToLayout "Gimp")
--     <+> composeOne
--     [ stringProperty "WM_WINDOW_ROLE" =? "gimp-image-window"
--        -?> doSink
--     , stringProperty "WM_WINDOW_ROLE" =? "gimp-image-new"
--        -?> doCenterFloat
--     , stringProperty "WM_WINDOW_ROLE" =? "gimp-dock"
--        -?> doSink
--     , stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox"
--        -?> doSink
--     , stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog"
--        -?> doBottomRightFloat
--     , return True -?> doFloat ]
-- 
-- doSink :: ManageHook
-- doSink = ask >>= doF . W.sink
-- 
-- doBottomRightFloat :: ManageHook
-- doBottomRightFloat = ask
--                      >>= \w -> doF . W.float w . position . snd
--                      =<< liftX (floatLocation w)
--   where position (W.RationalRect _ _ w h) = W.RationalRect (1-w) (1-h) w h
-- 
-- doJumpToLayout :: String -> ManageHook
-- doJumpToLayout l = liftX (sendMessage $ JumpToLayout l) >>= idHook
-- 
-- doNamedShift :: WorkspaceId -> ManageHook
-- doNamedShift n = liftX (addWorkspace n) >>= (doF . W.shift)


------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------

-- apps such as chrome emit correct ewmh events and are handled properly
-- while apps such as vlc use other fullscreen event messages & require
-- X.L.Fullscreen, hence the use of E.fullscreenEventHook and the 
-- XMonad.Layout.fullscreenEventHook below

confEventHook = E.ewmhDesktopsEventHook
    <+> E.fullscreenEventHook
    <+> fullscreenEventHook


------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------

-- workspaces   = ["web", "irc", "code" ] ++ map show [4..9]
-- confWorkspaces  = ["1","2","3","4","5","6","7","8","9"]
-- confWorkspaces  = ["1","2","3","4","5","wrk.1","wrk.2","pow","abc"]
confWorkspaces   = map show [1..9]

-- irc, web, com, org, txt, wrk, img, dev, des
-- consider topic spaces
-- consider dynamic workspace modules
-- consider showWName

-- cf https://github.com/aniederl/config-xmonad/blob/master/xmonad.hs
-- for an example of someone storing topics in a separate txt file
-- though I'd consider also using extensible state for this?
-- note their method of sourcing the topic information directly in the main config

-- another example of external storage of topics (and an interesting xmonad.hs in general)
-- https://github.com/vdemeester/xmonad-config/blob/master/.xmonad/xmonad.hs
-- nb how they update the config with new workspace list after updating

-- also cf http://pastebin.com/yrSeKhvu for prompt to open new proj, etc. ideas there? 

-- also by's xmonad.hs with topic setup: http://goo.gl/WNXm5

-- entirely modularized: https://github.com/reenberg/.xmonad/blob/master/lib/XMonad/Stack/MyTopics.hs

-- cf this for gridselect/topic stuff: https://github.com/wtfhax/dotfiles/blob/master/xmonad.hs
-- and this also: https://github.com/mortenbp/config/blob/master/xmonad.hs

-- see http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo%27s_xmonad.hs
-- for some helper functions
------------------------------------------------------------------------



-- data TopicItem = TI { topicName :: Topic
--                     , topicDir  :: Dir
--                     , topicAction :: X ()
--                     }
-- 
-- myTopics :: [TopicItem]
-- myTopics = [ TI "os" "" (runInTerm "" "vim")
--            , TI "web" "" (spawn "chromium")
--            , TI "irc" "" (spawn "skype" >> spawn "terminal -e 'weechat-curses'")
--            , TI "mail" "" (runInTerm "" "mutt")
--            , TI "music" "music" (spawn "amarok")
--            , TI "files" "" (spawn "dolphin")
--            , TI "doc" "" (return ())
--            , TI "alt" "" (return ())
--            ]
-- 
-- myTopicNames :: [Topic]
-- myTopicNames = map topicName myTopics
-- 
-- myTopicConfig :: TopicConfig
-- myTopicConfig = TopicConfig
--   { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics
--   , defaultTopicAction = const (return ())
--   , defaultTopic = "web"
--   , maxTopicHistory = 10
--   , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics
--   }

-- for reference/ideas
-- goto :: Topic -> X ()
-- goto = switchTopic myTopicConfig
-- 
-- promptedGoto :: X ()
-- promptedGoto = workspacePrompt myXPConfig goto
-- 
-- promptedGotoOtherScreen :: X ()
-- promptedGotoOtherScreen =
--   workspacePrompt myXPConfig $ \ws -> do
--     nextScreen
--     goto ws
-- 
-- promptedShift :: X ()
-- promptedShift = workspacePrompt myXPConfig $ windows . W.shift


------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

-- my goal here is to have one main layout and one alternate layout per
-- workspace (e.g. tabbed and marginalia), with other layouts summoned
-- directly on an as desired basis
--
-- The addition of noBorders in addition to smartBorders on all layouts
-- is to address borders showing up on full screen windows while in 
-- multimonitor mode.
--
-- the tabs/simplest combo below doesn't need noBorders except in 
-- dual monitor spanned instances


-- Out of the myriad X.L.* modules, I'm choosing to focus on
-- XMonad.Layout.PerWorkspace
-- XMonad.Layout.CycleSelectedLayouts
-- XMonad.Layout.Combo
-- and XMonad.Layout.WindowNavigation,
-- as these seem to be updated and the most recent generation of
-- solution to the navigation and combined layout problem.
-- Keeping an eye out for 2DNavigation, which handles multimonitor
-- setups well (though I don't know yet if X.L.WindowNavigation.
--
-- There is also XMonad.Layout.LayoutCombinators to consider,
-- which has the useful JumpToLayout function, recreated below.
--
-- Other contenders in lieu of Combo: X.L.SubLayouts.
--
-- In terms of navigation: WindowNavigation and WindowArranger seem to
-- have some overlap and it might be worth trialing the latter.
--
-- Note that order of "addTabs" and "addBars" prior to setting
-- noBorders is intentional. Inverse causes problems.

-- TODO: clean this up
-- cf http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Nnoell%27s_xmonad.hs
-- for clear formatting

confLayoutHook  = id
    $ avoidStruts
    $ onWorkspaces ["example","andanother"]
      (tabs ||| full ||| float)
    $ (tabs ||| tiledX ||| tiledXNude ||| full)
    where

    allLayouts  = tabs ||| tiledX ||| tiledXNude ||| full
    mainLayouts = tabs ||| tiledX

    -- drawer is probably best used in conjunction with tagging 
    -- the drawer window boring
    drawer      = renamed [Replace "Drawer"] $ leftDrawer `onLeft` tabs
                  where
                    leftDrawer = simpleDrawer 0.2 0.5 
                        (Resource "drawer" `Or` ClassName "Drawer")

    tiledX      = renamed [Replace "Tiles"] 
                $ spacing 1 $ dragBars $ noBorders
                $ Tall nmaster delta thirds

    tiledXNude  = renamed [Replace "Tiles Trim"] 
                $ smartBorders $ Tall nmaster delta thirds

    tiledY      = renamed [Replace "Tiles Wide"] 
                $ dragBars $ noBorders $ Mirror
                $ Tall nmaster delta halfs

    full        = renamed [Replace "Fullscreen"]
                $ noBorders $ Full

    float       = renamed [Replace "Floating"]
                $ smartBorders $ simpleFloat

    tabs        = renamed [Replace "Tabs"] 
                $ spacing 1 $ dragTabs $ noBorders $ Simplest

    simpleTabs  = renamed [Replace "Simple Tabbed"] 
                $ addTabs $ noBorders $ Simplest

    -- addTabs uses the official X.L.TabBarDecoration
    addTabs  l  = tabBar shrinkText tabTheme Top 
                $ resizeVertical (fi $ decoHeight tabTheme) $ l

    -- dragTabs uses the my custom X.L.TabbedWindowSwitcherDecoration
    dragTabs l  = tabbedWindowSwitcherDecorationWithImageButtons 
                  shrinkText myTabbedThemeWithImageButtons 
                  (draggingVisualizer $ l)

    -- dragBars uses either the official X.L.WindowSwitcherDecoration
    -- or (in this case) my custom X.L.TabbedWindowSwitcherDecoration
    dragBars l  = windowSwitcherDecorationWithImageButtons 
                  shrinkText myTiledThemeWithImageButtons 
                  (draggingVisualizer $ l)

    nmaster     = 1
    halfs       = 1/2
    thirds      = 1/3
    delta       = 3/100

cycleAllLayouts = sendMessage NextLayout
cycleMainLayouts = cycleThroughLayouts ["Tabs", "Tiles" ]
cycleTiledLayouts = cycleThroughLayouts ["Tiles", "Trim Tiles"]
cycleAlternateLayouts = cycleThroughLayouts ["Tabs", "Tiles" ]
myJumpToLayout l = cycleThroughLayouts [l]
-- refresh layout to unfullscreen quickly and restore struts
-- TODO: make this a toggle function
fullScreen = cycleThroughLayouts ["Fullscreen"] 
             >> (sendMessage $ SetStruts [] [minBound .. maxBound])


------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------

-- Solarized Colors
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#719e07"

confFocusFollowsMouse       = False
confBorderWidth             = 1

confNormalBorderColor       = base03
confFocusedBorderColor      = base01

myFontSize s                = "-*-terminus-medium-r-normal--" 
                              ++ show s ++ "-*-*-*-*-*-*-*"
myFontExtraBig              = myFontSize 20
myFontBig                   = myFontSize 16
myFont                      = myFontSize 14
myFontSmall                 = myFontSize 12
myFontExtraSmall            = myFontSize 10

myFontBoldSize s            = "-*-terminus-bold-r-normal--" 
                            ++ show s ++ "-*-*-*-*-*-*-*"
myFontBoldExtraBig          = myFontBoldSize 20
myFontBoldBig               = myFontBoldSize 16
myFontBold                  = myFontBoldSize 14
myFontBoldSmall             = myFontBoldSize 12
myFontBoldExtraSmall        = myFontBoldSize 10

baseTheme :: Theme
baseTheme = defaultTheme
    { activeColor           = base03
    , activeBorderColor     = base03
    , activeTextColor       = base01 -- blue also good
    , inactiveBorderColor   = base02
    , inactiveColor         = base02
    , inactiveTextColor     = base01
    , urgentColor           = yellow
    , urgentBorderColor     = yellow
    , urgentTextColor       = base02
    , fontName              = myFont
    , decoHeight            = 22
    }

tabTheme :: Theme
tabTheme = baseTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = base03
    , activeBorderColor     = base03
    , activeTextColor       = base01
    }

tileTheme :: Theme
tileTheme = baseTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = base01
    , activeBorderColor     = base01
    , activeTextColor       = base03
    }

altTileTheme :: Theme
altTileTheme = baseTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = blue
    , activeBorderColor     = blue
    , activeTextColor       = base03
    }

myTabbedThemeWithImageButtons :: Theme
myTabbedThemeWithImageButtons = tabTheme {
      windowTitleIcons = [ (nullButton, CenterLeft 0),
      (closeButton, CenterRight 6)]
      }

myTiledThemeWithImageButtons :: Theme
myTiledThemeWithImageButtons = tileTheme {
      windowTitleIcons = [ (nullButton, CenterLeft 0),
      (closeButton, CenterRight 6)]
      }
--  windowTitleIcons = [ (menuButton, CenterLeft 6),
--      (closeButton, CenterRight 6)]
--      (maxiButton, CenterRight 18),
--      (miniButton, CenterRight 33) ]
--      }

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (\x -> x == 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

nullButton' :: [[Int]]
nullButton' = [[]]
nullButton :: [[Bool]]
nullButton = convertToBool nullButton'

menuButton' :: [[Int]]
menuButton' = [[1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,0,0,0,0,0,0,0,0,0],
                [0,1,0,0,0,0,0,0,1,0],
                [0,0,1,0,0,0,0,1,0,0],
                [0,0,0,1,0,0,1,0,0,0],
                [0,0,0,0,1,1,0,0,0,0],
                [0,0,0,0,1,1,0,0,0,0],
                [0,0,0,1,0,0,1,0,0,0],
                [0,0,1,0,0,0,0,1,0,0],
                [0,1,0,0,0,0,0,0,1,0],
                [0,0,0,0,0,0,0,0,0,0]]


closeButton :: [[Bool]]
closeButton = convertToBool closeButton' 

myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { font                  = myFontBig
    , bgColor               = base02
    , fgColor               = base0
    , fgHLight              = base03
    , bgHLight              = blue
    , borderColor           = base03
    , promptBorderWidth     = 1
    , height                = 22
    , autoComplete          = Just 500000
    }


------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------

myPP = defaultPP
    { ppCurrent             = xmobarColor base02 blue . wrap " " " "
    -- , ppTitle            = xmobarColor blue "" . shorten 40
    , ppTitle               = xmobarColor base02 blue . shorten 40 . wrap " " " "
    , ppVisible             = wrap "(" ")"
    , ppUrgent              = xmobarColor base02 yellow . wrap " " " "
    , ppHidden              = id
    , ppHiddenNoWindows     = const ""
    , ppSep                 = " : "
    , ppWsSep               = " "
    , ppLayout              = id
    , ppOrder               = id
    , ppOutput              = putStrLn
    , ppSort                = fmap 
                              (namedScratchpadFilterOutWorkspace.)
                              (ppSort defaultPP)
    , ppExtras              = []
    }

confLogHook = do
    historyHook
    -- fadeInactiveLogHook 0xbbbbbbbb
    copies <- wsContainingCopies
    let check ws | ws `elem` copies = xmobarColor yellow base02 $ ws
                 | otherwise = ws
    dynamicLogString myPP {ppHidden = check} >>= xmonadPropLog


------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

-- toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
-- and add the configuration of the module to your main function:

main = do
    initStatusBar
    xmonad
        (E.ewmh
        $ withUrgencyHook NoUrgencyHook 
        -- $ withNavigation2DConfig myNavigation2DConfig
        $ addDescrKeys' ((confModMask, xK_F1), showKeybindings) myKeys
        $ defaultConfig 
        { terminal              = confTerminal
        , focusFollowsMouse     = confFocusFollowsMouse
        , borderWidth           = confBorderWidth
        , modMask               = confModMask
        , workspaces            = confWorkspaces
        , normalBorderColor     = confNormalBorderColor
        , focusedBorderColor    = confFocusedBorderColor
        , layoutHook            = confLayoutHook
        , manageHook            = confManageHook
        , handleEventHook       = confEventHook
        , logHook               = confLogHook
        , startupHook           = confStartupHook
        }) where
    
        showKeybindings :: [((KeyMask, KeySym), NamedAction)]
                           -> NamedAction
        showKeybindings x = addName "Show Keybindings" $ io $ do
            -- h <- spawnPipe "zenity --text-info"
            h <- spawnPipe $  "cat > ~/tmp/xmonadkeys.txt " 
                           ++ " && urxvtc -e less ~/tmp/xmonadkeys.txt"
            System.IO.UTF8.hPutStr h (unlines $ showKm x)
            hClose h
            return ()


{-
------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- from: http://code.google.com/p/guiyongzhe/source/browse/trunk/.xmonad/xmonad.hs?r=8
-- A nice little example of extensiblestate
newtype FocusFollow = FocusFollow {getFocusFollow :: Bool } deriving (Typeable,Read,Show)
instance ExtensionClass FocusFollow where
    initialValue = FocusFollow True
    extensionType = PersistentExtension
 
-- this eventHook is the same as from xmonad for handling crossing events
focusFollow e@(CrossingEvent {ev_window=w, ev_event_type=t})
                | t == enterNotify, ev_mode e == notifyNormal =
        whenX (XS.gets getFocusFollow) (focus w) >> return (All True)
focusFollow _ = return (All True)
 
toggleFF = XS.modify $ FocusFollow . not . getFocusFollow
--------------------------------------------------------------------------------
-}
