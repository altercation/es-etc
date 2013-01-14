-- Ethan Schoonover's XMonad Config File
--
-- es@ethanschoonover.com  /  @ethanschoonover  / http://ethanschoonover.com
-- Available at: http://github.com/altercation/es-etc/


-- IMPORTS --------------------------------------------------------- {{{

-- `! sort -k1.18` in vim for a sort of these that ignores "qualified"

import           Data.Char                                      -- toLower, et al
import           Data.List                                      -- isInfixOf, et al
import qualified Data.Map as M                              -- basics
import           Data.Monoid                                    -- basics
import           System.Exit                                    -- basics
import           XMonad                                         -- basics
import           XMonad.Actions.CopyWindow                      -- copy a window to multiple workspaces
import           XMonad.Actions.CycleSelectedLayouts            -- cycle through subset of available layouts
import           XMonad.Actions.CycleWS                         -- move to workspaces / screens
import           XMonad.Actions.WindowGo                        -- raising/going to windows
import           XMonad.Actions.WithAll                         -- actions on multiple windows (killAll, etc.)
import           XMonad.Actions.GroupNavigation                 -- used for history navigation
import           XMonad.Hooks.DynamicLog                        -- support for "untethered" xmobar
import           XMonad.Hooks.FadeInactive                      -- fade out windows that aren't currently active
import           XMonad.Hooks.ManageDocks                       -- avoid statusbar/systray
import           XMonad.Layout.Decoration                       -- themes, decorated layouts
import           XMonad.Layout.DraggingVisualizer               -- see windows when dragging them
import           XMonad.Layout.Fullscreen                       -- fullscreen mgmt
import           XMonad.Layout.NoBorders                        -- useful with fullscreen windows
import           XMonad.Layout.Renamed                          -- rename layouts for clarity
import           XMonad.Layout.SideSpacing                      -- customized version of X.L.Spacing (CUSTOM)
import           XMonad.Layout.Simplest                         -- super simple, used for my tabbed layout
import           XMonad.Layout.TabBarDecoration                 -- themes, decorated layouts
import           XMonad.Layout.TabbedWindowSwitcherDecoration   -- window title bars (CUSTOM)
import           XMonad.Prompt                                  -- general prompt module
import           XMonad.Prompt.RunOrRaise                       -- run apps
import qualified XMonad.StackSet as W                           -- basics
import           XMonad.Util.EZConfig                           -- clean keybindings
import           XMonad.Util.Image                              -- for window decoration icons
import           XMonad.Util.NamedActions                       -- self documenting keybindings (CUSTOM)
import           XMonad.Util.NamedScratchpad                    -- summon/dismiss running app windows
import           XMonad.Util.SpawnOnce                          -- startup, etc.

import           XMonad.Prompt.Workspace                        -- prompt go to / shift to workspace
import           XMonad.Actions.UpdatePointer                   -- cursor management, useful changing screens

import           XMonad.Layout.WindowNavigation                 -- moving around in 2d space instead of stack
import           XMonad.Actions.Navigation2D                    -- moving around in 2d space instead of stack

import           XMonad.Hooks.UrgencyHook                       -- visually alert urgency in status bar

import           XMonad.Layout.Combo                            -- combine multiple layouts
import           XMonad.Layout.ComboP                           -- combine layouts with designated master apps
import           XMonad.Layout.TwoPane                          -- simple two pane layout used w/X.L.Combo(P)

-- TODO: research what i'm losing by not using ewmh
-- import        XMonad.Hooks.EwmhDesktops                      -- standard window manager hints support

-------------------------------------------------------------------- }}}
-- MAIN ------------------------------------------------------------ {{{

-- main = xmonad defaults

main = xmonad
     $ withUrgencyHook NoUrgencyHook
     $ withNavigation2DConfig myNavigation2DConfig
     $ addDescrKeys' ((controlMask, xK_F1), xMessage) myKeys myConfig

myNavigation2DConfig = defaultNavigation2DConfig
    { defaultTiledNavigation    = lineNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

myConfig = defaultConfig {

    terminal            = myTerminal,
    focusFollowsMouse   = myFocusFollowsMouse,
    clickJustFocuses    = myClickJustFocuses,
    borderWidth         = myBorderWidth,
    modMask             = myModMask,
    workspaces          = myWorkspaces,
    normalBorderColor   = myNormalBorderColor,
    focusedBorderColor  = myFocusedBorderColor,

    -- keys             = myKeys,
    mouseBindings       = myMouseBindings,

    layoutHook          = myLayout,
    manageHook          = myManageHook,
    handleEventHook     = myEventHook,
    logHook             = myLogHook,
    startupHook         = myStartupHook
    }

-------------------------------------------------------------------- }}}
-- THEME & INTERFACE ----------------------------------------------- {{{

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

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 1

myNormalBorderColor       = base03
myFocusedBorderColor      = base01

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
    , autoComplete          = Just 100
    , searchPredicate = isInfixOf . (map toLower)
    , promptKeymap          = M.fromList
                              [((controlMask, xK_space), quit)]
                             `M.union` promptKeymap defaultXPConfig
    }


-------------------------------------------------------------------- }}}
-- STATUS BARS & LOGGING ------------------------------------------- {{{

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

myPP = defaultPP
    { ppCurrent             = xmobarColor base02 blue . wrap " " " "
    , ppTitle               = xmobarColor blue "" . shorten 40
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

myLogHook = do
    historyHook -- used for history navigation from X.A.GroupNavigation
    fadeInactiveCurrentWSLogHook 0xccccdddd -- 0xbbbbbbbb
    copies <- wsContainingCopies -- from XMonad.Actions.CopyWindow
    let check ws | ws `elem` copies = xmobarColor yellow base02 $ ws
                 | otherwise = ws
    dynamicLogString myPP {ppHidden = check} >>= xmonadPropLog

-------------------------------------------------------------------- }}}
-- KEYBINDINGS ----------------------------------------------------- {{{

myModMask = mod1Mask -- alt key

myKeys c =
       keysXmonad c
    ++ keysMainApps c
    ++ keysSecondaryApps c
    ++ keysRunPromptSubmap c
    ++ keysWindows c
    ++ keysWorkspaces c
    ++ keysScreens c
    ++ keysLayouts c

keysXmonad conf =

    (subtitle "KILL & RESTART":) $ mkNamedKeymap conf $

    [("C-<Backspace>",      addName "Kill current window"           $ kill1)    -- from X.A.CopyWindow
    ,("C-S-<Backspace>",    addName "Kill all windows on workspace" $ killAll)  -- from X.A.WithAll
    ,("C-M-<Backspace>",    addName "Restart XMonad"                $ restartXmonad)
    ,("C-M-S-<Backspace>",  addName "Quit XMonad"                   $ quitXmonad)
    ]

keysWindows conf =

    (subtitle "WINDOWS":) $ mkNamedKeymap conf $

    [("C-j",                addName "GO next window"                $ windows W.focusDown)
    ,("C-k",                addName "GO previous window"            $ windows W.focusUp)
    ,("C-h",                addName "GO other side of combo"        $ sendMessage $ SwapWindow) -- fr. X.L.ComboP
    ,("C-l",                addName "GO last active window"         $ nextMatch History (return True))

    ,("C-S-j",              addName "MOVE next window"              $ windows W.swapDown)
    ,("C-S-k",              addName "MOVE previous window"          $ windows W.swapUp)
    ,("C-S-h",              addName "MOVE other side of combo"      $ return ())
    ,("C-S-l",              addName "MOVE last active window"       $ return ())

    ,("C-M-<Left>",         addName "Navigate left"                 $ sendMessage $ Go L)
    ,("C-M-<Right>",        addName "Navigate right"                $ sendMessage $ Go R)
    ,("C-M-<Up>",           addName "Navigate up"                   $ sendMessage $ Go U)
    ,("C-M-<Down>",         addName "Navigate down"                 $ sendMessage $ Go D)

    ,("C-M-S-<Left>",       addName "Navigate left"                 $ sendMessage $ Swap L)
    ,("C-M-S-<Right>",      addName "Navigate right"                $ sendMessage $ Swap R)
    ,("C-M-S-<Up>",         addName "Navigate up"                   $ sendMessage $ Swap U)
    ,("C-M-S-<Down>",       addName "Navigate down"                 $ sendMessage $ Swap D)

    ,("C-<Left>",           addName "Go left"                       $ windowGo L True)
    ,("C-<Right>",          addName "Go right"                      $ windowGo R True)
    ,("C-<Up>",             addName "Go up"                         $ windowGo U True)
    ,("C-<Down>",           addName "Go down"                       $ windowGo D True)

    ,("C-S-<Left>",         addName "Swap left"                     $ windowSwap L True)
    ,("C-S-<Right>",        addName "Swap right"                    $ windowSwap R True)
    ,("C-S-<Up>",           addName "Swap up"                       $ windowSwap U True)
    ,("C-S-<Down>",         addName "Swap down"                     $ windowSwap D True)
    ]

keysWorkspaces conf =

    (subtitle "WORKSPACES":) $ mkNamedKeymap conf $

    [("C-;",                addName "Go to workspace prompt"        $ gotoWSPrompt)
    ,("C-S-;",              addName "Send to workspace prompt"      $ shiftWSPrompt)
    ] where
        -- gotoWSPrompt   = workspacePrompt myPromptConfig $ windows . W.greedyView
        gotoWSPrompt   = workspacePrompt myPromptConfig $ windows . W.view
        shiftWSPrompt  = workspacePrompt myPromptConfig $ windows . W.shift

keysScreens conf =

    (subtitle "SCREENS":) $ mkNamedKeymap conf $

    [("C-o",                addName "Focus to other screen"         $ goToOtherScreen)
    ,("C-S-o",              addName "Send window to other screen"   $ moveToOtherScreen)
    ] where
        moveCursor          = updatePointer (Relative 0.99 0.99) -- X.A.UpdatePointer
        goToOtherScreen     = nextScreen >> moveCursor
        moveToOtherScreen   = shiftNextScreen >> nextScreen >> moveCursor
        chngNextScreen      = swapNextScreen  >> nextScreen >> moveCursor

keysMainApps conf =

    (subtitle "MAIN APPS":) $ mkNamedKeymap conf $

    [("C-<Return>",         addName "New terminal"                  $ spawn myTerminal)
    ,("C-\\",               addName "New broswer"                   $ spawn myBrowser)
    ]

keysSecondaryApps conf =

    (subtitle "OTHER APPS/SCRATCHPADS":) $ mkNamedKeymap conf $

    [("M-s",                addName "Spotify"                       $ spawn "spotify")
    ,("M-f",                addName "Filemanager"                   $ toggle "filemanager")
    ,("M-x",                addName "Audio Mixer"                   $ toggle "mixer")
    ,("M-p",                addName "Process monitor"               $ toggle "htop")
    ,("M-c",                addName "Calendar - week"               $ toggle "calweek")
    ,("M-S-c",              addName "Calendar - month"              $ toggle "calmonth")
    ,("M-v",                addName "Virtual Box Manager"           $ spawn "VirtualBox")
    ,("M-w",                addName "Wifi connection menu"          $ toggle "wifi")
    ]

keysRunPromptSubmap conf =

    (subtitle "RUN PROMPT":) $ mkNamedKeymap conf $

    [(runPK "C-<Space>",    addName "Run or raise prompt"           $ runOrRaisePrompt myPromptConfig)
    ,(runPK "M-;",          addName "New terminal 1"                $ spawn myTerminal)
    ,(runPK "M-C-;",        addName "New terminal 2"                $ spawn myTerminal)
    ,(runPK "M-S-;",        addName "New terminal 3"                $ spawn myTerminal)
    ,(runPK "M-<Return>",   addName "New term 1"                    $ spawn myTerminal)
    ,(runPK "M-S-<Return>", addName "New term 2"                    $ spawn myTerminal)
    ,(runPK "M-\\",         addName "New broswer 2"                 $ spawn myBrowser)
    ] where
        runPK k = "C-<Space> " ++ k -- main prompt key trigger combination

keysLayouts conf =

    (subtitle "APPS/SCRATCHPADS":) $ mkNamedKeymap conf $
    [("C-'",                addName "Next layout"                   $ sendMessage NextLayout)
    ,("C-S-'",              addName "Reset layout"                  $ setLayout $ XMonad.layoutHook conf)
    ,("C-M-'",              addName "Sink & Reset layout"           $ sinkReset)
    ,("C-1",                addName "Layout: 1 window, tabs"        $ setLayout $ XMonad.layoutHook conf)
    ,("C-2",                addName "Layout: 2 up combo 1/2"        $ setLayout $ XMonad.layoutHook conf)
    ,("C-3",                addName "Layout: 2 up combo 1/3"        $ setLayout $ XMonad.layoutHook conf)
    ,("C-4",                addName "Layout: 4 up grid"             $ setLayout $ XMonad.layoutHook conf)
    ] where
        sinkReset = do
            sinkAll
            sendMessage NextLayout -- ugly but otherwise window draw refresh doesn't work
            setLayout $ XMonad.layoutHook conf

-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-------------------------------------------------------------------- }}}
-- PROMPTS --------------------------------------------------------- {{{

-------------------------------------------------------------------- }}}
-- APPS & SCRATCHPADS ---------------------------------------------- {{{

-- TERMINAL -----------------------

myShell             = "$SHELL"
myTerminal          = "urxvtc"
terminalClass       = "URxvt"

initTerminal :: X ()
initTerminal        = spawn "pgrep urxvtd || urxvtd -f -o -q"

newTerminal :: X ()
newTerminal         = spawn myTerminal

newNamedTerminal :: String -> X ()
newNamedTerminal n  = spawn $ myTerminal ++ " -name " ++ show n

-- BROWSER ------------------------

browserClass          = "Chromium"
browserBase           = "chromium "
                        ++ " --memory-model=low"
                        ++ " --enable-smooth-scrolling"
                        ++ " --enable-sync-extensions"
                        ++ " --enable-webgl"
                        ++ " --ignore-gpu-blacklist"
                        ++ " --enable-print-preview"

myBrowser             = browserBase
                        ++ " --class=" ++ browserClass 
                        ++ " --name=chromium"

newBrowser :: X ()
newBrowser           = spawn $ myBrowser

nextBrowser          = raiseNextMaybe
                       (spawn $ myBrowser) (className =? browserClass)

-- SCRATCHPADS --------------------

toggle sp = namedScratchpadAction myScratchpads sp

spTerminal :: String -> String -> String -> String
spTerminal f a c    = myTerminal
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

-------------------------------------------------------------------- }}}
-- INITIALIZATION -------------------------------------------------- {{{

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHook = do
    initCompositor
    initStatusBar
    initSystemTray
    initAudioTray
    initNotifier
    initTerminal
    spawn "bloop up"

quitXmonad = do
    io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = do
    killAudioTray
    killSystemTray
    killStatusBar
    killCompositor
    killNotifier
    spawn "xmonad --restart"

unspawn :: String -> X ()
unspawn p = spawn $ "for pid in $(pgrep " ++ p ++ "); do kill -9 $pid; done" 

initSystemTray :: X ()
initSystemTray = spawn
    $  " pgrep trayer || trayer"
    ++ " --edge top"
    ++ " --align right"
    ++ " --SetDockType true"
    ++ " --SetPartialStrut true"
    ++ " --expand false"
    ++ " --widthtype percent"
    ++ " --width 6"
    ++ " --transparent true"
    ++ " --tint 0x073642"
    ++ " --alpha 0"
    ++ " --margin 0"
    ++ " --padding 0"
    ++ " --heighttype pixel"
    ++ " --height 20"

killSystemTray :: X ()
killSystemTray = unspawn "trayer"

initCompositor :: X ()
initCompositor = spawn
    $  " pgrep compton || compton -f -D 6 -m 0.95"
    ++ " --vsync drm"
    ++ " --unredir-if-possible"
    ++ " --detect-transient"
    ++ " --detect-client-leader"
    ++ " --use-ewmh-active-win"
    ++ " --paint-on-overlay"
    -- or: "xcompmgr -f -D 6" "cairo-compmgr"

killCompositor :: X ()
killCompositor = unspawn "compton"


initNotifier :: X ()
initNotifier = spawn "pgrep dunst || dunst"

killNotifier :: X ()
killNotifier = unspawn "dunst"


initStatusBar :: X ()
initStatusBar = spawn "pgrep xmobar || xmobar ~/etc/xorg/.xmobarrc-minimal"

killStatusBar :: X ()
killStatusBar = unspawn "xmobar"

initAudioTray :: X ()
initAudioTray = spawn   "pgrep pasystray || pasystray &"

killAudioTray :: X ()
killAudioTray = unspawn "pasystray"

initWallpaper :: X ()
initWallpaper = spawn   "~/bin/live"

initFileManager :: X ()
initFileManager = spawn "pgrep spacefm || spacefm -d"

flash :: String -> X ()
flash s = spawn $ "flash "++ s

notify :: String -> X ()
notify s = spawn $ "notify "++ s

alert :: String -> X ()
alert s = spawn $ "alert "++ s

warn :: String -> X ()
warn s = spawn $ "warn "++ s


-------------------------------------------------------------------- }}}
-- EVENT HANDLING -------------------------------------------------- {{{

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.

myEventHook = fullscreenEventHook <+> docksEventHook

-------------------------------------------------------------------- }}}
-- LAYOUTS --------------------------------------------------------- {{{

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

myLayout = (fullscreenFloat . fullscreenFull) $ noBorders $ avoidStruts $ windowNavigation

    $ tabs ||| borderTabs ||| combotabs ||| combotabsth ||| Mirror tiled ||| smartFull

    where

        tiled   = Tall nmaster delta ratio

        tabs        = renamed [Replace "Tabs"] 
                    $ spacing 1 $ dragTabs $ noBorders $ Simplest

        borderTabs  = renamed [Replace "Border Tabs"] 
                    $ spacing 1 $ dragTabs $ Simplest

        simpleTabs  = renamed [Replace "Simple Tabbed"] 
                    $ addTabs $ noBorders $ Simplest

        combotabs   = renamed [Replace "Read/Write"]
                    $ combineTwoP (TwoPane 0.03 0.5) borderTabs borderTabs
                    (ClassName browserClass `Or` ClassName "PDFViewer")
                    -- TODO: add other common source/reference items here,
                    -- such as man pages, etc.

        combotabsth = renamed [Replace "Read/Note"]
                    $ combineTwoP (TwoPane 0.03 0.33) borderTabs borderTabs
                    (ClassName browserClass `Or` ClassName "PDFViewer")

        smartFull   = noBorders $ Full

        -- addTabs uses the official X.L.TabBarDecoration
        addTabs  l  = tabBar shrinkText tabTheme Top 
                    $ resizeVertical (fi $ decoHeight tabTheme) $ l

        -- dragTabs uses custom X.L.TabbedWindowSwitcherDecoration
        dragTabs l  = tabbedWindowSwitcherDecorationWithImageButtons 
                      shrinkText myTabbedThemeWithImageButtons 
                      (draggingVisualizer $ l)

        -- dragBars uses either official X.L.WindowSwitcherDecoration
        -- or (this case) custom X.L.TabbedWindowSwitcherDecoration
        dragBars l  = windowSwitcherDecorationWithImageButtons 
                      shrinkText myTiledThemeWithImageButtons 
                      (draggingVisualizer $ l)

        nmaster     = 1      -- default num of windows in master pane
        ratio       = 1/2    -- proportion of screen for master pane
        delta       = 3/100  -- increment % of scrn when resizing panes

-------------------------------------------------------------------- }}}
-- WORKSPACES ------------------------------------------------------ {{{

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-------------------------------------------------------------------- }}}
-- WINDOW MANAGEMENT ----------------------------------------------- {{{

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
    <+> manageDocks

-------------------------------------------------------------------- }}}
-- OLD ------------------------------------------------------------- {{{

-- myFocusFollowsMouse :: Bool
-- myFocusFollowsMouse = True

-- myClickJustFocuses :: Bool
-- myClickJustFocuses = False

-- Width of the window border in pixels.
--
-- myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--

--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

-- Border colors for unfocused and focused windows, respectively.
--
-- myNormalBorderColor  = "#dddddd"
-- myFocusedBorderColor = "#ff0000"

-- ------------------------------------------------------------------------
-- -- Key bindings. Add, modify or remove key bindings here.
-- --
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
-- 
--     -- launch a terminal
--     [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
-- 
--     -- launch dmenu
--     , ((modm,               xK_p     ), spawn "dmenu_run")
-- 
--     -- launch gmrun
--     , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
-- 
--     -- close focused window
--     , ((modm .|. shiftMask, xK_c     ), kill)
-- 
--      -- Rotate through the available layout algorithms
--     , ((modm,               xK_space ), sendMessage NextLayout)
-- 
--     --  Reset the layouts on the current workspace to default
--     , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
-- 
--     -- Resize viewed windows to the correct size
--     , ((modm,               xK_n     ), refresh)
-- 
--     -- Move focus to the next window
--     , ((modm,               xK_Tab   ), windows W.focusDown)
-- 
--     -- Move focus to the next window
--     , ((modm,               xK_j     ), windows W.focusDown)
-- 
--     -- Move focus to the previous window
--     , ((modm,               xK_k     ), windows W.focusUp  )
-- 
--     -- Move focus to the master window
--     , ((modm,               xK_m     ), windows W.focusMaster  )
-- 
--     -- Swap the focused window and the master window
--     , ((modm,               xK_Return), windows W.swapMaster)
-- 
--     -- Swap the focused window with the next window
--     , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
-- 
--     -- Swap the focused window with the previous window
--     , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
-- 
--     -- Shrink the master area
--     , ((modm,               xK_h     ), sendMessage Shrink)
-- 
--     -- Expand the master area
--     , ((modm,               xK_l     ), sendMessage Expand)
-- 
--     -- Push window back into tiling
--     , ((modm,               xK_t     ), withFocused $ windows . W.sink)
-- 
--     -- Increment the number of windows in the master area
--     , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
-- 
--     -- Deincrement the number of windows in the master area
--     , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
-- 
--     -- Toggle the status bar gap
--     -- Use this binding with avoidStruts from Hooks.ManageDocks.
--     -- See also the statusBar function from Hooks.DynamicLog.
--     --
--     -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
-- 
--     -- Quit xmonad
--     , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
-- 
--     -- Restart xmonad
--     , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
-- 
--     -- Run xmessage with a summary of the default keybindings (useful for beginners)
--     , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
--     ]
--     ++
-- 
--     --
--     -- mod-[1..9], Switch to workspace N
--     -- mod-shift-[1..9], Move client to workspace N
--     --
--     [((m .|. modm, k), windows $ f i)
--         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--     ++
-- 
--     --
--     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
--     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--     --
--     [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--         | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

