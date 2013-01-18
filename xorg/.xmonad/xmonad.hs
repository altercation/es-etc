-- Ethan Schoonover's XMonad Config File
--
-- es@ethanschoonover.com  /  @ethanschoonover  / http://ethanschoonover.com
-- Available at: http://github.com/altercation/es-etc/

-- IMPORTS --------------------------------------------------------- {{{

-- `! sort -k1.18` in vim for a sort of these that ignores "qualified"

import           Data.Char                                      -- toLower, et al
import           Data.List                                      -- isInfixOf, et al
import qualified Data.Map as M                                  -- basics
import           Data.Monoid                                    -- basics
import           System.Directory                               -- for killall prompt, et al
import           System.Exit                                    -- basics
import           System.IO                                      -- for killall prompt, et al
import           XMonad hiding ((|||))                          -- basics (hiding ||| for X.A.CycleSelected)
import           XMonad.Actions.CopyWindow                      -- copy a window to multiple workspaces
import           XMonad.Actions.CycleSelectedLayouts            -- cycle through subset of available layouts
import           XMonad.Actions.CycleWS                         -- move to workspaces / screens
import           XMonad.Actions.GroupNavigation                 -- used for history navigation
import           XMonad.Actions.Navigation2D                    -- moving around in 2d space instead of stack
import           XMonad.Actions.TopicSpace                      -- workspaces with directories and actions
import           XMonad.Actions.UpdatePointer                   -- cursor management, useful changing screens
import           XMonad.Actions.WindowGo                        -- raising/going to windows
import           XMonad.Actions.WithAll                         -- actions on multiple windows (killAll, etc.)
import           XMonad.Hooks.CurrentWorkspaceOnTop             -- ensure win dragged between screens staus on top
import           XMonad.Hooks.DynamicLog                        -- support for "untethered" xmobar
import           XMonad.Hooks.FadeInactive                      -- fade out windows that aren't currently active
import           XMonad.Hooks.InsertPosition                    -- config where new windows insert
import           XMonad.Hooks.ManageDocks                       -- avoid statusbar/systray
import           XMonad.Hooks.ManageHelpers                     -- manage dialogs, etc.
import           XMonad.Hooks.UrgencyHook                       -- visually alert urgency in status bar
import           XMonad.Layout.Combo                            -- combine multiple layouts
import           XMonad.Layout.ComboP                           -- combine layouts with designated master apps
import           XMonad.Layout.Decoration                       -- themes, decorated layouts
import           XMonad.Layout.DraggingVisualizer               -- see windows when dragging them
import           XMonad.Layout.Drawer                           -- used for mail layout
import           XMonad.Layout.Fullscreen                       -- fullscreen mgmt
import           XMonad.Layout.LayoutCombinators                -- JumpToLayout, support X.A.CycleSelectedLayouts
import           XMonad.Layout.NoBorders                        -- useful with fullscreen windows
import           XMonad.Layout.PerWorkspace                     -- different default workspaces per workspace
import           XMonad.Layout.Renamed                          -- rename layouts for clarity
import           XMonad.Layout.SideSpacing                      -- customized version of X.L.Spacing (CUSTOM)
import           XMonad.Layout.Simplest                         -- super simple, used for my tabbed layout
import           XMonad.Layout.TabBarDecoration                 -- themes, decorated layouts
import           XMonad.Layout.TabbedWindowSwitcherDecoration   -- window title bars (CUSTOM)
import           XMonad.Layout.TwoPane                          -- simple two pane layout used w/X.L.Combo(P)
import           XMonad.Prompt                                  -- general prompt module
import           XMonad.Prompt.Input                            -- using for killall prompt, et al
import           XMonad.Prompt.RunOrRaise                       -- run apps
import           XMonad.Prompt.Shell                            -- shell prompt, natch
import           XMonad.Prompt.Workspace                        -- prompt go to / shift to workspace
import qualified XMonad.StackSet as W                           -- basics
import           XMonad.Util.EZConfig                           -- clean keybindings
import           XMonad.Util.Image                              -- for window decoration icons
import           XMonad.Util.NamedActions                       -- self documenting keybindings (CUSTOM)
import           XMonad.Util.NamedScratchpad                    -- summon/dismiss running app windows
import           XMonad.Util.Run                                -- used for runInTerm
import           XMonad.Util.SpawnOnce                          -- startup, etc.
import           XMonad.Util.WorkspaceCompare                   -- see workspace keybinding cycling below

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
    workspaces          = myTopics,
    normalBorderColor   = myNormalBorderColor,
    focusedBorderColor  = myFocusedBorderColor,
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

-- 'active' value changes the entire highlight "theme"
-- including xmobar, window/tab drag bars, prompts, etc.
-- active  = blue
active  = green

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 0

-- bug in compositing? #000000 ends up transparent, which is nice
myNormalBorderColor       = "#000000"
myFocusedBorderColor      = base03  

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

tabThemeDim :: Theme
tabThemeDim = baseTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = base03
    , activeBorderColor     = base03
    , activeTextColor       = base01
    }

tabTheme :: Theme
tabTheme = baseTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = active
    , activeBorderColor     = base03 
    , activeTextColor       = base03
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
      activeColor           = active
    , activeBorderColor     = active
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

-- alternate color scheme
myPromptConfig = basePromptConfig
    {font               = myFontBig
    ,bgColor            = active
    ,fgColor            = base03
    ,fgHLight           = active
    ,bgHLight           = base03
    ,borderColor        = active}

basePromptConfig :: XPConfig
basePromptConfig = defaultXPConfig
    {font               = myFontBig
    ,bgColor            = base02
    ,fgColor            = base0
    ,fgHLight           = base03
    ,bgHLight           = active
    ,borderColor        = base03
    ,promptBorderWidth  = 1
    ,height             = 22
    ,autoComplete       = Just 100
    -- default false? , showCompletionOnTab   = True     
    ,searchPredicate    = isInfixOf . (map toLower)
    ,promptKeymap       = M.fromList
                            [((controlMask, xK_space), quit)
                            ,((controlMask, xK_h), quit)
                            ,((controlMask, xK_j), quit)
                            ,((controlMask, xK_k), quit)
                            ,((controlMask, xK_l), quit)
                            ,((controlMask, xK_semicolon), quit)
                            ,((controlMask, xK_apostrophe), quit)
                            ,((mod1Mask .|. controlMask, xK_semicolon), quit)
                            ,((mod1Mask .|. controlMask, xK_apostrophe), quit)]
                            `M.union` promptKeymap defaultXPConfig
    }


-------------------------------------------------------------------- }}}
-- STATUS BARS & LOGGING ------------------------------------------- {{{

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

myPP = defaultPP
    { ppCurrent             = xmobarColor base02 active . wrap " " " "
    , ppTitle               = xmobarColor active "" . shorten 40
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
    -- fadeInactiveCurrentWSLogHook 0xeeeeeeee -- 0xbbbbbbbb
    currentWorkspaceOnTop
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
    ++ keysSystem c
    ++ keysSystemCodes c
--    ++ keysMedia c

keysXmonad conf =

    (subtitle "KILL & RESTART":) $ mkNamedKeymap conf $

    [("C-<Backspace>",      addName "Kill current window"           $ kill1)
    ,("C-S-<Backspace>",    addName "Kill all windows on workspace" $ killAll)
    ,("C-M-<Backspace>",    addName "Restart XMonad"                $ restartXmonad)
    ,("C-M-S-<Backspace>",  addName "Quit XMonad"                   $ quitXmonad)
    ]

keysWindows conf =

    (subtitle "WINDOWS":) $ mkNamedKeymap conf $

    [("C-j",                addName "GO next window"                $ windows W.focusDown)
    ,("C-k",                addName "GO previous window"            $ windows W.focusUp)
    ,("C-h",                addName "GO other side of combo"        $ windowGo L True)
    ,("C-l",                addName "GO to next nonempty workspace" $ nextNonEmptyWS)
    ,("C-p",                addName "GO previously active window"   $ nextMatch History (return True))

    ,("C-S-j",              addName "MOVE next window"              $ windows W.swapDown)
    ,("C-S-k",              addName "MOVE previous window"          $ windows W.swapUp)
    ,("C-S-h",              addName "MOVE other side of combo"      $ sendMessage $ SwapWindow)
    ,("C-S-l",              addName "MOVE last active window"       $ nextNonEmptyWS)

    ,("C-<Left>",           addName "Go left"                       $ windowGo L True)
    ,("C-<Right>",          addName "Go right"                      $ windowGo R True)
    ,("C-<Up>",             addName "Go up"                         $ windowGo U True)
    ,("C-<Down>",           addName "Go down"                       $ windowGo D True)

    ,("C-S-<Left>",         addName "Swap left"                     $ windowSwap L True)
    ,("C-S-<Right>",        addName "Swap right"                    $ windowSwap R True)
    ,("C-S-<Up>",           addName "Swap up"                       $ windowSwap U True)
    ,("C-S-<Down>",         addName "Swap down"                     $ windowSwap D True)

    ,("C-m",                addName "Focus on master"               $ windows W.focusMaster)
--    ,("C-S-m",              addName "Swap with master"              $ windows W.swapMaster)
    ]

keysWorkspaces conf =

    (subtitle "WORKSPACES":) $ mkNamedKeymap conf $

    [("C-;",                addName "Go to workspace prompt"        $ gotoWSPrompt)
    ,("C-M-;",              addName "Send to workspace prompt"      $ shiftWSPrompt)
    ] where
        gotoWSPrompt   = workspacePrompt wsPromptConfig $ windows . W.view -- n.b. not W.greedyView
        shiftWSPrompt  = workspacePrompt wsPromptConfig $ windows . W.shift
        wsPromptConfig = myPromptConfig {searchPredicate = isPrefixOf . (map toLower) } 

keysScreens conf =

    (subtitle "SCREENS":) $ mkNamedKeymap conf $

    [("C-o",                addName "Focus to other screen"         $ goToOtherScreen)
    ,("C-S-o",              addName "Send window to other screen"   $ moveToOtherScreen)
    ,("C-M-S-o",            addName "Swap workspace w/other screen" $ swapNextScreen)
    ] where
        moveCursor          = updatePointer (Relative 0.99 0.99) -- X.A.UpdatePointer
        goToOtherScreen     = nextScreen >> moveCursor
        moveToOtherScreen   = shiftNextScreen >> nextScreen >> moveCursor
        chngNextScreen      = swapNextScreen  >> nextScreen >> moveCursor

keysMainApps conf =

    (subtitle "MAIN APPS":) $ mkNamedKeymap conf $

    [("C-<Return>",         addName "New terminal"                  $ spawn myTerminal)
    ,("C-M-<Return>",       addName "New terminal"                  $ currentTopicAction myTopicConfig)
    ,("C-\\",               addName "New broswer"                   $ spawn myBrowser)
    ]

keysSecondaryApps conf =

    (subtitle "OTHER APPS/SCRATCHPADS":) $ mkNamedKeymap conf $

    [("M-s",                addName "Spotify"                       $ toggleSP "spotify")
    ,("M-f",                addName "Filemanager"                   $ toggleSP "filemanager")
    ,("M-z",                addName "Zim wiki"                      $ toggleSP "wiki")
    ,("M-x",                addName "Audio Mixer"                   $ toggleSP "mixer")
    ,("M-e",                addName "Audio Equalizer"               $ toggleSP "equalizer")
    ,("M-p",                addName "Process monitor"               $ toggleSP "htop")
    ,("M-c",                addName "Calendar - week"               $ toggleSP "calweek")
    ,("M-S-c",              addName "Calendar - month"              $ toggleSP "calmonth")
    ,("M-v",                addName "Virtual Box Manager"           $ spawn    "VirtualBox")
    ,("M-w",                addName "Wifi connection menu"          $ toggleSP "wifi")
    ,("M-k",                addName "Kazam screen recorder"         $ spawn "kazam")
    ]

keysRunPromptSubmap conf =

    (subtitle "RUN PROMPT":) $ mkNamedKeymap conf $

    [(runPK "C-<Space>",    addName "Run or raise prompt"           $ myPrompt)
    ,(runPK "C-<Backspace>", addName "Kill Prompt"                  $ killAllPrompt)
    ,(runPK "C-<F7>",       addName "Display prompt"                $ displayPrompt)
    ,(runPK "M-;",          addName "New terminal 1"                $ spawn myTerminal)
    ,(runPK "M-C-;",        addName "New terminal 2"                $ spawn myTerminal)
    ,(runPK "M-S-;",        addName "New terminal 3"                $ spawn myTerminal)
    ,(runPK "M-<Return>",   addName "New term 1"                    $ spawn myTerminal)
    ,(runPK "M-S-<Return>", addName "New term 2"                    $ spawn myTerminal)
    ,(runPK "M-\\",         addName "New broswer 2"                 $ spawn myBrowser)
    ] where
        oldPrompt = runOrRaisePrompt myPromptConfig
        myPrompt = shellPrompt myPromptConfig {autoComplete = Nothing}
        runPK k = "C-<Space> " ++ k -- main prompt key trigger combination

keysLayouts conf =

    (subtitle "LAYOUTS":) $ mkNamedKeymap conf $
    [("C-'",                addName "Next layout"                   $ nextLayout)
    ,("C-S-'",              addName "Sink & refresh layout"         $ sinkAll >> refresh)
    ,("C-M-'",              addName "Sing & hard reset layout"      $ sinkReset)
    ,("C-M-1",              addName "Layout: 1 window, tabs"        $ goLayout "Magic Tabs")
    ,("C-M-2",              addName "Layout: Half & Half"           $ goLayout "Half & Half")
    ,("C-M-3",              addName "Layout: Read Write"            $ goLayout "Read Write")
    ,("C-M-4",              addName "Layout: maximum"               $ goLayout "Maximum")
    ,("C-M-f",              addName "Full Screen"                   $ fullScreen)
    ] where
        showStruts = sendMessage $ SetStruts [minBound .. maxBound] []
        hideStruts = sendMessage $ SetStruts [] [minBound .. maxBound]
        nextLayout = sendMessage NextLayout >> showStruts
        sinkReset = sinkAll >> showStruts >> (setLayout $ XMonad.layoutHook conf) >> refresh
        goLayout l = (sendMessage $ (JumpToLayout l)) >> showStruts
        fullScreen = (sendMessage $ JumpToLayout "Maximum") >> hideStruts

keysSystem conf =

    (subtitle "SYSTEM KEYS":) $ mkNamedKeymap conf $
    [("<XF86Sleep>",        addName "System sleep"          $ spawn "system sleep")
    ,("<XF86PowerOff>",     addName "System power off"      $ spawn "system off")
    ,("S-<XF86PowerOff>",   addName "System reboot"         $ spawn "system reboot")
    ,("<XF86ScreenSaver>",  addName "Lock screen"           $ spawn "displays lock")
    ,("<XF86Display>",      addName "Cycle display mode"    $ spawn "displays toggle")
    ,("S-<XF86Display>",    addName "Mirror display mode"   $ spawn "displays mirror")
    ,("M-<XF86Display>",    addName "Span display mode"     $ spawn "displays span")
    ,("<XF86Launch1>",      addName "Bluetooth toggle"      $ spawn "wireless bluetooth toggle")
    ,("<XF86TouchpadOn>",   addName "Trackpad toggle"       $ spawn "trackpad toggle")
    ,("<Print>",            addName "Screendraw start/stop" $ spawn "screendraw")
    ,("S-<Print>",          addName "Screendraw force stop" $ spawn "screendraw finish")
    ,("M-<Print>",          addName "Screendraw cancel"     $ spawn "screendraw cancel")
    ,("<XF86RotateWindows>", addName "Screendraw clear"     $ spawn "screendraw clear")
    ]

keysSystemCodes :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keysSystemCodes conf =

    [((0, btnBatt), addName "Toggle min/max power modes" $ spawn "power toggle")
    -- Pinned power modes suspend all screen saver and screen power saving for presentation/video
    ,((0 .|. shiftMask, btnBatt), addName "Pinned power modes" $ spawn "power toggle pinned")
    ,((0 .|. controlMask, btnBatt), addName "Auto power mode" $ spawn "auto power mode")]
    where
        btnBatt      = 0x1008ff93
        btnSuspend   = 0x1008ffa7

keysMedia conf =

    (subtitle "MEDIA KEYS":) $ mkNamedKeymap conf $
    -- c.f. key names at http://goo.gl/KqOA6

    [(volUpK,         addName"Volume up by 1"         $ volUp "1")
    ,(volDnK,         addName"Volume down by 1"       $ volDn "1")
    ,("S-" ++ volUpK, addName"Volume up by 10"        $ volUp "10")
    ,("S-" ++ volDnK, addName"Volume down by 10"      $ volDn "10")
    ,("C-" ++ volUpK, addName"Volume at 50%"          $ volMax)
    ,("C-" ++ volDnK, addName"Volume at maximum"      $ volMid)
    ,(volMtK,         addName"Volume mute toggle"     $ volTog)]
    where
        volUpK       = "<XF86AudioRaiseVolume>"
        volDnK       = "<XF86AudioLowerVolume>"
        volMtK       = "<XF86AudioMute>"
        volUp i      = spawn $ "volume up "   ++ i
        volDn i      = spawn $ "volume down " ++ i
        volMax       = spawn $ "volume max"
        volMid       = spawn $ "volume mid"
        volTog       = spawn $ "volume toggle"

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

-- some simpler prompts defined above in key bindings

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

toggleSP sp = namedScratchpadAction myScratchpads sp

spTerminal :: String -> String -> String -> String
spTerminal f a c    = myTerminal
                        ++ " -fn " ++ show f ++ " -fb " ++ show f
                        ++ " -fi " ++ show f ++ " -fbi " ++ show f
                        ++ " +sb " ++ " -b 15 " ++ a ++ " -e " ++ c
myScratchpads =
    [ NS "htop"         (spTerminal myFont "" "htop") (title =? "htop") centWin
    , NS "wifi"         (spTerminal myFont "-name wifi" "wifi") (resource =? "wifi") centSquare
    , NS "filemanager"  "spacefm" (className =? "Spacefm") nonFloating
    , NS "wiki"         "zim" (className =? "Zim") nonFloating
    , NS "spotify"      "spotify" (className =? "Spotify") centWinVBig
    , NS "notepad"      (spTerminal myFont "-name notepad" "vim")
                        (resource =? "notepad") centSquare
    , NS "mixer" "      pavucontrol" (className =? "Pavucontrol") centWinBig
    , NS "equalizer"    "pulseaudio-equalizer-gtk"
                        (className =? "Pulseaudio-equalizer.py") centWin
    , NS "calweek"      (spTerminal myFont ("-name calweek -cr " ++ show base03) "gcal view week")
                        (resource =? "calweek") centWinThin
    , NS "calmonth"     (spTerminal myFontSmall
                        ("-name calmonth -cr " ++ show base03) "gcal view month")
                        (resource =? "calmonth") centWinVBig
    ] where
        -- order of ratios: left-margin top-margin width height
        centWin     = (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
        centWinBig  = (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
        centWinVBig = (customFloating $ W.RationalRect (1/40) (1/20) (19/20) (9/10))
        centWinMax  = (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/1))
        centWinThin = (customFloating $ W.RationalRect (1/30) (1/4) (28/30) (1/2))
        centSquare  = (customFloating $ W.RationalRect (1/3) (1/4) (1/3) (1/2))
        lowerThird  = (customFloating $ W.RationalRect (0) (2/3) (1) (1/3))

-- TOPIC ACTIONS ------------------

comTA = do
        restartIMAP -- TODO: handle this better, in systemd directly or on wakeup?
        initMailLog
        initMail
        where
            restartIMAP = spawn "restart-offlineimap"
            initMail    = raiseMaybe (runInTerm "-title mail" "mutt") (title =? "mail")
            initMailLog = raiseMaybe (runInTerm "-title maillog" "tail -F ~/.offlineimaplog") (title =? "maillog")

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
    initMiscApps
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

initStalonetray :: X ()
initStalonetray = spawn
    $  " pgrep stalonetray || stalonetray"
    ++ " --dockapp-mode none"
    ++ " --background '#073642'"
    ++ " --icon-size 18"
    ++ " --slot-size 20"
    -- ++ " --sticky"
    ++ " --window-strut auto"
    ++ " --window-type dock"
    ++ " --geometry 6x1-1+0"
    ++ " --max-geometry 6x1"
    ++ " --icon-gravity W" -- NE"
    ++ " --grow-gravity W"
    ++ " --kludges fix_window_pos,force_icons_size" -- ,use_icon_hints"
    ++ " --window-layer bottom"

killStaloneTray :: X ()
killStaloneTray = unspawn "stalonetray"

initSystemTray :: X ()
initSystemTray = spawn "systray"
killSystemTray :: X ()
killSystemTray = spawn "systray" -- systray just repositions, so we do this early

initTrayer :: X ()
initTrayer = spawn
    $  " pgrep trayer || trayer"
    ++ " --edge top"
    ++ " --align right"
    ++ " --SetDockType true"
    ++ " --SetPartialStrut true"
    ++ " --expand false"
    ++ " --widthtype percent"
    ++ " --width 6"
    ++ " --tint 0x073642"
    ++ " --transparent true"
    ++ " --alpha 100" -- 0?
    ++ " --margin 0"
    ++ " --padding 0"
    ++ " --heighttype pixel"
    ++ " --height 20"

killTrayer :: X ()
killTrayer = unspawn "trayer"

initCompositor :: X ()
initCompositor = spawn
    $  " pgrep compton || compton -f -D 6 -m 0.95 -cCGz" -- add for shadows: "-cCG"
    ++ " --vsync drm"
    ++ " --unredir-if-possible"
    ++ " --detect-transient"
    ++ " --detect-client-leader"
    ++ " --paint-on-overlay"
    -- ++ " --blur-background-fixed"
    -- ++ " --use-ewmh-active-win"
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
killStatusBar = return () -- unspawn "xmobar"

initAudioTray :: X ()
initAudioTray = spawn   "pgrep pasystray || pasystray &"

killAudioTray :: X ()
killAudioTray = unspawn "pasystray"

runIfNot :: String -> String -> X ()
runIfNot c e = spawn $ "pgrep " ++ c ++ " || " ++ e

initMiscApps = do
    runIfNot "zim" "zim --plugin trayicon"
    runIfNot "dropbox" "dropboxd &"
    runIfNot "xscreensaver" "/usr/bin/xscreensaver -no-splash -no-capture-stderr &"

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

-- myLayout = (fullscreenFloat . fullscreenFull) $ avoidStruts $ noBorders

myLayout = avoidStruts -- $ smartBorders
     $ onWorkspaces ["com"] (mailCall ||| tileTall ||| maximum)
     $ onWorkspaces ["sys","pro"] (halfHalf ||| magicTabs ||| maximum ||| halfHalf)
     $ (magicTabs ||| halfHalf ||| tileTall ||| tileWide ||| maximum ||| halfHalf)
    where

--      -- noBorders if border width > 0
--      tileTallD   = renamed [Replace "Tiled Tall"]
--                    $ (addBarsD $ Tall nmaster delta halves)
--      tileWideD   = renamed [Replace "Tiled Wide"]
--                    $ (addBarsD $ Mirror $ Tall nmaster delta halves)

        -- smartBorders if border width > 0
        tileTall    = renamed [Replace "Tiled Tall"] $ (Tall nmaster delta halves)
        tileWide    = renamed [Replace "Tiled Wide"] $ (Mirror $ Tall nmaster delta halves)

        readWrite   = renamed [Replace "Read Write"] $ 
                      combineTwoP (TwoPane (3/100) (57/100)) tabs tabs -- 57/100 optimal?
                      (ClassName browserClass `Or` ClassName "PDFViewer")

        halfHalf    = renamed [Replace "Half & Half"] $ 
                      combineTwoP (TwoPane (3/100) (1/2)) tabs tabs -- 57/100 optimal?
                      (ClassName browserClass `Or` ClassName "PDFViewer")

        readNote    = renamed [Replace "Read Note"] $ 
                      stickyNotes `onRight` tabs -- 57/100 optimal?

--      readColumn  = renamed [Replace "Read Column"] $ 
--                    combineTwoP () tabs tabs 
--                    (ClassName browserClass `Or` ClassName "PDFViewer")

--      fixedColumn = renamed [Replace "Fixed 80"] $
--                    reflectHoriz $ limitWindows 2 $ FixedColumn 1 20 80 10

--      readWriteD  = renamed [Replace "Read Write Drag"] $ 
--                    combineTwoP (TwoPane 0.03 0.5) magicTabs magicTabs
--                    (ClassName browserClass `Or` ClassName "PDFViewer")

        tabs        = renamed [Replace "Tabs"] $ addTabs $ Simplest
        addTabs l   = spacing 1 $ tabBar shrinkText tabTheme Top 
                      $ resizeVertical (fi $ decoHeight tabTheme) $ l
                      -- w/normal X.L.TabBarDecoration

        magicTabs   = renamed [Replace "Magic Tabs"] $ addTabsD $ Simplest
        addTabsD l  = spacing 1 $ tabbedWindowSwitcherDecorationWithImageButtons 
                      shrinkText myTabbedThemeWithImageButtons (draggingVisualizer $ l)

--      addBarsD l  = spacing 1 $ windowSwitcherDecorationWithImageButtons 
--                    shrinkText myTiledThemeWithImageButtons 
--                    (draggingVisualizer $ l)

        maximum     = renamed [Replace "Maximum"]    $ Full
        mailCall    = renamed [Replace "Mail Call"]  $ drawer `onRight` magicTabs
        drawer      = simpleDrawer 0.0 0.3 (ClassName "Zim")
        notes       = simpleDrawer 0.0 0.3 (ClassName "Zim")
        stickyNotes = simpleDrawer (1/3) (1/2) (Not $ ClassName browserClass)

        nmaster     = 1      -- default num of windows in master pane
        halves      = 1/2    -- proportion of screen for master pane
        thirds      = 1/3    -- proportion of screen for master pane
        delta       = 3/100  -- increment % of scrn when resizing panes


-------------------------------------------------------------------- }}}
-- WORKSPACES ------------------------------------------------------ {{{

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.

notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
-- | any workspace but scratchpad
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
-- | hidden, non-empty workspaces less scratchpad
shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
-- | toggle any workspace but scratchpad
myToggle = windows $ W.view =<< W.tag . head . filter 
        ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

myTopics :: [Topic]
myTopics =
    ["com"
    ,"des"
    ,"gen"
    ,"irc"
    ,"org"
    ,"pro"
    ,"sys"
    ,"twt"
    ,"txt"
    ,"vid" -- av vid aud med
    ,"web"
    ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
    [ ("gen", "")
    , ("sys", "")
    , ("wrk.smt", "wrk/smt")
    , ("wrk.rws", "wrk/web/rws")
    ]
    , defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic = "dashboard"
    , topicActions = M.fromList $
        [ ("conf",       spawnShell >> spawnShellIn "wd/ertai/private")
        , ("darcs",      spawnShell >*> 3)
        , ("yi",         spawnShell >*> 3)
        , ("haskell",    spawnShell >*> 2 >> spawnShellIn "wd/dev-haskell/ghc")
        , ("xmonad",     spawnShellIn "wd/x11-wm/xmonad" >>
                         spawnShellIn "wd/x11-wm/xmonad/contrib" >>
                         spawnShellIn "wd/x11-wm/xmonad/utils" >>
                         spawnShellIn ".xmonad" >>
                         spawnShellIn ".xmonad")
        , ("irc",        spawn "weechat")
        , ("com",        comTA) -- offlineimap >> showMail) 
        , ("dashboard",  spawnShell)
        , ("twitter",    spawnShell)
        , ("web",        newBrowser)
        , ("sys",        spawnShell)
        , ("movie",      spawnShell)
        , ("documents",  spawnShell >*> 2 >>
                         spawnShellIn "Documents" >*> 2)
        ]
   }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvtc '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myPromptConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myPromptConfig $ windows . W.shift


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
    [ transience'
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isDialog                      --> doFloat ]
    <+> insertPosition Below Newer
    <+> manageDocks
    <+> namedScratchpadManageHook myScratchpads

-------------------------------------------------------------------- }}}
