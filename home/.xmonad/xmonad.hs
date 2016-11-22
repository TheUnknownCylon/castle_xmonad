---
-- My Xmonad config file
-- Not formatted as the often seen 'Haskell' way.
--
-- If you want to use this file, you probably want to change the command
--  piped to myRightBar to something else (conky script or so).
---
import Dzen

import XMonad
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run
import XMonad.Layout.Named
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Util.CustomKeys
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import XMonad.Hooks.InsertPosition

import XMonad.Layout.ResizableTile    -- Rezise
import XMonad.Layout.Simplest         -- Simple Layout
import Data.Map as M
import XMonad.Actions.Submap as SM
import XMonad.Actions.Search as S

import XMonad.Layout.BoringWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

-- General settings
modMask'   = mod4Mask             -- Xmonad default actions key: Left Alt key, which is mapped in xinitrc to the WIN key
terminal'  = "/usr/bin/xfce4-terminal"     -- Default terminal (ModMask + ENTER)
term_exec  = terminal' ++ " -e "  -- Command to execute terminal applications on
barBgColor = "#1B1D1E"
barHeight  = 13

-- Window border settings
myBorderWidth       = 1
normalBorderColor'  = "#3F3E3C"
focusedBorderColor' = "orange" -- "orange" -- "#5F5E5C",

-- Workspaces
workspaces' = ["1:main", "2:web", "3:multimedia", "4:chat", "5", "6", "7", "8", "9:_"]

--Layouts and layoutHook
gridLayout = named "-|-" $ avoidStruts $ GridRatio(4/3)

promptLayout = defaultXPConfig {
    XMonad.Prompt.bgColor = barBgColor,
    XMonad.Prompt.height  = barHeight ,
    position              = Top,
    promptBorderWidth     = 0
}

tabBar = defaultTheme {
    activeTextColor     = "#ebac54",
    activeColor         = barBgColor,
    inactiveColor       = barBgColor,
    fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*",
    decoHeight          = 15,
    activeBorderColor   = focusedBorderColor',
    inactiveBorderColor  = "#804000"
}


layoutHook' = lessBorders XMonad.Layout.NoBorders.Never $ avoidStruts $ windowNavigation $  addTabs shrinkText tabBar $ subLayout [] Simplest $ boringWindows $
        tall ||| mtile
    where
        tall = Tall 1 (3/100) (1/2)
        mtile = Mirror tall


--Xmonad 'status bar'
logHook' h = dynamicLogWithPP $ defaultPP {
    ppCurrent = dzenColor "#ebac54" barBgColor . pad,
    ppVisible = dzenColor "yellow" barBgColor . pad,
    ppHidden = dzenColor "white" barBgColor . pad,
    ppHiddenNoWindows = dzenColor "#7b7b7b" barBgColor . pad,
    ppUrgent = dzenColor "red" barBgColor . pad,
    ppWsSep = "",
    ppSep = " | ",
    ppLayout = dzenColor "#ebac54" barBgColor,
    ppTitle = dzenColor "white" barBgColor . dzenEscape,
    ppOutput = hPutStrLn h
}


--Custom keys
delkeys XConfig {modMask = modMask'} = []
inskeys conf@(XConfig {modMask = modMask'}) = [
    ((modMask',               xK_p        ), runOrRaisePrompt promptLayout),

    ---------------------
    -- Sublayout controls
    ((modMask' .|. mod1Mask, xK_h), sendMessage $ pullGroup L), --take left window, add to own sublayout group
    ((modMask' .|. mod1Mask, xK_l), sendMessage $ pullGroup R), --take right window, ''
    ((modMask' .|. mod1Mask, xK_k), sendMessage $ pullGroup U), --take upper window, ''
    ((modMask' .|. mod1Mask, xK_j), sendMessage $ pullGroup D), --take down window, ''

    ((modMask' .|. mod1Mask, xK_m), withFocused (sendMessage . MergeAll)), --put all windows in a single sublyout
    ((modMask' .|. mod1Mask, xK_u), withFocused (sendMessage . UnMerge)),  --remove the current window from its sublayout

    ---- cursor movements (requires boringWindows)
    ((modMask', xK_comma), onGroup W.focusUp'),  -- move to next window in sublayout
    ((modMask', xK_m), onGroup W.focusDown'),    -- move to previous window in sublayout
    ((modMask', xK_period), spawn ""),           -- remove default behaviour for the dot

    ---- move to next visible window, outside the current sublayout
    ((modMask', xK_Tab), focusDown),             -- move to next visible window
    ((modMask', xK_j), focusDown),               -- move to next visible window
    ((modMask', xK_k), focusUp),                 -- move to previous visible window

    -- increase or decrease number of windows in the master area
    ((modMask', xK_equal), sendMessage (IncMasterN 1)),     -- Increment the number of windows in the master area
    ((modMask', xK_minus), sendMessage (IncMasterN (-1))),  -- Deincrement the number of windows in the master area

    ------------------------
    -- application shortcuts
    ((modMask' .|. shiftMask, xK_e        ), spawn (term_exec ++ "~/programs/chat")),  -- weechat over SSH
    ((modMask' .|. shiftMask, xK_w        ), spawn (term_exec ++ "wicd-curses")),      -- start wicd-curses
    ((modMask' .|. shiftMask, xK_a        ), spawn (term_exec ++ "alsamixer")),        -- start alsamixer
    ((modMask' .|. shiftMask, xK_t        ), spawn (term_exec ++ "htop")),             -- start htop
    ((modMask' .|. shiftMask, xK_d        ), spawn ("dolphin")),                       -- start dolphin
    ((modMask' .|. shiftMask, xK_s        ), spawn ("systemctl suspend  slock")),
    ((modMask'              , xK_d        ), spawn ("autorandr --change")),
    ((modMask' .|. shiftMask, xK_x        ), spawn ("xkill")),                         -- xkill mouse pointer

    ((modMask', xK_s), SM.submap $ searchEngineMap $ S.promptSearch promptLayout),

    ((0,                      0x1008ff2a  ), spawn "sudo pm-suspend"),                 -- XF86PowerOff
    ((0,                      0x1008ff02  ), spawn "xbacklight +20"),                  -- XF86MonBrightnessUp
    ((0,                      0x1008ff03  ), spawn "xbacklight -20"),                  -- XF86MonBrightnessDown
    ((0,                      0x1008ff12  ), spawn "amixer -q sset Master toggle"),    -- XF86AudioMute
    ((0,                      0x1008ff11  ), spawn "amixer -q sset Master 6-"),        -- XF86AudioLowerVolume
    ((0,                      0x1008ff13  ), spawn "amixer -q sset Master 6+ unmute"), -- XF86AudioRaiseVolume
    ((0,                      0x1008ff05  ), spawn "~/bin/keyboard.sh incr"),          -- xK_XF86KbdBrightnessUp
    ((0,                      0x1008ff06  ), spawn "~/bin/keyboard.sh decr")           -- xK_XF86KbdBrightnessDown
    ]

searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google)
       , ((0, xK_h), method S.hoogle)
       , ((0, xK_w), method S.wikipedia)
       ]


-- Manage hook: how new windows are placed (workspace and/or floating)
doFullFloat' = doF W.focusDown <+> doFullFloat
myManageHook = manageDocks <+> (composeAll . concat $
    [ [resource     =? r            --> doIgnore           |   r   <- ignores ],
      [className    =? c            --> doCenterFloat      |   c   <- floats  ],
      [isFullscreen                 --> doFullFloat'                          ]
    ])

    where
        floats  = ["MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        ignores = ["desktop","desktop_window","notify-osd","stalonetray", "obshutdown"]


--Xmonad main loop, all comes together here!
main = do
    statusBar <- spawnPipe "~/programs/pybar/mybar2.py"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        borderWidth = myBorderWidth,
        normalBorderColor  = normalBorderColor',
        focusedBorderColor = focusedBorderColor',

        XMonad.keys       = customKeys delkeys inskeys,

        layoutHook = layoutHook',
        logHook    = takeTopFocus >> logHook' statusBar,
        workspaces = workspaces',
        manageHook = myManageHook,
        modMask    = modMask',
        terminal   = terminal'
    }

