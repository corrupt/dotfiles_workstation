-- imports {{{
import XMonad hiding ( (|||) ) -- don't import the ||| operator, it comes in layoutcombinators
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.Plane
import XMonad.Actions.SpawnOn (shellPromptHere, mkSpawner, manageSpawn)
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName --hopefully making matlab run
import XMonad.Hooks.UrgencyHook
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.Cross
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.HintedTile
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier hiding (Toggle)
import XMonad.Layout.Maximize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle (Toggle(..), mkToggle, single)
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
--import XMonad.Layout.TabBarDecoration
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.Dmenu
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Run
--import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

import Graphics.X11.Xlib.Display (displayWidth)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.IO
import System.Process
import Data.List
import Data.Maybe ( catMaybes, isJust )

-- }}}

-- hooks {{{
myManageHook = (composeAll . concat $
    [ [className =? c --> doF (W.shift "web") | c <- myWebs]
    , [className =? c --> doF (W.shift "img") | c <- myImgs]
    , [className =? c --> doF (W.shift "im") | c <- myIms]
    , [className =? c --> doF (W.shift "fm") | c <- myFms]
    , [className =? c --> doF (W.shift "laut") | c <- myLauts]
    , [className =? c --> doF (W.swapDown) | c <- mySwapDowns]
    , [className =? c --> doFloat | c <- myFloats]
    , [name      =? c --> doFloat | c <- myFloats]
    , [name      =? c --> doFullFloat | c <- myFullFloats]
    , [name      =? c --> doCenterFloat | c <- myCenterFloats]
    , [className =? c --> doIgnore | c <- myIgnores]
    ]) <+> mySpecialHooks
    where
       myWebs         = ["Firefox", "Iron"]
       myImgs         = ["Gimp", "Gimp-2.6"]
       myIms          = ["Pidgin", "Skype", "Kopete"]
       myFms          = ["pcmanfm", "Krusader", "Dolphin"]
       myLauts        = ["Amarok"]
       mySwapDowns    = ["Skype", "Pidgin", "Kopete"]
       myFloats       = ["Truecrypt", ".", "Download", "Downloads", "Akregator", "Amarok", "Gmail Manager Login"]
       myFullFloats   = ["Liferea"]
       myCenterFloats = ["Copying", "Copying files"]
       myIgnores      = ["Photoshop.exe", "VCLSalFrame"]

       mySpecialHooks = composeAll
            [ (role =? "gimp-toolbox" <||> 
               role =? "gimp-image-window" <||> 
               role =? "gimp-dock")
               --> (ask >>= doF . W.sink)
            , isFullscreen --> doFullFloat -- fullscreen flash and stuff
            , transience' -- focus parent windows of transient ones
            , name =? "Eclipse" --> doFloat --The Eclipse splash and hopefully not the main window
            ]
        
       role = stringProperty "WM_WINDOW_ROLE"
       name = stringProperty "WM_NAME"


newManageHook sp   = manageSpawn sp -- windows spawned by shell promt spawn on their workspace
                 <+> manageDocks
                 <+> myManageHook
                 <+> manageHook defaultConfig

myLayoutHook = onWorkspace "im"  (named "myIM" imlayout)
             $ onWorkspace "img" (named "myImg" gimp)
             $ mkToggle (single REFLECTX)
             $ named "myTall" (myLayoutMods tiled)
             ||| (named "myMirrorTall"  $ myLayoutMods mouseResizableTileMirrored)
             ||| (named "myMagnifyTall" $ myLayoutMods $ magnify tiled)
             ||| (named "myFull"        $ myLayoutMods $ noBorders Full)
    where
      tiled    = mouseResizableTile
      imlayout = avoidStruts 
               $ withIM (0.137) (ClassName "Pidgin")
               $ reflectHoriz 
               $ ((withIM (0.159) (ClassName "Skype") (reflectHoriz tiled )))
      gimp     = avoidStruts 
               $ withIM (0.15) (Role "gimp-toolbox") 
               $ reflectHoriz
               $ withIM (0.2) (Role "gimp-dock") tiled
      magnify  = magnifiercz (1.2)
      myLayoutMods x = smartBorders
                     $ maximize
                     $ avoidStruts
                     $ layoutHintsWithPlacement (0.5, 0.5) x --center in bounding box


-- I haven't found out whether to set LG3D in startup or logHook. So I set them both
myLogHook = ewmhDesktopsLogHook >> setWMName "LG3D"
myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
--}}}

-- defines {{{

-- mtrDragger stuff for window resizing with the mouse
mrtDraggerOffset :: Position
mrtDraggerOffset = 1

mrtDraggerSize :: Dimension
mrtDraggerSize = 12

myWorkspaces = ["main", "web", "fm", "im", "img", "laut"] ++ map show [7 .. 8 :: Int]

escapeColor :: String -> String
escapeColor = wrap "'" "'"

myModMask = mod4Mask
myFont = "'-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*'"
--myFont = "'-*-terminus-*-r-normal-*-10-*-*-*-*-*-uni-*'"

-- myFgColor = "#59bbe8"
-- myBgColor = "#0d0d0d"
-- myFontColor = "#cccccc"
--
-- Zenburn
--myFgColor = "#d7d7af"
--myBgColor = "#1c1c1c"
--myFontColor = "#7f9f7f"
--myHighlightColor = "#d7d7af"
myFgColor   = "#418BD4"
myBgColor   = "#2E3436"
myFontColor = "#BFBFBF"
myHighlightColor = "#418BD4"

myFocusedBorderColor = myFgColor
myNormalBorderColor  = myBgColor

-- myTabTheme = Theme 
--     {   activeColor       = myBgColor
--     ,   inactiveColor     = myBgColor
--     ,   urgentColor       = myFgColor
--     ,   activeBorderColor = myFgColor
--     ,   urgentBorderColor = myFgColor
--     ,   activeTextColor   = myFgColor
--     ,   inactiveTextColor = myFontColor
--     ,   urgentTextColor   = myBgColor
--     ,   fontName          = myFont
-- --    ,   decoWidth         = 0
--     ,   decoHeight        = 15
--     }

myPanelHeight = "16"
myPanelY      = "0"
myTerminal    = "urxvt"

myMainPanelWidth  = "560"
myConkyPanelWidth = "924"
myTrayerWidth     = "110"
myTrayerMargin    = "1680" --mainpanel + conkypanel

myTray = "tint2"

myTrayCmd = myTray

--myTrayCmd = myTray 
--            ++ " --edge top "
--            ++ " --align left "
--            ++ " --margin " ++ myTrayerMargin
--            ++ " --height " ++ myPanelHeight
--            ++ " --widthtype request "
--            ++ " --transparent true "
--            ++ " --alpha 0 "
--            ++ " --tint 0x1A1A1A "
--            ++ " --SetPartialStrut true "

myDzenFlags = " -bg " ++ escapeColor myBgColor
            ++ " -fg " ++ escapeColor myFontColor
            ++ " -e 'onstart=lower' "
            ++ " -h " ++ myPanelHeight
            ++ " -fn " ++ myFont
            ++ " -sa c "
            ++ " -y " ++ myPanelY
            ++ " -xs 2 "

statusBarCmd = "dzen2 "
             ++ myDzenFlags
             ++ " -w " ++ myMainPanelWidth
             ++ " -ta l "

secondBarCmd = "conky -c ~/.xmonad/conkyrc | dzen2 "
             ++ myDzenFlags
             ++ " -w " ++ myConkyPanelWidth
             ++ " -x " ++ myMainPanelWidth
             ++ " -ta r "

myXcompmgrCmd = "xcompmgr"

myDmenuString = "dmenu_run "
              ++ " -fn " ++ myFont
              ++ " -nb " ++ escapeColor myBgColor
              ++ " -nf " ++ escapeColor myFontColor
              ++ " -sb " ++ escapeColor myFgColor
              ++ " -sf " ++ escapeColor myBgColor
              ++ " -i " -- match case-insensitively
-- }}}

-- The prompt config {{{
myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { position = Top
    , promptBorderWidth = 0
    , font = myFont
    , height = (read myPanelHeight)::Dimension
    , bgColor = myBgColor
    , fgColor = myFontColor
    , bgHLight = myFgColor
    , fgHLight = myBgColor
    , autoComplete = Nothing
    }
-- }}}

-- the keys config {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) sp = M.fromList $
    [ ((modm, xK_p), spawn myDmenuString)
    , ((modm, xK_b), sendMessage ToggleStruts)
    -- , ((modm, xK_y), sendMessage MirrorShrink) --resizableTall keys
    -- , ((modm, xK_a), sendMessage MirrorExpand)
    , ((modm, xK_y), sendMessage ShrinkSlave) --mouseResizableTile keys
    , ((modm, xK_a), sendMessage ExpandSlave)
    , ((modm .|. shiftMask, xK_l), spawn ("xlock"))
    , ((modm .|. shiftMask, xK_x), spawn ("alock -auth pam"))
    , ((modm .|. shiftMask, xK_d), spawn ("pcmanfm"))
    , ((modm .|. shiftMask, xK_f), spawn ("firefox"))
    , ((modm .|. shiftMask, xK_a), spawn ("amarok"))
    , ((modm .|. shiftMask, xK_p), spawn ("pidgin"))
    , ((modm, xK_numbersign), spawn ("liferea"))
    , ((modm, xK_q), spawn (  "killall dzen2;"
                           ++ "killall conky;"
                           ++ "killall " ++ myTray ++ ";"
                           ++ "killall xcompmgr;"
                           ++ "xmonad --recompile && xmonad --restart"
                           ))
    , ((modm, xK_F1), (sendMessage $ JumpToLayout "myTall"))
    , ((modm, xK_F2), (sendMessage $ JumpToLayout "myMirrorTall"))
    , ((modm, xK_F3), (sendMessage $ JumpToLayout "myMagnifyTall"))
    , ((modm, xK_F4), (sendMessage $ JumpToLayout "myFull"))
    , ((modm, xK_F5), (sendMessage $ JumpToLayout "myCross"))
    , ((modm, xK_F8), (sendMessage $ JumpToLayout "multimedia"))
    , ((modm, xK_F11), spawn "killall xcompmgr")
    , ((modm, xK_F12), spawn ("killall xcompmgr;" ++ myXcompmgrCmd))
    , ((modm, xK_u), withFocused (sendMessage . maximizeRestore))
    , ((modm, xK_g), withFocused toggleBorder)
    , ((modm, xK_o), shellPromptHere sp myPromptConfig)
    , ((modm, xK_m), sendMessage $ Toggle REFLECTX) 
    , ((modm, xK_Tab), goToSelected defaultGSConfig)
    ]
-- }}}
   
-- the PP config {{{

imagePath = "/home/corrupt/.xmonad/images/"

ppCurrentColor = dzenColor "#1a1a1a" myHighlightColor 
--ppVisibleColor = dzenColor myFocusedBorderColor ""
ppVisibleColor = dzenColor myHighlightColor ""
ppHiddenColor = dzenColor myFontColor ""
ppHiddenNWColor = dzenColor myFontColor ""
ppLayoutColor = dzenColor myFontColor ""
ppTitleColor = dzenColor myFontColor ""
ppUrgentColor = dzenColor "#1a1a1a" myFontColor

myPP = dzenPP
    { ppCurrent = ppCurrentColor . \a -> setBgColor ++ image "window-active" ++ a ++ "^fg(" ++ myHighlightColor ++ ")" ++ image "vspace"
    , ppVisible = ppVisibleColor . wrapClickable . (\a -> (a,a))
    , ppHidden  = ppHiddenColor  . wrapClickable . (\a -> (a,setFgColor ++ image "window" ++ setTextColor ++ a))
    , ppHiddenNoWindows = ppHiddenNWColor . wrapClickable . (\wsId -> (wsId,if (':' `elem` wsId) then drop 2 wsId else wsId))
    , ppUrgent  = ppUrgentColor  . wrapClickable . (\a -> (a,image "window-active" ++ a ++ setTextColor ++ image "vspace")) . dzenStrip 
    , ppLayout  = ppLayoutColor  . wrapLayoutSwitch . 
                          (\ x -> fill (case x of
                              "myTall"        -> "Tall" ++ setFgColor ++ imagePad "tall"
                              "myMirrorTall"  -> "MirrorTall" ++ setFgColor ++ imagePad "mtall"
                              "myFull"        -> "Full" ++ setFgColor ++ imagePad "full"
                              "myCross"       -> "Cross" ++ setFgColor ++ imagePad "cross"
                              "myMagnifyTall" -> "MagnifyTall" ++ setFgColor ++ imagePad "magnify"
                              "myIM"          -> "im" ++ setFgColor ++ imagePad "im"
                              "myImg"         -> "Gimp Grid" ++ setFgColor ++ imagePad "gimp"
                              _ -> pad x) 4)
    , ppSep = " | "
    , ppWsSep = " "
    , ppTitle = ppTitleColor . dzenEscape
    }
    where
      setFgColor = "^fg(" ++ myFgColor ++ ")"
      setTextColor = "^fg(" ++ myFontColor ++ ")"
      setBgColor = "^fg(" ++ myBgColor ++ ")"
      fill :: String -> Int -> String
      fill h i = "^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")"
      image :: String -> String
      image img = "^i(" ++ imagePath ++ img ++ ".xbm)"
      imagePad :: String -> String
      imagePad img = " " ++ (image img)
      currentWsIndex w = case (elemIndex w myWorkspaces) of -- needs to be modified should I decide to use DynamicWorkspaces one day
                                Nothing -> "1"
                                Just n -> show (n+1)
      -- wrapClickable expects a tuple in the form (<workspace index>, <text to display>)
      wrapClickable (idx,str) = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "e;" ++ xdo index ++ ")" ++ str ++ "^ca()^ca()"
        where
            index = currentWsIndex idx
            xdo key   = "xdotool key super+" ++ key
      wrapLayoutSwitch content = "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"

-- }}}

main = do
     din <- spawnPipe statusBarCmd
     sp  <- mkSpawner
     spawn secondBarCmd
     spawn myXcompmgrCmd
     spawn myTrayCmd
     xmonad $ ewmh defaultConfig
        { manageHook = newManageHook sp
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor = myNormalBorderColor
        , borderWidth = 2
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = \c -> myKeys c sp `M.union` keys defaultConfig c
        , logHook = myLogHook >> (dynamicLogWithPP $ myPP
                { ppOutput = hPutStrLn din
                })
, terminal = myTerminal
}

-- vim: fdm=marker ts=4 sw=4 sts=4 et:
