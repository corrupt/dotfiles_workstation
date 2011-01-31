-- imports {{{
import XMonad hiding ( (|||) ) -- don't import the ||| operator, it comes in layoutcombinators
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.Plane
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
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.TabBarDecoration
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.Dmenu
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

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
    , [className =? c --> doF (W.shift "IM") | c <- myIms]
    , [className =? c --> doF (W.shift "fm") | c <- myFms]
    , [className =? c --> doF (W.shift "laut") | c <- myLauts]
    , [className =? c --> doF (W.swapDown) | c <- mySwapDowns]
    , [className =? c --> doFloat | c <- myFloats]
    , [className =? c --> doIgnore | c <- myIgnores]
    ]) <+> mySpecialHooks
    where
       myWebs = ["Firefox", "Iron"]
       myImgs = ["Gimp","Gimp-2.6"]
       myIms = ["Pidgin","Skype", "Kopete"]
       myFms = ["pcmanfm","Krusader","Dolphin"]
       myLauts = ["Amarok"]
       mySwapDowns = ["Skype","Pidgin", "Kopete"]
       myFloats = ["Truecrypt",".","Download","Akregator","Amarok"]
       myIgnores = ["Photoshop.exe"]

       mySpecialHooks = composeAll
            [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window" <||> role =? "gimp-dock") --> (ask >>= doF . W.sink)
            , name =? "Copying" --> doCenterFloat -- Krusader copy dialog
            , isFullscreen --> doFullFloat -- fullscreen flash and stuff
            , transience' -- focus parent windows of transient ones
            ]
        
       role = stringProperty "WM_WINDOW_ROLE"
       name = stringProperty "WM_NAME"


newManageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig


-- My custom ThinkLight urgency hook. On thinkpads, this will flash the light
-- <blinks> number of times when a window is set urgent
data ThinkLightUrgencyHook = ThinkLightUrgencyHook
                    { blinks :: Int -- ^ number of times to blink the thinklight
                    }
     deriving (Read, Show)

instance UrgencyHook ThinkLightUrgencyHook where
    urgencyHook ThinkLightUrgencyHook { blinks = d } w = do
    spawn ("thinkalert " ++ show d)
    return ()

-- My urgency hook with the ThinkLight hook
myUrgencyHook = withUrgencyHook ThinkLightUrgencyHook
    { blinks = 2 }

myLayoutHook = onWorkspace "IM" (named "myIM" imlayout) $
               onWorkspace "img" (named "myImg" gimp) $
               named "myTall" (myLayoutMods tiled) |||
               named "myMirrorTall" (myLayoutMods $ Mirror tiled) |||
               named "myMagnifyTall" (myLayoutMods $ magnify tiled) |||
               named "myFull" (myLayoutMods $ noBorders Full) |||
               named "myCross" (myLayoutMods simpleCross) |||
               named "multimedia" (noBorders Simplest)
    where
      --tiled = mouseResizableTile nmaster delta ratio []
      tiled = mouseResizableTile
      -- imlayout makes pidgin and skype occupy 175px at either side of the screen and puts a regular tiled layout in the middle
      -- using myLayoutMods on the gimp or IM layout does more damage than it helps, so just avoidStruts here
      -- also can't just use "tiled" layout on IM because a single window wouldn't have borders
      imlayout = avoidStruts $ withIM (0.137) (ClassName "Kopete") $ reflectHoriz $ ((withIM (0.159) (ClassName "Skype") (reflectHoriz tiled )))
      gimp = avoidStruts $ withIM (0.15) (Role "gimp-toolbox") $
             --reflectHoriz $ withIM (0.2) (Role "gimp-dock") (mouseResizableTile 3 delta ratio [])
             reflectHoriz $ withIM (0.2) (Role "gimp-dock") (mouseResizableTile)
      nmaster = 1
      delta = 3/100
      ratio = 1/2
      magnify = magnifiercz (1.2)
      myLayoutMods x = maximize $ avoidStruts $ layoutHintsToCenter $ smartBorders $ myTabBar x  
      myTabBar = tabBar shrinkText myTabTheme Bottom . resizeVerticalBottom 15


-- I haven't found out whether to set LG3D in startup or logHook. So I set them both
myLogHook = ewmhDesktopsLogHook >> setWMName "LG3D"
myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
--}}}

-- defines {{{

-- mtrDragger stuff for window resizing with the mouse
-- this intruduced such wide gaps between windows that I decided to leave it for now
mrtDraggerOffset :: Position
mrtDraggerOffset = 1

mrtDraggerSize :: Dimension
mrtDraggerSize = 2

myWorkspaces = ["main", "web", "fm", "IM", "img", "laut"] ++ map show [7 .. 8 :: Int]

escapeColor :: String -> String
escapeColor = wrap "'" "'"

myModMask = mod4Mask
myFont = "'-*-terminus-*-r-normal-*-*-100-*-*-*-*-iso8859-*'"

-- myFgColor = "#59bbe8"
-- myBgColor = "#0d0d0d"
-- myFontColor = "#cccccc"
--
-- Zenburn
--myFgColor = "#d7d7af"
--myBgColor = "#1c1c1c"
--myFontColor = "#7f9f7f"
--myHighlightColor = "#d7d7af"
myFgColor = "#418BD4"
myBgColor = "#2E3436"
myFontColor = "#BFBFBF"
myHighlightColor = "#418BD4"

myFocusedBorderColor = myFgColor
myNormalBorderColor = myBgColor

myTabTheme = Theme 
    {   activeColor       = myBgColor
    ,   inactiveColor     = myBgColor
    ,   urgentColor       = myFgColor
    ,   activeBorderColor = myFgColor
    ,   urgentBorderColor = myFgColor
    ,   activeTextColor   = myFgColor
    ,   inactiveTextColor = myFontColor
    ,   urgentTextColor   = myBgColor
    ,   fontName          = myFont
--    ,   decoWidth         = 0
    ,   decoHeight        = 15
    }

myPanelHeight = "16"
myPanelY = "0"
myTerminal = "urxvt"

myMainPanelWidth = "600"
myConkyPanelWidth = "1080"
myTrayerWidth = "110"
myTrayerMargin = "1680" --mainpanel + conkypanel

myTrayerCmd = "trayer "
            ++ " --edge top "
            ++ " --align left "
            ++ " --margin " ++ myTrayerMargin
            ++ " --height " ++ myPanelHeight
            ++ " --widthtype request "
            ++ " --transparent true "
            ++ " --alpha 0 "
            ++ " --tint 0x1A1A1A "
            ++ " --SetPartialStrut true "

myDzenFlags = " -bg " ++ escapeColor myBgColor
            ++ " -fg " ++ escapeColor myFontColor
            ++ " -e 'onstart=lower' "
            ++ " -h " ++ myPanelHeight
            ++ " -fn " ++ myFont
            ++ " -sa c "
            ++ " -y " ++ myPanelY
            ++ " -xs 0 "

statusBarCmd = "dzen2 "
             ++ myDzenFlags
             ++ " -w " ++ myMainPanelWidth
             ++ " -ta l "

secondBarCmd = "conky -c ~/.xmonad/conkyrc | dzen2 "
             ++ myDzenFlags
             ++ " -w " ++ myConkyPanelWidth
             ++ " -x " ++ myMainPanelWidth
             ++ " -ta r "

rightScreenBar = "dzen2 "
               ++ myDzenFlags
               ++ " -w 1680 "
               ++ " -x 1680 "
   
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
    , height = 16
    , bgColor = myBgColor
    , fgColor = myFgColor
    , bgHLight = myFgColor
    , fgHLight = myBgColor
    }
-- }}}

-- the keys config {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((myModMask, xK_p), spawn myDmenuString)
    , ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_y), sendMessage MirrorShrink) --resizableTall keys
    , ((myModMask, xK_a), sendMessage MirrorExpand)
    , ((myModMask .|. shiftMask, xK_l), spawn ("xlock"))
    , ((myModMask .|. shiftMask, xK_x), spawn ("alock -auth pam"))
    , ((myModMask .|. shiftMask, xK_e), spawn ("dolphin"))
    , ((myModMask .|. shiftMask, xK_f), spawn ("firefox"))
    , ((myModMask .|. shiftMask, xK_a), spawn ("amarok"))
    , ((myModMask .|. shiftMask, xK_g), spawn ("gimp"))
    , ((myModMask .|. shiftMask, xK_p), spawn ("pidgin"))
    , ((myModMask .|. shiftMask, xK_w), spawn (myTerminal ++ " -e wicd-curses"))
    , ((myModMask, xK_q), spawn ("killall dzen2 ; killall conky ; killall trayer ; killall xcompmgr ; xmonad --recompile && xmonad --restart"))
    , ((myModMask, xK_F1), (sendMessage $ JumpToLayout "myTall"))
    , ((myModMask, xK_F2), (sendMessage $ JumpToLayout "myMirrorTall"))
    , ((myModMask, xK_F3), (sendMessage $ JumpToLayout "myMagnifyTall"))
    , ((myModMask, xK_F4), (sendMessage $ JumpToLayout "myFull"))
    , ((myModMask, xK_F5), (sendMessage $ JumpToLayout "myCross"))
    , ((myModMask, xK_F8), (sendMessage $ JumpToLayout "multimedia"))
    , ((myModMask, xK_F11), spawn "killall xcompmgr")
    , ((myModMask, xK_F12), spawn ("killall xcompmgr;" ++ myXcompmgrCmd))
    , ((myModMask, xK_u), withFocused (sendMessage . maximizeRestore))
    , ((myModMask, xK_g), withFocused toggleBorder)
    , ((myModMask, xK_o), shellPrompt myPromptConfig)
    , ((myModMask, xK_Tab), goToSelected defaultGSConfig)
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
    { ppCurrent = ppCurrentColor . \a -> image "window-active" ++ a ++ "^fg(" ++ myHighlightColor ++ ")" ++ image "vspace"
    , ppVisible = ppVisibleColor . wrapClickable . (\a -> (a,a))
    , ppHidden = ppHiddenColor . wrapClickable . (\a -> (a,setFgColor ++ image "window" ++ setTextColor ++ a))
    , ppHiddenNoWindows = ppHiddenNWColor . wrapClickable . (\wsId -> (wsId,if (':' `elem` wsId) then drop 2 wsId else wsId))
    , ppUrgent = ppUrgentColor . wrapClickable . (\a -> (a,image "window-active" ++ a ++ setTextColor ++ image "vspace")) . dzenStrip 
    , ppLayout = ppLayoutColor . wrapLayoutSwitch . 
                          (\ x -> fill (case x of
                              "myTall" -> "Tall" ++ setFgColor ++ imagePad "tall"
                              "myMirrorTall" -> "MirrorTall" ++ setFgColor ++ imagePad "mtall"
                              "myFull" -> "Full" ++ setFgColor ++ imagePad "full"
                              "myCross" -> "Cross" ++ setFgColor ++ imagePad "cross"
                              "myMagnifyTall" -> "MagnifyTall" ++ setFgColor ++ imagePad "magnify"
                              "myIM" -> "IM" ++ setFgColor ++ imagePad "im"
                              "myImg" -> "Gimp Grid" ++ setFgColor ++ imagePad "gimp"
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
     spawn secondBarCmd
     spawn myXcompmgrCmd
     spawn rightScreenBar
     spawn myTrayerCmd
     xmonad $ myUrgencyHook
            $ ewmh defaultConfig
        { manageHook = newManageHook
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor = myNormalBorderColor
        , borderWidth = 2
        , workspaces = myWorkspaces
        , modMask = myModMask
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook = myLogHook >> (dynamicLogWithPP $ myPP
                { ppOutput = hPutStrLn din
                })
, terminal = myTerminal
}

-- vim: fdm=marker ts=4 sw=4 sts=4 et:
