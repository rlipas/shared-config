module Main (main) where

--------------------------------------------------------------------------------
import System.Exit
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Decoration (Theme(..), shrinkText)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..), MirrorResize(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
-- import XMonad.Layout.PerAspect (ifVertical)
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.Run (unsafeSpawn, safeSpawn, safeSpawnProg, runProcessWithInput)
import XMonad.Util.Themes
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS (nextWS, prevWS)

--------------------------------------------------------------------------------
main =
  -- Start xmonad using the default configuration with some overrides:
  xmonad $ ewmhFullscreen $ ewmh $ docks def
      { modMask     = mod4Mask -- Use the "Win" key for the mod key
      , startupHook = myStartupHook
      , logHook     = myDynLog
      , manageHook  = myManageHook
      , layoutHook  = myLayouts
      , terminal    = "kitty"
      }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("M-S-q",   confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p",     shellPrompt myXPConfig)
      , ("M-<Left>", sendMessage Shrink)
      , ("M-<Right>", sendMessage Expand)
      , ("M-<Down>", sendMessage MirrorShrink)
      , ("M-<Up>", sendMessage MirrorExpand)
      , ("M-m", sendMessage Mag.Toggle)
      , ("M-r",     rotateScreen)
      , ("M-<Esc>", sendMessage (Toggle "Full"))
      , ("C-M1-<Left>",  prevWS)
      , ("C-M1-<Right>",  nextWS)
      , ("<XF86AudioMute>", spawn "amixer -q -D pulse set Master toggle")
      , ("<XF86AudioLowerVolume>", spawn "amixer -q -D pulse set Master 5%-")
      , ("<XF86AudioRaiseVolume>", spawn "amixer -q -D pulse set Master 5%+")
      , ("<XF86AudioPlay>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
      , ("<XF86AudioNext>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
      , ("<XF86AudioPrev>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
      ]

myStartupHook = do
  setDefaultCursor xC_left_ptr

  screencount <- countScreens
  if screencount > 1
    then do
        spawn "xrandr --output DP-2 --auto --primary --pos 0x0 --output eDP-1 --auto --right-of DP-2"
        spawn "xsetwacom set 'Wacom Bamboo 2FG 4x5 Pen stylus' MapToOutput next"
        spawn "xsetwacom set 'Wacom Bamboo 2FG 4x5 Pen eraser' MapToOutput next"
    else spawn "xrandr --output DP-2 --off --output eDP-1 --auto --primary"

  spawn "xinput set-prop 'Wacom Bamboo 2FG 4x5 Finger touch' 343 5.0"
  spawn "xinput set-prop 'Wacom Bamboo 2FG 4x5 Finger touch' 344 5.0"

  spawn "sleep 1 && setxkbmap -layout us,pt,us -variant ,,intl -option  'grp:lctrl_lwin_toggle'"
  spawn "feh --no-fehbg --bg-fill -z Imagens/Wallpapers/;"
  spawn "xcompmgr -f -D 5;"

  safeSpawnProg "xmobar"
  startupHook def

myDynLog = dynamicLogString def
                      { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                      , ppVisible = xmobarColor "green"  "" . wrap "(" ")"
                      , ppUrgent = xmobarColor "black" "red" . wrap "[" "]"
                      , ppTitle = const ""
                      , ppLayout = const ""
                      } >>= xmonadPropLog

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
myLayouts = toggleLayouts (noBorders Full) $ Mag.magnifierOff (avoidStruts $ noFrillsDeco shrinkText myTheme $ gaps 5 others)
  where
    gaps i = spacingRaw False (Border i i i i) True (Border i i i i) True
    others = ResizableTall 1 (1.5/100) (3/5) [] -- `ifVertical` (Mirror $ ResizableTall 1 (3/100) (3/4) [])

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:InconsolataGo Nerd Font:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
  [ className =? "Pidgin" -?> doFloat
  , isDialog              -?> doCenterFloat

    -- Move transient windows to their parent:
  , transience
  ]


myTheme :: Theme
myTheme = def { activeColor         = "#a0afa0"
              , inactiveColor       = "#2e3330"
              , activeBorderColor   = "#a0afa0"
              , inactiveBorderColor = "#2e3330"
              , activeTextColor     = "#040404"
              , inactiveTextColor   = "#a0afa0"
              , fontName =          "xft:InconsolataGo Nerd Font:size=9"
}

rotateScreen = io $ do
      primary_rotation <- runProcessWithInput "/bin/sh" ["-c", "xrandr --current --verbose | grep primary | cut -d\\  -f1,6"] ""
      let [primary, rotation] = words primary_rotation
          rotate "normal" = "left"
          rotate "left" = "inverted"
          rotate "inverted" = "right"
          rotate "right" = "normal"
      safeSpawn "xrandr" ["--output", primary, "--rotation", rotate rotation]


