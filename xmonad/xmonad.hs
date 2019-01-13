module Main (main) where

--------------------------------------------------------------------------------
import System.Exit
import System.IO
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Decoration (Theme(..), shrinkText)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

--------------------------------------------------------------------------------
main = do
  screencount <- countScreens
  if screencount > 1
    then spawn "xrandr --output HDMI-1 --auto --primary --pos 0x0 --output eDP-1 --auto --pos 277x1080"
    else spawn "xrandr --output HDMI-1 --off --output eDP-1 --auto --primary"

  spawn $ "setxkbmap -layout us,pt,us -variant ,,intl -option  'grp:lctrl_lwin_toggle';" ++
          "feh --no-fehbg --bg-fill Imagens/Wallpapers/frozen_lines_of_force_by_b33rheart_dbq7orp.png Imagens/Wallpapers/caramelemarac_by_senzune_dbxl64j.png;" ++
          "xcompmgr -f -D 5;"

  xmobar <- spawnPipe "xmobar"

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ desktopConfig
    { modMask    = mod4Mask -- Use the "Win" key for the mod key
    , logHook    = myDynLog xmobar
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = noFrillsDeco shrinkText myTheme $ desktopLayoutModifiers $ myLayouts
    , terminal   = "kitty"
    }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("M-S-q",   confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p",     shellPrompt myXPConfig)
      , ("M-<Esc>", sendMessage (Toggle "Full"))
      , ("<XF86AudioMute>", spawn "amixer -q -D pulse set Master toggle")
      , ("<XF86AudioLowerVolume>", spawn "amixer -q -D pulse set Master 5%-")
      , ("<XF86AudioRaiseVolume>", spawn "amixer -q -D pulse set Master 5%+")
      , ("<XF86AudioPlay>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
      , ("<XF86AudioNext>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
      , ("<XF86AudioPrev>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
      ]

myDynLog    h = dynamicLogWithPP def
                      { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                      , ppVisible = xmobarColor "green"  "" . wrap "(" ")"
                      , ppUrgent = xmobarColor "black" "red" . wrap "[" "]"
                      , ppTitle = const ""
                      , ppLayout = const ""
                      , ppOutput  = hPutStrLn h
                      }

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
myLayouts = toggleLayouts (noBorders Full) $ gaps 5 others
  where
    gaps i = spacingRaw False (Border i i i i) True (Border i i i i) True
    others = ResizableTall 1 (1.5/100) (3/5) [] ||| emptyBSP

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
