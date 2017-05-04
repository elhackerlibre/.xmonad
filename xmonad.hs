#! /usr/bin/env runhugs +l
--
-- xmonad.hs
-- Copyright (C) 2017 Felix Molero <fmolero69@gmail.com>
--
-- Distributed under terms of the GPLv3+ license.
--

-- Módulos importados:
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops 
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Data.Monoid
import System.Exit
import System.IO
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Funciones base
myModMask               = mod4Mask
myTerminal              = "termite"
myWorkspaces            = ["1","2","3","4","5","6","7","8","9"]
myBorderWidth           = 2
myNormalBorderColor     = "#323232"
myFocusedBorderColor    = "#707070"
myFocusFollowsMouse     :: Bool
myFocusFollowsMouse     =  True

-- Función * main *
--
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
   -- Startup:
    spawn "mpd"
    spawn "numlock on"
    spawn "compton -cCGb"
    spawn "thunar --daemon"
    spawn "feh --bg-scale ~/.background/xmonad2.png"
    spawn "conky -d; sleep 1 && transset-df .5 -n Conky"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , handleEventHook = docksEventHook <+> handleEventHook 
                             defaultConfig <+> fullscreenEventHook
        , logHook    = dynamicLogWithPP xmobarPP  
                            { ppOutput  = hPutStrLn xmproc
                            , ppTitle   = xmobarColor "#EE9A00" "" . shorten 50
                            , ppCurrent = xmobarColor "#EE9A00" ""
                            , ppVisible = xmobarColor "#404040" ""
                            , ppUrgent  = xmobarColor "#900000" "" . wrap "[" "]"
                            }

        -- Simple stuff
       -- , keys                  = myKeys 
          , modMask               = myModMask 
          , terminal              = myTerminal
          , workspaces            = myWorkspaces
          , borderWidth           = myBorderWidth
          , mouseBindings         = myMouseBindings
          , focusFollowsMouse     = myFocusFollowsMouse
          , normalBorderColor     = myNormalBorderColor
          , focusedBorderColor    = myFocusedBorderColor
        }  `additionalKeys`

-- myKeys conf@(XConfig {XMonad.modMask = mod4Mask}) = M.fromList $
-------------------------------------------------------------------------------
----------------------------  Atajos de teclado: ------------------------------
-------------------------------------------------------------------------------
--
--------------------------- Lanzador de útilidades ----------------------------
        [ (( mod4Mask,               xK_d       ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") 
        , (( mod4Mask,               xK_g       ), spawn "gmrun"   ) 
        , (( mod4Mask,               xK_Return  ), spawn "termite" ) 

--------------------------- Lanzador de aplicaciones --------------------------
        , (( mod4Mask,               xK_F1      ), spawn "firefox" )
        , (( mod4Mask,               xK_F2      ), spawn "thunar"  ) 
        , (( mod4Mask,               xK_F3      ), spawn "mousepad")
        , (( mod4Mask .|. shiftMask, xK_F1      ), spawn "palemoon")
        , (( mod4Mask .|. shiftMask, xK_F2      ), spawn "gimp"    )
        , (( mod4Mask .|. shiftMask, xK_F3      ), spawn "inkscape")

-------------------------- Control de música [MPC] ----------------------------
        , (( mod4Mask,               xK_p       ), spawn "mpc toggle") -- Play | Pausa
        , (( mod4Mask,               xK_s       ), spawn "mpc stop"  ) -- Stop
        , (( mod4Mask,               xK_b       ), spawn "mpc prev"  ) -- Back
        , (( mod4Mask,               xK_n       ), spawn "mpc next"  ) -- Next
        , (( mod4Mask,               xK_plus    ), spawn "mpc volume +2 ") --Sube el volumen
        , (( mod4Mask,               xK_minus   ), spawn "mpc volume -3 ") --Baja el volumen

--------------------------- Control de volumen --------------------------------
        , (( mod4Mask .|. shiftMask, xK_plus    ), spawn "amixer set Master 5%+ unmute")
        , (( mod4Mask .|. shiftMask, xK_minus   ), spawn "amixer set Master 5%- unmute")
        , (( mod4Mask .|. shiftMask, xK_m       ), spawn "amixer set Master toggle")

------------------------- Funcionalidad de las Ventanas -----------------------
        , (( mod4Mask,                xK_r      ), spawn "xmonad --recompile; xmonad --restart") -- reiniciar Xmonad
        , (( mod4Mask,                xK_e      ), io (exitWith ExitSuccess)) -- Salir de Xmonad
        , (( mod4Mask .|. shiftMask,  xK_q      ), kill                     ) -- Cerrar ventana
        , (( mod4Mask .|. shiftMask,  xK_r      ), refresh                  ) -- Reajustar ventana
        , (( mod4Mask,                xK_Tab    ), windows W.focusDown      ) -- Mover focus de ventana abajo
        , (( mod4Mask,                xK_j      ), windows W.focusDown      ) -- Mover focus de ventana abajo
        , (( mod4Mask,                xK_k      ), windows W.focusUp        ) -- Mover focus de ventana arriba
        , (( mod4Mask,                xK_m      ), windows W.focusMaster    ) -- Mover focus de ventana principal
        , (( mod4Mask .|. shiftMask,  xK_Return ), windows W.swapMaster     ) -- Cambiar enfoque de la ventana principal
        , (( mod4Mask .|. shiftMask,  xK_j      ), windows W.swapDown       ) -- Cambiar enfoque de la ventana abajo
        , (( mod4Mask .|. shiftMask,  xK_k      ), windows W.swapUp         ) -- Cambiar enfoque de la ventana arriba
        , (( mod4Mask,                xK_h      ), sendMessage Shrink       ) -- Reduce el area de la ventana principal
        , (( mod4Mask,                xK_l      ), sendMessage Expand       ) -- Expandir el area de la ventana principal
        , (( mod4Mask,                    xK_t      ), withFocused $ windows . W.sink) -- Empujar la ventana a la cuadricula
        , (( mod4Mask,                xK_comma  ), sendMessage (IncMasterN 1)) -- Incrementar la ventana en el area.
        , (( mod4Mask,                xK_period ), sendMessage (IncMasterN (-1))) -- Incrementar la ventana en el area.
        , (( mod4Mask,                xK_space  ), sendMessage NextLayout    ) -- Rotar las ventanas.
     --   , (( mod4Mask .|. shiftMask,  xK_space ), setLayout $ XMonad.layoutHook conf)

        ]
-------------------------------------------------------------------------------
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
