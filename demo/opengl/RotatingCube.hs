module Main (main) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL.GL as GL

main :: IO ()
main = do 
  Gtk.initGUI
  
  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  args <- GtkGL.initGL
  print args

  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]
  
  -- Create an OpenGL drawing area widget
  canvas <- GtkGL.glDrawingAreaNew glconfig
  
  Gtk.widgetSetSizeRequest canvas 350 350

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    clearColor $= (Color4 0.0 0.0 0.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers

  ref <- newIORef (0, 0, 0)

  -- Set the repaint handler
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      (r_x, r_y, r_z) <- readIORef ref
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      drawCube (r_x, r_y, r_z)
      GtkGL.glDrawableSwapBuffers glwindow
    return True

  -- Setup the animation
  Gtk.timeoutAddFull (do
    modifyIORef ref (\(r_x, r_y, r_z) -> (r_x + dx, r_y + dy, r_z + dz))
    Gtk.widgetQueueDraw canvas
    return True)
    Gtk.priorityDefaultIdle animationWaitTime

  --------------------------------
  -- Setup the rest of the GUI:
  --
  window <- Gtk.windowNew
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.set window [ Gtk.containerBorderWidth Gtk.:= 8,
                   Gtk.windowTitle Gtk.:= "Gtk2Hs + HOpenGL demo" ]

  vbox <- Gtk.vBoxNew False 4
  Gtk.set window [ Gtk.containerChild Gtk.:= vbox ]

  label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
  button <- Gtk.buttonNewWithLabel "Close"
  Gtk.onPressed button Gtk.mainQuit
  Gtk.set vbox [ Gtk.containerChild Gtk.:= canvas,
                 Gtk.containerChild Gtk.:= label,
                 Gtk.containerChild Gtk.:= button ]

  Gtk.widgetShowAll window
  Gtk.mainGUI

drawCube :: (GLfloat, GLfloat, GLfloat) -> IO ()
drawCube (r_x, r_y, r_z) = do
  loadIdentity
  rotate r_x (Vector3 1 0 0 :: Vector3 GLfloat)
  rotate r_y (Vector3 0 1 0 :: Vector3 GLfloat)
  rotate r_z (Vector3 0 0 1 :: Vector3 GLfloat)
  mapM_ drawFace (zip colours faces)

  where drawFace :: (Color3 GLfloat, IO ()) -> IO ()
        drawFace (colour, face) = do color colour
                                     renderPrimitive Quads face
        faces = map (mapM_ vertex) faceVertices :: [IO ()]
        colours = [red, green, yellow, blue, purple, cyan]
        faceVertices = [
              [Vertex3     to     to     to,
               Vertex3   from     to     to,
               Vertex3   from   from     to,
               Vertex3     to   from     to],
              [Vertex3     to     to   from,
               Vertex3   from     to   from,
               Vertex3   from   from   from,
               Vertex3     to   from   from],
              [Vertex3     to     to     to,
               Vertex3   from     to     to,
               Vertex3   from     to   from,
               Vertex3     to     to   from],
              [Vertex3     to   from     to,
               Vertex3   from   from     to,
               Vertex3   from   from   from,
               Vertex3     to   from   from],
              [Vertex3     to     to     to,
               Vertex3     to   from     to,
               Vertex3     to   from   from,
               Vertex3     to     to   from],
              [Vertex3   from     to     to,
               Vertex3   from   from     to,
               Vertex3   from   from   from,
               Vertex3   from     to   from]]

to, from :: GLfloat
to = 0.4
from   =  -0.4

animationWaitTime :: Int
animationWaitTime = 3

dx, dy, dz :: GLfloat
dx = 0.1
dy = 0.3
dz = 0.7

red, green, yellow, blue, purple, cyan :: Color3 GLfloat
red    = Color3 1 0 0
green  = Color3 0 1 0
yellow = Color3 1 1 0
blue   = Color3 0 0 1
purple = Color3 1 0 1
cyan   = Color3 0 1 1
