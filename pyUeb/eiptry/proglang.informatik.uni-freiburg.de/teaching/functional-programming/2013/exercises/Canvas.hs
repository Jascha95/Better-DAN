module Canvas where

-- For printf
import Text.Printf

-- A script is just a string
type Script = String


-- | Example Canvas script: draw some lines, a rectangle and a circle.
-- Test it in ghci: writeFile "example.html" (mkPage 200 200 testScript)
testScript :: Script
testScript = drawLine (0,0) (100,100) 
             ++ drawLine (0, 100) (100, 0)
             ++ drawRect (0, 0) 100 100
             ++ drawCircle (50, 50) 50
                       
-- | Create an html page that displays a Canvas script
mkPage :: Double -> -- ^ the width of the canvas to paint on
          Double -> -- ^ the height of the canvas to paint on
          Script -> -- ^ The Javascript code to paste into the canvas
          String    -- ^ The resulting html page 
mkPage w h script = header w h ++ scriptHeader ++ script ++ scriptFooter ++ footer
                                       

-- | Draw a line on the canvas
drawLine :: (Double, Double) -> -- ^ the start coordinates (x,y)
            (Double, Double) -> -- ^ the end coordinates (x,y)
            Script
drawLine (xFrom, yFrom) (xTo, yTo) =
  printf "ctx.beginPath(); ctx.moveTo(%f,%f); ctx.lineTo(%f,%f); ctx.stroke();" xFrom yFrom xTo yTo

-- | Draw a rectangle on the canvas
drawRect :: (Double, Double) -> -- ^ coordinates of the upper left corner
            Double -> -- ^ width of the rectangle
            Double -> -- ^ height of the rectangle
            Script
drawRect (x, y) w h =
  printf "ctx.strokeRect(%f, %f, %f, %f);" x y w h

-- | Draw a circle on the canvas
drawCircle :: (Double, Double) -> -- ^ coordinates of the center
              Double -> -- ^ the radius
              Script
drawCircle (x, y) r =
  printf "ctx.beginPath(); ctx.arc(%f, %f, %f, 0*Math.PI, 2*Math.PI); ctx.stroke();" x y r
  

-- Boring HTML boilerplate:

header w h = printf "\
\<!DOCTYPE html> <html> <body> \n\
\<canvas id=\"funproCanvas\" width=\"%f\" height=\"%f\" \n\
\        style=\"border:1px solid #d3d3d3;\">\n\
\Your browser does not support the HTML5 canvas tag.\n\
\</canvas>" w h

footer = "</body></html>"

scriptHeader = "\
\<script>\n\
\var c=document.getElementById(\"funproCanvas\");\n\
\var ctx=c.getContext(\"2d\");\n"

scriptFooter = "\
\</script>"

