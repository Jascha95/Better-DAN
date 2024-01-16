module Widgets
       ( Input(..)
       , getInputValue
       , setInputValue
       , Output(..)
       , setOuputValue
       , Checkbox(..)
       , getCheckboxValue
       , setCheckboxValue
       , onCheckboxChange
       , Button(..)
       , onButtonPress
       , PlotRange(..)
       , makePlotRanges
       , updatePlotRanges
       , FunctionDef(..)
       , makeFunctionDef
       , Editor(..)
       , makeEditor
       , DefinitionBlock(..)
       , makeDefinitionBlock
       , updateDefinitionBlock
       , Plot ()
       , plotElement
       , DrawStyle(..)
       , makePlotCanvas
       -- , clearPlot
       -- , setXLabels
       -- , setYLabels
       -- , drawPoint
       , updatePlot
       )
       where
import Control.Monad
import Data.List (maximumBy)
import qualified Data.Function as F

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Text.JSON.Types


data Button = Button { btElement :: Element }
onButtonPress :: Button -> IO a -> IO ()
onButtonPress b a = on UI.click (btElement b) (const a)

data Input = Input { inpElement :: Element }
getInputValue :: Input -> IO String
getInputValue Input{inpElement = el} = el # get UI.value
setInputValue :: Input -> String -> IO ()
setInputValue = setValue . inpElement 

data Output = Output { outElement :: Element }
setOuputValue :: Output -> String -> IO ()
setOuputValue = setValue . outElement 

data Checkbox = Checkbox { cbElement :: Element }
setCheckboxValue :: Checkbox -> String -> IO ()
setCheckboxValue = setValue . cbElement 
getCheckboxValue :: Checkbox -> IO Bool
getCheckboxValue cb =  cbElement cb # get UI.checked
onCheckboxChange :: Checkbox -> (Bool -> IO a) -> IO ()
onCheckboxChange cb a = on UI.checkedChange (cbElement cb) a


setValue :: Element -> String -> IO ()
setValue el s = element el # set UI.value s >> return ()

data PlotRange = PlotRange { prElement :: Element
                           , prX :: (Input, Input, Input)
                           , updX :: Button
                           , prY :: (Input, Input, Input)
                           , updY :: Button
                           }

makePlotRanges :: IO PlotRange
makePlotRanges = do
    let threeInputs = replicateM 3 (fmap Input $ UI.input # set UI.size "5")
        updButton = fmap Button $ UI.input # set UI.type_ "button"
                                           # set UI.value "Update"
    [minXInput, maxXInput, stepXInput] <- threeInputs
    uX <- updButton
    [minYInput, maxYInput, stepYInput] <- threeInputs
    uY <- updButton


    el <- UI.table #+ [ UI.tr #+ [ UI.td
                                 , UI.th # set text "min"
                                 , UI.th # set text "max"
                                 , UI.th # set text "step"
                                 , UI.td]
                      , UI.tr #+ [ UI.th # set text "x"
                                 , UI.td #+ [element (inpElement minXInput)]
                                 , UI.td #+ [element (inpElement maxXInput)]
                                 , UI.td #+ [element (inpElement stepXInput)]
                                 , UI.td #+ [element (btElement uX)]
                                 ]
                      , UI.tr #+ [ UI.th # set text "y"
                                 , UI.td #+ [element (inpElement minYInput)]
                                 , UI.td #+ [element (inpElement maxYInput)]
                                 , UI.td #+ [element (inpElement stepYInput)]
                                 , UI.td #+ [element (btElement uY)]
                                 ]
                      ]
    return $ PlotRange el (minXInput, maxXInput, stepXInput) uX
                          (minYInput, maxYInput, stepYInput) uY

updatePlotRanges :: PlotRange -> (Double, Double, Double) -> (Double, Double, Double) -> IO ()
updatePlotRanges pr xranges yranges = do
  updRange (prX pr) xranges 
  updRange (prY pr) yranges
  return ()
  where updRange (elMin, elMax, elStep) (mn, ma, st) = do
          element (inpElement elMin) # set value (show mn)
          element (inpElement elMax) # set value (show ma)
          element (inpElement elStep) # set value (show st)

  
data FunctionDef = FDef { fdElement :: Element
                        , fdEnabled :: Checkbox
                        , fdInput :: Input
                        , fdOutput :: Output
                        , fdUpdate :: Button }

makeFunctionDef :: String -> IO FunctionDef
makeFunctionDef s = do
  box <- UI.input # set UI.type_ "checkbox"
  inp <- UI.input # set UI.size "7"
  out <- UI.input # set UI.size "10"
                  # set UI.enabled False
  upd <- UI.input # set UI.type_ "button"
                  # set UI.value "Update"
  el <- UI.tr #+ [ UI.td #+ [element box]
                 , UI.td #+ [UI.p # set text s]
                 , UI.td #+ [element inp]
                 , UI.td #+ [row [element upd, element out] ]]
  return (FDef el (Checkbox box) (Input inp) (Output out) (Button upd))

data Editor = Editor { edElement :: Element
                     , edText :: Input
                     , edSubmit :: Button
                     }

makeEditor :: Int -> Int -> IO Editor
makeEditor cols rs = do
    l <- UI.p # set UI.text "Definitionen:"
    area <- UI.textarea # set UI.cols (show cols)
                        # set UI.rows (show rs) 
    
    submit <- UI.button # set UI.text "Submit"
    defns <- column $ [ element l
                      , element area
                      , element submit]
    return (Editor defns (Input area) (Button submit))

data DefinitionBlock = DefBlock { dbElement :: Element }
                                
makeDefinitionBlock :: IO DefinitionBlock
makeDefinitionBlock = do
   el <- UI.table # set UI.border 1
   let db = DefBlock el
   updateDefinitionBlock db []
   return db

updateDefinitionBlock :: DefinitionBlock -> [FunctionDef] -> IO ()
updateDefinitionBlock (DefBlock{ dbElement = el }) fs = do
  tableHeader <-  UI.tr #+ [ UI.th 
                           , UI.th # set text "Funktion:"
                           , UI.th # set text "Eingabe:"
                           , UI.th # set text "Ausgabe:"
                           ]
  element el # set children (tableHeader : map (fdElement) fs)
  return ()


data Plot = Plot { plotElement :: UI.Element
                 , plotWidth :: Int
                 , plotHeight :: Int
                 }



plotMargins :: Plot -> [String] -> [String] -> IO (Int, Int)
plotMargins Plot{plotElement = el} xlabels ylabels = do
  -- for y, the width is important
  ymargin <- if (null ylabels)
             then return 10
             else do
    let m = snd $ maximumBy (compare `F.on` fst) $ zip (map length ylabels) ylabels
    Just w <- getWindow el
    runFunction w $ ffi (ctx ++ ".font = %1") labelFont
    JSRational _ ri <- callFunction w $ ffi (ctx ++ ".measureText(%1).width") (m :: String)
    return $ round (ri + 5)

  -- for x, we only care about the font size
  let xmargin = 10 
  return (xmargin, ymargin)

makePlotCanvas :: Int -> Int -> -- [String] -> [String] ->
           IO Plot
makePlotCanvas w h  = do
    el <- UI.canvas
    let p = Plot el w h 
    return p 

data DrawStyle = DrawStyle { dsStrokeColor :: String
                           , dsFillColor :: String
                           , dsPointSize :: Int
                           }

updatePlot :: Plot -> [String] -> [String] -> [(Int, Int)] -> DrawStyle -> IO ()
updatePlot p@Plot{ plotHeight = height, plotWidth = width, plotElement = el }
           xlabels ylabels points
           DrawStyle{ dsStrokeColor = strokeColor
                    , dsFillColor = fillColor
                    , dsPointSize = pointSize} = do
  mw <- getWindow el
  case mw of
    Nothing -> return () -- error "Attempt to reset plot without a window!"
    Just win -> do
      element el # set UI.id_ plotId
      (xmargin, ymargin) <- plotMargins p xlabels ylabels
      element el # set UI.width (width + ymargin)
                 # set UI.height (height + xmargin)
      runFunction win $ ffi (ctx ++ ".strokeRect(%1,%2,%3,%4)")
                            ymargin (0::Int) width (height - xmargin)
      -- draw axis
      forM_ [ (xlabels, width, False, (ymargin, ymargin, height - xmargin, height))
            , (reverse ylabels, (height -xmargin), True, (0, 0, ymargin, 0))]
        $ \ (labels, w, transp, (xTickStart, xStringStart, yTickStart, yStringStart)) -> do
            when (not (null labels)) $ do
              forM_ (distribute (0, w) labels) $ \(x, s) -> do
                drawTick (x + xTickStart, yTickStart) p transp
                drawString s (x + xStringStart, yStringStart) p transp
      -- draw points
      forM_ points $ \point -> do
        drawPoint xmargin ymargin point pointSize (Just strokeColor) (Just fillColor) p

distribute :: (Int, Int) -> [String] -> [(Int, String)]
distribute _ [] = []
distribute (mi, ma) ls = zip coords ls
  where maD :: Double
        maD = fromIntegral ma
        minD = fromIntegral mi
        countD = fromIntegral $ (length ls) - 1
        coords = map round $ map (* ((maD - minD) / countD)) [0..countD-1]



plotId = "PlotArea_plotArea"
ffiEl = "document.getElementById('" ++ plotId ++ "')"
ctx = ffiEl ++ ".getContext('2d')"
type Color = String

inPlotArea :: Plot -> (Int, Int) -> Int -> Int -> IO (Int, Int)
inPlotArea p (x, y) xmargin ymargin = do
  return (x + ymargin, (negate y)  - xmargin + (plotHeight p))

-- xmargin, ymargin,
tickHeight :: Int
-- xmargin = 15
-- ymargin = 15
tickHeight = 3
labelFont = "10px"

transposeDraw :: ((Int, Int) -> a) -> (Int, Int) -> a
transposeDraw draw (x, y) = draw (y, x)

drawTick :: (Int, Int) -> Plot -> Bool -> IO () 
drawTick (x,y) pl@Plot{plotElement = el} transpose = do
  let yStart = y - tickHeight
      yStop = y + tickHeight
      (x1,x2,y1,y2) = if transpose then (yStart,yStop,x,x) else (x,x,yStart,yStop)
  Just w <- getWindow el 
  runFunction w $ ffi (ctx ++ ".moveTo(%1,%2)") x1 y1 
  runFunction w $ ffi (ctx ++ ".lineTo(%1,%2)") x2 y2
  runFunction w $ ffi (ctx ++ ".stroke()") 
       
drawString :: String -> (Int, Int) -> Plot -> Bool -> IO ()
drawString s (x,y) Plot{plotElement = el} transpose = do
  let (x',y') = if transpose then (y, x) else (x, y)
  Just w <- getWindow el 
  runFunction w $ ffi (ctx ++ ".font = %1") labelFont
  runFunction w $ ffi (ctx ++ ".fillStyle = '#000000'") 
  if transpose
  then runFunction w $ ffi (ctx ++ ".textAlign = 'left'") 
  else runFunction w $ ffi (ctx ++ ".textBaseline = 'hanging'") 
  runFunction w $ ffi (ctx ++ ".fillText(%1,%2,%3)") s x' y'

drawPoint :: Int -> Int -> (Int, Int) -> Int -> Maybe Color -> Maybe Color -> Plot -> IO ()
drawPoint xmargin ymargin p sz color fillColor pl@Plot{plotElement = el} = do
   (x,y) <- inPlotArea pl p xmargin ymargin 
   Just w <- getWindow el
   runFunction w $ ffi (ctx ++ ".beginPath()")
   runFunction w $ ffi (ctx ++ ".arc(%1,%2,%3,0,2*Math.PI)") x y sz
   whenJust color $ \c -> do
     runFunction w $ ffi (ctx ++ ".strokeStyle = %1") c
     runFunction w $ ffi (ctx ++ ".stroke()")
   whenJust fillColor $ \c -> do
     runFunction w $ ffi (ctx ++ ".fillStyle = %1") c
     runFunction w $ ffi (ctx ++ ".fill()")
     
   -- runFunction w $ ffi "%1.getContext('2d').stroke()" cnv
     -- ffi "%1.getContext('2d').fill()"
     
   -- updateElement $ runFunction 

whenJust (Just x) f = f x
whenJust Nothing  _ = return ()

