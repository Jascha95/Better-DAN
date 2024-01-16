module Calculator where

import Control.Monad
import Data.Time.Clock

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import Widgets

main = do
     startGUI defaultConfig
         { tpPort       = 10000
         , tpStatic     = Nothing
         } setup 

setup :: Window -> IO ()
setup  w = do

---------------------------------
-- Aufbau der Benutzeroberfläche
---------------------------------

---------
--- Fenstertitel, etc
  w # set' UI.title "Grafischer Taschenrechner"
  welcome <- UI.p # set UI.text "Der Grafische Taschenrechner!"

-------- 
-- Das Plot-Widget
  plotCanv <- makePlotCanvas 500     -- Breite des Widgets
                             400     -- Höhe des Widgets
-------
-- Plot-Bereiche
  plotRanges <- makePlotRanges

-----------
--- Definierte Funktionen
  definitionBlock <- makeDefinitionBlock 


--------
--- Das Editierfenster
  editor <- makeEditor 60 -- Zeilenlänge
                       10 -- Anzahl Zeilen

--------    
-- Das Nachrichtenfenster
  output <- makeMessageBox 500 -- Breite
                       100 -- Höhe
  -- dummy Einträge

-- Alles zusammensetzen:
  body <- column $ [ element welcome
                   , UI.table #+
                     [ UI.tr #+
                       [ UI.td #+ [column [ element (plotElement plotCanv)
                                          , element (mbElement output)
                                          , element (edElement editor)
                                          ]
                                  ]
                       , UI.td # set UI.valign "top"
                               #+ [column [ element (prElement plotRanges)
                                          , element (dbElement definitionBlock) ]]
                       ]
                     ]
                    ]
  getBody w # set children [body]

--------------------------------
-- Setzen von Werten für die einzelnen Widgets

-- Dummy-Kurve auf den  Plot-Canvas zeichnen
  let f x' = round $ 0.05 * (fromIntegral (x'  - 250)^2)
      xs = [0,5..500] 
      points = zip xs (map f xs) 
      dstyle = DrawStyle "#000000" "#000000" 2
      xlabels = map show [0..10]
      ylabels = map show [0,30..1000]
  updatePlot plotCanv xlabels ylabels points dstyle

  -- Plotbereiche
  updatePlotRanges plotRanges (0, 10, 1) -- x-Bereiche
                              (0, 1000, 30) -- y-Bereiche

  -- Funktionsdefinitionen:
  fx <- makeFunctionDef "f(x) = x^2"
  gx <- makeFunctionDef "g(x) = x + f(x+1)" 
  updateDefinitionBlock definitionBlock [fx, gx]
  -- Setzen von Dummy Werten für die Eingabe und Ausgabe
  setInputValue (fdInput fx) "0"
  setOuputValue (fdOutput fx) "0"

  -- Definitionen
  setInputValue (edText editor) "f(x) = x^2\ng(x) = x + f(x+1)"

  -- Nachrichten:
  let messages = take 20 $ cycle [ Message "Info message" Info , Message "Warning!" Warn]
  updateMessageBox output $ messages

--------------------------------
-- Auslesen der Werte für die einzelnen Widgets

  -- Plotbereiche
  let getRanges (inp1, inp2, inp3) = do
        v1 <- getInputValue inp1
        v2 <- getInputValue inp2
        v3 <- getInputValue inp3
        return (v1, v2, v3)
  xranges <- getRanges (prX plotRanges)
  yranges <- getRanges (prY plotRanges)

  -- Eingabewerte für Funktionen
  x_fx <- getInputValue (fdInput fx)
  x_gx <- getInputValue (fdInput gx)
  check_fx <- getCheckboxValue (fdEnabled fx)

  -- Definitionen
  userDefinitions <- getInputValue (edText editor)

  -- .. und eine Ausgabe der Dinge, die gelesen wurden
  let values = map (\s -> Message s Info)
               [ "Ranges: " ++ show xranges ++ ", " ++ show yranges
               , "fx: " ++ show check_fx ++ ", " ++ x_fx
               , "gx: " ++ x_gx
               , "Userdefs: " ++ show userDefinitions
               ]
  updateMessageBox output $ values ++ messages

--------------------------------
-- Registrieren von Callbacks für Widgets

   -- Knöpfe
  onButtonPress (edSubmit editor) $ do
    now <- getCurrentTime
    updateMessageBox output $ [Message (show now ++ ": submit is not implemented") Warn]
  onCheckboxChange (fdEnabled fx) $ \b -> do
    now <- getCurrentTime
    updateMessageBox output $ [Message (show now ++ ": checkbox is now " ++ show b) Info]

  return ()

-- examples `widgets'
labeledInput :: Int -> String -> String -> IO (Element, Element)
labeledInput sz lab val = do
  inp <- UI.input # set value val
                  # set UI.size (show sz)
  el <- row [UI.p # set text lab, element inp] 
  return (el, inp)



data MessageBox = MessageBox { mbElement :: Element
                             , mbArea :: Element }
data Message = Message String MessageType 
data MessageType = Warn | Info

makeMessageBox :: Int -> Int -> IO MessageBox
makeMessageBox width height = do
  area <- UI.new # set UI.style [ ("overflow", "auto")
                                , ("width", show width ++ "px")
                                , ("height", show height ++ "px")
                                , ("border", "thin solid") ]
  el <- column [ UI.p # set UI.text "Messages:"
               , element area ]
  return (MessageBox el area)

updateMessageBox :: MessageBox -> [Message] -> IO ()
updateMessageBox MessageBox{mbArea = el} ms = do
  mEls <- column $ flip map ms $ \(Message m t) ->
                     UI.new # set UI.text m
                            # set UI.style [ ("color", color t)
                                           , ("font", "70% normal")
                                           ]
  element el # set children [mEls] 
  return ()
  where color Warn = "#FF0000"
        color Info = "#000000"
                            
