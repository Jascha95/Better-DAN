-- Dieses soll die grundlegenden Aspekte der Benutzung von
-- `threepenny-gui' demonstrieren. Es ist eine unfertige
-- Dummy-Implementierung von ex06, Aufgabe 1.
--
-- Als erstes sollten Sie versuchen das Programm auszuführen:
-- - Stellen Sie sicher, dass threepenny-gui-0.3.0.1 installiert ist (Siehe Blatt Ex06)
-- - Laden sie diese Datei in ghci und führen Sie den Befehl `:main' aus
-- - (Alternativ: compilieren Sie diese Datei mit `ghc --make
--   ThreepennyExample.hs' und führen Sie das produzierte Programm
--   `ThreepennyExampl' aus)
-- - Besuchen Sie mit einem Browser die url http://localhost:10000/
-- - Sie müssten nun eine rudimentäre GUI sehen
-- Bitte melden Sie sich im Forum, falls sie Probleme habe sollten.

-- Versuchen Sie nun den unten stehenden Code anhand der Kommentare zu
-- verstehen. Sie können in natürlich auch als Basis für Ex06.1
-- verwenden.
--
-- Weitere API Dokumentation zu threepenny-gui finden Sie unter:
--  http://hackage.haskell.org/package/threepenny-gui-0.3.0.1

-- Diese Imports sind notwendig:
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

-- Die Standard main-Aktion für threepenny-gui. Hier braucht man
-- nichts zu ändern.
main = do
     startGUI defaultConfig
         { tpPort       = 10000
         , tpStatic     = Nothing
         } setup -- hier fängt der Spass an.. s.u.

-- `Window' ist das Browser-Fenster. Wir werden nun GUI-Elemente
-- erzeugen und am Schluss in das Window einfügen.
setup :: Window -> IO ()
setup  w = do
  -- In threepenny-gui gibt es Elemente, Attribute und Events:

  --  - Elemente sind Haskell-Repräsentationen der HTML-Elemente, die
  --    im Browser dargestellt werden. Das Haupt-Element ist das Window
  --    w, das wir als Argument mitgeliefert bekommen
  --  - Attribute sind HTML-Attribute
  --  - Events sind HTML-Input-Events.. wir interessieren uns hier nur für Maus-Klicks.

  -- Hier wird auf dem Window Element das title-Attribut gesetzt
  w # set' UI.title "Zahlenraten!"
  -- - (#) :: a -> (a -> b) -> b
  --   (#) x f = f x
  --    umgekehrte Funktions-Applikation... nur damit es schöner aussieht
  --    also ginge alternativ auch:
  -- set' UI.title "Zahlenraten!" $ w

  -- - set' :: WriteAttr x i -> i -> x -> IO ()
  --     setzt ein Attribut mit Inhalts-Typ i des Element-Typ x auf einen Wert

  -- - UI.title :: WriteAttr Window String 
  --   Das title-Attribut des Fenster Elements

  
  -- Hier kreiren wir ein neues Textfeld-Element (UI.p, erzeugt ein HTML <p>...</p> Element)
  welcome <- UI.p # set UI.text "Denke an eine Zahl zwischen 0-100!"
  -- UI.p :: IO Element
  --  -- Da wir das Element neu erzeugen, ist UI.p eine IO Aktion

  --  -- ACHTUNG: set ist nicht set' (aber sehr ähnlich)
  --  -- set :: WriteAttr x i -> i -> IO x -> IO ()
  --  -- set attr v eAktion = eAktion >>= \e -> e # set' attr v

  --  -- set ist wie set', nur dass es IO-Aktionen als letztes
  --     Argument nehmen kann (zum Abkürzen) 
  --  -- Wir könnten genausogut Schreiben:
  --     welcome <- UI.p
  --     welcome # set' UI.text "..."

  -- Hier erstellen wir einen Knopf (UI.button)
  start <- UI.button # set UI.text "Start!"
  -- Um einem Knopf Funktionalität zuzuweisen, registrieren wir einen Handler für den click Event:
  on UI.click start $ const (dummy w)
  -- on :: Event a -> Element -> (a -> IO ()) -> IO ()
  --
  --  UI.click :: Event () wird ausgelöst, wenn das Element geklickt wird
  --  `dummy w' ist unser Handler (s.u); es ist eine simple IO-Aktion.
  --  Wir benutzen `const', weil wir nicht an dem (bei Knöpfen
  --  uninteressanten) Rückgabewerts des Events interessiert sind.
    
  -- Jetzt setzten wir die neu erstellten Element in das Window ein:
  getBody w # set children [welcome, start]
  -- `getBody' liefert das Element, welches den aktuellen Inhalt des
  -- Fensters repräsentiert; daher ist es eine IO-Aktion
  -- `children' erlaubt den HTML-Inhalt zu definieren; wir setzen
  -- einfach das oben kreirte Textfeld und den Knopf ein
  return ()

-- Das ist der Handler für den in `setup' erstellten Knopf: Er wird
-- den Inhalt des Fensters neu setzen.
dummy :: Window -> IO ()
dummy w = do
  -- Erstelle eine neue Nachricht
  msg <- UI.p # set UI.text "Ist es die 50?"
  -- Erstelle drei Knöpfe, (siehe unten für die Handler)
  buttons <- row $ zipWith (setupButton msg) ["höher", "tiefer", "genau!"] (replicate 3 UI.button)
  -- setze den Inhalt des Windows neu:
  getBody w # set children [msg, buttons]
  return ()
  where setupButton msg t mkB = do
          b <- mkB # set UI.text t
          on UI.click b $ const (msg # set' UI.text "[TODO: not implemented]")
          return b
