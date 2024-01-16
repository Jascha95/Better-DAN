module LogTypes where
       
-- | The possible choices the players can make
data Choice = Schere | Stein | Papier
  deriving (Eq, Show)

-- | Calendar time
type Time = Int

-- | Each time a player sets a choice, time and choice are logged
type PlayerLog = [(Time, Choice)]

-- | Players are identified by their names
type Player = String

-- | Games can either start or stop
data GameEvent = Stop | Start 
  deriving (Show, Eq)

-- | An entry fo the game log. It describes an event (start or stop)
-- that happended at a certain time, involving the two opponents
data GameLogEntry = GLE { time :: Time, player1 :: Player, player2 :: Player, event :: GameEvent }
  deriving (Show, Eq)
                 
-- | The history of game log entries
type GameLog = [GameLogEntry]


-- | The entire log consist of the game log and a player log for each player
data Log = Log { gameLog :: GameLog, playerLogs :: [(Player, PlayerLog)] }
  deriving (Show, Eq)

-- | An entry in the highscore table: a player and how many games he/she has won
data HighScoreEntry = HSE { player :: Player, gamesWon :: Int }
  deriving (Eq, Show)

-- | The highscore table is a list or highscore entries, containing
-- one entry for each player, and ordered by the number of won games
type HighScore = [HighScoreEntry]
