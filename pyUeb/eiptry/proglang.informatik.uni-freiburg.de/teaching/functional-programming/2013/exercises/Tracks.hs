module Tracks where
type TrackList = [(String, String, Int)]
-- no album:
noAlbum :: TrackList
noAlbum = 
  [ ("Cosmic Cars (Detroit Style)","Cybotron",267)
  , ("Dream Girl","VARIOUS",528)
  , ("Boddika's House","BODDIKA",352)
  , ("The Alps","Braiden",411)
  , ("Groove Of The Ghetto","A GUY CALLED GERALD",373)
  , ("A1 - The Final Frontier","UR",497)
  ]


-- album: Electronic Warfare 2.0
electronic :: TrackList
electronic = 
  [ ("Kut","UNDERGROUND RESISTANCE",299)
  , ("Technology Gap","UNDERGROUND RESISTANCE",275)
  , ("Kill My Radio Station","UNDERGROUND RESISTANCE",265)
  ]


-- album: Shifted Phases
shifted :: TrackList
shifted = 
  [ ("Implosive Regions","Drexciya",298)
  , ("Scattering Pulsars","Drexciya",308)
  ]


-- album: The Collective
collective :: TrackList
collective = 
  [ ("Point Blank","OCTAVE ONE",445)
  , ("Meridian","OCTAVE ONE",286)
  , ("Nicolette","OCTAVE ONE",273)
  ]

