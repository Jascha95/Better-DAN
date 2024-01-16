module GameLogs where
import LogTypes



sequentialLog=
  Log
    { gameLog =
        [ GLE { time = 10 , player1 = "a" , player2 = "b" , event = Start }
        , GLE { time = 19 , player1 = "a" , player2 = "b" , event = Stop }
        , GLE { time = 20 , player1 = "c" , player2 = "d" , event = Start }
        , GLE { time = 29 , player1 = "c" , player2 = "d" , event = Stop }
        , GLE { time = 30 , player1 = "e" , player2 = "f" , event = Start }
        , GLE { time = 39 , player1 = "e" , player2 = "f" , event = Stop }
        , GLE { time = 40 , player1 = "e" , player2 = "f" , event = Start }
        , GLE { time = 49 , player1 = "e" , player2 = "f" , event = Stop }
        ]
    , playerLogs =
        [ ( "a" , [ ( 11 , Schere ) , ( 13 , Stein ) ] )
        , ( "b" , [ ( 14 , Schere ) , ( 16 , Papier ) ] )
        , ( "d" , [ ( 21 , Schere ) , ( 23 , Schere ) ] )
        , ( "e" , [ ( 31 , Schere ) , ( 41 , Schere ) ] )
        , ( "f"
          , [ ( 32 , Schere )
            , ( 34 , Schere )
            , ( 42 , Schere )
            , ( 44 , Papier )
            ]
          )
        ]
    }


concurrentLog=
  Log
    { gameLog =
        [ GLE
            { time = 26
            , player1 = "Daniela"
            , player2 = "Berta"
            , event = Start
            }
        , GLE
            { time = 40
            , player1 = "Daniela"
            , player2 = "Berta"
            , event = Stop
            }
        , GLE
            { time = 55
            , player1 = "Claudius"
            , player2 = "Daniela"
            , event = Start
            }
        , GLE
            { time = 62
            , player1 = "Berta"
            , player2 = "Alfred"
            , event = Start
            }
        , GLE
            { time = 64
            , player1 = "Claudius"
            , player2 = "Daniela"
            , event = Stop
            }
        , GLE
            { time = 68
            , player1 = "Berta"
            , player2 = "Alfred"
            , event = Stop
            }
        , GLE
            { time = 81
            , player1 = "Claudius"
            , player2 = "Eugen"
            , event = Start
            }
        , GLE
            { time = 93
            , player1 = "Claudius"
            , player2 = "Eugen"
            , event = Stop
            }
        , GLE
            { time = 95
            , player1 = "Fabienne"
            , player2 = "Claudius"
            , event = Start
            }
        , GLE
            { time = 102
            , player1 = "Fabienne"
            , player2 = "Claudius"
            , event = Stop
            }
        , GLE
            { time = 102
            , player1 = "Berta"
            , player2 = "Daniela"
            , event = Start
            }
        , GLE
            { time = 110
            , player1 = "Alfred"
            , player2 = "Fabienne"
            , event = Start
            }
        , GLE
            { time = 114
            , player1 = "Berta"
            , player2 = "Daniela"
            , event = Stop
            }
        , GLE
            { time = 117
            , player1 = "Berta"
            , player2 = "Eugen"
            , event = Start
            }
        , GLE
            { time = 122
            , player1 = "Berta"
            , player2 = "Eugen"
            , event = Stop
            }
        , GLE
            { time = 123
            , player1 = "Alfred"
            , player2 = "Fabienne"
            , event = Stop
            }
        , GLE
            { time = 128
            , player1 = "Eugen"
            , player2 = "Daniela"
            , event = Start
            }
        , GLE
            { time = 130
            , player1 = "Alfred"
            , player2 = "Berta"
            , event = Start
            }
        , GLE
            { time = 138
            , player1 = "Alfred"
            , player2 = "Berta"
            , event = Stop
            }
        , GLE
            { time = 142
            , player1 = "Eugen"
            , player2 = "Daniela"
            , event = Stop
            }
        ]
    , playerLogs =
        [ ( "Alfred"
          , [ ( 63 , Papier )
            , ( 64 , Stein )
            , ( 65 , Stein )
            , ( 66 , Stein )
            , ( 113 , Stein )
            , ( 116 , Schere )
            , ( 121 , Papier )
            , ( 132 , Papier )
            , ( 135 , Schere )
            , ( 137 , Schere )
            ]
          )
        , ( "Berta"
          , [ ( 26 , Papier )
            , ( 37 , Papier )
            , ( 62 , Stein )
            , ( 64 , Schere )
            , ( 67 , Stein )
            , ( 102 , Stein )
            , ( 104 , Schere )
            , ( 111 , Schere )
            , ( 112 , Schere )
            , ( 117 , Stein )
            , ( 119 , Papier )
            , ( 120 , Schere )
            , ( 121 , Schere )
            , ( 132 , Stein )
            , ( 134 , Papier )
            , ( 137 , Stein )
            ]
          )
        , ( "Claudius"
          , [ ( 56 , Schere )
            , ( 57 , Papier )
            , ( 60 , Papier )
            , ( 62 , Stein )
            , ( 63 , Stein )
            , ( 81 , Stein )
            , ( 87 , Schere )
            , ( 88 , Papier )
            , ( 89 , Schere )
            , ( 96 , Schere )
            , ( 98 , Papier )
            , ( 100 , Schere )
            , ( 101 , Schere )
            ]
          )
        , ( "Daniela"
          , [ ( 30 , Papier )
            , ( 31 , Schere )
            , ( 32 , Stein )
            , ( 55 , Papier )
            , ( 63 , Schere )
            , ( 102 , Papier )
            , ( 104 , Schere )
            , ( 105 , Schere )
            , ( 108 , Stein )
            , ( 129 , Stein )
            , ( 132 , Stein )
            , ( 139 , Papier )
            ]
          )
        , ( "Eugen"
          , [ ( 81 , Papier )
            , ( 89 , Stein )
            , ( 90 , Papier )
            , ( 117 , Stein )
            , ( 118 , Papier )
            , ( 119 , Papier )
            , ( 129 , Schere )
            , ( 140 , Schere )
            ]
          )
        , ( "Fabienne"
          , [ ( 96 , Papier )
            , ( 97 , Papier )
            , ( 101 , Stein )
            , ( 111 , Schere )
            , ( 115 , Schere )
            , ( 122 , Papier )
            ]
          )
        ]
    }


sequentialHighscore=
  [ HSE { player = "e" , gamesWon = 1 }
  , HSE { player = "b" , gamesWon = 1 }
  ]


concurrentHighscore=
  [ HSE { player = "Berta" , gamesWon = 3 }
  , HSE { player = "Claudius" , gamesWon = 2 }
  , HSE { player = "Fabienne" , gamesWon = 1 }
  , HSE { player = "Eugen" , gamesWon = 1 }
  , HSE { player = "Daniela" , gamesWon = 1 }
  ]


