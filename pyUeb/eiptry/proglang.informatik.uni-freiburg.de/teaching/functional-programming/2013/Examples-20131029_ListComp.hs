users :: [(String, Int)]
--        ^ name   ^ Year of birth 
users = [ ("Klein Anton", 2000)
        , ("Alter Verwalter", 1950)
        ]
purchases :: [(String,      String    , Int)]
--             ^ user id  ^ item Id     ^ date of purchase
purchases =
  [ ("Alter Verwalter", "Die Körperfresser kommen", 1978)
  , ("Klein Anton", "Texas Chainsaw Massacre", 2013)
  , ("Klein Anton", "Bambi", 2013)
  , ("Alter Verwalter", "Bambi", 1954)
  ]

items :: [(String, Int)]
--         ^ title ^ fsk
items =
  [ ("Die Körperfresser kommen", 18)
  , ("Bambi", 6)
  , ("Texas Chainsaw Massacre", 18) ]

-- return all fsk-illicit purchases 
fskProblems :: [(String, String, Int, Int)]
--               ^ buyer  ^ title ^ age ^fsk              
fskProblems = [ (name, title, age, fsk)|
                 (name, bYear) <- users,
                 (buyer, title, date) <- purchases,
                 buyer == name,
                 (fname, fsk) <- items,
                 fname == title,
                 let age = (date - bYear),
                 age < fsk ]
