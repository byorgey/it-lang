module Example where

import ItLang

expS =
  Block
  [ Assign "b" (Lit (S (S Z)))
  , Assign "e" (Lit (S (S (S (S Z)))))
  , Assign "a" (Lit (S Z))
  , Repeat (V "e") $
      Assign "a" (Times (V "a") (V "b"))
  ]
