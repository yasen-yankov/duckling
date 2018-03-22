-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
<<<<<<< HEAD

module Duckling.Ordinal.BG.Rules
  ( rules ) where

import Control.Monad (join)
import qualified Data.Text as Text
import Prelude
import Data.String
=======
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.BG.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
>>>>>>> upstream/master

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

<<<<<<< HEAD
=======
ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "първ"         , 1 )
  , ( "втор"         , 2 )
  , ( "трет"         , 3 )
  , ( "четвърт"      , 4 )
  , ( "пет"          , 5 )
  , ( "шест"         , 6 )
  , ( "седм"         , 7 )
  , ( "осм"          , 8 )
  , ( "девет"        , 9 )
  , ( "десет"        , 10 )
  , ( "единадесет"   , 11 )
  , ( "дванадесет"   , 12 )
  , ( "тринадесет"   , 13 )
  , ( "четиринадесет", 14 )
  , ( "петнадесет"   , 15 )
  , ( "шестнадесет"  , 16 )
  , ( "седемнадесет" , 17 )
  , ( "осемнадесет"  , 18 )
  , ( "деветнадесет" , 19 )
  , ( "двадесет"     , 20 )
  ]

>>>>>>> upstream/master
ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(първ|втор|трет|четвърт|пет|шест|седм|осм|девет|десет|единадесет|дванадесет|тринадесет|четиринадесет|петнадесет|шестнадесет|седемнадесет|осемнадесет|деветнадесет|двадесет)(и(я(т)?|те)?|а(та)?|о(то)?)"
    ]
  , prod = \tokens -> case tokens of
<<<<<<< HEAD
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "първ" -> Just $ ordinal 1
        "втор" -> Just $ ordinal 2
        "трет" -> Just $ ordinal 3
        "четвърт" -> Just $ ordinal 4
        "пет" -> Just $ ordinal 5
        "шест" -> Just $ ordinal 6
        "седм" -> Just $ ordinal 7
        "осм" -> Just $ ordinal 8
        "девет" -> Just $ ordinal 9
        "десет" -> Just $ ordinal 10
        "единадесет" -> Just $ ordinal 11
        "дванадесет" -> Just $ ordinal 12
        "тринадесет" -> Just $ ordinal 13
        "четиринадесет" -> Just $ ordinal 14
        "петнадесет" -> Just $ ordinal 15
        "шестнадесет" -> Just $ ordinal 16
        "седемнадесет" -> Just $ ordinal 17
        "осемнадесет" -> Just $ ordinal 18
        "деветнадесет" -> Just $ ordinal 19
        "двадесет" -> Just $ ordinal 20
        _ -> Nothing
      _ -> Nothing
  }

=======
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

dozensMap :: HashMap Text Int
dozensMap = HashMap.fromList
  [ ( "двадесет"  , 20 )
  , ( "тридесет"  , 30 )
  , ( "четирдесет", 40 )
  , ( "петдесет"  , 50 )
  , ( "шестдесет" , 60 )
  , ( "седемдесет", 70 )
  , ( "осемдесет" , 80 )
  , ( "деветдесет", 90 )
  ]

>>>>>>> upstream/master
ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(двадесет|тридесет|четирдесет|петдесет|шестдесет|седемдесет|осемдесет|деветдесет)"
<<<<<<< HEAD
    , regex "и (първ|втор|трет|четвърт|пет|шест|седм|осм|девет)(и(я(т)?|те)?|а(та)?|о(то)?)"
=======
    , regex "и (първ|втор|трет|четвърт|пет|шест|седм|осм|девет)(и(ят?|те)?|а(та)?|о(то)?)"
>>>>>>> upstream/master
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
<<<<<<< HEAD
         dozen <- case Text.toLower m1 of
           "двадесет" -> Just 20
           "тридесет" -> Just 30
           "четирдесет" -> Just 40
           "петдесет" -> Just 50
           "шестдесет" -> Just 60
           "седемдесет" -> Just 70
           "осемдесет" -> Just 80
           "деветдесет" -> Just 90
           _ -> Nothing
         unit <- case Text.toLower m2 of
           "първ" -> Just 1
           "втор" -> Just 2
           "трет" -> Just 3
           "четвърт" -> Just 4
           "пет" -> Just 5
           "шест" -> Just 6
           "седм" -> Just 7
           "осм" -> Just 8
           "девет" -> Just 9
           _ -> Nothing
=======
         dozen <- HashMap.lookup (Text.toLower m1) dozensMap
         unit <- HashMap.lookup (Text.toLower m2) ordinalsMap
>>>>>>> upstream/master
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((в|р|м|т)(и(я(т)?|те)?|а(та)?|о(то)?))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinal
  , ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
