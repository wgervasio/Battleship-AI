{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main ( main ) where

import {-@ HTF_TESTS @-} BattleShipTest
import {-@ HTF_TESTS @-} EnemyTest
import {-@ HTF_TESTS @-} ShipDataTest
import {-@ HTF_TESTS @-} ShotTest

main :: IO ()
main = htfMain htf_importedTests