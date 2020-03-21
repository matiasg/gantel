module Gantt (
      Condition
    , Task(..)
    , realEnd
    , taskWithStartAndSecondsDuration
    , aTimePoint
    ) where

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Fixed as Fixed
import Control.Exception (assert)
import Data.Maybe (catMaybes, mapMaybe)

aTimePoint :: Integer -> Int -> Int -> Integer -> Integer -> Integer -> Clock.UTCTime
aTimePoint y m d hh mm ss = Clock.UTCTime (Calendar.fromGregorian y m d) (Clock.secondsToDiffTime $ hh * 3600 + mm * 60 + ss)

-- Intervals
type Interval = (Clock.UTCTime, Clock.UTCTime)
anInterval :: Clock.UTCTime -> Clock.UTCTime -> Interval
anInterval f1 f2 = (f1, f2)


-- Conditions
data Condition = RightAfter Task | At Clock.UTCTime deriving (Show, Eq)

conditionStarts :: Condition -> Maybe Clock.UTCTime
conditionStarts (RightAfter task) = start task
conditionStarts (At t) = Just t

taskDependence :: Condition -> Maybe Task
taskDependence (RightAfter t) = Just t
taskDependence (At _) = Nothing

-- Tasks
data Task = Task { name :: String
                 , start :: Maybe Clock.UTCTime
                 , end :: Maybe Clock.UTCTime
                 , startConditions :: [Condition]
                 } deriving (Show, Eq)

taskWithStartAndSecondsDuration :: String -> Clock.UTCTime -> Integer -> [Condition] -> Task
taskWithStartAndSecondsDuration name start duration startConditions = Task name (Just start) (Just dur) startConditions
    where dur = Clock.addUTCTime (Clock.secondsToNominalDiffTime $ fromInteger duration) start

realEnd :: Task -> Clock.UTCTime
realEnd (Task _ _ (Just e) _) = e


taskDuration :: Task -> Maybe Clock.NominalDiffTime
taskDuration (Task _ Nothing _ _) = Nothing
taskDuration (Task _ _ Nothing _) = Nothing
taskDuration (Task _ (Just s) (Just e) _) = Just $ Clock.diffUTCTime e s


recomputeStart :: Task -> Task
recomputeStart t@(Task _ _ _ []) = t
recomputeStart (Task n s e cs) = Task n max_start e cs
        where max_start = maximum $ s:(map conditionStarts cs)


taskDependencies :: Task -> [Task]
taskDependencies (Task _ _ _ cs) = mapMaybe taskDependence cs


type Project = [Task]




-- Tests
f1 = aTimePoint 2020 2 29 18 5 23
f2 = aTimePoint 2020 3 1 1 5 23

t1 = Task "task 1" (Just f1) (Just f2) []
t2 = Task "task 2" Nothing Nothing [RightAfter t1]

t3 = taskWithStartAndSecondsDuration "task 3" f1 (3 * 60 * 60) [At f2]

t4 = Task "task 4" Nothing Nothing [RightAfter t1, RightAfter t2, At f1, RightAfter t3]

main = do
    print $ assert (taskDuration t3 == Just (3 * 60 * 60)) ("test 1 passed")
    print $ assert (start (recomputeStart t3) == Just f2) ("test 2 passed")
    print $ assert (taskDependencies t4 == [t1, t2, t3]) ("test 3 passed")
    print $ assert (start (recomputeStart (Task "nothing" Nothing Nothing [RightAfter t2])) == Nothing) ("test 4 passed")
