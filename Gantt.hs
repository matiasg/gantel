module Gantt (
      Condition
    , Task(..)
    , taskWithStartAndSecondsDuration
    , aTimePoint
    ) where

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Fixed as Fixed
import Control.Exception (assert)
import Data.Maybe (catMaybes, mapMaybe, fromJust)

aTimePoint :: Integer -> Int -> Int -> Integer -> Integer -> Integer -> Clock.UTCTime
aTimePoint y m d hh mm ss = Clock.UTCTime (Calendar.fromGregorian y m d) (Clock.secondsToDiffTime $ hh * 3600 + mm * 60 + ss)

-- Intervals
type Interval = (Clock.UTCTime, Clock.UTCTime)
anInterval :: Clock.UTCTime -> Clock.UTCTime -> Interval
anInterval f1 f2 = (f1, f2)


-- Conditions
data Condition = RightAfter Task | At Clock.UTCTime deriving (Show, Eq)

conditionStarts :: Condition -> Maybe Clock.UTCTime
conditionStarts (RightAfter task) = end task
conditionStarts (At t) = Just t

taskDependence :: Condition -> Maybe Task
taskDependence (RightAfter t) = Just t
taskDependence (At _) = Nothing

-- Tasks
data Task = Task { name :: String
                 , duration :: Clock.NominalDiffTime
                 , startConditions :: [Condition]
                 } deriving (Show, Eq)

taskWithStartAndSecondsDuration :: String -> Clock.UTCTime -> Integer -> [Condition] -> Task
taskWithStartAndSecondsDuration name start duration startConditions = Task name dur ((At start):startConditions)
    where dur = (Clock.secondsToNominalDiffTime $ fromInteger duration)

start :: Task -> Maybe Clock.UTCTime
start (Task _ _ cs) = maximum $ map conditionStarts cs

end :: Task -> Maybe Clock.UTCTime
end t@(Task _ d cs) =
    let strt = start t in
        case strt of
          Just beg -> Just $ Clock.addUTCTime d beg
          Nothing -> Nothing


taskDependencies :: Task -> [Task]
taskDependencies (Task _ _ cs) = mapMaybe taskDependence cs


type Project = [Task]




-- Tests
f1 = aTimePoint 2020 2 29 18 5 23
f2 = aTimePoint 2020 3 1 1 5 23
dur = Clock.diffUTCTime f2 f1

t1 = Task "task 1" dur [At f1]
t2 = Task "task 2" 0 [RightAfter t1]

t3 = taskWithStartAndSecondsDuration "task 3" f1 (3 * 60 * 60) [At f2]

t4 = Task "task 4"  (Clock.secondsToNominalDiffTime 60 * 60) [RightAfter t1, RightAfter t2, At f1, RightAfter t3]

main = do
    print $ assert (duration t3 == Clock.secondsToNominalDiffTime (3 * 60 * 60)) ("test 1 passed")
    print $ assert (start t3 == Just f2) ("test 2 passed")
    print $ assert (taskDependencies t4 == [t1, t2, t3]) ("test 3 passed")
    print $ assert (start (Task "nothing" 0 [RightAfter t2]) == (Just f2)) ("test 4 passed")
