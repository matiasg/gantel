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
import Data.Maybe (mapMaybe, fromJust, isNothing)
import qualified Data.Map.Strict as Map

aTimePoint :: Integer -> Int -> Int -> Integer -> Integer -> Integer -> Clock.UTCTime
aTimePoint y m d hh mm ss = Clock.UTCTime (Calendar.fromGregorian y m d) (Clock.secondsToDiffTime $ hh * 3600 + mm * 60 + ss)

-- Intervals
type Interval = (Clock.UTCTime, Clock.UTCTime)
anInterval :: Clock.UTCTime -> Clock.UTCTime -> Interval
anInterval f1 f2 = (f1, f2)


-- Conditions
data Condition
    = RightAfter Task
    | RightAfterBoundTask TaskWithFixedBounds
    | At Clock.UTCTime
    deriving (Show, Eq)

conditionStarts :: Condition -> Maybe Clock.UTCTime
conditionStarts (RightAfter task) = end task
conditionStarts (At t) = Just t

taskDependence :: Condition -> Maybe Task
taskDependence (RightAfter t) = Just t
taskDependence (At _) = Nothing

boundCondition :: Condition -> Condition
boundCondition RightAfter task
  | isNothing s = task
  | otherwise   = RightAfterBoundTask (name task) s (fromJust $ end task)
    where s = start task
boundCondition RightAfterBoundTask t = t
boundCondition At t = t

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

dependsOn :: Task -> Task -> Bool
dependsOn t1 t2
    | null deps2    = False
    | elem t1 deps2 = True
    | otherwise     = any (dependsOn t1) deps2
    where deps2 = mapMaybe taskDependence (startConditions t2)

revDependsOn :: Task -> Task -> Bool
revDependsOn = flip dependsOn

data TaskWithFixedBounds = TaskWithFixedBounds { taskName :: String
                                               , taskStart :: Clock.UTCTime
                                               , taskEnd :: Clock.UTCTime
                                               } deriving (Show, Eq)
fromTask :: Task -> Maybe TaskWithFixedBounds
fromTask task
  | isNothing s = Nothing
  | otherwise   = Just (TaskWithFixedBounds (name task) (fromJust s) (fromJust $ end task))
    where s = start task

fromTaskWithMap :: Task -> Map Task TaskWithFixedBounds -> Maybe TaskWithFixedBounds
fromTaskWithMap (Task n d cs) m
  | isNothing s = Nothing
  | otherwise   = TaskWithFixedBounds n s e
    where s = maximum $  .... TODO


-- Projects
type Project = [Task]

oneHead :: Project -> (Task, Project)
oneHead [] = error "No head for empty project"
oneHead (t:ts)
  | any (revDependsOn t) ts = (hts, t:tts)
  | otherwise = (t, ts)
  where (hts, tts) = oneHead ts

sortProject :: Project -> Project
sortProject [] = []
sortProject ts = hts:(sortProject tts)
  where (hts, tts) = oneHead ts


--updateProject :: Project -> Project
--updateProject p = elems $ snd $ updateSortedProject (sortProject p, empty)
--  where updateSortedProject :: (Project, Map Task Task) -> (Project, Map Task Task)
--        updateSortedProject [] = [] -- TODO
--        updateSortedProject (t:ts, map) = updateSortedProject (ts, insert t newt map)
--            where newt = Task

-- Tests
f1 = aTimePoint 2020 2 29 18 5 23
f2 = aTimePoint 2020 3 1 1 5 23
dur = Clock.diffUTCTime f2 f1

t1 = Task "task 1" dur [At f1]
t2 = Task "task 2" 0 [RightAfter t1]

t3 = taskWithStartAndSecondsDuration "task 3" f1 (3 * 60 * 60) [At f2]

t4 = Task "task 4"  (Clock.secondsToNominalDiffTime 60 * 60) [RightAfter t1, RightAfter t2, At f1, RightAfter t3]
t5 = Task "task 5"  (Clock.secondsToNominalDiffTime 60 * 60) [RightAfter t2, RightAfter t3]

main = do
    print $ assert (duration t3 == Clock.secondsToNominalDiffTime (3 * 60 * 60)) ("test 1 passed")
    print $ assert (start t3 == Just f2) ("test 2 passed")
    print $ assert (taskDependencies t4 == [t1, t2, t3]) ("test 3 passed")
    print $ assert (start (Task "nothing" 0 [RightAfter t2]) == (Just f2)) ("test 4 passed")
    print $ assert (dependsOn t1 t2) ("test 5 passed")
    print $ assert (dependsOn t3 t4) ("test 6 passed")
    print $ assert (dependsOn t1 t5) ("test 7 passed")
    print $ assert (oneHead [t5, t3, t1] == (t3, [t5, t1])) ("test 8 passed")
    print $ assert (oneHead [t5, t2, t1] == (t1, [t5, t2])) ("test 9 passed")
    print $ assert (sortProject [t5, t2, t1] == [t1, t2, t5]) ("test 10 passed")
    print $ assert (sortProject [t5, t1, t2] == [t1, t2, t5]) ("test 11 passed")
    print $ assert (fromTask t2 == Just (TaskWithFixedBounds "task 2" (fromJust $ end t1) (fromJust $ end t1))) ("test 12 passed")
