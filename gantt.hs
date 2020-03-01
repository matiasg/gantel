import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Fixed as Fixed

fecha :: Integer -> Int -> Int -> Integer -> Integer -> Integer -> Clock.UTCTime
fecha y m d hh mm ss = Clock.UTCTime (Calendar.fromGregorian y m d) (Clock.secondsToDiffTime $ hh * 3600 + mm * 60 + ss)

-- Intervals
type Interval = (Clock.UTCTime, Clock.UTCTime)
intervalo :: Clock.UTCTime -> Clock.UTCTime -> Interval
intervalo f1 f2 = (f1, f2)


-- Conditions
data Condition = RightAfter Task | At Clock.UTCTime deriving (Show)

conditionStarts :: Condition -> Maybe Clock.UTCTime
conditionStarts (RightAfter task) = start task
conditionStarts (At t) = Just t

-- Tasks
data Task = Task { name :: String
                 , start :: Maybe Clock.UTCTime
                 , end :: Maybe Clock.UTCTime
                 , dependencies :: [Condition]
                 } deriving (Show)

taskWithStartAndSecondsDuration :: String -> Clock.UTCTime -> Integer -> [Condition] -> Task
taskWithStartAndSecondsDuration name start duration dependencies = Task name (Just start) (Just dur) dependencies
    where dur = Clock.addUTCTime (Clock.secondsToNominalDiffTime $ fromInteger duration) start

-- Tests
f1 = fecha 2020 2 29 18 5 23
f2 = fecha 2020 3 1 1 5 23

t1 = Task "task 1" (Just f1) (Just f2) []
t2 = Task "task 2" Nothing Nothing [RightAfter t1]

t3 = taskWithStartAndSecondsDuration "task 3" f1 (3 * 60 * 60) [At f2]

main = do
    let f1 = fecha 2020 2 29 18 5 23
    let f2 = fecha 2020 3 1 1 5 23
    let i = intervalo f1 f2
    print i
    print $ start t1
    print $ conditionStarts $ head $ dependencies t2
