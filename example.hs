import Gantt

main = do
    let launch = aTimePoint 2020 3 26 1 51 10
    let leop = taskWithStartAndSecondsDuration "LEOP" launch (60 * 60 * 10) []
    let bacp = taskWithStartAndSecondsDuration "BACP" (realEnd leop) (60 * 60 * 24 * 7) []
    let bcpcp = taskWithStartAndSecondsDuration "BCPCP" (realEnd bacp) (60 * 60 * 24 * 35) []
    print $ end bcpcp
