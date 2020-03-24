import qualified Gantt

main = do
    let launch = Gantt.aTimePoint 2020 3 26 1 51 10
    let leop = Gantt.Task "LEOP" (60 * 60 * 10) [Gantt.At launch]
    let bacp = Gantt.Task "BACP" (60 * 60 * 24 * 7) [Gantt.RightAfter leop]
    let bcpcp = Gantt.Task "BCPCP" (60 * 60 * 24 * 35) [Gantt.RightAfter bacp]
    print $ Gantt.end bcpcp
