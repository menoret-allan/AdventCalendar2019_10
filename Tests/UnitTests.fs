namespace Tests

open Xunit
open FsUnit
open Core.MonitoringStation

module Unit =

    [<Fact>]
    let ``translation map of one coord`` () =
        let input = """.#
.."""
        let result = input |> toAstCoord
        result |> should equivalent [|(1,0)|]


    [<Fact>]
    let ``translation map of many coords`` () =
        let input = """.#..#
.....
#####
....#
...##"""
        let result = input |> toAstCoord
        result |> should equivalent [|(1,0);(4,0);(0,2);(1,2);(2,2);(3,2);(4,2);(4,3);(3,4);(4,4)|]

    [<Fact>]
    let ``Propagation from one point to dist`` () =
        let result = propagate (1,4) 1
        result |> should equivalent [|(0,3);(0,4);(0,5);(1,3);(1,5);(2,3);(2,4);(2,5)|]

    [<Fact>]
    let ``Propagation from one point to dist 3`` () =
        let result = propagate (5,0) 3
        result |> should equivalent [|(2,-3);(2,-2);(2,-1);(2,0);(2,1);(2,2);(2,3);(3,-3);(3,3);(4,-3);(4,3);(5,-3);(5,3);(6,-3);(6,3);(7,-3);(7,3);(8,-3);(8,-2);(8,-1);(8,0);(8,1);(8,2);(8,3);|]

    [<Fact>]
    let ``Point in line true`` () =
        arePointsInLine (1, 1) (1, 4) (1, 5) |> should equal true

    [<Fact>]
    let ``Point in line false`` () =
        arePointsInLine (1, 5) (2, 5) (4, 6) |> should equal false

    [<Fact>]
    let ``is target in the side left`` () =
        isTargetOnSide (1, 1) (1, 4) (1, 5) |> should equal true

    [<Fact>]
    let ``is target in the side right`` () =
        isTargetOnSide (1, 7) (1, 4) (1, 5) |> should equal true

    [<Fact>]
    let ``target in the middel return false`` () =
        isTargetOnSide  (1, 5) (1, 7) (1, 4) |> should equal false


    