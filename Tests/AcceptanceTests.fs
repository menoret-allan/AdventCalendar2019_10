namespace Tests

open Xunit
open FsUnit
open Core.MonitoringStation

module Acceptance =

    [<Fact>]
    let ``Small acceptance test`` () =
        let input = """.#..#
.....
#####
....#
...##"""
        let result = input |> findBestLocation
        result.dimension |> should equal (3,4)
        result.covered |> should equal 8
    

    [<Fact>]
    let ``Small acceptance test2`` () =
        let input  = """......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"""
        let result = input |> findBestLocation
        result.dimension |> should equal (5,8)
        result.covered |> should equal 33
    

    [<Fact>]
    let ``Small acceptance test3`` () =
        let input  = """#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###."""
        let result = input |> findBestLocation
        result.dimension |> should equal (1,2)
        result.covered |> should equal 35
    

    [<Fact>]
    let ``Small acceptance test4`` () =
        let input  = """.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#.."""
        let result = input |> findBestLocation
        result.dimension |> should equal (6,3)
        result.covered |> should equal 41
    

    [<Fact>]
    let ``Master acceptance test <3`` () =
        let input  = """.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"""
        let result = input |> findBestLocation
        result.dimension |> should equal (11,13)
        result.covered |> should equal 210
    