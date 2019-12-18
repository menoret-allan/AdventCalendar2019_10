namespace Core

module MonitoringStation =
    type bestLocation = {dimension:(int * int);covered:int}
    type Round = {values:seq<int*int>;radius:int}

    let toAstCoord (map:string) =
        map.Split [|'\n'|]
        |> Seq.mapi (fun y str -> str |> Seq.mapi (fun x c -> if c = '#' then Some(x, y) else None))
        |> Seq.concat
        |> Seq.choose id

    let propagate (posX, posY) (nb:int) =
        seq {for y in -nb..nb do for x in -nb..nb do (x, y)}
        |> Seq.where (fun (x, y) -> x=nb || x= -nb || y=nb || y= -nb)
        |> Seq.map (fun (x, y) -> (x+posX, y+posY))

    let arePointsInLine p1 p2 p3 =
        let (px1, py1) = p1
        let (px2, py2) = p2
        let (px3, py3) = p3
        (px1*(py2-py3)+px2*(py3-py1)+px3*(py1-py2))=0

    let isValOnSide v1 v2 v3 =
        (v1 >= v2 && v1 >= v3) || (v1 <= v2 && v1 <= v3)

    let isTargetOnSide p1 p2 p3 =
        let (px1, py1) = p1
        let (px2, py2) = p2
        let (px3, py3) = p3
        (isValOnSide px1 px2 px3) && (isValOnSide py1 py2 py3)

    let isShadingPoint target p1 p2 =
        (arePointsInLine target p1 p2) && (isTargetOnSide target p1 p2)

    let removeIntersections target coords inter =
        let (tx, ty) = target
        coords
        |> Set.filter (fun x -> Set.contains x inter |> not)
        |> Set.filter (fun p1 -> inter |> Set.exists (fun p2 -> isShadingPoint target p1 p2) |> not)

    let compute coords target =
        let rec loop coords round =
            let intersect = round.values |> Set.ofSeq |> Set.intersect coords
            match coords |> Set.isEmpty with
            | true -> intersect |> Set.count
            | false -> (intersect |> Set.count) + loop (removeIntersections target coords intersect) {values=(propagate target (round.radius+1));radius=round.radius+1}
        {dimension=target;covered=loop (coords |> Set.ofSeq |> Set.remove target) {values=(propagate target 1);radius=1}}

    let findBestLocation map =
        let coords = toAstCoord map
        coords |> Seq.map (compute coords) |> Seq.maxBy (fun loc -> loc.covered)
