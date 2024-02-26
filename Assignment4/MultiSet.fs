module MultiSet

type MultiSet<'a when 'a : comparison> = MultiSet of Map<'a, uint>

let empty = MultiSet(Map.empty)

let isEmpty (MultiSet(s)) = Map.isEmpty s

let size (MultiSet(s)) =
    Map.fold (fun acc _ value -> acc + value) 0u s

let contains a (MultiSet(s)) = Map.containsKey a s

let numItems a (MultiSet(s)) = 
    match Map.tryFind a s with
    | Some count -> count
    | None -> 0u

let add a n (MultiSet(s)) =
    match Map.tryFind a s with
    | Some count -> MultiSet(Map.add a (count + n) s)
    | None -> MultiSet(Map.add a n s)

let addSingle a (MultiSet(s)) =
    add a 1u (MultiSet(s))

let remove a n (MultiSet(s)) =
    match Map.tryFind a s with
    | Some count ->
        if n >= count then MultiSet(Map.remove a s)
        else MultiSet(Map.add a (count - n) s)
    | None -> MultiSet(s)

let removeSingle a (MultiSet(s)) =
    remove a 1u (MultiSet(s))

let fold f acc (MultiSet(s)) = 
    Map.fold f acc s

let foldBack f (MultiSet(s)) acc =
    Map.foldBack f s acc

let ofList (l: 'a list) =
    List.fold (fun acc item -> addSingle item acc) empty l

let toList (MultiSet(s)) =
    Map.foldBack (fun key _ acc -> key :: acc) s []

let map f (MultiSet(s)) =
    fold (fun acc key value -> add (f key) value acc) empty (MultiSet(s))

// I am unsure how to make it fail gracefully, so here is the Sum function again
let union (MultiSet(s1)) (MultiSet(s2)) = 
    fold (fun acc key value -> add key value acc) (MultiSet(s1)) (MultiSet(s2))

let sum (MultiSet(s1)) (MultiSet(s2)) =
    fold (fun acc key value -> add key value acc) (MultiSet(s1)) (MultiSet(s2))

let subtract (MultiSet(s1)) (MultiSet(s2)) =
    fold (fun acc key value -> remove key value acc) (MultiSet(s1)) (MultiSet(s2))

let intersection (MultiSet(s1)) (MultiSet(s2)) =
    fold (fun acc key value ->
        let count = numItems key (MultiSet(s2))
        if count > 0u then add key (min value count) acc
        else acc
    ) empty (MultiSet(s1))
