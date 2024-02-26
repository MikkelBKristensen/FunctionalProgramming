module MultiSet

    type MultiSet<'a when 'a : comparison> = Map<'a, uint> 


    let empty = MultiSet<'a>

    let isEmpty (s: MultiSet<'a>) = s.IsEmpty

    let size (s: MultiSet<'a>) = 
        Map.fold (fun acc _ value -> acc + value) 0u s
    
    let contains (a: 'a) (s: MultiSet<'a>) = s.ContainsKey a

    let numItems (a: 'a) (s: MultiSet<'a>) = s.Item a

    let add (a: 'a) (n: uint) (s: MultiSet<'a>) =
        match Map.tryFind a s with
            | Some elementCount -> s.Add(a, n + elementCount)
            | None -> s.Add(a, n)

    let addSingle (a: 'a) (s: MultiSet<'a>) =
        match Map.tryFind a s with
            | Some elementCount -> s.Add(a, elementCount + uint 1)
            | None -> s.Add(a, uint 1)
    
    let remove (a: 'a) (n: uint) (s: MultiSet<'a>) =
        match Map.tryFind a s with
            | Some elementCount ->
                if n >= elementCount then s.Add(a, uint 0)
                else s.Add(a, elementCount - n)
            | None -> s

    let removeSingle (a: 'a) (s: MultiSet<'a>) =
        match Map.tryFind a s with
            | Some elementCount ->
                if uint 1 >= elementCount then s.Add(a, uint 0)
                else s.Add(a, elementCount - uint 1)
            | None -> s

    let fold (f: 'a -> 'b -> uint32 -> 'a) acc (s: MultiSet<'b>) : 'a = 
        Map.fold (fun acc key value -> f acc key value) acc s
        
    let foldBack (f: 'a -> uint32 -> 'b -> 'b) (s: MultiSet<'a>) (acc: 'b) : 'b =
        Map.foldBack (fun key value acc -> f key value acc) s acc
    
    let ofList (l: 'a List) = ()
    let toList (s: MultiSet<'a>) = []


    let map (a, b) (s: MultiSet<'a>) = ()


    let union (a: MultiSet<'a>)  (b: MultiSet<'a>) = ()
    let sum (a: MultiSet<'a>)  (b: MultiSet<'a>) = ()
    
    let subtract (a: MultiSet<'a>)  (b: MultiSet<'a>) = ()
    
    let intersection (a: MultiSet<'a>)  (b: MultiSet<'a>) = ()
       
    