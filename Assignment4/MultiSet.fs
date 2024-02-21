module MultiSet

open System

    type MultiSet<'a when 'a : comparison> = Map<'a, uint> 


    let empty = MultiSet<'a>

    let isEmpty (s: MultiSet<'a>) = s.IsEmpty

    let size (s: MultiSet<'a>) = 
        Map.fold (fun acc _ value -> acc + value) 0u
    
    let contains (a: 'a) (s: MultiSet<'a>) = s.ContainsKey a

    let numItems (a: 'a) (s: MultiSet<'a>) = s.Item a

    let add _ _ _ = ()

    let addSingle _ _ = ()
    
    let remove _ _ _ = ()

    let removeSingle _ _ = ()


    let fold _ x _ = x 
    let foldBack _ _ x = x
    
    let ofList _ = ()
    let toList _ = []


    let map _ _ = ()


    let union _ _ = ()
    let sum _ _ = ()
    
    let subtract _ _ = ()
    
    let intersection _ _ = ()
       
    