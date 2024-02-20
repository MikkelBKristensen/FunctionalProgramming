module MultiSet

    type MultiSet<'a when 'a : comparison> private (elements: Map<'a, uint>) = 


    let empty _ = ()

    let isEmpty _ = true

    let size _ = 0u
    
    let contains _ _ = true

    let numItems _ _ = 0u

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
       
    