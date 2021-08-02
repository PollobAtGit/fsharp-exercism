module One

// type abbreviations

type Id = int

type ProductCode = string

type ProductCodeRetreival = Id -> ProductCode

let id : Id = 5

let code : ProductCode = "random string"

let getProduct (retriever: ProductCodeRetreival) = [ 1 .. 5 ] |> List.map retriever

let codes =
    getProduct (fun x -> ($"Product Code: {x.ToString()}"))

printfn "Codes: %A" codes

type Product = Map<Id, ProductCode list>

let products : Product =
    Map
        .empty
        .Add(4, [ "Product Code: XYZ" ])
        .Add(5, [ "Product Code: UOP" ])
        .Add(6, [])

printfn "Products %A" products

printfn $"Try getting {products.TryGetValue 5}"

let find (productId: Id) =
    match products.TryGetValue productId with
    | (hasFound, productCodes) when hasFound -> printfn $"Has Found: {hasFound} | Product Codes: {productCodes}"
    | (hasFound, productCodes) when not hasFound -> printfn "Not found"
    | _ -> failwith "not matched"

find 7
find 4
find 5
