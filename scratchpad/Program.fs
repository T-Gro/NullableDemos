module NullableDISABLE
open System
open System.IO

module NullNotPossibleAtFsharpTypes = 

    type Person = { Name : string}
    type Vehicle = Car | Bike
    type TwoStrings = string*string

    let x : Person = null
    let x : Vehicle = null
    let x : TwoStrings = null
    let x : {|X:int|} = null

    // Idiomatic FSharp for 'possibly absent value'
    let possiblyAbsentValue = 
        [ Some {Name = "Petr"}; None]
        |> List.map (Option.map _.Name)








module AllowNullLiteral = 

    [<AllowNullLiteral>]
    type NullPersonRecord = { Name : string}

    [<AllowNullLiteral>]
    type NullPersonType(name:string) =
        member _.Name = name

    let x : NullPersonType = null








module UncheckedOps = 
    open NullNotPossibleAtFsharpTypes

    let x  = Unchecked.defaultof<TwoStrings>
    let manyX = Array.zeroCreate<Person> 50








module NonFSharpTypes = 
    let x : string = null
    let workWithString (s:string) =
        match s with
        | null -> 42
        | nonNull -> 
            if isNull nonNull then
                0
            else 2112

    let handleCsharpString (s:string) = Option.ofObj s








module GenericCode =
    let dowork1 x =
        let mutable z = x
        z <- null
        (x,z)

    let dowork2 x = 
        match x with
        | null -> 0
        | _ -> 1