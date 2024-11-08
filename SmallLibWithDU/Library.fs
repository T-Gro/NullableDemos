namespace SmallLibWithDU

[<Struct>]
type MyDU = A  | B of stringField:string


type Singleton<'T> =
    static member val Instance:'T = Unchecked.defaultof<'T> with get,set

module BreakIt = 

    Singleton<string>.Instance <- "hello"
    Singleton<string|null>.Instance <- null

    printfn "%i" Singleton<string>.Instance.Length

