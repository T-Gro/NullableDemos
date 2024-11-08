namespace NullableScratchPad

open System.IO
open System

module ConsumeBCL =
    let fparent f = Directory.GetParent f    
    let fCreate f = File.Create f
    let fExists f = File.Exists f


module FSharpSyntax = 
    type AB = A | B
    type AbNull = AB | null

    type RecordField = {X:string | null}
    type TupleField = string * string | null

    type NestedGenerics = { Z : List<List<string | null> | null> | null }
    //type DUField = N of string | null

    type DuField =  I of int | S of (string|null)
    let showMatching (x:obj | null) =
        match x with
        | :? string | null -> 16
        | _ -> 32

module TypeAliasesZoo = 
    type NullString = string | null
    type Maybe<'a> = 'a | null
    type 'a ``?`` = 'a | null
    type 'a ``¯|_(ツ)_|¯`` = 'a | null

    let myFunction (x:string ``?``) = x
    let myFunction2 (x:string ``¯|_(ツ)_|¯``) = x

module NullableWarnings =
    open FSharpSyntax
    open System.Collections.Generic
    let instanceMethod (x:AB | null) = x.IsA
    let methodArgument (s:string|null) = File.Create s
    let methodArgumentWithLiteral() = File.Create null
    let uselessNullConversion(s:string) = Option.ofObj s
    let uselessNullCheck(s:string) = match s with null -> () | _ -> ()
    let invalidGenericTypeParameter() = Dictionary<string|null,int>()

    //let cannotAddnullToValueType() : int | null =  15
    let systemNullableWorks() = Nullable<int>(15)

    let showGenericHints( x: _|null) =
        let y = x
        match x with
        | Null -> 0
        | NonNull x -> hash x

    type NullToString = A | B | C
        with override this.ToString() = if this=A then null else "not A"

module NullHandling = 
    open FSharpSyntax

    let matchNullableString(s:string|null) =    
        match s with
        | null -> 0
        | notNull -> notNull.Length

    let matchingAndActivePatterns (p:RecordField|null) = 
        printfn "%A" p

        match p with
        | null -> 0
        | notNullAnyMore -> notNullAnyMore.X.Length
        |> ignore

        match p with
        | NonNull {X = NonNull x} -> x.Length
        | NonNull {X = Null} -> 0
        | Null -> 0

    type ABNull = A | B of (string|null)

    let handleString (s:string|null) =
        match s with
        | null -> 0
        | NonNull s -> s.Length

    module ArgValidation = 

        let argValidate_shadowing (arg1:string|null) =
            let arg1 = nullArgCheck (nameof arg1) arg1
            arg1.Length

        let argumentValidation (p:RecordField|null) = 
            let p = nonNull p
            let x = nonNull p.X
            x.Length

        let automaticValidationViaActivePattern (NonNullQuick p) = 
            p.X

        let (|NullOrEmpty|NonEmpty|) s = 
            match s with
            | Null | NonNull "" -> NullOrEmpty
            | NonNull s -> NonEmpty s

        let getStringLengthSafe s = 
            match s with
            | NullOrEmpty -> 0
            | NonEmpty s -> s.Length

        let readAllLines (sr:System.IO.StreamReader) =
            seq{
                while not sr.EndOfStream do
                    yield sr.ReadLine() |> Unchecked.nonNull}

        let extractNonNullables (l:List<string|null>) = l |> List.choose Option.ofObj


    module ShutUp__Unchecked__DANGEROUS =      
        let shutUp (p:RecordField) = (p.X |> Unchecked.nonNull).Length
        let (!) = Unchecked.nonNull
        let shutUp2 (p:RecordField) = (!p.X).Length

        let automaticShutUp (Unchecked.NonNullQuick p) = p.X

module TypeInference =
    open FSharpSyntax

    let getList() =
        let mutable x = null
        x <- ["a";"b"]
        x

    let processNullableList l =
        let l = nullArgCheck (nameof l) l
        l |> List.map (fun x -> x + 1)

    let processNullString s =
        match s with
        | Null -> 0
        | NonNull s -> String.length s


module GenericCode = 
    let funcMustnull<'a when 'a : null> (x:'a) = x
    let mustBeNotNull x = 
        // Automatically generalizes to 'not null' because of being 'TKey in a dictionairy
        let d = System.Collections.Generic.Dictionary<_,string>()
        d[x] <- "hello"
        d

    let d = mustBeNotNull "x"
    let d2 = mustBeNotNull ("x" : string | null)
    let d3 = mustBeNotNull 15

    let allowsNull(x:_|null) = (x,x)
    let an = allowsNull "x"
    let an2 = allowsNull (null:string|null)
    //let an3 = allowsNull 15



module Nullables_201_Advanced = 
    open FSharpSyntax
    // Value types
    let first__nullNeverPossible : int = Unchecked.defaultof<_>

    let second__nullOnlyAtRuntime () =
        let x : _|null = (1,2)
        let y : _|null = {|x=15|}

        let but : (int*int) = Unchecked.defaultof<_>

        x,y

    let third__nullableModifierPossibleAtCompileTime () =
        let x : _|null = {RecordField.X = "hello"}
        let y : _|null = AB.A
        let z : _|null = obj()
        let processfunc(x:(int->int)|null) =
            match x with
            | null -> 0
            | f -> f 15
        x,y,z

    [<AllowNullLiteral>]
    type CanHaveNull(s:string) = 
        member _.S = s

    let fourth__nullIsRegularValueAlready() =
        let x : _|null = ()
        let x : _|null = Some 15
        let x : _|null = CanHaveNull("hello")
        x
