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

module NewWarnings =
    open FSharpSyntax
    let processAB (x:AB | null) = x.IsA

    let createFile (s:string|null) = ConsumeBCL.fCreate s

module NullHandling = 
    open FSharpSyntax
    let matchingAndActivePatterns (p:RecordField|null) = 
        printfn "%i" p.X.Length
        printfn "%A" p

        match p with
        | null -> 0
        | notNullAnyMore -> notNullAnyMore.X.Length
        |> ignore

        match p with
        | NonNull {X = NonNull x} -> x.Length
        | NonNull {X = Null} -> 0
        | Null -> 0

    module ArgValidation = 

        let argValidate_shadowing (arg1:string|null) =
            let arg1 = nullArgCheck (nameof arg1) arg1
            arg1.Length

        let argumentValidation (p:RecordField|null) = 
            let p = nonNull p
            let x = nonNull p.X
            x.Length

        let automaticValidationViaActivePattern (NonNullQuick p) = p.X

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
        l |> List.map (fun x -> x + x)

    let processNullString s =
        match s with
        | Null -> 0
        | NonNull s -> String.length s

module GenericCode = 
    let funcMustnull<'a when 'a : null> (x:'a) = x
    let mustNotNull x = 
        // Automatically generalizes to 'not null' because of being 'TKey in a dictionairy
        let d = System.Collections.Generic.Dictionary<_,string>()
        d[x] <- "hello"
        d

    let d = mustNotNull "x"
    let d2 = mustNotNull ("x" : string | null)
    let d3 = mustNotNull 15

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
        x,y,z

    [<AllowNullLiteral>]
    type CanHaveNull(s:string) = 
        member _.S = s

    let fourth__nullIsRegularValueAlready() =
        let x : _|null = ()
        let x : _|null = Some 15
        let x : _|null = CanHaveNull("hello")
        x
