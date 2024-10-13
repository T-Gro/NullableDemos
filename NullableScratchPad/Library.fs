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
    let processPerson

