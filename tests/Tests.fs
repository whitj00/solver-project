namespace SlvrTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.IntegerReturnsANum () =
        let input = "2"
        let expected = Program [Num 2]
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws);
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.BoolReturnsBool () =
        let input = "true"
        let expected = Program [Bool true]
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws);
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.ApplicationReturnsApplication () =
        let input = "(+ 1 4 5)"
        let expected = Program [Application (Operation "+", [Num 1; Num 4; Num 5])]
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws);
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.MathEvaluates () =
        let input = "(+ 1 4 5)"
        let expected = Program [Num 10]
        match parse input with
        | Some ws ->
            let result = snd (eval Map.empty ws)
            Assert.AreEqual(expected, result)
        | None -> Assert.IsTrue false


    [<TestMethod>]
    member this.ListsAppend () =
        let input = "(append (list 1 2) (list 3 4))"
        let expected = Program [List [Num 1; Num 2; Num 3; Num 4]]
        match parse input with
        | Some ws ->
            let result = snd (eval Map.empty ws)
            Assert.AreEqual(expected, result)
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.BoardStateSaves () =
        let input = "(defBoard (list 0 0)) board"
        let expected = Program [NoRet; List [Num 0; Num 0]]
        match parse input with
        | Some ws ->
            let result = snd (eval Map.empty ws)
            Assert.AreEqual(expected, result)
        | None -> Assert.IsTrue false
