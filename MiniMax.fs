module MiniMax

open ProjectInterpreter

type GameTree = {Board:List; Children: GameTree list; Player: Player; Value: int}


let valueFun x = 0

let rec value (gt:GameTree) =
    if List.isEmpty gt.Children
    then valueFun Board
    else
    
