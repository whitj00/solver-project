module MiniMax

open ProjectParser

type Max = {Board:List; Children: Min list option; Player: Player; Value: int}
type Min = {Board:List; Children: Max list option; Player: Player; Value: int}

let valueFun x = 0

let rec value (gt:GameTree) =
    if List.isEmpty gt.Children
    then valueFun Board
        
