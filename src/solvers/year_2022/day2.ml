open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type move = Rock | Paper | Scisors

  let move_of_string = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scisors
    | _ -> failwith "Invalid move"

  type outcome = Win | Loss | Draw

  let outcome_of_string = function
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "Invalid outcome"

  let score_outcome = function Win -> 6 | Draw -> 3 | Loss -> 0

  let calculate_move_from_outcome move outcome =
    match (move, outcome) with
    | Rock, Win -> Paper
    | Rock, Draw -> Rock
    | Rock, Loss -> Scisors
    | Paper, Win -> Scisors
    | Paper, Draw -> Paper
    | Paper, Loss -> Rock
    | Scisors, Win -> Rock
    | Scisors, Draw -> Scisors
    | Scisors, Loss -> Paper

  let score_move = function Rock -> 1 | Paper -> 2 | Scisors -> 3

  let play m1 m2 =
    match (m1, m2) with
    | Rock, Rock | Paper, Paper | Scisors, Scisors -> Draw
    | Rock, Paper | Paper, Scisors | Scisors, Rock -> Loss
    | Rock, Scisors | Paper, Rock | Scisors, Paper -> Win

  let parse_play line =
    match String.split_on_char ' ' line with
    | [ m1; m2 ] -> (move_of_string m1, move_of_string m2)
    | _ -> failwith "Invalid input"

  let parse_outcome_play line =
    match String.split_on_char ' ' line with
    | [ m1; m2 ] ->
        let m1 = move_of_string m1 in
        (m1, calculate_move_from_outcome m1 (outcome_of_string m2))
    | _ -> failwith "Invalid input"

  let score_game (m1, m2) =
    let outcome = play m2 m1 in
    score_move m2 + score_outcome outcome

  let naloga1 data =
    let moves = List.lines data |> List.map parse_play in
    let total = moves |> List.map score_game |> List.sum in
    string_of_int total

  let naloga2 data _part1 =
    let moves = List.lines data |> List.map parse_outcome_play in
    let total = moves |> List.map score_game |> List.sum in
    string_of_int total
end
