open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  module CSet = Set.Make (struct
    type t = char

    let compare = compare
  end)

  let score_letter c =
    if c = Char.lowercase_ascii c then Char.code c - Char.code 'a' + 1
    else Char.code c - Char.code 'A' + 27

  let naloga1 data =
    let lines = List.lines data in
    lines
    |> List.map (fun line ->
           let len = String.length line in
           let f =
             String.explode (String.sub line 0 (len / 2)) |> CSet.of_list
           in
           let s =
             String.explode (String.sub line (len / 2) (len / 2))
             |> CSet.of_list
           in
           CSet.inter f s |> CSet.elements)
    |> List.map (function [ c ] -> score_letter c | _ -> assert false)
    |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let lines = List.lines data in
    lines |> List.map String.explode |> List.map CSet.of_list |> List.chunkify 3
    |> List.map (List.reduce CSet.inter)
    |> List.map CSet.elements
    |> List.map (function [ c ] -> score_letter c | _ -> assert false)
    |> List.sum |> string_of_int
end
