open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let splitter lines =
    let data =
      List.split_on_p lines (fun x -> x = "")
      |> List.map (List.map int_of_string)
    in
    data |> List.map List.sum

  let naloga1 data =
    let lines = List.lines data in
    lines |> splitter |> List.maximum |> string_of_int

  let naloga2 data _part1 =
    let lines = List.lines data in
    lines |> splitter |> List.sort Stdlib.compare |> List.rev |> List.take 3
    |> List.sum |> string_of_int
end
