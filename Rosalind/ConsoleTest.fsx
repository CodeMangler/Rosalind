#light
open System

let readInput = 
  let rec _readInput result =
    let input = Console.ReadLine()
    match input with
      | "" ->
        printf "Empty\n"
        result
      | null ->
        printf "Null\n"
        result
      | _ -> _readInput (result @ [input])
  _readInput []

let input = readInput
printf "\n----------\n"
printf "%O\n" input
