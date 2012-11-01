(*
Problem

A string is simply an ordered collection of symbols selected from some alphabet and formed into a word; the length of a string is the number of symbols that it contains.

An example of a DNA string (whose alphabet contains the symbols A, C, G, and T) is "ATGCTTCAGAAAGGTCTTACG".

Given: A DNA string s of length at most 1000 nt.

Return: Four integers separated by space corresponding to the number of times that the symbols A, C, G, and T occur in s.

Sample Dataset

AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
Sample Output

20 12 17 21
*)

#light

open System

let stringToList text = [for c in text -> c]

let countSymbols input =
  let rec _countInner input a c g t =
    match input with
      | [] -> [a; c; g; t]
      | char::rest when char = 'A' -> _countInner rest (a+1) c g t
      | char::rest when char = 'C' -> _countInner rest a (c+1) g t
      | char::rest when char = 'G' -> _countInner rest a c (g+1) t
      | char::rest when char = 'T' -> _countInner rest a c g (t+1)
      | char::rest -> _countInner rest a c g t
  _countInner input 0 0 0 0

let counts = Console.ReadLine() |> stringToList |> countSymbols
printfn "%d %d %d %d" counts.[0] counts.[1] counts.[2] counts.[3]

// cat Datasets/rosalind_dna.txt | fsi 1.DNA.fsx
// 250 251 239 245
