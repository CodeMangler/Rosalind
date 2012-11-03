(*
Problem

Given two strings s and t of equal length, the Hamming distance between s and t, denoted dH(s,t), is the number of corresponding symbols that differ in s and t. See Figure 2.

Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).

Return: The Hamming distance dH(s,t).

Sample Dataset

GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT
Sample Output

7
*)

#light

open System

let stringToList text = Seq.toList text

// Not really a proper Hamming implementation. Works only for strings of equal length.
let hammingDistance left right =
  let _countDifference acc left right =
    if left <> right then acc + 1
    else acc
  
  let rec _hamming leftSeq rightSeq =
    List.fold2 _countDifference 0 leftSeq rightSeq

  _hamming (stringToList left) (stringToList right)

let first = Console.ReadLine()
let second = Console.ReadLine()

printf "%d\n" (hammingDistance first second)

// cat Datasets/rosalind_hamm.txt | fsi 5.HAMM.fsx 
