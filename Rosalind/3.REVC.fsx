(*
Problem

In a DNA string, the complement of A is T, and the complement of C is G.

The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the complement of each symbol (e.g., the reverse complement of GTCA is TGAC).

Given: A DNA string s of length at most 1000 bp.

Return: The reverse complement of s.

Sample Dataset

AAAACCCGGT
Sample Output

ACCGGGTTTT
*)

#light

let stringToList (text:string) = text.ToCharArray() |> List.ofArray

let reverse (text:string) = new string(text.ToCharArray() |> Array.rev)

let dnaCompliment dnaSeq =
  let rec _buildCompliment dnaSeq newSeq =
    match dnaSeq with
      | [] -> newSeq
      | nucleotide::seqRest when nucleotide = 'A' -> _buildCompliment seqRest (newSeq @ ['T'])
      | nucleotide::seqRest when nucleotide = 'T' -> _buildCompliment seqRest (newSeq @ ['A'])
      | nucleotide::seqRest when nucleotide = 'G' -> _buildCompliment seqRest (newSeq @ ['C'])
      | nucleotide::seqRest when nucleotide = 'C' -> _buildCompliment seqRest (newSeq @ ['G'])
      | nucleotide::seqRest -> _buildCompliment seqRest newSeq
  _buildCompliment dnaSeq []

let reverseCompliment (dna:string) = new string(dna |> reverse |> stringToList |> dnaCompliment |> List.toArray)

printf "%s" (System.Console.ReadLine() |> reverseCompliment)

// cat Datasets/rosalind_revc.txt | fsi 3.REVC.fsx > out.txt
