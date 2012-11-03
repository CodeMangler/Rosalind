(*
Problem

DNA strings must be labeled when they are consolidated into a database. A commonly used method of string labeling is called FASTA format. In this format, the string is introduced by a line that begins with ">", followed by some information naming and characterizing the string. Subsequent lines contain the string itself; the next line starting with ">" indicates the label of the next string.

In Rosalind's implementation, a string in FASTA format will be labeled by the ID "Rosalind_xxxx", where "xxxx" denotes a four-digit code between 0000 and 9999.

Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).

Return: The ID of the string having the highest GC-content, followed by the GC-content of that string. The GC-content should have an accuracy of 4 decimal places (see the note below on decimal accuracy).

Sample Dataset

>Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT
Sample Output

Rosalind_0808
60.919540%
*)

#light

open System

let fastaParseInput = 
  let _parseLine line result = 
    match line with
      | "" | null -> result
      | _ when line.StartsWith(">") -> ([(line.Substring(1), "")] @ result)
      | _ -> 
        let name, genome = result.Head
        ([(name, genome + line)] @ result.Tail)

  let rec _parseNext result =
    let line = Console.ReadLine()
    match line with
      | "" | null -> result
      | _ -> _parseNext (_parseLine line result)
  
  _parseNext []

let stringToList text = Seq.toList text

let gcContent (genome:string) =
  let _countGC acc char = 
    match char with
      | 'G' | 'C' -> acc + 1
      | _ -> acc

  let gcCount = genome |> stringToList |> List.fold _countGC 0
  let gcPercentage = ((float) gcCount) / ((float) genome.Length)
  gcPercentage * 100.0

let toGCContentTuple genomeTuple =
  let name, genome = genomeTuple
  let gcContentPercentage = gcContent genome
  (name, gcContentPercentage)

let gcComparator gcTuple =
  let name, gcContent = gcTuple
  gcContent

let maxGCContent fastaInput =
  fastaInput |> List.map toGCContentTuple |> List.maxBy gcComparator

let fastaInput = fastaParseInput
let maxGCName, maxGCPercentage = fastaInput |> maxGCContent

printf "%s\n%f%%\n" maxGCName maxGCPercentage

// cat Datasets/rosalind_gc.txt | fsi 4.GC.fsx
