(*
    Lab 8: The Overlapping Line Coverage Problem
    cc: Akshar Patel (akshar20@uab.edu), Michael Gathara (mikegtr@uab.edu)

    This assignment will cover implementing the overlapping line coverage problem. This
    problem entails recieving a list of lines that may overlap in 1-dimensional space
    and calculating the total distance the lines cover in a time-efficient manner.
    The solution to this problem requires representing the 1-dimensional space as a
    binary tree.

    Lines in this problem are represented as float * float tuples where the first value
    is the beginning of the line while the second is the end of the line. Note that lines
    where the values are the same or the first value is greater than the second would be
    considered lines of length zero.

    You must implement insertLine, sumLineTree, and lineCoverage. We supplied a shuffle
    function because the inputs must be shuffled to ensure a good time complexity for
    this algorithm. We also supplied the data structure you will use to represent your
    binary trees.

    After implementing these functions, please make a report:
        (1) Please explain the algorithm briefly in your own words
        (2) Give the runtime of the algorithm and a brief justification
        (3) Include screenshots of you running our tests cases and getting the desired
        output in utop

    Submission:
    - On Canvas: Required: This file, Report (pdf)
    - On Github: Required: This file
                 Optional: Report
*)

type 'a linetree =
  | Empty
  | Covered
  | LineTree of {
    p: float;
    left: 'a linetree;
    right: 'a linetree;
  }

let shuffle lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond

(*
  insertLine: This function will take a line and insert it
  into our tree representation of 1-dimensional space,
  returning that updated tree

  Input:
      - tree: a binary tree using the linetree datastructure
      - line: a float * float tuple encoding one line
  Output:
      - An updated linetree with the line added
  *)
  let rec insertLine tree (start, finish) =
    if start >= finish then tree
    else
      match tree with
      | Empty -> LineTree { p = start; left = Empty; right = insertLine Empty (start, finish) }
      | Covered -> Covered
      | LineTree { p; left; right } ->
          if start <= p then
            if finish <= p then
              LineTree { p; left = insertLine left (start, finish); right }
            else
              LineTree { p; left = insertLine left (start, p); right = insertLine right (p, finish) }
          else
            LineTree { p; left; right = insertLine right (start, finish) }
  

(*
  sumLineTree: A function that takes in a binary tree encoding 
  1-dimensional space and outputs the total length covered by
  the lines inserted into the tree

  Input:
      - tree: a binary tree using the linetree datastructure
  Output:
      - A float giving the length covered by the lines
  *)
  let rec sumLineTree ?(s=neg_infinity) ?(e=infinity) tree =
    match tree with
    | Empty -> 0.0
    | Covered -> e -. s
    | LineTree { p; left; right } ->
        let leftSum = sumLineTree ~s:s ~e:p left in
        let rightSum = sumLineTree ~s:p ~e:e right in
        leftSum +. rightSum
  

(*
  lineCovereage: A function that takes the two previous
  functions to perform the overlapping line coverage problem.
  Do not forget to shuffle the order in which you insert your
  lines in the binary tree.

  Input:
      - lines: A list of lines encoded as float * float tuples
  Output:
      - A float giving the length covered by the lines
  *)
  let lineCoverage lines =
    let shuffled = shuffle lines in
    let tree = List.fold_left (fun acc line -> insertLine acc line) Empty shuffled in
    sumLineTree tree
  



(*-----------------------------TEST CASES-----------------------------*)

(*ANSWER = 0*)
let testInput1 = []

(*ANSWER = 110*)
let testInput2 = [(1.,2.); (2.,3.); (4.,10.); (8.,12.); (50.,100.); (-100.,-50.)]

(*ANSWER = 0*)
let testInput3 = [(2., 1.); (5000., 5000.); (5., 5.); (100., 99.); (99., 0.); (infinity, neg_infinity)]

(*ANSWER = infinity*)
let testInput4 = [(0., 1.); (4., 5.); (neg_infinity, 0.); (102321., 4000000.)]

(*ANSWER = 121.876876180545253*)
let testInput5 = [(1.12312341, 2.12398719823); (2.1231234, 3.); (3.12312451, 100.12312412341); (98.1231231, 123.12312410054525)]

