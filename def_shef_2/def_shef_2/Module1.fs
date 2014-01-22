// Learn more about F# at http://fsharp.net

module Module1

//Warmup 1
let doubled = List.map(fun x -> x * 2)
doubled [1;2;3;4]

//Warmup 2
let evens = List.filter(fun x -> x % 2 = 0)
evens [1;2;3;4;5;6;7]

//Warmup 3
let multiplied = List.reduce(fun acc elem -> acc * elem)
multiplied [1;2;3;4;5]

//Warmup 4
let rec reverse_list l =
    match l with
    | [] -> []
    | head :: tail -> reverse_list tail @ [head]
// let rec reverse_list l = List.rev
reverse_list [1;2;3;4;5]

//Warmup 5
let rec get_element l n =
    match l with
    | head :: tail ->
        match n with 
        | 1 -> head
        | _ -> get_element tail (n - 1)
get_element [5;6;7;8;] 3       

//Warmup 5 v2
let rec get_element l n =
    match l, n with
    | [], _ -> None
    | head :: tail, 1 -> Some(head)
    | head :: tail, _ -> get_element tail (n - 1)
get_element [5;6;7;8;] 3        

//Beginner 1
let odd_square_sum l = l |> List.filter(fun x -> x % 2 <> 0) 
                         |> List.map(fun x -> x * x) 
                         |> List.reduce (fun acc x -> acc + x)
odd_square_sum [4;5;6;7;8;9]

//Beginner 2
let rec slice l m n =
    match l, m, n with
    | [], _, _ -> []
    | _, _, 0 -> []
    | h::t, 0, _ -> h :: slice t 0 (n-1)
    | h::t, _, _ -> slice t (m-1) n 
slice [3; 4; 5; 6; 7; 8; 9] 2 4

//Beginner 3
let rec fib n = 
    match n with
    | 1 -> [1]
    | 2 -> [1; 1]
    | _ -> fib (n-1) @ [fib(n-1).Item(n-2) + fib(n-1).Item(n-3)] 
fib 9

//Beginner 3 v2
let rec fib n =
    let rec fib_value m =
        if m <= 2 then 1
        else fib_value(m-1) + n
    
    match n with
    | 0 -> []
    | _ -> fib (n-1) @ [fib_value n]        
fib 9

//Beginner 4
open System.Collections.Generic
let memoize f n =
    let cache = new Dictionary<_,_>()
    if cache.ContainsKey(n)
    then cache.[n]
    else 
        let memo = f n
        cache.Add(n, memo)
        memo

let rec fib n =
  let rec fib_value m =
    if m <= 2 then 1
    else fib_value(m-1) + n
                        
  match n with
  | 0 -> []
  | _ -> fib (n-1) @ [memoize fib_value n]        
fib 9

//Expert 1
let lines (rows : char list list) =
     [[rows.[0].[0]; rows.[0].[1]; rows.[0].[2]];
      [rows.[1].[0]; rows.[1].[1]; rows.[1].[2]];
      [rows.[2].[0]; rows.[2].[1]; rows.[2].[2]];
      [rows.[0].[0]; rows.[1].[0]; rows.[2].[0]];
      [rows.[0].[1]; rows.[1].[1]; rows.[2].[1]];
      [rows.[0].[2]; rows.[1].[2]; rows.[2].[2]];
      [rows.[0].[0]; rows.[1].[1]; rows.[2].[2]];
      [rows.[0].[2]; rows.[1].[1]; rows.[2].[0]]]
      
let winners lines =
    List.filter (fun (line : char list) -> line.[0] = line.[1] && line.[1] = line.[2]) lines
    
let winner rows = 
    if  winners(rows) = [] then ' '
    elif winners(rows).[0].[0] = 'x' then 'x'
    else 'o'
          
winner (lines [['o'; ' '; 'o'];['x';'x';'x'];[' ';' ';' ']])