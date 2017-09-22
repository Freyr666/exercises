let rec last = function
  | [] -> failwith "empty"
  | [x] -> x
  | _::tl -> last tl

let gb_coins = [50;20;10;5;2;1]
let us_coins = [25;10;5;1]

let rec all_change = function
  | (coins, coinvals, 0) -> [coins]
  | (coins, [], amount)  -> []
  | (coins, c::coinvals, amount) ->
     if amount < 0 then []
     else all_change (c::coins, c::coinvals, amount - c) @ all_change (coins, coinvals, amount)

let ilist_to_string l = String.concat " " @@ List.map string_of_int l
let ilistlist_to_string l = String.concat " " @@ List.map (fun x -> Printf.sprintf "[ %s ]" (ilist_to_string x)) l
    
let () =
  Printf.printf "last [1;2;3;4]: %d\n" (last [1;2;3;4]);
  Printf.printf "all change gb 16: %s\n" (ilistlist_to_string @@ all_change ([], gb_coins, 16))
