let root = [Rule.initial (Fol.Pred("No goal yet!",[]))]

let curr_state = ref root

let question acc s =
  ("?" ^ s) :: acc

let print_param = function
  | (_, []) -> ()
  | (name, vars) ->
     print_endline (String.concat
                      " "
                      (name::"not in"::(List.fold_left question [] vars)))

let rec print_goals n = function
  | [] -> ()
  | g::gs ->
     Print.goal n g;
     print_goals (succ n) gs

let print state =
  let p = Rule.main state in
  let gs = Rule.subgoals state in
  Print.form p;
  if Rule.final state
  then print_endline "No subgoals left!"
  else begin
      print_goals 1 gs;
      List.iter print_param (List.fold_left Fol.goal_params [] gs)
    end

let seq_hd seq =
  match seq () with
  | Seq.Cons(h,_) -> h
  | _ -> failwith "Empty Seq"

let update_state state =
  print state;
  curr_state := state::(!curr_state)

let reset_state state =
  print state;
  curr_state := state::root

let get_state () = List.hd !curr_state

let undo () =
  if !curr_state != root
  then curr_state := List.tl !curr_state;
  print (List.hd !curr_state)

let goal s = reset_state (Rule.initial (Read.read s))

let by tactic =
  try update_state (seq_hd (tactic (get_state ())))
  with _ -> print_endline "** Tactic Failed **"
