
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

type account_spec = {name : string; id : id; balance : int} ;;

let id_list = ref [] ;;
let id_info = ref [] ;;

let rec initialize lst =
  match lst with
  | [] -> ()
  | {name; id; balance} :: tl ->
    id_list := string_of_int id :: !id_list;
    id_info := {name; id; balance} :: !id_info;
    initialize tl ;;

let get_name id_input =
  let lst = !id_info in
  let rec find_name lst' =
    match lst' with
    | [] -> raise Not_found
    | {name; id; balance = _} :: tl ->
      if id = id_input then name
      else find_name tl in
  find_name lst ;;

let get_balance id_input =
  let lst = !id_info in
  let rec find_balance lst' =
    match lst' with
    | [] -> raise Not_found
    | {name = _; id; balance} :: tl ->
      if id = id_input then balance
      else find_balance tl in
  find_balance lst ;;

let rec acquire_id () =
  Printf.printf "Enter customer id: ";
  let id = read_line () in
  let new_id =
  if List.mem id !id_list then int_of_string id
  else
    begin
      print_endline "Invalid id";
      acquire_id ()
    end in
  new_id ;;

let acquire_amount () =
  Printf.printf "Enter amount: ";
  let amount = read_int () in
  amount ;;

let acquire_act () =
  Printf.printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
  let str = read_line () in
  let act =
  match str with
    | "B" -> Balance
    | "+" -> Deposit (acquire_amount ())
    | "-" -> Withdraw (acquire_amount ())
    | "=" -> Next
    | "X" -> Finished
    | _ -> raise (Invalid_argument "not a choice") in
  act ;;

let update_balance id_input value =
  let lst = !id_info in
  let updated_lst =
  let rec find_balance prev lst' =
    match lst' with
    | [] -> raise Not_found
    | {name; id; balance} :: tl ->
      if id = id_input then prev @ [{name; id; balance = value}] @ tl
      else find_balance ({name; id; balance} :: prev) tl in
  find_balance [] lst in
  id_info := updated_lst ;;

let present_message = print_endline ;;

let deliver_cash value =
  Printf.printf "Here's your cash: ";
  for _ = 1 to value / 20 do
    Printf.printf "[20 @ 20]";
  done ;
  Printf.printf " and ";
  print_int (value mod 20);
  print_endline " more" ;;
