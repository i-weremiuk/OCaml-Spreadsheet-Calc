open Ast

module M = Map.Make(String)

let parse (s : string) : expr =
  Parser.main Lexer.read (Lexing.from_string s)


type calc_phase = Waiting | Started | Finished

exception Cycle_error

type env = value M.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value
  | VClosure of ident * expr * env
  | VRecClosure of ident * ident * expr * env

let rec show_value v =
  match v with
  | VInt n -> string_of_int n
  | VBool v -> string_of_bool v
  | VUnit -> "()"
  | VPair (v1,v2) -> "(" ^ show_value v1 ^ ", " ^ show_value v2 ^ ")"
  | VClosure _ | VRecClosure _ -> "<fun>"

let eval_op (op : bop) (val1 : value) (val2 : value) : value =
  match op, val1, val2 with
  | Add,  VInt  v1, VInt  v2 -> VInt  (v1 + v2)
  | Sub,  VInt  v1, VInt  v2 -> VInt  (v1 - v2)
  | Mult, VInt  v1, VInt  v2 -> VInt  (v1 * v2)
  | Div,  VInt  v1, VInt  v2 -> VInt  (v1 / v2)
  | And,  VBool v1, VBool v2 -> VBool (v1 && v2)
  | Or,   VBool v1, VBool v2 -> VBool (v1 || v2)
  | Leq,  VInt  v1, VInt  v2 -> VBool (v1 <= v2)
  | Lt,   VInt  v1, VInt  v2 -> VBool (v1 < v2)
  | Gt,   VInt  v1, VInt  v2 -> VBool (v1 > v2)
  | Geq,  VInt  v1, VInt  v2 -> VBool (v1 >= v2)
  | Neq,  _,        _        -> VBool (val1 <> val2)
  | Eq,   _,        _        -> VBool (val1 = val2)
  | _,    _,        _        -> failwith "type error"

let rec eval_env (env : env) (e : expr) (expr_mat : expr array array) (v_mat : value option array array) (phase_mat : calc_phase array array) : value = 
  match e with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Binop (op, e1, e2) ->
      eval_op op (eval_env env e1 expr_mat v_mat phase_mat) 
      (eval_env env e2 expr_mat v_mat phase_mat)
  | If (b, t, e) ->
      (match eval_env env b expr_mat v_mat phase_mat with
        | VBool true -> eval_env env t expr_mat v_mat phase_mat
        | VBool false -> eval_env env e expr_mat v_mat phase_mat
        | _ -> failwith "type error")
  | Var x ->
     (match M.find_opt x env with
       | Some v -> v
       | None -> failwith "unknown var")
  | Let (x, e1, e2) ->
      eval_env (M.add x (eval_env env e1 expr_mat v_mat phase_mat) env) e2 expr_mat v_mat phase_mat
  | Pair (e1, e2) -> VPair (eval_env env e1 expr_mat v_mat phase_mat, eval_env env e2 expr_mat v_mat phase_mat)
  | Unit -> VUnit
  | Fst e ->
      (match eval_env env e expr_mat v_mat phase_mat with
        | VPair (v1, _) -> v1
        | _ -> failwith "Type error")
  | Snd e ->
      (match eval_env env e expr_mat v_mat phase_mat with
        | VPair (_, v2) -> v2
        | _ -> failwith "Type error")
  | Match (_e1, _x, _y, _e2) ->
      failwith "Not implemented"
  | IsPair e ->
      (match eval_env env e expr_mat v_mat phase_mat with
        | VPair _ -> VBool true
        | _ -> VBool false)
  | Fun (x, e) -> VClosure (x, e, env)
  | Funrec (f, x, e) -> VRecClosure (f, x, e, env)
  | App (e1, e2) ->
      let v1 = eval_env env e1 expr_mat v_mat phase_mat in
      let v2 = eval_env env e2 expr_mat v_mat phase_mat in
      (match v1 with
        | VClosure (x, body, clo_env) ->
            eval_env (M.add x v2 clo_env) body expr_mat v_mat phase_mat
        | VRecClosure (f, x, body, clo_env) as c ->
            eval_env (clo_env |> M.add x v2 |> M.add f c) body expr_mat v_mat phase_mat
        | _ -> failwith "not a function")
  | Cell (row, col) ->
      (match phase_mat.(row).(col) with
      | Waiting -> 
          phase_mat.(row).(col) <- Started;
          let v = eval_env M.empty (expr_mat.(row).(col)) expr_mat v_mat phase_mat in
          v_mat.(row).(col) <- Some v;
          phase_mat.(row).(col) <- Finished;
          v
      | Started -> raise Cycle_error 
      | Finished ->
          (match v_mat.(row).(col) with
          | Some v -> v
          | None -> failwith "Logic_bug_error" ) )


let eval_spreadsheet (s : expr list list) : value list list option =
  try
    let row_num = List.length s in
    let col_num = if row_num = 0 then 0 else List.length (List.hd s) in
    let expr_mat : expr array array = Array.of_list (List.map Array.of_list s) in
    let v_mat = Array.make_matrix row_num col_num None in
    let phase_mat = Array.make_matrix row_num col_num Waiting in

    for i = 0 to row_num - 1 do
      for j = 0 to col_num - 1 do
        if phase_mat.(i).(j) = Waiting then
          let v = eval_env M.empty expr_mat.(i).(j) expr_mat v_mat phase_mat in
          v_mat.(i).(j) <- Some v;
          phase_mat.(i).(j) <- Finished;
          ()
        else ()
      done;
    done;

    let res : value list list = 
      List.mapi (fun i row -> List.mapi (fun j _ -> 
        (match v_mat.(i).(j) with
        | Some v -> v
        | None -> failwith "Logic_bug_error") )
        row
      )
      s
    in
    Some res  

  with Cycle_error -> None




let parse_and_eval_spreadsheet (s : string list list) : string list list option =
  let es = List.map (List.map parse) s in
  let vs = eval_spreadsheet es in
  Option.map (List.map (List.map show_value)) vs

