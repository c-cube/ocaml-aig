(* Random Tests for AIG *)

module A = QCheck.Arbitrary

module Bool = struct
  type 'a view =
    | True
    | Var of int
    | Not of 'a
    | And of 'a * 'a
    | Or of 'a * 'a

  (* simple, stupid boolean expressions *)
  type t = { view: t view }

  let make_ view = {view}
  let true_ = make_ True
  let neg x = make_ (Not x)
  let false_ = neg true_
  let and_ x y = make_ (And (x,y))
  let or_ x y = make_ (Or (x,y))
  let var v = make_ (Var v)

  let rec eval_fun f t = match t.view with
    | True -> true
    | Var v -> f v
    | Not t -> not (eval_fun f t)
    | And (a,b) -> eval_fun f a && eval_fun f b
    | Or (a,b) -> eval_fun f a || eval_fun f b

  let eval map t = eval_fun (fun v -> Aig.VarMap.find v map) t

  let rec pp out t = match t.view with
    | True -> Format.fprintf out "1"
    | Not t -> Format.fprintf out "@[<hov1>¬@ %a@]" pp_inner t
    | Var v -> Format.fprintf out "v%d" v
    | And (a,b) -> Format.fprintf out "@[<hv2>(and@ %a@ %a)@]" pp_inner a pp_inner b
    | Or (a,b) -> Format.fprintf out "@[<hv2>(or@ %a@ %a)@]" pp_inner a pp_inner b
  and pp_inner out t = match t.view with
    | True
    | Or _
    | And _
    | Var _ -> pp out t
    | Not _ -> Format.fprintf out "(%a)" pp t

  let to_string t = CCFormat.sprintf "%a" pp t

  let vars t =
    let rec vars_rec acc t = match t.view with
      | True -> acc
      | Var v -> Aig.VarSet.add v acc
      | Not a -> vars_rec acc a
      | And (a,b)
      | Or (a,b) ->
        let acc = vars_rec acc a in
        vars_rec acc b
    in
    vars_rec Aig.VarSet.empty t

  (* convert into an AIG *)
  let rec to_aig man t = match t.view with
    | True -> Aig.true_ man
    | Or (a, b) -> Aig.or_ (to_aig man a) (to_aig man b)
    | And (a, b) -> Aig.and_ (to_aig man a) (to_aig man b)
    | Not a -> Aig.neg (to_aig man a)
    | Var v -> Aig.var man v

  let rec list_range i j = if i=j then [] else i:: (list_range (i+1) j)

  let rand : t A.t =
    let open A in
    (1 -- 30) >>= fun n_var ->
    (* random vars + true *)
    let base = among ([true_; false_] @ List.map var (list_range 1 n_var)) in
    fix ~base
      (fun self ->
         choose
           [ pure neg <*> self
           ; pure and_ <*> self <*> self
           ; pure or_ <*> self <*> self
           ]
      )
end

(*
let () =
  let cases = A.generate ~n:3 Bool.rand in
  List.iter
    (fun c -> Format.printf "case: %a@." Bool.pp c)
    cases
*)

let pp_valuation out (map:bool Aig.VarMap.t) =
  let items = Aig.VarMap.fold (fun k v acc -> (k,v)::acc) map [] in
  Format.fprintf out "@[<hv1>%a@]"
    (CCFormat.list (CCFormat.pair CCFormat.int CCFormat.bool)) items

(* [(var * bool) list -> VarMap.t] *)
let mk_val
  : (int * bool) list -> bool Aig.VarMap.t
  = fun l -> List.fold_left (fun acc (v,b) -> Aig.VarMap.add v b acc) Aig.VarMap.empty l

(* set of variables -> random valuation *)
let rand_valuation vars : bool Aig.VarMap.t A.t =
  let l = Aig.VarSet.elements vars in
  A.(
    let gens = List.map (fun v -> bool >>= fun b -> return (v, b)) l in
    fun st -> mk_val (List.map (fun g -> g st) gens)
  )

let rand_valuations vars = A.list ~len:A.(1 -- 8) (rand_valuation vars)

let test_eval =
  let man = Aig.create () in
  (* generates (form, aig(form), random valuations) *)
  let gen = A.(
    Bool.rand >>= fun form ->
    let vars = Bool.vars form in
    let aig = Bool.to_aig man form in
    rand_valuations vars >>= fun valuations ->
    return (form, aig, valuations)
  )
  and prop (form, aig, valuations) =
    List.for_all
      (fun v -> Bool.eval v form = Aig.eval v aig)
      valuations
  and pp (form, aig, valuations) =
    CCFormat.sprintf "@[<v>form:%a@,aig:%a@,valuations:@[<hv1>%a@]@]"
      Bool.pp form Aig.pp aig (CCFormat.list pp_valuation) valuations
  in
  QCheck.mk_test ~pp ~name:"eval_correct" gen prop

let suite = [ test_eval ]

let () =
  let ok = QCheck.run_tests suite in
  if ok then () else exit 1

