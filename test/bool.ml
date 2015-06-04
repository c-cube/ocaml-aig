module A = QCheck.Arbitrary

(* specification: trivial boolean formulas *)
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

let or_list = List.fold_left or_ false_
let and_list = List.fold_left and_ true_

let rec eval_fun f t = match t.view with
  | True -> true
  | Var v -> f v
  | Not t -> not (eval_fun f t)
  | And (a,b) -> eval_fun f a && eval_fun f b
  | Or (a,b) -> eval_fun f a || eval_fun f b

let eval map t = eval_fun (fun v -> AIG.VarMap.find v map) t

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
    | Var v -> AIG.VarSet.add v acc
    | Not a -> vars_rec acc a
    | And (a,b)
    | Or (a,b) ->
      let acc = vars_rec acc a in
      vars_rec acc b
  in
  vars_rec AIG.VarSet.empty t

(* apply subst to n *)
let rec subst map n = match n.view with
  | True -> n
  | Var v ->
    begin try
      AIG.VarMap.find v map
    with Not_found ->
      n
    end
  | Not n -> neg (subst map n)
  | And (a,b) -> and_ (subst map a) (subst map b)
  | Or (a,b) -> or_ (subst map a) (subst map b)

(* build all valuations over [vars] *)
let all_valuations_ vars =
  AIG.VarSet.fold
    (fun v l ->
      CCList.flat_map
        (fun sigma ->
           [ AIG.VarMap.add v true_ sigma
           ; AIG.VarMap.add v false_ sigma
           ]
        ) l
    ) vars [AIG.VarMap.empty]

(* quantification over [vars] *)
let exists vars n =
  let sigma_l = all_valuations_ vars in
  let l = List.map (fun sigma -> subst sigma n) sigma_l in
  or_list l

let for_all vars n =
  let sigma_l = all_valuations_ vars in
  let l = List.map (fun sigma -> subst sigma n) sigma_l in
  and_list l

(* convert into an AIG *)
let rec to_aig man t = match t.view with
  | True -> AIG.true_
  | Or (a, b) -> AIG.or_ ~man (to_aig man a) (to_aig man b)
  | And (a, b) -> AIG.and_ ~man (to_aig man a) (to_aig man b)
  | Not a -> AIG.neg (to_aig man a)
  | Var v -> AIG.var ~man v

let rec list_range i j = if i=j then [] else i:: (list_range (i+1) j)

let rand size : t A.t =
  let open A in
  (1 -- 30) >>= fun n_var ->
  (* random vars + true *)
  let base = among ([true_; false_] @ List.map var (list_range 1 n_var)) in
  fix ~max:size ~base
    (fun self ->
       choose
         [ pure neg <*> self
         ; pure and_ <*> self <*> self
         ; pure or_ <*> self <*> self
         ]
    )

