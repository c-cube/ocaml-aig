
(*
copyright (c) 2015, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 And-Inverter Graphs} *)

type var = int

(* unsigned node *)
type unode_cell =
  | NTrue
  | NVar of var
  | NAnd of node * node
and node = {
  cell: unode_cell;
  id: int;  (* > 0 iff the node is positive, < 0 if negated *)
  mutable flag: int;  (* used for traversals *)
  mutable opp: node; (* opposite node *)
}

let id n = n.id

(** Weak set of unodes *)
module Tbl = Weak.Make(struct
  type t = node (* stores positive nodes *)
  let equal n1 n2 = match n1.cell, n2.cell with
    | NTrue, NTrue -> true
    | NVar v1, NVar v2 -> v1==v2
    | NAnd (a1,b1), NAnd (a2,b2) -> a1 == a2 && b1 == b2
    | NTrue, _
    | NVar _, _
    | NAnd _, _ -> false
  let hash n = match n.cell with
    | NTrue -> 42
    | NVar v -> Hashtbl.hash v
    | NAnd (a,b) -> Hashtbl.hash (a.id, b.id)
end)

type manager = {
  tbl : Tbl.t;
  mutable unode_id : int;
}

let create ?(size=1024 * 32) () =
  let man = {
    tbl=Tbl.create size;
    unode_id=2;  (* [0] is invalid, [1] is [true_] *)
  } in
  man

type t = {
  node: node;
  man: manager;
}

let fail_man_ () = invalid_arg "incompatible managers"

let equal a b =
  a.man == b.man && a.node == b.node

let compare a b =
  if a.man != b.man then fail_man_ ();
  Pervasives.compare a.node.id b.node.id

let manager t = t.man

let sign t = t.node.id > 0

let abs t = if t.node.id > 0 then t else {t with node=t.node.opp; }

let neg t = {t with node=t.node.opp}

let apply_sign t sign =
  if sign then abs t else neg (abs t)

let rec bot_ = {cell=NTrue; id=0; flag=0; opp=bot_}  (* absurd value, do not export *)

let rec mk_true_ = { cell=NTrue; id= 1; flag=0; opp=mk_false_ }
and mk_false_ = { cell=NTrue; id= ~-1; flag=0; opp=mk_true_ }

let true_ man = {man; node=mk_true_; }
let false_ man = {man; node=mk_false_; }

let hashcons_ man n =
  let n' = Tbl.merge man.tbl n in
  (* new node *)
  if n==n' then (
    assert (n'.opp == bot_ && n'.flag==0);
    man.unode_id <- man.unode_id + 1;
    n'.opp <- {cell=n'.cell; id= - n'.id; flag=0; opp=n';}
  );
  n'

let var man v =
  let node = hashcons_ man {cell=NVar v; id=man.unode_id; flag=0; opp=bot_ } in
  { man; node; }

let mk_neg t = t.opp

(* low level constructor *)
let mk_and_real_ man n1 n2 =
  hashcons_ man {cell=NAnd (n1, n2); id=man.unode_id; flag=0; opp=bot_ }

(* build [a and b] but enforces some invariants *)
let mk_and ~man a b = match () with
  | () when a==mk_true_ -> b (* true and b --> b *)
  | () when b==mk_true_ -> a (* a and true --> a *)
  | () when a == mk_false_ || b == mk_false_ -> mk_false_ (* false and _ --> false *)
  | () when a == b ->
    (* a and a --> a *)
    a
  | () when a.id == - b.id ->
    (* a and -a --> false *)
    mk_false_
  | () when a.id > b.id ->
    (* smallest node first *)
    mk_and_real_ man b a
  | () ->
    mk_and_real_ man a b

(* [a or b] is [not (not a and not b)] *)
let mk_or ~man a b = mk_neg (mk_and ~man (mk_neg a) (mk_neg b))

(* [a => b] is [not (a and (not b))] *)
let mk_imply ~man a b = mk_neg (mk_and ~man a (mk_neg b))

(* [a equiv b] is [(a => b) and (b => a)] *)
let mk_equiv ~man a b = mk_and ~man (mk_imply ~man a b) (mk_imply ~man b a)

(* [a xor b] is [(a and not b) or (not a and b)] *)
let mk_xor ~man a b = mk_or ~man (mk_and ~man a (mk_neg b)) (mk_and ~man (mk_neg a) b)

let and_ a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_and ~man:a.man a.node b.node; }

let or_ a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_or ~man:a.man a.node b.node; }

let imply a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_imply ~man:a.man a.node b.node; }

let equiv a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_equiv ~man:a.man a.node b.node; }

let xor a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_xor ~man:a.man a.node b.node; }

module Infix = struct
  let (~-) = neg
  let (&&) = and_
  let (||) = or_
  let (==>) = imply
  let (<=>) = equiv
  end

(** {2 Iteration} *)

type 'a view =
  | True
  | Var of var
  | Neg of 'a
  | And of 'a * 'a

let rec fold_rec tbl f n =
  try
    Hashtbl.find tbl n.id
  with Not_found ->
    let res = match n.id with
      | 0 -> assert false
      | x when x < 0 ->
        (* negation *)
        f (Neg (fold_rec tbl f n.opp))
      | _ ->
        match n.cell with
        | NTrue -> f True
        | NVar v -> f (Var v)
        | NAnd (a, b) ->
          let a' = fold_rec tbl f a in
          let b' = fold_rec tbl f b in
          f (And (a', b'))
    in
    (* memoize *)
    Hashtbl.replace tbl n.id res;
    res

let heuristic_tbl_size_ t = max 16 (min 128 (Tbl.count t.man.tbl))

let fold f t =
  fold_rec (Hashtbl.create (heuristic_tbl_size_ t)) f t.node

(* TODO: pretty printing (with and without sharing, using backrefs) *)

(* TODO: AIGER parser (see "http://fmv.jku.at/aiger/") 
   TODO: printer to AIGER, too? *)

(* TODO: graphviz export *)

(* TODO: compression by depth-two scanning, see
  "DAG-Aware Circuit Compression For Formal Verification"*)

(** {2 Evaluation} *)

let rec eval_rec tbl f n =
  try
    Hashtbl.find tbl n.id
  with Not_found ->
    let res = match n.cell with
      | NTrue -> n.id > 0
      | NVar v ->
        if n.id > 0 then f v else not (f v)
      | NAnd (a, b) ->
        let v =
          eval_rec tbl f a
          && eval_rec tbl f b (* evaluated only if a=true *)
        in
        if n.id > 0 then v else not v
    in
    Hashtbl.replace tbl n.id res;
    res

let eval valuation t =
  eval_rec (Hashtbl.create (heuristic_tbl_size_ t)) valuation t.node

(* XXX: naive version:
let eval valuation t =
  fold
    (function
      | True -> true
      | Var v -> valuation v
      | Neg b -> not b
      | And (b1, b2) -> b1 && b2
    ) t
 *)

