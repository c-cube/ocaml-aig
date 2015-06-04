
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
(** A boolean variable *)

module VarSet : Set.S with type elt = var
module VarMap : Map.S with type key = var

type manager
(** Manager, stores graph nodes and ensures maximal sharing *)

val create : ?size:int -> unit -> manager
(** New manager *)

type t
(** A node of an AIG *)

(** {2 Basics} *)

val equal : t -> t -> bool
(** [O(1)] equality between nodes of the same manager *)

val compare : t -> t -> int
(** Total order between nodes of the same manager *)

val sign : t -> bool
(** Sign of the node (positive or negative) *)

val abs : t -> t
(** Remove sign (make positive) *)

val apply_sign : t -> bool -> t
(** [apply_sign t s] sets the sign of [t] to [s] *)

(** {2 Support} *)

val vars : t -> VarSet.t
(** Set of variables of [t] *)

val vars_fold : ('a -> var -> 'a) -> 'a -> t -> 'a
(** Fold once on each variable *)

val vars_iter : t -> (var -> unit) -> unit
(** [vars_iter t f] calls [f] on every variable of [t], possibly several
    times per variable *)

(** {2 Boolean Operators} *)

val true_ : t

val false_ : t

val var : man:manager -> var -> t
(** Variable node *)

val neg : t -> t
(** Negate node *)

val and_ : man:manager -> t -> t -> t
(** Conjunction of nodes *)

val or_ : man:manager -> t -> t -> t
(** Disjunction of nodes *)

val imply : man:manager -> t -> t -> t
(** implication *)

val equiv : man:manager -> t -> t -> t

val xor : man:manager -> t -> t -> t

val exists : man:manager -> VarSet.t -> t -> t
(** [exists ~man vars t] quantifies existentially on each variable of [vars]
    upon the formula [t] *)

val for_all : man:manager -> VarSet.t -> t -> t
(** [for_all ~man vars t] quantifies universally on each variable of [vars]
    upon the formula [t] *)

(** {2 Iteration} *)

type 'a view =
  | True
  | Var of var
  | Neg of 'a
  | And of 'a * 'a

val fold : ('a view -> 'a) -> t -> 'a
(** Fold on the formula, down to the leaves *)

val fold_nodes : ('a -> t view -> 'a) -> 'a -> t -> 'a
(** Traverse the nodes exactly once, in an unspecified order *)

val size : t -> int
(** Traverse the graph to compute the number of nodes it contains *)

(** {2 Evaluation} *)

val eval_fun : (var -> bool) -> t -> bool
(** Evaluate a graph under a boolean valuation as a function *)

val eval : bool VarMap.t -> t -> bool
(** Evaluate a graph under a boolean valuation as a map *)

(** {2 IO} *)

val pp : Format.formatter -> t -> unit
(** Recursive printing, without sharing *)

val pp_shared : Format.formatter -> t -> unit
(** Recursive printing, with sharing *)

val pp_dot : Format.formatter -> t -> unit
(** [pp_dot out t] prints the graph [t] as a "digraph" on the given formatter *)

val pp_dot_l : Format.formatter -> t list -> unit
(** Print a list of graphs that possibly share sub-parts *)

val dot_to_file : string -> t -> unit
(** [dot_to_file name t] writes the graph [t] in the file [name] *)

