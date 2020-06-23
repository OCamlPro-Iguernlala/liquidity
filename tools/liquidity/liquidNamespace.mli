(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                             Steven De Oliveira                           *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

open LiquidTypes

exception Unknown_namespace of string list * location

(** Parse namespaces in name *)
val unqualify : string -> string list * string

(** Prepend name with namespace *)
val add_path_name : string list -> string -> string

(** Normalize type present in environment [in_env] with fully
    qualified names, constructors and fields, with respect to
    [from_env] (or from top-level if no argument [~from_env] is
    given). *)
val normalize_type : ?from_env:env -> in_env:env -> datatype -> datatype

val find_env : loc:location -> string list -> env ->  env

(** Find a type by its qualified alias, returns a fully qualified
    normalized type *)
val find_type : loc:location -> string -> env -> datatype list -> datatype

(** Find a contract signature by its qualified alias *)
val find_contract_type : loc:location -> string -> env -> contract_sig

(** Find the type to which a label belongs, returns its normalized
    version together with the type of the field value and its position
    in the record *)
val find_label : loc:location -> string -> env -> datatype * (string * datatype * int)

(** Find the type to which a constructor belongs, returns its normalized
    version together with the type of the constructor argument and its position
    in the sum type *)
val find_constr : loc:location -> string -> env -> datatype * (string * datatype * int)

(** Find a qualified external primitive in the environment *)
val find_extprim : loc:location -> string -> env -> extprim

val is_extprim : string -> env -> bool

(** Look for a qualified global value exported in a sub module or
    another contract *)
val lookup_global_value :
  loc:location -> string -> typecheck_env -> typed_exp value

val find_contract :
    loc:location -> string -> env -> 'a contract StringMap.t -> 'a contract

val find_module :
    loc:location -> string list -> env -> 'a contract list -> 'a contract

(** For debug *)
val qual_contract_name : 'a contract -> string


(** Replaces aliases by their aliased values in a qualified name *)
val unalias_name : string -> env -> string
