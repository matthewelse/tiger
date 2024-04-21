open! Core
module Array_id = Unique_id.Int ()
module Record_id = Unique_id.Int ()

type t =
  | Int
  | String
  | Record of
      { fields : (Field_id.t * t) list
      ; id : Record_id.t
      }
  | Array of
      { element_type : t
      ; id : Array_id.t
      }
  | Nil
  | Unit
  | Name of
      { name : Type_id.t
      ; mutable type_ : t option
      }
[@@deriving equal, typed_variants, sexp_of]

let named name = Name { name; type_ = None }

let rec require_no_illegal_cycles_exn t reached =
  match t with
  | Name { name; type_ } ->
    if Set.mem reached name
    then failwith [%string "Illegal cycle in type definition %{Type_id.to_string name}"]
    else (
      match type_ with
      | None -> ()
      | Some type_ -> require_no_illegal_cycles_exn type_ (Set.add reached name))
  | Record _ ->
    (* Recursive types via records are allowed. *)
    ()
  | Array _ | Nil | Int | String | Unit -> ()
;;

let rec resolve t (tenv : (Type_id.t, t) Scoped_table.t) =
  match t with
  | Name named ->
    (match Scoped_table.find tenv named.name with
     | Some t' ->
       named.type_ <- Some t';
       require_no_illegal_cycles_exn t Type_id.Set.empty
     | None -> failwith "unresolved type")
  | Array { element_type; _ } -> resolve element_type tenv
  | Record { fields; _ } -> List.iter fields ~f:(fun (_, t) -> resolve t tenv)
  | Int | String | Nil | Unit -> ()
;;

let rec require_exn : 'k. t -> 'k Typed_variant.t -> 'k =
  fun t variant ->
  match t with
  | Name { type_ = Some t; _ } -> require_exn t variant
  | _ ->
    (match Typed_variant.get variant t with
     | Some x -> x
     | None ->
       let { f = T typ } = Typed_variant.which t in
       failwith
         [%string
           "Expected a type of variant %{Typed_variant.name variant} but got \
            %{Typed_variant.name typ}"])
;;

let rec require_match_exn t1 t2 =
  match t1, t2 with
  | Name { type_ = Some t1; _ }, t2 | t1, Name { type_ = Some t2; _ } ->
    require_match_exn t1 t2
  | Record _, Nil | Nil, Record _ -> ()
  | _ -> if equal t1 t2 then () else raise_s [%message "Type mismatch" (t1 : t) (t2 : t)]
;;

module Function = struct
  type typ = t

  type t =
    { formal_args : typ list
    ; result : typ
    }
end
