open Monolith

let integer_of_int = Ortac_runtime.Gospelstdlib.integer_of_int

module C = struct
  include Ortac_runtime.Gospelstdlib.Set

  let equal s1 s2 = compare s1 s2 = integer_of_int 0
  let map_plus_one s = map (fun x -> x + 1) s
  let fold_plus s = fold ( + ) s 0
  let exists = _exists
  let desc_values s = to_list s
end

module IntOrderedType = struct
  type t = int

  let compare = compare
end

module IntSet = Set.Make (IntOrderedType)

module R = struct
  include IntSet

  let cardinal s = integer_of_int (cardinal s)
  let map_plus_one s = map (fun x -> x + 1) s
  let fold_plus s = fold ( + ) s 0
  let desc_values s = List.of_seq (to_rev_seq s)
  let choose_opt = max_elt_opt
end

let () =
  let set = declare_abstract_type ()
  and int' = int_within (Gen.closed_interval (-20) 100)
  and integer =
    deconstructible (fun x -> Obj.magic x |> Z.to_string |> Print.string)
  and preds =
    [
      ((fun x -> x mod 2 = 0), constant "even");
      ((fun x -> x > 10), constant "(> 10)");
    ]
  in
  let pred = constructible (Gen.choose preds) in
  (* compare: there is no reason the order should be similar in the two
     implementations, let us only compare them for equality *)
  declare "equal" (set ^> set ^> bool) R.equal C.equal;
  declare "empty" set R.empty C.empty;
  declare "is_empty" (set ^> bool) R.is_empty C.is_empty;
  declare "mem" (int' ^> set ^> bool) R.mem C.mem;
  declare "add" (int' ^> set ^> set) R.add C.add;
  declare "singleton" (int' ^> set) R.singleton C.singleton;
  declare "remove" (int' ^> set ^> set) R.remove C.remove;
  declare "union" (set ^> set ^> set) R.union C.union;
  declare "inter" (set ^> set ^> set) R.inter C.inter;
  declare "disjoint" (set ^> set ^> bool) R.disjoint C.disjoint;
  declare "diff" (set ^> set ^> set) R.diff C.diff;
  declare "subset" (set ^> set ^> bool) R.subset C.subset;
  declare "cardinal" (set ^> integer) R.cardinal C.cardinal;
  (* choose *)
  declare "choose_opt" (set ^> option int) R.choose_opt C.choose_opt;
  declare "map_plus_one" (set ^> set) R.map_plus_one C.map_plus_one;
  declare "fold_plus" (set ^> int) R.fold_plus C.fold_plus;
  declare "for_all" (pred ^> set ^> bool) R.for_all C.for_all;
  declare "exists" (pred ^> set ^> bool) R.exists C.exists;
  declare "filter" (pred ^> set ^> set) R.filter C.filter;
  (* filter_map *)
  declare "partition" (pred ^> set ^> (set *** set)) R.partition C.partition;
  (* as a way to test [to_list] *)
  declare "desc_values" (set ^> list int) R.desc_values C.desc_values;
  main 20
