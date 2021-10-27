  $ ortac report ../suite/arrays/lib.mli
  Lib
  File "../suite/arrays/lib.mli", line 1, characters 0-216:
  the value create:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 3, characters 13-19:
      `n >= 0' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 4, characters 12-32:
      `Array.length arr = n' has  been translated
    + File "../suite/arrays/lib.mli", line 5, characters 12-46:
      `Array.for_all (fun x -> x = v) arr' has  been translated
    + File "../suite/arrays/lib.mli", line 6, characters 12-47:
      `forall i. 0 <= i < n -> arr.(i) = v' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    n is not consumed and is not modified
    + Invariants involved:
      
    v is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 8, characters 0-126:
  the value bad_create:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 10, characters 12-47:
      `forall i. 0 <= i < n -> arr.(i) = v' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    n_1 is not consumed and is not modified
    + Invariants involved:
      
    v_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_1
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 12, characters 0-125:
  the value get:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 14, characters 13-38:
      `0 <= i < Array.length arr' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 15, characters 12-23:
      `o = arr.(i)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_2 is not consumed and is not modified
    + Invariants involved:
      
    i_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 17, characters 1-89:
  the Gospel axiom a has been translatedFile "../suite/arrays/lib.mli", line 21, characters 0-133:
  the value bad_get:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 23, characters 13-38:
      `0 <= i < Array.length arr' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 24, characters 12-23:
      `o = arr.(i)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_4 is not consumed and is not modified
    + Invariants involved:
      
    i_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_1
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 26, characters 0-244:
  the value set:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 28, characters 13-38:
      `0 <= i < Array.length arr' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 29, characters 13-24:
      `arr.(i) = v' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_5 is not consumed and is not modified
    + Invariants involved:
      
    i_5 is not consumed and is not modified
    + Invariants involved:
      
    v_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arrays/lib.mli", line 35, characters 0-197:
  the value fill:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arrays/lib.mli", line 37, characters 13-54:
      `0 <= ofs <= ofs + len <= Array.length arr' has  been translated
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 38, characters 13-58:
      `forall j. ofs <= j < ofs + len -> arr.(j) = v' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    arr_6 is not consumed and is not modified
    + Invariants involved:
      
    ofs is not consumed and is not modified
    + Invariants involved:
      
    len is not consumed and is not modified
    + Invariants involved:
      
    v_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arrays/lib.mli", line 40, characters 0-98:
  the value length:
   - Pure: true
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 43, characters 12-30:
      `i = Array.length a' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    i_6
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 45, characters 0-170:
  the value map:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 47, characters 12-33:
      `length arr = length a' has  been translated
    + File "../suite/arrays/lib.mli", line 48, characters 12-60:
      `forall i. 0 <= i < length a -> arr.(i) = f a.(i)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    f is not consumed and is not modified
    + Invariants involved:
      
    a_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_7
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 50, characters 0-192:
  the value bad_map_length:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 52, characters 12-33:
      `length arr = length a' has  been translated
    + File "../suite/arrays/lib.mli", line 53, characters 12-60:
      `forall i. 0 <= i < length a -> arr.(i) = f a.(i)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    f_1 is not consumed and is not modified
    + Invariants involved:
      
    a_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_8
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 55, characters 0-191:
  the value bad_map_fun:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 57, characters 12-33:
      `length arr = length a' has  been translated
    + File "../suite/arrays/lib.mli", line 58, characters 12-60:
      `forall i. 0 <= i < length a -> arr.(i) = f a.(i)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    f_2 is not consumed and is not modified
    + Invariants involved:
      
    a_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    arr_9
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 60, characters 0-176:
  the value sort:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 62, characters 12-123:
      `forall i. 0 <= i < Array.length a
              -> forall j. i < j < Array.length a
              -> a.(i) <= a.(j)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arrays/lib.mli", line 66, characters 0-463:
  the value copy_sort:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 68, characters 12-43:
      `Array.length r = Array.length a' has  been translated
    + File "../suite/arrays/lib.mli", line 69, characters 12-123:
      `forall i. 0 <= i < Array.length r
              -> forall j. i < j < Array.length r
              -> r.(i) <= r.(j)' has  been translated
    + File "../suite/arrays/lib.mli", line 72, characters 12-111:
      `forall i. 0 <= i < Array.length r
              -> exists j. 0 <= j < Array.length a /\ r.(i) = a.(j)' has  been translated
    + File "../suite/arrays/lib.mli", line 74, characters 12-111:
      `forall i. 0 <= i < Array.length a
              -> exists j. 0 <= j < Array.length r /\ a.(i) = r.(j)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 77, characters 0-193:
  the value bad_sort:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 79, characters 12-123:
      `forall i. 0 <= i < Array.length r
              -> forall j. i < j < Array.length r
              -> r.(i) <= r.(j)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_6 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_1
    + Invariants involved:
      
  File "../suite/arrays/lib.mli", line 83, characters 0-471:
  the value constant_sort:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/arrays/lib.mli", line 85, characters 12-43:
      `Array.length r = Array.length a' has  been translated
    + File "../suite/arrays/lib.mli", line 86, characters 12-123:
      `forall i. 0 <= i < Array.length r
              -> forall j. i < j < Array.length r
              -> r.(i) <= r.(j)' has  been translated
    + File "../suite/arrays/lib.mli", line 89, characters 12-111:
      `forall i. 0 <= i < Array.length r
              -> exists j. 0 <= j < Array.length a /\ r.(i) = a.(j)' has  been translated
    + File "../suite/arrays/lib.mli", line 91, characters 12-111:
      `forall i. 0 <= i < Array.length a
              -> exists j. 0 <= j < Array.length r /\ a.(i) = r.(j)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    a_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_2
    + Invariants involved:
      
  $ ortac report ../suite/terms/lib.mli
  Lib
  File "../suite/terms/lib.mli", line 3, characters 0-126:
  the value lazy_bool:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 5, characters 13-29:
      `x = x || 1/0 = 2' has  been translated
    + File "../suite/terms/lib.mli", line 6, characters 13-36:
      `not (x <> x && 1/0 = 2)' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 8, characters 0-93:
  the value not_lazy_or:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 10, characters 13-29:
      `x = x \/ 1/0 = 2' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 12, characters 0-102:
  the value not_lazy_and:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 14, characters 13-36:
      `not (x <> x /\ 1/0 = 2)' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 18, characters 0-115:
  the value scope1:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 20, characters 13-30:
      `let x = true in x' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 21, characters 13-30:
      `let y = true in y' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_3
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 25, characters 0-130:
  the value if_forall:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 27, characters 13-70:
      `if forall i. 0 <= i < 10 -> x <> i then x = 10 else x = 3' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_5
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 29, characters 0-82:
  the value equiv:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 31, characters 12-31:
      `(1 = 2) <-> (2 = 3)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 33, characters 0-97:
  the value exists_:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 35, characters 12-42:
      `exists x. 0 <= x < 10 /\ x = 3' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 39, characters 0-24:
  the type t:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 41, characters 0-134:
  the value a:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 43, characters 13-76:
      `match x with
              | A -> true
              | B s -> false' has  been translated
    + File "../suite/terms/lib.mli", line 46, characters 13-18:
      `x = A' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 48, characters 0-115:
  the value b:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 50, characters 13-76:
      `match x with
              | A -> false
              | B _ -> true' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 54, characters 0-27:
  the type peano:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 56, characters 0-74:
  the value succ:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 58, characters 14-21:
      `y = S x' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_9 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_6
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 60, characters 0-123:
  the value add:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 62, characters 14-30:
      `x <> O -> z <> O' has  been translated
    + File "../suite/terms/lib.mli", line 63, characters 14-30:
      `y <> O -> z <> O' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_10 is not consumed and is not modified
    + Invariants involved:
      
    y_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 65, characters 0-131:
  the value bad_add:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 67, characters 14-30:
      `x <> O -> z <> O' has  been translated
    + File "../suite/terms/lib.mli", line 68, characters 14-30:
      `y <> O -> z <> O' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_11 is not consumed and is not modified
    + Invariants involved:
      
    y_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 70, characters 0-38:
  the type tree:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 72, characters 1-102:
  the Gospel function __logical_size__017_ has been translatedFile "../suite/terms/lib.mli", line 75, characters 0-115:
  the value size:
   - Pure: true
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 78, characters 14-29:
      `t <> E -> s > 0' has  been translated
    + File "../suite/terms/lib.mli", line 79, characters 14-24:
      `s = size t' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 81, characters 0-95:
  the value size_wrong_spec:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 83, characters 12-23:
      `s <> size t' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 85, characters 0-171:
  the value test_tree:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 87, characters 14-110:
      `b = match t with
                    | E -> true
                    | N (l, x, t) -> l = t && x = 0' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    b
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 91, characters 0-109:
  the value make_tree:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 93, characters 14-29:
      `t = N (l, x, r)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_2 is not consumed and is not modified
    + Invariants involved:
      
    x_13 is not consumed and is not modified
    + Invariants involved:
      
    r_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_5
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 95, characters 0-171:
  the value fill:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 97, characters 15-43:
      `0 <= start <= Array.length a' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 98, characters 15-46:
      `start <= stop <= Array.length a' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_6 is not consumed and is not modified
    + Invariants involved:
      
    a is not consumed and is not modified
    + Invariants involved:
      
    start is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    stop
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 100, characters 0-58:
  the type alt_tree:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 102, characters 0-145:
  the value make_alt_tree:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 104, characters 14-45:
      `let c = (l, x, r) in t = Nalt c' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_3 is not consumed and is not modified
    + Invariants involved:
      
    x_14 is not consumed and is not modified
    + Invariants involved:
      
    r_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_7
    + Invariants involved:
      
  $ ortac report ../suite/arith/lib.mli
  Lib
  File "../suite/arith/lib.mli", line 1, characters 0-115:
  the value test_forall:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 3, characters 13-42:
      `forall x. i <= x < j -> x > 0' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
    j is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r
    + Invariants involved:
      
  File "../suite/arith/lib.mli", line 5, characters 0-146:
  the value double_forall:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 7, characters 13-70:
      `forall i. lo <= i < hi -> forall j. i <= j < hi -> i <= j' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    lo is not consumed and is not modified
    + Invariants involved:
      
    hi is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arith/lib.mli", line 9, characters 0-104:
  the value power:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 11, characters 13-18:
      `n >=0' has  been translated
  - Postconditions:
    + File "../suite/arith/lib.mli", line 12, characters 13-24:
      `r = pow x n' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
    n is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_1
    + Invariants involved:
      
  $ ortac report ../suite/types/lib.mli
  Lib
  File "../suite/types/lib.mli", line 1, characters 0-65:
  the type t:
  - Mutable: false
  - Ghost: false
  - Invariants:
    + File "../suite/types/lib.mli", line 2, characters 11-20:
      `((y ):bool = (False ):bool):prop' has been translated
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/types/lib.mli", line 4, characters 0-43:
  the constant value e:
  - Ghost: false
  - Checks:
    + File "../suite/types/lib.mli", line 5, characters 9-17:
      `e.x >= 0' has  been translated
  - Invariants:
    + File "../suite/types/lib.mli", line 2, characters 11-20:
      `((y ):bool = (False ):bool):prop' has been translatedFile "../suite/types/lib.mli", line 7, characters 0-69:
  the value get_x:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/types/lib.mli", line 9, characters 12-19:
      `r = t.x' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t is not consumed and is not modified
    + Invariants involved:
      + File "../suite/types/lib.mli", line 2, characters 11-20:
        `((y ):bool = (False ):bool):prop' has been translated
  - Return:
    r
    + Invariants involved:
      
  $ ortac report ../suite/exceptions/lib.mli
  Lib
  File "../suite/exceptions/lib.mli", line 1, characters 0-59:
  the value raise_oom:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 4, characters 0-79:
  the value raise_stackoverflow:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_1
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 7, characters 0-91:
  the value undeclared_raise_notfound:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_2
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 10, characters 0-107:
  the value bad_raise_notfound:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_3
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 14, characters 0-98:
  the value raise_notfound:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_4
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 18, characters 0-212:
  the value raise_invalidarg:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Invalid_argument have been translated
    + the clauses concerning the exception Invalid_argument have been translated
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_5
    + Invariants involved:
      

  $ ortac report ../suite/arith/lib.mli
  Lib
  File "../suite/arith/lib.mli", line 1, characters 0-115:
  the value test_forall:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 3, characters 13-42:
      `forall x. i <= x < j -> x > 0' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
    j is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r
    + Invariants involved:
      
  File "../suite/arith/lib.mli", line 5, characters 0-146:
  the value double_forall:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 7, characters 13-70:
      `forall i. lo <= i < hi -> forall j. i <= j < hi -> i <= j' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    lo is not consumed and is not modified
    + Invariants involved:
      
    hi is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/arith/lib.mli", line 9, characters 0-104:
  the value power:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/arith/lib.mli", line 11, characters 13-18:
      `n >=0' has  been translated
  - Postconditions:
    + File "../suite/arith/lib.mli", line 12, characters 13-24:
      `r = pow x n' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
    n is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    r_1
    + Invariants involved:
      

  $ ortac report ../suite/terms/lib.mli
  Lib
  File "../suite/terms/lib.mli", line 3, characters 0-126:
  the value lazy_bool:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 5, characters 13-29:
      `x = x || 1/0 = 2' has  been translated
    + File "../suite/terms/lib.mli", line 6, characters 13-36:
      `not (x <> x && 1/0 = 2)' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 8, characters 0-93:
  the value not_lazy_or:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 10, characters 13-29:
      `x = x \/ 1/0 = 2' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 12, characters 0-102:
  the value not_lazy_and:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 14, characters 13-36:
      `not (x <> x /\ 1/0 = 2)' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 18, characters 0-115:
  the value scope1:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 20, characters 13-30:
      `let x = true in x' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 21, characters 13-30:
      `let y = true in y' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_3
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 25, characters 0-130:
  the value if_forall:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 27, characters 13-70:
      `if forall i. 0 <= i < 10 -> x <> i then x = 10 else x = 3' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_5
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 29, characters 0-82:
  the value equiv:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 31, characters 12-31:
      `(1 = 2) <-> (2 = 3)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 33, characters 0-97:
  the value exists_:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 35, characters 12-42:
      `exists x. 0 <= x < 10 /\ x = 3' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    () is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 39, characters 0-24:
  the type t:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 41, characters 0-134:
  the value a:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 43, characters 13-76:
      `match x with
              | A -> true
              | B s -> false' has  been translated
    + File "../suite/terms/lib.mli", line 46, characters 13-18:
      `x = A' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 48, characters 0-115:
  the value b:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 50, characters 13-76:
      `match x with
              | A -> false
              | B _ -> true' has  been translated
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    x_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    
  File "../suite/terms/lib.mli", line 54, characters 0-27:
  the type peano:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 56, characters 0-74:
  the value succ:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 58, characters 14-21:
      `y = S x' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_9 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    y_6
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 60, characters 0-123:
  the value add:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 62, characters 14-30:
      `x <> O -> z <> O' has  been translated
    + File "../suite/terms/lib.mli", line 63, characters 14-30:
      `y <> O -> z <> O' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_10 is not consumed and is not modified
    + Invariants involved:
      
    y_7 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 65, characters 0-131:
  the value bad_add:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 67, characters 14-30:
      `x <> O -> z <> O' has  been translated
    + File "../suite/terms/lib.mli", line 68, characters 14-30:
      `y <> O -> z <> O' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    x_11 is not consumed and is not modified
    + Invariants involved:
      
    y_8 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    z_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 70, characters 0-38:
  the type tree:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 72, characters 1-102:
  the Gospel function __logical_size__017_ has been translatedFile "../suite/terms/lib.mli", line 75, characters 0-115:
  the value size:
   - Pure: true
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 78, characters 14-29:
      `t <> E -> s > 0' has  been translated
    + File "../suite/terms/lib.mli", line 79, characters 14-24:
      `s = size t' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_1
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 81, characters 0-95:
  the value size_wrong_spec:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 83, characters 12-23:
      `s <> size t' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    s_2
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 85, characters 0-171:
  the value test_tree:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 87, characters 14-110:
      `b = match t with
                    | E -> true
                    | N (l, x, t) -> l = t && x = 0' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    b
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 91, characters 0-109:
  the value make_tree:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 93, characters 14-29:
      `t = N (l, x, r)' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_2 is not consumed and is not modified
    + Invariants involved:
      
    x_13 is not consumed and is not modified
    + Invariants involved:
      
    r_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_5
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 95, characters 0-171:
  the value fill:
   - Pure: false
   - Ghost: false
   - Preconditions:
    + File "../suite/terms/lib.mli", line 97, characters 15-43:
      `0 <= start <= Array.length a' has  been translated
  - Postconditions:
    + File "../suite/terms/lib.mli", line 98, characters 15-46:
      `start <= stop <= Array.length a' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t_6 is not consumed and is not modified
    + Invariants involved:
      
    a is not consumed and is not modified
    + Invariants involved:
      
    start is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    stop
    + Invariants involved:
      
  File "../suite/terms/lib.mli", line 100, characters 0-58:
  the type alt_tree:
  - Mutable: false
  - Ghost: false
  - Invariants:
    
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/terms/lib.mli", line 102, characters 0-145:
  the value make_alt_tree:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/terms/lib.mli", line 104, characters 14-45:
      `let c = (l, x, r) in t = Nalt c' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    l_3 is not consumed and is not modified
    + Invariants involved:
      
    x_14 is not consumed and is not modified
    + Invariants involved:
      
    r_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    t_7
    + Invariants involved:
      

  $ ortac report ../suite/exceptions/lib.mli
  Lib
  File "../suite/exceptions/lib.mli", line 1, characters 0-59:
  the value raise_oom:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 4, characters 0-79:
  the value raise_stackoverflow:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_1 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_1
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 7, characters 0-91:
  the value undeclared_raise_notfound:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    
  - Arguments:
    i_2 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_2
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 10, characters 0-107:
  the value bad_raise_notfound:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_3 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_3
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 14, characters 0-98:
  the value raise_notfound:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_4 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_4
    + Invariants involved:
      
  File "../suite/exceptions/lib.mli", line 18, characters 0-212:
  the value raise_invalidarg:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    
  - Exceptional postconditions:
    + the clauses concerning the exception Invalid_argument have been translated
    + the clauses concerning the exception Invalid_argument have been translated
    + the clauses concerning the exception Not_found have been translated
  - Arguments:
    i_5 is not consumed and is not modified
    + Invariants involved:
      
  - Return:
    o_5
    + Invariants involved:
      

  $ ortac report ../suite/types/lib.mli
  Lib
  File "../suite/types/lib.mli", line 1, characters 0-65:
  the type t:
  - Mutable: false
  - Ghost: false
  - Invariants:
    + File "../suite/types/lib.mli", line 2, characters 11-20:
      `((y ):bool = (False ):bool):prop' has been translated
  - Equality: has not been derived
  - Comparison: has not been derived
  - Copy: has not been derived
  File "../suite/types/lib.mli", line 4, characters 0-43:
  the constant value e:
  - Ghost: false
  - Checks:
    + File "../suite/types/lib.mli", line 5, characters 9-17:
      `e.x >= 0' has  been translated
  - Invariants:
    + File "../suite/types/lib.mli", line 2, characters 11-20:
      `((y ):bool = (False ):bool):prop' has been translatedFile "../suite/types/lib.mli", line 7, characters 0-69:
  the value get_x:
   - Pure: false
   - Ghost: false
   - Preconditions:
    
  - Postconditions:
    + File "../suite/types/lib.mli", line 9, characters 12-19:
      `r = t.x' has  been translated
  - Exceptional postconditions:
    
  - Arguments:
    t is not consumed and is not modified
    + Invariants involved:
      + File "../suite/types/lib.mli", line 2, characters 11-20:
        `((y ):bool = (False ):bool):prop' has been translated
  - Return:
    r
    + Invariants involved:
      
