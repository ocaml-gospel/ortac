include Lib
module Ortac_runtime = Ortac_runtime
let __invariant___001_ __error___002_ __position___003_ t_1 =
  if
    not
      (try
         let __t1__004_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int 0)
             (Ortac_runtime.Gospelstdlib.integer_of_int t_1.size) in
         let __t2__005_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int t_1.size)
             (Ortac_runtime.Gospelstdlib.integer_of_int 32) in
         __t1__004_ && __t2__005_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term = "0 <= t.size <= 32";
                 term_kind = __position___003_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___002_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= t.size <= 32"; position = __position___003_ })
      |> (Ortac_runtime.Errors.register __error___002_)
let __invariant___006_ __error___007_ __position___008_ t_1 =
  if
    not
      (try
         let __t1__009_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int 0)
             (Ortac_runtime.Gospelstdlib.integer_of_int t_1.mask) in
         let __t2__010_ =
           Ortac_runtime.Gospelstdlib.(<)
             (Ortac_runtime.Gospelstdlib.integer_of_int t_1.mask)
             (Ortac_runtime.Gospelstdlib.pow
                (Ortac_runtime.Gospelstdlib.integer_of_int 2)
                (Ortac_runtime.Gospelstdlib.integer_of_int t_1.size)) in
         __t1__009_ && __t2__010_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term = "0 <= t.mask < pow 2 t.size";
                 term_kind = __position___008_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___007_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= t.mask < pow 2 t.size"; position = __position___008_ })
      |> (Ortac_runtime.Errors.register __error___007_)
let __logical_mem__011_ i bv =
  not
    ((Ortac_runtime.Gospelstdlib.logand
        (Ortac_runtime.Gospelstdlib.integer_of_int bv.mask)
        (Ortac_runtime.Gospelstdlib.pow
           (Ortac_runtime.Gospelstdlib.integer_of_int 2) i))
       = (Ortac_runtime.Gospelstdlib.integer_of_int 0))
let create n =
  let __error__012_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 9;
            pos_bol = 399;
            pos_cnum = 399
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 13;
            pos_bol = 585;
            pos_cnum = 638
          }
      } "create" in
  if
    not
      (try
         let __t1__013_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int 0)
             (Ortac_runtime.Gospelstdlib.integer_of_int n) in
         let __t2__014_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int n)
             (Ortac_runtime.Gospelstdlib.integer_of_int 32) in
         __t1__013_ && __t2__014_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= n <= 32"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__012_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "0 <= n <= 32"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__012_);
  Ortac_runtime.Errors.report __error__012_;
  (let bv_1 =
     try create n
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__012_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__012_);
          Ortac_runtime.Errors.report __error__012_;
          raise e) in
   if
     not
       (try
          Ortac_runtime.Z.forall
            (Ortac_runtime.Gospelstdlib.integer_of_int 0)
            (Ortac_runtime.Gospelstdlib.pred
               (Ortac_runtime.Gospelstdlib.integer_of_int n))
            (fun i_1 -> not (__logical_mem__011_ i_1 bv_1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "forall i. 0 <= i < n -> not (mem i bv)";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__012_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "forall i. 0 <= i < n -> not (mem i bv)"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__012_);
   if
     not
       (try bv_1.size = n
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "bv.size = n"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__012_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "bv.size = n"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__012_);
   __invariant___006_ __error__012_ Post bv_1;
   __invariant___001_ __error__012_ Post bv_1;
   Ortac_runtime.Errors.report __error__012_;
   bv_1)
let add i_2 bv_2 =
  let __error__015_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 15;
            pos_bol = 640;
            pos_cnum = 640
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 20;
            pos_bol = 871;
            pos_cnum = 926
          }
      } "add" in
  if
    not
      (try
         let __t1__016_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int 0)
             (Ortac_runtime.Gospelstdlib.integer_of_int i_2) in
         let __t2__017_ =
           Ortac_runtime.Gospelstdlib.(<)
             (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
             (Ortac_runtime.Gospelstdlib.integer_of_int bv_2.size) in
         __t1__016_ && __t2__017_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= i < bv.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__015_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "0 <= i < bv.size"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__015_);
  __invariant___006_ __error__015_ Pre bv_2;
  __invariant___001_ __error__015_ Pre bv_2;
  Ortac_runtime.Errors.report __error__015_;
  (let () =
     try add i_2 bv_2
     with
     | Stack_overflow | Out_of_memory as e ->
         ((__invariant___006_ __error__015_ XPost bv_2;
           __invariant___001_ __error__015_ XPost bv_2;
           Ortac_runtime.Errors.report __error__015_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__015_);
          (__invariant___006_ __error__015_ XPost bv_2;
           __invariant___001_ __error__015_ XPost bv_2;
           Ortac_runtime.Errors.report __error__015_);
          raise e) in
   __invariant___006_ __error__015_ Post bv_2;
   __invariant___001_ __error__015_ Post bv_2;
   Ortac_runtime.Errors.report __error__015_;
   ())
let mem i_3 bv_3 =
  let __error__020_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 22;
            pos_bol = 928;
            pos_cnum = 928
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 25;
            pos_bol = 1074;
            pos_cnum = 1103
          }
      } "mem" in
  if
    not
      (try
         let __t1__021_ =
           Ortac_runtime.Gospelstdlib.(<=)
             (Ortac_runtime.Gospelstdlib.integer_of_int 0)
             (Ortac_runtime.Gospelstdlib.integer_of_int i_3) in
         let __t2__022_ =
           Ortac_runtime.Gospelstdlib.(<)
             (Ortac_runtime.Gospelstdlib.integer_of_int i_3)
             (Ortac_runtime.Gospelstdlib.integer_of_int bv_3.size) in
         __t1__021_ && __t2__022_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= i < bv.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__020_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "0 <= i < bv.size"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__020_);
  __invariant___006_ __error__020_ Pre bv_3;
  __invariant___001_ __error__020_ Pre bv_3;
  Ortac_runtime.Errors.report __error__020_;
  (let b =
     try mem i_3 bv_3
     with
     | Stack_overflow | Out_of_memory as e ->
         ((__invariant___006_ __error__020_ XPost bv_3;
           __invariant___001_ __error__020_ XPost bv_3;
           Ortac_runtime.Errors.report __error__020_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__020_);
          (__invariant___006_ __error__020_ XPost bv_3;
           __invariant___001_ __error__020_ XPost bv_3;
           Ortac_runtime.Errors.report __error__020_);
          raise e) in
   if
     not
       (try
          (b = true) =
            (__logical_mem__011_
               (Ortac_runtime.Gospelstdlib.integer_of_int i_3) bv_3)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "b <-> mem i bv"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__020_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "b <-> mem i bv"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__020_);
   __invariant___006_ __error__020_ Post bv_3;
   __invariant___001_ __error__020_ Post bv_3;
   Ortac_runtime.Errors.report __error__020_;
   b)
let __invariant___023_ __error___024_ __position___025_ t_2 =
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.(>=)
           (Ortac_runtime.Gospelstdlib.integer_of_int t_2.value)
           (Ortac_runtime.Gospelstdlib.integer_of_int 0)
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term = "t.value >= 0";
                 term_kind = __position___025_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___024_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "t.value >= 0"; position = __position___025_ })
      |> (Ortac_runtime.Errors.register __error___024_)
let create_int n_1 =
  let __error__026_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 33;
            pos_bol = 1274;
            pos_cnum = 1274
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 36;
            pos_bol = 1409;
            pos_cnum = 1436
          }
      } "create_int" in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.(>=)
           (Ortac_runtime.Gospelstdlib.integer_of_int n_1)
           (Ortac_runtime.Gospelstdlib.integer_of_int 0)
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "n >= 0"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__026_);
            true))
  then
    (Ortac_runtime.Violated_condition { term = "n >= 0"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__026_);
  Ortac_runtime.Errors.report __error__026_;
  (let r =
     try create_int n_1
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__026_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__026_);
          Ortac_runtime.Errors.report __error__026_;
          raise e) in
   if
     not
       (try r.value = n_1
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "r.value = n"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__026_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "r.value = n"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__026_);
   __invariant___023_ __error__026_ Post r;
   Ortac_runtime.Errors.report __error__026_;
   r)
let bad_create_int n_2 =
  let __error__027_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 42;
            pos_bol = 1584;
            pos_cnum = 1584
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 44;
            pos_bol = 1707;
            pos_cnum = 1734
          }
      } "bad_create_int" in
  Ortac_runtime.Errors.report __error__027_;
  (let r_1 =
     try bad_create_int n_2
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__027_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__027_);
          Ortac_runtime.Errors.report __error__027_;
          raise e) in
   if
     not
       (try r_1.value = n_2
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "r.value = n"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__027_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "r.value = n"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__027_);
   __invariant___023_ __error__027_ Post r_1;
   Ortac_runtime.Errors.report __error__027_;
   r_1)
let increment_int x =
  let __error__028_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 46;
            pos_bol = 1736;
            pos_cnum = 1736
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 51;
            pos_bol = 1952;
            pos_cnum = 1988
          }
      } "increment_int" in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.(<)
           (Ortac_runtime.Gospelstdlib.integer_of_int x.value)
           Ortac_runtime.Gospelstdlib.max_int
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "x.value < max_int"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__028_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "x.value < max_int"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__028_);
  __invariant___023_ __error__028_ Pre x;
  (let __check__029_ =
     try
       Ortac_runtime.Gospelstdlib.(>=)
         (Ortac_runtime.Gospelstdlib.integer_of_int x.value)
         (Ortac_runtime.Gospelstdlib.integer_of_int 0)
     with
     | e ->
         ((Ortac_runtime.Specification_failure
             { term = "x.value >= 0"; term_kind = Check; exn = e })
            |> (Ortac_runtime.Errors.register __error__028_);
          true) in
   Ortac_runtime.Errors.report __error__028_;
   (let r_2 =
      try increment_int x
      with
      | Int_overflow as __e___036_ ->
          ((match __e___036_ with
            | Int_overflow ->
                if
                  not
                    (try true
                     with
                     | e ->
                         ((Ortac_runtime.Specification_failure
                             { term = "true"; term_kind = XPost; exn = e })
                            |> (Ortac_runtime.Errors.register __error__028_);
                          true))
                then
                  (Ortac_runtime.Violated_condition
                     { term = "true"; term_kind = XPost })
                    |> (Ortac_runtime.Errors.register __error__028_)
            | _ -> assert false);
           (if not __check__029_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
                (Ortac_runtime.Errors.register __error__028_);
            __invariant___023_ __error__028_ XPost x;
            Ortac_runtime.Errors.report __error__028_);
           raise __e___036_)
      | Invalid_argument _ as e ->
          ((if true && __check__029_
            then
              (Ortac_runtime.Unexpected_checks { terms = [] }) |>
                (Ortac_runtime.Errors.register __error__028_);
            __invariant___023_ __error__028_ XPost x;
            Ortac_runtime.Errors.report __error__028_);
           raise e)
      | Stack_overflow | Out_of_memory as e ->
          ((if not __check__029_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
                (Ortac_runtime.Errors.register __error__028_);
            __invariant___023_ __error__028_ XPost x;
            Ortac_runtime.Errors.report __error__028_);
           raise e)
      | e ->
          ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
             |> (Ortac_runtime.Errors.register __error__028_);
           (if not __check__029_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
                (Ortac_runtime.Errors.register __error__028_);
            __invariant___023_ __error__028_ XPost x;
            Ortac_runtime.Errors.report __error__028_);
           raise e) in
    if
      not
        (try
           (Ortac_runtime.Gospelstdlib.integer_of_int r_2) =
             (Ortac_runtime.Gospelstdlib.(+)
                (Ortac_runtime.Gospelstdlib.integer_of_int x.value)
                (Ortac_runtime.Gospelstdlib.integer_of_int 1))
         with
         | e ->
             ((Ortac_runtime.Specification_failure
                 { term = "r = x.value + 1"; term_kind = Post; exn = e })
                |> (Ortac_runtime.Errors.register __error__028_);
              true))
    then
      (Ortac_runtime.Violated_condition
         { term = "r = x.value + 1"; term_kind = Post })
        |> (Ortac_runtime.Errors.register __error__028_);
    if not __check__029_
    then
      (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
        (Ortac_runtime.Errors.register __error__028_);
    __invariant___023_ __error__028_ Post x;
    Ortac_runtime.Errors.report __error__028_;
    r_2))
let bad_increment_int x_1 =
  let __error__030_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 56;
            pos_bol = 2082;
            pos_cnum = 2082
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 60;
            pos_bol = 2275;
            pos_cnum = 2311
          }
      } "bad_increment_int" in
  __invariant___023_ __error__030_ Pre x_1;
  (let __check__031_ =
     try
       Ortac_runtime.Gospelstdlib.(>=)
         (Ortac_runtime.Gospelstdlib.integer_of_int x_1.value)
         (Ortac_runtime.Gospelstdlib.integer_of_int 0)
     with
     | e ->
         ((Ortac_runtime.Specification_failure
             { term = "x.value >= 0"; term_kind = Check; exn = e })
            |> (Ortac_runtime.Errors.register __error__030_);
          true) in
   Ortac_runtime.Errors.report __error__030_;
   (let r_3 =
      try bad_increment_int x_1
      with
      | Int_overflow as __e___035_ ->
          ((match __e___035_ with
            | Int_overflow ->
                if
                  not
                    (try true
                     with
                     | e ->
                         ((Ortac_runtime.Specification_failure
                             { term = "true"; term_kind = XPost; exn = e })
                            |> (Ortac_runtime.Errors.register __error__030_);
                          true))
                then
                  (Ortac_runtime.Violated_condition
                     { term = "true"; term_kind = XPost })
                    |> (Ortac_runtime.Errors.register __error__030_)
            | _ -> assert false);
           (if not __check__031_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
                (Ortac_runtime.Errors.register __error__030_);
            __invariant___023_ __error__030_ XPost x_1;
            Ortac_runtime.Errors.report __error__030_);
           raise __e___035_)
      | Invalid_argument _ as e ->
          ((if true && __check__031_
            then
              (Ortac_runtime.Unexpected_checks { terms = [] }) |>
                (Ortac_runtime.Errors.register __error__030_);
            __invariant___023_ __error__030_ XPost x_1;
            Ortac_runtime.Errors.report __error__030_);
           raise e)
      | Stack_overflow | Out_of_memory as e ->
          ((if not __check__031_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
                (Ortac_runtime.Errors.register __error__030_);
            __invariant___023_ __error__030_ XPost x_1;
            Ortac_runtime.Errors.report __error__030_);
           raise e)
      | e ->
          ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
             |> (Ortac_runtime.Errors.register __error__030_);
           (if not __check__031_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
                (Ortac_runtime.Errors.register __error__030_);
            __invariant___023_ __error__030_ XPost x_1;
            Ortac_runtime.Errors.report __error__030_);
           raise e) in
    if
      not
        (try
           (Ortac_runtime.Gospelstdlib.integer_of_int r_3) =
             (Ortac_runtime.Gospelstdlib.(+)
                (Ortac_runtime.Gospelstdlib.integer_of_int x_1.value)
                (Ortac_runtime.Gospelstdlib.integer_of_int 1))
         with
         | e ->
             ((Ortac_runtime.Specification_failure
                 { term = "r = x.value + 1"; term_kind = Post; exn = e })
                |> (Ortac_runtime.Errors.register __error__030_);
              true))
    then
      (Ortac_runtime.Violated_condition
         { term = "r = x.value + 1"; term_kind = Post })
        |> (Ortac_runtime.Errors.register __error__030_);
    if not __check__031_
    then
      (Ortac_runtime.Uncaught_checks { term = "x.value >= 0" }) |>
        (Ortac_runtime.Errors.register __error__030_);
    __invariant___023_ __error__030_ Post x_1;
    Ortac_runtime.Errors.report __error__030_;
    r_3))
let bad2_increment_int x_2 =
  let __error__032_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 65;
            pos_bol = 2410;
            pos_cnum = 2410
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 70;
            pos_bol = 2636;
            pos_cnum = 2672
          }
      } "bad2_increment_int" in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.(<)
           (Ortac_runtime.Gospelstdlib.integer_of_int x_2.value)
           Ortac_runtime.Gospelstdlib.max_int
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "x.value < max_int"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__032_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "x.value < max_int"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__032_);
  __invariant___023_ __error__032_ Pre x_2;
  (let __check__033_ =
     try
       not
         ((Ortac_runtime.Gospelstdlib.integer_of_int x_2.value) =
            (Ortac_runtime.Gospelstdlib.integer_of_int 1))
     with
     | e ->
         ((Ortac_runtime.Specification_failure
             { term = "x.value <> 1"; term_kind = Check; exn = e })
            |> (Ortac_runtime.Errors.register __error__032_);
          true) in
   Ortac_runtime.Errors.report __error__032_;
   (let r_4 =
      try bad2_increment_int x_2
      with
      | Int_overflow as __e___034_ ->
          ((match __e___034_ with
            | Int_overflow ->
                if
                  not
                    (try true
                     with
                     | e ->
                         ((Ortac_runtime.Specification_failure
                             { term = "true"; term_kind = XPost; exn = e })
                            |> (Ortac_runtime.Errors.register __error__032_);
                          true))
                then
                  (Ortac_runtime.Violated_condition
                     { term = "true"; term_kind = XPost })
                    |> (Ortac_runtime.Errors.register __error__032_)
            | _ -> assert false);
           (if not __check__033_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value <> 1" }) |>
                (Ortac_runtime.Errors.register __error__032_);
            __invariant___023_ __error__032_ XPost x_2;
            Ortac_runtime.Errors.report __error__032_);
           raise __e___034_)
      | Invalid_argument _ as e ->
          ((if true && __check__033_
            then
              (Ortac_runtime.Unexpected_checks { terms = [] }) |>
                (Ortac_runtime.Errors.register __error__032_);
            __invariant___023_ __error__032_ XPost x_2;
            Ortac_runtime.Errors.report __error__032_);
           raise e)
      | Stack_overflow | Out_of_memory as e ->
          ((if not __check__033_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value <> 1" }) |>
                (Ortac_runtime.Errors.register __error__032_);
            __invariant___023_ __error__032_ XPost x_2;
            Ortac_runtime.Errors.report __error__032_);
           raise e)
      | e ->
          ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
             |> (Ortac_runtime.Errors.register __error__032_);
           (if not __check__033_
            then
              (Ortac_runtime.Uncaught_checks { term = "x.value <> 1" }) |>
                (Ortac_runtime.Errors.register __error__032_);
            __invariant___023_ __error__032_ XPost x_2;
            Ortac_runtime.Errors.report __error__032_);
           raise e) in
    if
      not
        (try
           (Ortac_runtime.Gospelstdlib.integer_of_int r_4) =
             (Ortac_runtime.Gospelstdlib.(+)
                (Ortac_runtime.Gospelstdlib.integer_of_int x_2.value)
                (Ortac_runtime.Gospelstdlib.integer_of_int 1))
         with
         | e ->
             ((Ortac_runtime.Specification_failure
                 { term = "r = x.value + 1"; term_kind = Post; exn = e })
                |> (Ortac_runtime.Errors.register __error__032_);
              true))
    then
      (Ortac_runtime.Violated_condition
         { term = "r = x.value + 1"; term_kind = Post })
        |> (Ortac_runtime.Errors.register __error__032_);
    if not __check__033_
    then
      (Ortac_runtime.Uncaught_checks { term = "x.value <> 1" }) |>
        (Ortac_runtime.Errors.register __error__032_);
    __invariant___023_ __error__032_ Post x_2;
    Ortac_runtime.Errors.report __error__032_;
    r_4))
