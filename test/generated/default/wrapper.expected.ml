include Lib
module Ortac_runtime = Ortac_runtime
let __invariant___001_ __error___002_ __position___003_ t =
  if
    not
      (try
         let __t1__004_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int t.size) in
         let __t2__005_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int t.size)
             (Ortac_runtime.Z.of_int 32) in
         __t1__004_ && __t2__005_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term =
                   "(0:integer <= (integer_of_int \n(t:set).size):integer):prop /\\ ((integer_of_int \n(t:set).size):integer <= 32:integer):prop";
                 term_kind = __position___003_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___002_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       {
         term =
           "(0:integer <= (integer_of_int \n(t:set).size):integer):prop /\\ ((integer_of_int \n(t:set).size):integer <= 32:integer):prop";
         position = __position___003_
       })
      |> (Ortac_runtime.Errors.register __error___002_)
let __invariant___006_ __error___007_ __position___008_ t =
  if
    not
      (try
         let __t1__009_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int t.mask) in
         let __t2__010_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int t.mask)
             (Ortac_runtime.Z.pow (Ortac_runtime.Z.of_int 2)
                (Ortac_runtime.Z.of_int t.size)) in
         __t1__009_ && __t2__010_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term =
                   "(0:integer <= (integer_of_int \n(t:set).mask):integer):prop /\\ ((integer_of_int \n(t:set).mask):integer < (pow \n2:integer (integer_of_int  (t:set).size):integer):integer):prop";
                 term_kind = __position___008_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___007_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       {
         term =
           "(0:integer <= (integer_of_int \n(t:set).mask):integer):prop /\\ ((integer_of_int \n(t:set).mask):integer < (pow \n2:integer (integer_of_int  (t:set).size):integer):integer):prop";
         position = __position___008_
       })
      |> (Ortac_runtime.Errors.register __error___007_)
let __logical_mem__011_ i bv =
  not
    ((Ortac_runtime.Z.logand (Ortac_runtime.Z.of_int bv.mask)
        (Ortac_runtime.Z.pow (Ortac_runtime.Z.of_int 2) i))
       = (Ortac_runtime.Z.of_int 0))
let create n =
  let __error__012_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 9;
            pos_bol = 385;
            pos_cnum = 385
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 13;
            pos_bol = 571;
            pos_cnum = 624
          }
      } "create" in
  if
    not
      (try
         let __t1__013_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int n) in
         let __t2__014_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int n)
             (Ortac_runtime.Z.of_int 32) in
         __t1__013_ && __t2__014_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "size = n\n   "; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__012_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "size = n\n   "; position = Pre })
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
          Ortac_runtime.Z.forall (Ortac_runtime.Z.of_int 0)
            (Ortac_runtime.Z.pred (Ortac_runtime.Z.of_int n))
            (fun i_1 -> not (__logical_mem__011_ i_1 bv_1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term =
                    "forall i_1:integer. (0:integer <= i_1:integer):prop /\\ (i_1:integer < (integer_of_int \nn:int):integer):prop -> not (mem \ni_1:integer bv_1:set):prop";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__012_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        {
          term =
            "forall i_1:integer. (0:integer <= i_1:integer):prop /\\ (i_1:integer < (integer_of_int \nn:int):integer):prop -> not (mem \ni_1:integer bv_1:set):prop";
          position = Post
        })
       |> (Ortac_runtime.Errors.register __error__012_);
   if
     not
       (try bv_1.size = n
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "ll i. 0 <= "; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__012_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "ll i. 0 <= "; position = Post })
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
            pos_bol = 626;
            pos_cnum = 626
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 20;
            pos_bol = 857;
            pos_cnum = 912
          }
      } "add" in
  if
    not
      (try
         let __t1__016_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int i_2) in
         let __t2__017_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int i_2)
             (Ortac_runtime.Z.of_int bv_2.size) in
         __t1__016_ && __t2__017_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "s bv.mask\n    en"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__015_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "s bv.mask\n    en"; position = Pre })
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
            pos_bol = 914;
            pos_cnum = 914
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 25;
            pos_bol = 1060;
            pos_cnum = 1089
          }
      } "mem" in
  if
    not
      (try
         let __t1__021_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int i_3) in
         let __t2__022_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int i_3)
             (Ortac_runtime.Z.of_int bv_3.size) in
         __t1__021_ && __t2__022_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = " b <-> mem i bv "; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__020_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = " b <-> mem i bv "; position = Pre })
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
            (__logical_mem__011_ (Ortac_runtime.Z.of_int i_3) bv_3)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term =
                    "(b:bool = (True ):bool):prop <-> (mem \n(integer_of_int  i_3:int):integer bv_3:set):prop";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__020_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        {
          term =
            "(b:bool = (True ):bool):prop <-> (mem \n(integer_of_int  i_3:int):integer bv_3:set):prop";
          position = Post
        })
       |> (Ortac_runtime.Errors.register __error__020_);
   __invariant___006_ __error__020_ Post bv_3;
   __invariant___001_ __error__020_ Post bv_3;
   Ortac_runtime.Errors.report __error__020_;
   b)
