include Lib
module Ortac_runtime = Ortac_runtime
let __invariant___001_ __error___003_ __position___004_ __self___002_ =
  if
    not
      (try
         let __t1__005_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int __self___002_.size) in
         let __t2__006_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int __self___002_.size)
             (Ortac_runtime.Z.of_int 32) in
         __t1__005_ && __t2__006_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term =
                   "(0:integer <= (integer_of_int \n(size ):int):integer):prop /\\ ((integer_of_int \n(size ):int):integer <= 32:integer):prop";
                 term_kind = __position___004_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___003_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       {
         term =
           "(0:integer <= (integer_of_int \n(size ):int):integer):prop /\\ ((integer_of_int \n(size ):int):integer <= 32:integer):prop";
         position = __position___004_
       })
      |> (Ortac_runtime.Errors.register __error___003_)
let __invariant___007_ __error___009_ __position___010_ __self___008_ =
  if
    not
      (try
         let __t1__011_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int __self___008_.mask) in
         let __t2__012_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int __self___008_.mask)
             (Ortac_runtime.Z.pow (Ortac_runtime.Z.of_int 2)
                (Ortac_runtime.Z.of_int __self___008_.size)) in
         __t1__011_ && __t2__012_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term =
                   "(0:integer <= (integer_of_int \n(mask ):int):integer):prop /\\ ((integer_of_int  (mask ):int):integer < (pow \n2:integer (integer_of_int  (size ):int):integer):integer):prop";
                 term_kind = __position___010_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___009_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       {
         term =
           "(0:integer <= (integer_of_int \n(mask ):int):integer):prop /\\ ((integer_of_int  (mask ):int):integer < (pow \n2:integer (integer_of_int  (size ):int):integer):integer):prop";
         position = __position___010_
       })
      |> (Ortac_runtime.Errors.register __error___009_)
let __logical_mem__013_ i bv =
  not
    ((Ortac_runtime.Z.logand (Ortac_runtime.Z.of_int bv.mask)
        (Ortac_runtime.Z.pow (Ortac_runtime.Z.of_int 2) i))
       = (Ortac_runtime.Z.of_int 0))
let create n =
  let __error__014_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 8;
            pos_bol = 221;
            pos_cnum = 221
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 12;
            pos_bol = 322;
            pos_cnum = 376
          }
      } "create" in
  if
    not
      (try
         let __t1__015_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int n) in
         let __t2__016_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int n)
             (Ortac_runtime.Z.of_int 32) in
         __t1__015_ && __t2__016_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= n <= 32"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__014_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= n <= 32"; position = Pre })
      |> (Ortac_runtime.Errors.register __error__014_);
  Ortac_runtime.Errors.report __error__014_;
  (let bv_1 =
     try create n
     with
     | Stack_overflow|Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__014_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__014_);
          Ortac_runtime.Errors.report __error__014_;
          raise e) in
   if
     not
       (try
          Ortac_runtime.Z.forall (Ortac_runtime.Z.of_int 0)
            (Ortac_runtime.Z.pred (Ortac_runtime.Z.of_int n))
            (fun i_1 -> not (__logical_mem__013_ i_1 bv_1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "forall i. 0 <= i < n -> not (mem i bv)";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__014_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "forall i. 0 <= i < n -> not (mem i bv)"; position = Post })
       |> (Ortac_runtime.Errors.register __error__014_);
   if
     not
       (try bv_1.size = n
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "bv.size = n"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__014_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "bv.size = n"; position = Post })
       |> (Ortac_runtime.Errors.register __error__014_);
   __invariant___007_ __error__014_ Post bv_1;
   __invariant___001_ __error__014_ Post bv_1;
   Ortac_runtime.Errors.report __error__014_;
   bv_1)
let add i_2 bv_2 =
  let __error__017_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 14;
            pos_bol = 378;
            pos_cnum = 378
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 19;
            pos_bol = 522;
            pos_cnum = 578
          }
      } "add" in
  if
    not
      (try
         let __t1__018_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int i_2) in
         let __t2__019_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int i_2)
             (Ortac_runtime.Z.of_int bv_2.size) in
         __t1__018_ && __t2__019_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= i < bv.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__017_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= i < bv.size"; position = Pre })
      |> (Ortac_runtime.Errors.register __error__017_);
  __invariant___007_ __error__017_ Pre bv_2;
  __invariant___001_ __error__017_ Pre bv_2;
  Ortac_runtime.Errors.report __error__017_;
  (let () =
     try add i_2 bv_2
     with
     | Stack_overflow|Out_of_memory as e ->
         ((__invariant___007_ __error__017_ XPost bv_2;
           __invariant___001_ __error__017_ XPost bv_2;
           Ortac_runtime.Errors.report __error__017_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__017_);
          (__invariant___007_ __error__017_ XPost bv_2;
           __invariant___001_ __error__017_ XPost bv_2;
           Ortac_runtime.Errors.report __error__017_);
          raise e) in
   __invariant___007_ __error__017_ Post bv_2;
   __invariant___001_ __error__017_ Post bv_2;
   Ortac_runtime.Errors.report __error__017_;
   ())
let mem i_3 bv_3 =
  let __error__022_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "lib.mli";
            pos_lnum = 21;
            pos_bol = 580;
            pos_cnum = 580
          };
        Ortac_runtime.stop =
          {
            pos_fname = "lib.mli";
            pos_lnum = 24;
            pos_bol = 665;
            pos_cnum = 695
          }
      } "mem" in
  if
    not
      (try
         let __t1__023_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int i_3) in
         let __t2__024_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int i_3)
             (Ortac_runtime.Z.of_int bv_3.size) in
         __t1__023_ && __t2__024_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= i < bv.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__022_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= i < bv.size"; position = Pre })
      |> (Ortac_runtime.Errors.register __error__022_);
  __invariant___007_ __error__022_ Pre bv_3;
  __invariant___001_ __error__022_ Pre bv_3;
  Ortac_runtime.Errors.report __error__022_;
  (let b =
     try mem i_3 bv_3
     with
     | Stack_overflow|Out_of_memory as e ->
         ((__invariant___007_ __error__022_ XPost bv_3;
           __invariant___001_ __error__022_ XPost bv_3;
           Ortac_runtime.Errors.report __error__022_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__022_);
          (__invariant___007_ __error__022_ XPost bv_3;
           __invariant___001_ __error__022_ XPost bv_3;
           Ortac_runtime.Errors.report __error__022_);
          raise e) in
   if
     not
       (try
          (b = true) =
            (__logical_mem__013_ (Ortac_runtime.Z.of_int i_3) bv_3)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "b <-> mem i bv"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__022_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "b <-> mem i bv"; position = Post })
       |> (Ortac_runtime.Errors.register __error__022_);
   __invariant___007_ __error__022_ Post bv_3;
   __invariant___001_ __error__022_ Post bv_3;
   Ortac_runtime.Errors.report __error__022_;
   b)
