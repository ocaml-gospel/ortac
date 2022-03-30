include Lib
module Ortac_runtime = Ortac_runtime
let __repr__028_ =
  let open Ortac_runtime in
    let pp = Z.pp_print in
    let of_string _ = Ok Z.zero in
    let ejson : Z.t Repr.encode_json = fun _ -> fun _ -> () in
    let djson : Z.t Repr.decode_json = fun _ -> assert false in
    let json : (Z.t Repr.encode_json * Z.t Repr.decode_json) = (ejson, djson) in
    let ebin : Z.t Repr.encode_bin = fun _ -> fun _ -> () in
    let dbin : Z.t Repr.decode_bin = fun _ -> fun _ -> assert false in
    let bin = (ebin, dbin, (Repr.Size.custom_static 0)) in
    let equal = Z.equal in
    let compare = Z.compare in
    let short_hash ?seed:_  = Z.to_int in
    let pre_hash _ _ = () in
    Repr.abstract ~pp ~of_string ~json ~bin ~equal ~compare ~short_hash
      ~pre_hash ()
let __equal___001_ = let open Repr in unstage (equal __repr__028_)
let __repr__029_ = Repr.bool
let __equal___003_ = let open Repr in unstage (equal __repr__029_)
let __repr__030_ = Repr.int
let __equal___002_ = let open Repr in unstage (equal __repr__030_)
let __invariant___004_ __error___006_ __position___007_ __self___005_ =
  if
    not
      (try
         let __t1__008_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int __self___005_.size) in
         let __t2__009_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int __self___005_.size)
             (Ortac_runtime.Z.of_int 32) in
         __t1__008_ && __t2__009_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term =
                   "(0:integer <= (integer_of_int \n(size ):int):integer):prop /\\ ((integer_of_int \n(size ):int):integer <= 32:integer):prop";
                 term_kind = __position___007_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___006_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       {
         term =
           "(0:integer <= (integer_of_int \n(size ):int):integer):prop /\\ ((integer_of_int \n(size ):int):integer <= 32:integer):prop";
         position = __position___007_
       })
      |> (Ortac_runtime.Errors.register __error___006_)
let __invariant___010_ __error___012_ __position___013_ __self___011_ =
  if
    not
      (try
         let __t1__014_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int __self___011_.mask) in
         let __t2__015_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int __self___011_.mask)
             (Ortac_runtime.Z.pow (Ortac_runtime.Z.of_int 2)
                (Ortac_runtime.Z.of_int __self___011_.size)) in
         __t1__014_ && __t2__015_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term =
                   "(0:integer <= (integer_of_int \n(mask ):int):integer):prop /\\ ((integer_of_int  (mask ):int):integer < (pow \n2:integer (integer_of_int  (size ):int):integer):integer):prop";
                 term_kind = __position___013_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___012_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       {
         term =
           "(0:integer <= (integer_of_int \n(mask ):int):integer):prop /\\ ((integer_of_int  (mask ):int):integer < (pow \n2:integer (integer_of_int  (size ):int):integer):integer):prop";
         position = __position___013_
       })
      |> (Ortac_runtime.Errors.register __error___012_)
let __logical_mem__016_ i bv =
  not
    (__equal___001_
       (Ortac_runtime.Z.logand (Ortac_runtime.Z.of_int bv.mask)
          (Ortac_runtime.Z.pow (Ortac_runtime.Z.of_int 2) i))
       (Ortac_runtime.Z.of_int 0))
let create n =
  let __error__017_ =
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
         let __t1__018_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int n) in
         let __t2__019_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int n)
             (Ortac_runtime.Z.of_int 32) in
         __t1__018_ && __t2__019_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= n <= 32"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__017_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= n <= 32"; position = Pre })
      |> (Ortac_runtime.Errors.register __error__017_);
  Ortac_runtime.Errors.report __error__017_;
  (let bv_1 =
     try create n
     with
     | Stack_overflow|Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__017_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__017_);
          Ortac_runtime.Errors.report __error__017_;
          raise e) in
   if
     not
       (try
          Ortac_runtime.Z.forall (Ortac_runtime.Z.of_int 0)
            (Ortac_runtime.Z.pred (Ortac_runtime.Z.of_int n))
            (fun i_1 -> not (__logical_mem__016_ i_1 bv_1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "forall i. 0 <= i < n -> not (mem i bv)";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__017_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "forall i. 0 <= i < n -> not (mem i bv)"; position = Post })
       |> (Ortac_runtime.Errors.register __error__017_);
   if
     not
       (try __equal___002_ bv_1.size n
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "bv.size = n"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__017_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "bv.size = n"; position = Post })
       |> (Ortac_runtime.Errors.register __error__017_);
   __invariant___010_ __error__017_ Post bv_1;
   __invariant___004_ __error__017_ Post bv_1;
   Ortac_runtime.Errors.report __error__017_;
   bv_1)
let add i_2 bv_2 =
  let __error__020_ =
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
         let __t1__021_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int i_2) in
         let __t2__022_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int i_2)
             (Ortac_runtime.Z.of_int bv_2.size) in
         __t1__021_ && __t2__022_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= i < bv.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__020_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= i < bv.size"; position = Pre })
      |> (Ortac_runtime.Errors.register __error__020_);
  __invariant___010_ __error__020_ Pre bv_2;
  __invariant___004_ __error__020_ Pre bv_2;
  Ortac_runtime.Errors.report __error__020_;
  (let () =
     try add i_2 bv_2
     with
     | Stack_overflow|Out_of_memory as e ->
         ((__invariant___010_ __error__020_ XPost bv_2;
           __invariant___004_ __error__020_ XPost bv_2;
           Ortac_runtime.Errors.report __error__020_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__020_);
          (__invariant___010_ __error__020_ XPost bv_2;
           __invariant___004_ __error__020_ XPost bv_2;
           Ortac_runtime.Errors.report __error__020_);
          raise e) in
   __invariant___010_ __error__020_ Post bv_2;
   __invariant___004_ __error__020_ Post bv_2;
   Ortac_runtime.Errors.report __error__020_;
   ())
let mem i_3 bv_3 =
  let __error__025_ =
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
         let __t1__026_ =
           Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int 0)
             (Ortac_runtime.Z.of_int i_3) in
         let __t2__027_ =
           Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int i_3)
             (Ortac_runtime.Z.of_int bv_3.size) in
         __t1__026_ && __t2__027_
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "0 <= i < bv.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__025_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "0 <= i < bv.size"; position = Pre })
      |> (Ortac_runtime.Errors.register __error__025_);
  __invariant___010_ __error__025_ Pre bv_3;
  __invariant___004_ __error__025_ Pre bv_3;
  Ortac_runtime.Errors.report __error__025_;
  (let b =
     try mem i_3 bv_3
     with
     | Stack_overflow|Out_of_memory as e ->
         ((__invariant___010_ __error__025_ XPost bv_3;
           __invariant___004_ __error__025_ XPost bv_3;
           Ortac_runtime.Errors.report __error__025_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__025_);
          (__invariant___010_ __error__025_ XPost bv_3;
           __invariant___004_ __error__025_ XPost bv_3;
           Ortac_runtime.Errors.report __error__025_);
          raise e) in
   if
     not
       (try
          (__equal___003_ b true) =
            (__logical_mem__016_ (Ortac_runtime.Z.of_int i_3) bv_3)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "b <-> mem i bv"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__025_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "b <-> mem i bv"; position = Post })
       |> (Ortac_runtime.Errors.register __error__025_);
   __invariant___010_ __error__025_ Post bv_3;
   __invariant___004_ __error__025_ Post bv_3;
   Ortac_runtime.Errors.report __error__025_;
   b)
