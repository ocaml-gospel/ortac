open Monolith
module M = Ortac_runtime_monolith
module R =
  struct
    include Lib
    module Ortac_runtime = Ortac_runtime_monolith
    let __invariant___001_ __error___002_ __position___003_ t =
      if
        not
          (try
             let __t1__004_ =
               Ortac_runtime.Gospelstdlib.(<=)
                 (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                 (Ortac_runtime.Gospelstdlib.integer_of_int t.size) in
             let __t2__005_ =
               Ortac_runtime.Gospelstdlib.(<=)
                 (Ortac_runtime.Gospelstdlib.integer_of_int t.size)
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
    let __invariant___006_ __error___007_ __position___008_ t =
      if
        not
          (try
             let __t1__009_ =
               Ortac_runtime.Gospelstdlib.(<=)
                 (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                 (Ortac_runtime.Gospelstdlib.integer_of_int t.mask) in
             let __t2__010_ =
               Ortac_runtime.Gospelstdlib.(<)
                 (Ortac_runtime.Gospelstdlib.integer_of_int t.mask)
                 (Ortac_runtime.Gospelstdlib.pow
                    (Ortac_runtime.Gospelstdlib.integer_of_int 2)
                    (Ortac_runtime.Gospelstdlib.integer_of_int t.size)) in
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
           {
             term = "0 <= t.mask < pow 2 t.size";
             position = __position___008_
           })
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
        (Ortac_runtime.Violated_invariant
           { term = "0 <= n <= 32"; position = Pre })
          |> (Ortac_runtime.Errors.register __error__012_);
      Ortac_runtime.Errors.report __error__012_;
      (let bv_1 =
         try create n
         with
         | Stack_overflow | Out_of_memory as e ->
             (Ortac_runtime.Errors.report __error__012_; raise e)
         | e ->
             ((Ortac_runtime.Unexpected_exception
                 { allowed_exn = []; exn = e })
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
         (Ortac_runtime.Violated_invariant
            {
              term = "forall i. 0 <= i < n -> not (mem i bv)";
              position = Post
            })
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
         (Ortac_runtime.Violated_invariant
            { term = "bv.size = n"; position = Post })
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
        (Ortac_runtime.Violated_invariant
           { term = "0 <= i < bv.size"; position = Pre })
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
             ((Ortac_runtime.Unexpected_exception
                 { allowed_exn = []; exn = e })
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
        (Ortac_runtime.Violated_invariant
           { term = "0 <= i < bv.size"; position = Pre })
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
             ((Ortac_runtime.Unexpected_exception
                 { allowed_exn = []; exn = e })
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
         (Ortac_runtime.Violated_invariant
            { term = "b <-> mem i bv"; position = Post })
           |> (Ortac_runtime.Errors.register __error__020_);
       __invariant___006_ __error__020_ Post bv_3;
       __invariant___001_ __error__020_ Post bv_3;
       Ortac_runtime.Errors.report __error__020_;
       b)
  end
module C = Lib
module G = struct  end
module P = struct  end
module S = struct let set = declare_abstract_type ~var:"set" () end
let () =
  ((let spec = M.int ^!> S.set in
    declare "create is Ok" spec R.create C.create);
   (let spec = M.int ^> (S.set ^!> unit) in
    declare "add is Ok" spec R.add C.add));
  (let spec = M.int ^> (S.set ^!> bool) in
   declare "mem is Ok" spec R.mem C.mem)
let () = let fuel = 100 in main fuel
