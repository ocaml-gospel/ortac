include Braun_tree
module Ortac_runtime = Ortac_runtime
let size __arg0 =
  let __error__001_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 5;
            pos_bol = 158;
            pos_cnum = 158
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 5;
            pos_bol = 158;
            pos_cnum = 193
          }
      } "size" in
  Ortac_runtime.Errors.report __error__001_;
  (let result =
     try size __arg0
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__001_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__001_);
          Ortac_runtime.Errors.report __error__001_;
          raise e) in
   Ortac_runtime.Errors.report __error__001_; result)
let cont __arg0_1 =
  let __error__002_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 6;
            pos_bol = 194;
            pos_cnum = 194
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 6;
            pos_bol = 194;
            pos_cnum = 229
          }
      } "cont" in
  Ortac_runtime.Errors.report __error__002_;
  (let result_1 =
     try cont __arg0_1
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__002_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__002_);
          Ortac_runtime.Errors.report __error__002_;
          raise e) in
   Ortac_runtime.Errors.report __error__002_; result_1)
let get t_1 i =
  let __error__003_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 8;
            pos_bol = 231;
            pos_cnum = 231
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 11;
            pos_bol = 396;
            pos_cnum = 432
          }
      } "get" in
  Ortac_runtime.Errors.report __error__003_;
  (let x =
     try get t_1 i
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__003_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__003_);
          Ortac_runtime.Errors.report __error__003_;
          raise e) in
   if
     not
       (try
          x =
            (Ortac_runtime.Gospelstdlib.List.nth (cont t_1)
               (Ortac_runtime.Gospelstdlib.integer_of_int i))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "x = List.nth t.cont i"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__003_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "x = List.nth t.cont i"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__003_);
   Ortac_runtime.Errors.report __error__003_;
   x)
let head t_2 =
  let __error__004_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 13;
            pos_bol = 434;
            pos_cnum = 434
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 16;
            pos_bol = 590;
            pos_cnum = 623
          }
      } "head" in
  Ortac_runtime.Errors.report __error__004_;
  (let x_1 =
     try head t_2
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__004_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__004_);
          Ortac_runtime.Errors.report __error__004_;
          raise e) in
   if
     not
       (try x_1 = (Ortac_runtime.Gospelstdlib.List.hd (cont t_2))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "x = List.hd t.cont"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__004_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "x = List.hd t.cont"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__004_);
   Ortac_runtime.Errors.report __error__004_;
   x_1)
let left_t t_3 =
  let __error__005_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 18;
            pos_bol = 625;
            pos_cnum = 625
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 22;
            pos_bol = 766;
            pos_cnum = 768
          }
      } "left_t" in
  Ortac_runtime.Errors.report __error__005_;
  (let t' =
     try left_t t_3
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__005_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__005_);
          Ortac_runtime.Errors.report __error__005_;
          raise e) in
   Ortac_runtime.Errors.report __error__005_; t')
let right_t t_4 =
  let __error__006_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 24;
            pos_bol = 770;
            pos_cnum = 770
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 28;
            pos_bol = 913;
            pos_cnum = 915
          }
      } "right_t" in
  Ortac_runtime.Errors.report __error__006_;
  (let t'_1 =
     try right_t t_4
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__006_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__006_);
          Ortac_runtime.Errors.report __error__006_;
          raise e) in
   Ortac_runtime.Errors.report __error__006_; t'_1)
let empty () =
  let __error__007_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 30;
            pos_bol = 917;
            pos_cnum = 917
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 34;
            pos_bol = 1187;
            pos_cnum = 1213
          }
      } "empty" in
  Ortac_runtime.Errors.report __error__007_;
  (let t_5 =
     try empty ()
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__007_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__007_);
          Ortac_runtime.Errors.report __error__007_;
          raise e) in
   if
     not
       (try (cont t_5) = []
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.cont = []"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__007_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.cont = []"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__007_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int (size t_5)) =
            (Ortac_runtime.Gospelstdlib.integer_of_int 0)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.size = 0"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__007_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.size = 0"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__007_);
   Ortac_runtime.Errors.report __error__007_;
   t_5)
let cons x_2 t_6 =
  let __error__008_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 36;
            pos_bol = 1215;
            pos_cnum = 1215
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 41;
            pos_bol = 1579;
            pos_cnum = 1615
          }
      } "cons" in
  Ortac_runtime.Errors.report __error__008_;
  (let t'_2 =
     try cons x_2 t_6
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__008_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__008_);
          Ortac_runtime.Errors.report __error__008_;
          raise e) in
   if
     not
       (try (cont t'_2) = (x_2 :: (cont t_6))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t'.cont = x :: t.cont"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__008_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t'.cont = x :: t.cont"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__008_);
   if
     not
       (try (Ortac_runtime.Gospelstdlib.List.hd (cont t'_2)) = x_2
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "List.hd t'.cont = x"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__008_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "List.hd t'.cont = x"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__008_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int (size t'_2)) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int (size t_6))
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t'.size = t.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__008_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t'.size = t.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__008_);
   Ortac_runtime.Errors.report __error__008_;
   t'_2)
let tail t_7 =
  let __error__009_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 43;
            pos_bol = 1617;
            pos_cnum = 1617
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 47;
            pos_bol = 2051;
            pos_cnum = 2086
          }
      } "tail" in
  Ortac_runtime.Errors.report __error__009_;
  (let t'_3 =
     try tail t_7
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__009_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__009_);
          Ortac_runtime.Errors.report __error__009_;
          raise e) in
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int (size t_7)) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int (size t'_3))
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.size = t'.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__009_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.size = t'.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__009_);
   Ortac_runtime.Errors.report __error__009_;
   t'_3)
let snoc x_3 t_8 =
  let __error__010_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 49;
            pos_bol = 2088;
            pos_cnum = 2088
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 54;
            pos_bol = 2499;
            pos_cnum = 2553
          }
      } "snoc" in
  Ortac_runtime.Errors.report __error__010_;
  (let t'_4 =
     try snoc x_3 t_8
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__010_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__010_);
          Ortac_runtime.Errors.report __error__010_;
          raise e) in
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.List.rev (cont t'_4)) =
            (x_3 :: (Ortac_runtime.Gospelstdlib.List.rev (cont t_8)))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "List.rev t'.cont = x :: List.rev t.cont";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__010_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "List.rev t'.cont = x :: List.rev t.cont"; term_kind = Post
        })
       |> (Ortac_runtime.Errors.register __error__010_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.List.hd (cont t'_4)) =
            (Ortac_runtime.Gospelstdlib.List.hd (cont t_8))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "List.hd t'.cont = List.hd t.cont";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__010_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "List.hd t'.cont = List.hd t.cont"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__010_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int (size t'_4)) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int (size t_8))
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t'.size = t.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__010_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t'.size = t.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__010_);
   Ortac_runtime.Errors.report __error__010_;
   t'_4)
let liat t_9 =
  let __error__011_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 56;
            pos_bol = 2555;
            pos_cnum = 2555
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 61;
            pos_bol = 2991;
            pos_cnum = 2997
          }
      } "liat" in
  Ortac_runtime.Errors.report __error__011_;
  (let t'_5 =
     try liat t_9
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__011_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__011_);
          Ortac_runtime.Errors.report __error__011_;
          raise e) in
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int (size t_9)) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int (size t'_5))
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.size = t'.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__011_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.size = t'.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__011_);
   Ortac_runtime.Errors.report __error__011_;
   t'_5)
let to_list __arg0_2 =
  let __error__012_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 63;
            pos_bol = 2999;
            pos_cnum = 2999
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 63;
            pos_bol = 2999;
            pos_cnum = 3027
          }
      } "to_list" in
  Ortac_runtime.Errors.report __error__012_;
  (let result_2 =
     try to_list __arg0_2
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__012_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__012_);
          Ortac_runtime.Errors.report __error__012_;
          raise e) in
   Ortac_runtime.Errors.report __error__012_; result_2)
let of_list __arg0_3 =
  let __error__013_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 64;
            pos_bol = 3028;
            pos_cnum = 3028
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 64;
            pos_bol = 3028;
            pos_cnum = 3056
          }
      } "of_list" in
  Ortac_runtime.Errors.report __error__013_;
  (let result_3 =
     try of_list __arg0_3
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__013_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__013_);
          Ortac_runtime.Errors.report __error__013_;
          raise e) in
   Ortac_runtime.Errors.report __error__013_; result_3)
