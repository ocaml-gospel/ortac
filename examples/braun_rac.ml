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
let empty () =
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
            pos_lnum = 12;
            pos_bol = 499;
            pos_cnum = 525
          }
      } "empty" in
  Ortac_runtime.Errors.report __error__003_;
  (let t_1 =
     try empty ()
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
       (try t_1.cont = []
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.cont = []"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__003_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.cont = []"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__003_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int t_1.size) =
            (Ortac_runtime.Gospelstdlib.integer_of_int 0)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.size = 0"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__003_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.size = 0"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__003_);
   Ortac_runtime.Errors.report __error__003_;
   t_1)
let add_front x t_2 =
  let __error__004_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 14;
            pos_bol = 527;
            pos_cnum = 527
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 18;
            pos_bol = 879;
            pos_cnum = 915
          }
      } "add_front" in
  Ortac_runtime.Errors.report __error__004_;
  (let t' =
     try add_front x t_2
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
       (try t'.cont = (x :: t_2.cont)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t'.cont = x :: t.cont"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__004_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t'.cont = x :: t.cont"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__004_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int t'.size) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int t_2.size)
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t'.size = t.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__004_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t'.size = t.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__004_);
   Ortac_runtime.Errors.report __error__004_;
   t')
let remove_first t_3 =
  let __error__005_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 20;
            pos_bol = 917;
            pos_cnum = 917
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 25;
            pos_bol = 1419;
            pos_cnum = 1455
          }
      } "remove_first" in
  if
    not
      (try not (t_3.cont = [])
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "t.cont <> []"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__005_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "t.cont <> []"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__005_);
  Ortac_runtime.Errors.report __error__005_;
  (let (x_1, t'_1) =
     try remove_first t_3
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__005_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__005_);
          Ortac_runtime.Errors.report __error__005_;
          raise e) in
   if
     not
       (try t_3.cont = (x_1 :: t'_1.cont)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.cont = x :: t'.cont"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__005_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.cont = x :: t'.cont"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__005_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int t_3.size) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int t'_1.size)
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.size = t'.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__005_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.size = t'.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__005_);
   Ortac_runtime.Errors.report __error__005_;
   (x_1, t'_1))
let append x_2 t_4 =
  let __error__006_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 27;
            pos_bol = 1457;
            pos_cnum = 1457
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 31;
            pos_bol = 1831;
            pos_cnum = 1885
          }
      } "append" in
  Ortac_runtime.Errors.report __error__006_;
  (let t'_2 =
     try append x_2 t_4
     with
     | Stack_overflow | Out_of_memory as e ->
         (Ortac_runtime.Errors.report __error__006_; raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__006_);
          Ortac_runtime.Errors.report __error__006_;
          raise e) in
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.List.rev t'_2.cont) =
            (x_2 :: (Ortac_runtime.Gospelstdlib.List.rev t_4.cont))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "List.rev t'.cont = x :: List.rev t.cont";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__006_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "List.rev t'.cont = x :: List.rev t.cont"; term_kind = Post
        })
       |> (Ortac_runtime.Errors.register __error__006_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int t'_2.size) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int t_4.size)
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t'.size = t.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__006_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t'.size = t.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__006_);
   Ortac_runtime.Errors.report __error__006_;
   t'_2)
let remove_last t_5 =
  let __error__007_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 33;
            pos_bol = 1887;
            pos_cnum = 1887
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 38;
            pos_bol = 2403;
            pos_cnum = 2457
          }
      } "remove_last" in
  if
    not
      (try not (t_5.cont = [])
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "t.cont <> []"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__007_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "t.cont <> []"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__007_);
  Ortac_runtime.Errors.report __error__007_;
  (let (x_3, t'_3) =
     try remove_last t_5
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
       (try
          (Ortac_runtime.Gospelstdlib.List.rev t_5.cont) =
            (x_3 :: (Ortac_runtime.Gospelstdlib.List.rev t'_3.cont))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term = "List.rev t.cont = x :: List.rev t'.cont";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__007_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "List.rev t.cont = x :: List.rev t'.cont"; term_kind = Post
        })
       |> (Ortac_runtime.Errors.register __error__007_);
   if
     not
       (try
          (Ortac_runtime.Gospelstdlib.integer_of_int t_5.size) =
            (Ortac_runtime.Gospelstdlib.(+)
               (Ortac_runtime.Gospelstdlib.integer_of_int t'_3.size)
               (Ortac_runtime.Gospelstdlib.integer_of_int 1))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "t.size = t'.size + 1"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__007_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "t.size = t'.size + 1"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__007_);
   Ortac_runtime.Errors.report __error__007_;
   (x_3, t'_3))
let find i t_6 =
  let __error__008_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 40;
            pos_bol = 2459;
            pos_cnum = 2459
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 45;
            pos_bol = 2785;
            pos_cnum = 2821
          }
      } "find" in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.(<)
           (Ortac_runtime.Gospelstdlib.integer_of_int i)
           (Ortac_runtime.Gospelstdlib.integer_of_int t_6.size)
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = "i < t.size"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__008_);
            true))
  then
    (Ortac_runtime.Violated_condition
       { term = "i < t.size"; term_kind = Pre })
      |> (Ortac_runtime.Errors.register __error__008_);
  Ortac_runtime.Errors.report __error__008_;
  (let x_4 =
     try find i t_6
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
       (try
          x_4 =
            (Ortac_runtime.Gospelstdlib.List.nth t_6.cont
               (Ortac_runtime.Gospelstdlib.integer_of_int i))
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "x = List.nth t.cont i"; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__008_);
             true))
   then
     (Ortac_runtime.Violated_condition
        { term = "x = List.nth t.cont i"; term_kind = Post })
       |> (Ortac_runtime.Errors.register __error__008_);
   Ortac_runtime.Errors.report __error__008_;
   x_4)
