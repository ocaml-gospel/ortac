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
            pos_cnum = 158;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 5;
            pos_bol = 158;
            pos_cnum = 190;
          };
      }
      "size"
  in
  Ortac_runtime.Errors.report __error__001_;
  let result =
    try size __arg0 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__001_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__001_;
        Ortac_runtime.Errors.report __error__001_;
        raise e
  in
  Ortac_runtime.Errors.report __error__001_;
  result

let cont __arg0_1 =
  let __error__002_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 6;
            pos_bol = 191;
            pos_cnum = 191;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 6;
            pos_bol = 191;
            pos_cnum = 227;
          };
      }
      "cont"
  in
  Ortac_runtime.Errors.report __error__002_;
  let result_1 =
    try cont __arg0_1 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__002_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__002_;
        Ortac_runtime.Errors.report __error__002_;
        raise e
  in
  Ortac_runtime.Errors.report __error__002_;
  result_1

let get t_1 i =
  let __error__003_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 7;
            pos_bol = 228;
            pos_cnum = 228;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 10;
            pos_bol = 394;
            pos_cnum = 430;
          };
      }
      "get"
  in
  Ortac_runtime.Errors.report __error__003_;
  let x =
    try get t_1 i with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__003_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__003_;
        Ortac_runtime.Errors.report __error__003_;
        raise e
  in
  if
    not
      (try
         x
         = Ortac_runtime.Gospelstdlib.List.nth (cont t_1)
             (Ortac_runtime.Gospelstdlib.integer_of_int i)
       with e ->
         Ortac_runtime.Specification_failure
           { term = "x = List.nth t.cont i"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__003_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "x = List.nth t.cont i"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__003_;
  Ortac_runtime.Errors.report __error__003_;
  x

let head t_2 =
  let __error__004_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 12;
            pos_bol = 432;
            pos_cnum = 432;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 15;
            pos_bol = 589;
            pos_cnum = 622;
          };
      }
      "head"
  in
  Ortac_runtime.Errors.report __error__004_;
  let x_1 =
    try head t_2 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__004_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__004_;
        Ortac_runtime.Errors.report __error__004_;
        raise e
  in
  if
    not
      (try x_1 = Ortac_runtime.Gospelstdlib.List.hd (cont t_2)
       with e ->
         Ortac_runtime.Specification_failure
           { term = "x = List.hd t.cont"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__004_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "x = List.hd t.cont"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__004_;
  Ortac_runtime.Errors.report __error__004_;
  x_1

let left_t t_3 =
  let __error__005_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 17;
            pos_bol = 624;
            pos_cnum = 624;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 21;
            pos_bol = 766;
            pos_cnum = 768;
          };
      }
      "left_t"
  in
  Ortac_runtime.Errors.report __error__005_;
  let t' =
    try left_t t_3 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__005_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__005_;
        Ortac_runtime.Errors.report __error__005_;
        raise e
  in
  Ortac_runtime.Errors.report __error__005_;
  t'

let right_t t_4 =
  let __error__006_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 23;
            pos_bol = 770;
            pos_cnum = 770;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 27;
            pos_bol = 914;
            pos_cnum = 916;
          };
      }
      "right_t"
  in
  Ortac_runtime.Errors.report __error__006_;
  let t'_1 =
    try right_t t_4 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__006_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__006_;
        Ortac_runtime.Errors.report __error__006_;
        raise e
  in
  Ortac_runtime.Errors.report __error__006_;
  t'_1

let empty () =
  let __error__007_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 29;
            pos_bol = 918;
            pos_cnum = 918;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 33;
            pos_bol = 1189;
            pos_cnum = 1215;
          };
      }
      "empty"
  in
  Ortac_runtime.Errors.report __error__007_;
  let t_5 =
    try empty () with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__007_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__007_;
        Ortac_runtime.Errors.report __error__007_;
        raise e
  in
  if
    not
      (try cont t_5 = []
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t.cont = []"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__007_;
         true)
  then
    Ortac_runtime.Violated_condition { term = "t.cont = []"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__007_;
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.integer_of_int (size t_5)
         = Ortac_runtime.Gospelstdlib.integer_of_int 0
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t.size = 0"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__007_;
         true)
  then
    Ortac_runtime.Violated_condition { term = "t.size = 0"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__007_;
  Ortac_runtime.Errors.report __error__007_;
  t_5

let cons x_2 t_6 =
  let __error__008_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 35;
            pos_bol = 1217;
            pos_cnum = 1217;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 40;
            pos_bol = 1582;
            pos_cnum = 1618;
          };
      }
      "cons"
  in
  Ortac_runtime.Errors.report __error__008_;
  let t'_2 =
    try cons x_2 t_6 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__008_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__008_;
        Ortac_runtime.Errors.report __error__008_;
        raise e
  in
  if
    not
      (try cont t'_2 = x_2 :: cont t_6
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t'.cont = x :: t.cont"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__008_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "t'.cont = x :: t.cont"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__008_;
  if
    not
      (try Ortac_runtime.Gospelstdlib.List.hd (cont t'_2) = x_2
       with e ->
         Ortac_runtime.Specification_failure
           { term = "List.hd t'.cont = x"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__008_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "List.hd t'.cont = x"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__008_;
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.integer_of_int (size t'_2)
         = Ortac_runtime.Gospelstdlib.( + )
             (Ortac_runtime.Gospelstdlib.integer_of_int (size t_6))
             (Ortac_runtime.Gospelstdlib.integer_of_int 1)
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t'.size = t.size + 1"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__008_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "t'.size = t.size + 1"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__008_;
  Ortac_runtime.Errors.report __error__008_;
  t'_2

let tail t_7 =
  let __error__009_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 42;
            pos_bol = 1620;
            pos_cnum = 1620;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 47;
            pos_bol = 1982;
            pos_cnum = 2017;
          };
      }
      "tail"
  in
  Ortac_runtime.Errors.report __error__009_;
  let t'_3 =
    try tail t_7 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__009_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__009_;
        Ortac_runtime.Errors.report __error__009_;
        raise e
  in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.integer_of_int (size t_7)
         = Ortac_runtime.Gospelstdlib.( + )
             (Ortac_runtime.Gospelstdlib.integer_of_int (size t'_3))
             (Ortac_runtime.Gospelstdlib.integer_of_int 1)
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t.size = t'.size + 1"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__009_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "t.size = t'.size + 1"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__009_;
  Ortac_runtime.Errors.report __error__009_;
  t'_3

let snoc x_3 t_8 =
  let __error__010_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 49;
            pos_bol = 2019;
            pos_cnum = 2019;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 54;
            pos_bol = 2431;
            pos_cnum = 2485;
          };
      }
      "snoc"
  in
  Ortac_runtime.Errors.report __error__010_;
  let t'_4 =
    try snoc x_3 t_8 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__010_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__010_;
        Ortac_runtime.Errors.report __error__010_;
        raise e
  in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.List.rev (cont t'_4)
         = x_3 :: Ortac_runtime.Gospelstdlib.List.rev (cont t_8)
       with e ->
         Ortac_runtime.Specification_failure
           {
             term = "List.rev t'.cont = x :: List.rev t.cont";
             term_kind = Post;
             exn = e;
           }
         |> Ortac_runtime.Errors.register __error__010_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "List.rev t'.cont = x :: List.rev t.cont"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__010_;
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.List.hd (cont t'_4)
         = Ortac_runtime.Gospelstdlib.List.hd (cont t_8)
       with e ->
         Ortac_runtime.Specification_failure
           {
             term = "List.hd t'.cont = List.hd t.cont";
             term_kind = Post;
             exn = e;
           }
         |> Ortac_runtime.Errors.register __error__010_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "List.hd t'.cont = List.hd t.cont"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__010_;
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.integer_of_int (size t'_4)
         = Ortac_runtime.Gospelstdlib.( + )
             (Ortac_runtime.Gospelstdlib.integer_of_int (size t_8))
             (Ortac_runtime.Gospelstdlib.integer_of_int 1)
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t'.size = t.size + 1"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__010_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "t'.size = t.size + 1"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__010_;
  Ortac_runtime.Errors.report __error__010_;
  t'_4

let liat t_9 =
  let __error__011_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 56;
            pos_bol = 2487;
            pos_cnum = 2487;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 62;
            pos_bol = 2852;
            pos_cnum = 2858;
          };
      }
      "liat"
  in
  Ortac_runtime.Errors.report __error__011_;
  let t'_5 =
    try liat t_9 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__011_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__011_;
        Ortac_runtime.Errors.report __error__011_;
        raise e
  in
  if
    not
      (try
         Ortac_runtime.Gospelstdlib.integer_of_int (size t_9)
         = Ortac_runtime.Gospelstdlib.( + )
             (Ortac_runtime.Gospelstdlib.integer_of_int (size t'_5))
             (Ortac_runtime.Gospelstdlib.integer_of_int 1)
       with e ->
         Ortac_runtime.Specification_failure
           { term = "t.size = t'.size + 1"; term_kind = Post; exn = e }
         |> Ortac_runtime.Errors.register __error__011_;
         true)
  then
    Ortac_runtime.Violated_condition
      { term = "t.size = t'.size + 1"; term_kind = Post }
    |> Ortac_runtime.Errors.register __error__011_;
  Ortac_runtime.Errors.report __error__011_;
  t'_5

let to_list __arg0_2 =
  let __error__012_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 64;
            pos_bol = 2860;
            pos_cnum = 2860;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 64;
            pos_bol = 2860;
            pos_cnum = 2889;
          };
      }
      "to_list"
  in
  Ortac_runtime.Errors.report __error__012_;
  let result_2 =
    try to_list __arg0_2 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__012_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__012_;
        Ortac_runtime.Errors.report __error__012_;
        raise e
  in
  Ortac_runtime.Errors.report __error__012_;
  result_2

let of_list __arg0_3 =
  let __error__013_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 65;
            pos_bol = 2890;
            pos_cnum = 2890;
          };
        Ortac_runtime.stop =
          {
            pos_fname = "braun_tree.mli";
            pos_lnum = 65;
            pos_bol = 2890;
            pos_cnum = 2919;
          };
      }
      "of_list"
  in
  Ortac_runtime.Errors.report __error__013_;
  let result_3 =
    try of_list __arg0_3 with
    | (Stack_overflow | Out_of_memory) as e ->
        Ortac_runtime.Errors.report __error__013_;
        raise e
    | e ->
        Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e }
        |> Ortac_runtime.Errors.register __error__013_;
        Ortac_runtime.Errors.report __error__013_;
        raise e
  in
  Ortac_runtime.Errors.report __error__013_;
  result_3
