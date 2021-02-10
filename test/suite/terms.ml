open Terms__Lib_rtac
open Common

let bool_ops () =
  check_success "lazy bool no exception" (fun () -> lazy_bool 42 |> ignore);
  check_raises_gospel "not lazy bool or" (fun () -> not_lazy_or 42 |> ignore);
  check_raises_gospel "not lazy bool and" (fun () -> not_lazy_and 42 |> ignore)

let scopes () =
  check_success "override variable in let binding" (fun () ->
      scope1 42 |> ignore)

let logic () =
  check_success "forall in if" (fun () -> if_forall 3 |> ignore);
  check_success "equivalence" equiv;
  check_success "exists" exists_

let patterns () =
  check_success "valid match" (fun () -> a A);
  check_success "valid match with argument" (fun () -> b (B "hello"));
  check_raises_gospel "invalid match" (fun () -> a (B "hello"));
  check_raises_gospel "invalid match with arguments" (fun () -> b A)

let suite =
  ( "Terms",
    [
      ("boolean operators", `Quick, bool_ops);
      ("scopes", `Quick, scopes);
      ("logic", `Quick, logic);
      ("patterns", `Quick, patterns);
    ] )
