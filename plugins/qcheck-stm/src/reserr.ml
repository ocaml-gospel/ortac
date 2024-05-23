module W = Ortac_core.Warnings

type init_state_error =
  | Mismatch_number_of_arguments of string
  | No_appropriate_specifications of string * string list
  | No_specification of string
  | No_translatable_specification of string
  | Not_a_function_call of string
  | Not_returning_sut of string
  | Qualified_name of string

type W.kind +=
  | Constant_value of string
  | Ensures_not_found_for_next_state of (string * string)
  | Functional_argument of string
  | Ghost_values of (string * [ `Arg | `Ret ])
  | Ignored_modifies
  | Impossible_init_state_generation of init_state_error
  | Impossible_term_substitution of
      [ `Never | `New | `Old | `NotModel | `OutOfScope ]
  | Incompatible_sut of string
  | Incompatible_type of (string * string)
  | Incomplete_ret_val_computation of string
  | Incomplete_configuration_module of [ `Init_sut | `Sut ]
  | Multiple_sut_arguments of string
  | No_configuration_file of string
  | No_init_function of string
  | No_models of string
  | No_spec of string
  | No_sut_argument of string
  | No_sut_type of string
  | Returned_tuple of string
  | Returning_sut of string
  | Sut_type_not_specified of string
  | Sut_type_not_supported of string
  | Syntax_error_in_config_module of string
  | Type_not_supported of string
  | Type_not_supported_for_sut_parameter of string
  | Type_parameter_not_instantiated of string

let level kind =
  match kind with
  | Constant_value _ | Ensures_not_found_for_next_state _
  | Functional_argument _ | Ghost_values _ | Ignored_modifies
  | Impossible_term_substitution _ | Incompatible_type _
  | Incomplete_ret_val_computation _ | Multiple_sut_arguments _ | No_spec _
  | No_sut_argument _ | Returned_tuple _ | Returning_sut _
  | Type_not_supported _ ->
      W.Warning
  | Impossible_init_state_generation _ | Incompatible_sut _
  | Incomplete_configuration_module _ | No_configuration_file _
  | No_init_function _ | No_models _ | No_sut_type _ | Sut_type_not_specified _
  | Sut_type_not_supported _ | Syntax_error_in_config_module _
  | Type_not_supported_for_sut_parameter _ | Type_parameter_not_instantiated _
    ->
      W.Error
  | _ -> W.level kind

type 'a reserr = ('a, W.t list) result * W.t list

let ok x = (Result.ok x, [])
let error e = (Result.error [ e ], [])
let warns ws = (Result.ok (), ws)
let warn w = warns [ w ]

let ( let* ) x f =
  match x with
  | Ok v, warns1 ->
      let res, warns2 = f v in
      (res, warns1 @ warns2)
  | (Error _, _) as x -> x

let ( >>= ) = ( let* )

let ( and* ) (a, aw) (b, bw) =
  let r =
    match (a, b) with
    | Error e0, Error e1 -> Error (e0 @ e1)
    | Error e, _ | _, Error e -> Error e
    | Ok a, Ok b -> Ok (a, b)
  in
  (r, aw @ bw)

let fmap f r =
  let* r = r in
  ok (f r)

let ( <$> ) = fmap

let app f r =
  let* f = f and* r = r in
  ok (f r)

let ( <*> ) = app

let pp_kind ppf kind =
  let open Fmt in
  match kind with
  (* Warnings *)
  | Constant_value id ->
      pf ppf "Skipping %s:@ %a" id text "constants cannot be tested"
  | Returning_sut id ->
      pf ppf "Skipping %s:@ %a" id text
        "functions returning a SUT value cannot be tested"
  | No_sut_argument id ->
      pf ppf "Skipping %s:@ %a" id text
        "functions with no SUT argument cannot be tested"
  | Multiple_sut_arguments id ->
      pf ppf "Skipping %s:@ %a" id text
        "functions with multiple SUT arguments cannot be tested"
  | Incompatible_type (v, t) ->
      pf ppf "Skipping %s:@ %a%s" v text
        "the type of its SUT-type argument is incompatible with the configured \
         SUT type: "
        t
  | No_spec fct ->
      pf ppf "Skipping %s:@ %a" fct text
        "functions without specifications cannot be tested"
  | Impossible_term_substitution why ->
      let msg =
        match why with
        | `NotModel ->
            "occurrences of the SUT in clauses are only supported to access \
             its model fields"
        (* The  [`Never] case is used when generating [init_state] *)
        | `Never ->
            "impossible to define the initial value of the model with a \
             recursive expression"
        (* The following cases should not be reported to the user at the moment
           (because they should be caught at some other points) *)
        | `Old ->
            "occurrences of the SUT in clauses are not supported under old \
             operator"
        | `New ->
            "occurrences of the SUT in clauses are not supported above old \
             operator"
        | `OutOfScope ->
            "occurrences of returned values that are out of scope in the \
             next_state function"
      in
      pf ppf "Skipping clause:@ %a" text msg
  | Ignored_modifies ->
      pf ppf "Skipping unsupported modifies clause:@ %a" text
        "expected \"modifies x\" or \"modifies x.model\" where x is the SUT"
  | Ensures_not_found_for_next_state (f, m) ->
      pf ppf "Skipping %s:@ model@ %s %a@;%a%s%a" f m text
        "is declared as modified by the function but no suitable ensures \
         clause was found."
        text "Specifications should contain at least one \"ensures x." m text
        " = expr\" where x is the SUT and expr can refer to the SUT only under \
         an old operator and can't refer to the returned value"
  | Functional_argument f ->
      pf ppf "Skipping %s:@ %a" f text
        "functions are not supported yet as arguments"
  | Returned_tuple f ->
      pf ppf "Skipping %s:@ %a" f text
        "functions returning tuples are not supported yet"
  | Ghost_values (id, k) ->
      pf ppf "Skipping %s:@ %a%a%a" id text "functions with a ghost " text
        (match k with `Arg -> "argument" | `Ret -> "returned value")
        text " are not supported"
  (* This following message is broad and used in seemingly different contexts
     but in fact we support all the types that the Gospel type-checker supports,
     so that error message should never get reported to the end user *)
  | Type_not_supported ty -> pf ppf "Type %s not supported" ty
  (* Errors *)
  | No_sut_type ty -> pf ppf "Type %s not declared in the module" ty
  | No_init_function f -> pf ppf "Function %s not declared in the module" f
  | Syntax_error_in_config_module s ->
      pf ppf "Syntax error in OCaml configuration module %s" s
  | Sut_type_not_supported ty ->
      pf ppf "Unsupported SUT type %s:@ %a" ty text
        "SUT type must be a type constructor, possibly applied to type \
         arguments"
  | Type_parameter_not_instantiated ty ->
      pf ppf "Unsupported type parameter %s:@ %a" ty text
        "SUT type should be fully instantiated"
  | Type_not_supported_for_sut_parameter ty ->
      pf ppf "Unsupported type parameter %s:@ %a" ty text
        "only constructors and tuples are supported in arguments for the SUT \
         type"
  | Sut_type_not_specified ty ->
      pf ppf "Missing specification for the SUT type %s" ty
  | No_configuration_file file -> pf ppf "Missing configuration file %s" file
  | No_models ty -> pf ppf "Missing model(s) for the SUT type %s" ty
  | Impossible_init_state_generation (Not_a_function_call fct) ->
      pf ppf "Unsupported INIT expression %s:@ %a" fct text
        "the INIT expression is expected to be a function call (the \
         specification of that function is required to initialize the model \
         state)"
  | Impossible_init_state_generation (No_specification fct) ->
      pf ppf "Unsupported INIT function %s:@ %a" fct text
        "the function called in the INIT expression must be specified to \
         initialize the model state"
  | Impossible_init_state_generation
      (No_appropriate_specifications (fct, models)) ->
      pf ppf "Unsupported INIT function %s:@ %a:@ %a" fct text
        "the specification of the function called in the INIT expression does \
         not specify the following fields of the model"
        (Fmt.list ~sep:(Fmt.any ",@ ") Fmt.string)
        models
  | Impossible_init_state_generation (No_translatable_specification model) ->
      pf ppf "Unsupported INIT function:@ %a:@ %s" text
        "the specification of the function called in the INIT expression does \
         not provide a translatable specification for the following field of \
         the model"
        model
  | Impossible_init_state_generation (Not_returning_sut fct) ->
      pf ppf "Unsupported INIT expression %s:@ %a" fct text
        "the function called in the INIT expression must return a value of SUT \
         type"
  | Impossible_init_state_generation (Qualified_name fct) ->
      pf ppf "Unsupported INIT function %s:@ %a" fct text
        "qualified names are not yet supported"
  | Impossible_init_state_generation (Mismatch_number_of_arguments fct) ->
      pf ppf "Error in INIT expression %s:@ %a" fct text
        "mismatch in the number of arguments between the INIT expression and \
         the function specification"
  | Incomplete_configuration_module missing ->
      let what =
        match missing with
        | `Init_sut -> "the init_sut value declaration"
        | `Sut -> "the sut type declaration"
      in
      pf ppf "Incomplete configuration module: it is missing %s" what
  | Incompatible_sut t ->
      pf ppf "Incompatible declaration of SUT type:@ %a%s" text
        "the declaration of the SUT type is incompatible with the configured \
         one: "
        t
  | Incomplete_ret_val_computation fct ->
      pf ppf
        "Incomplete computation of the returned value in the specification of \
         %s. Failure message won't be able to display the expected returned \
         value"
        fct
  | _ -> W.pp_kind ppf kind

let pp_errors = W.pp_param pp_kind level |> Fmt.list

let pp quiet pp_ok ppf r =
  let open Fmt in
  match r with
  | Ok a, warns -> (
      pf ppf
        "(* This file is generated by ortac qcheck-stm,@\n\
        \   edit how you run the tool instead *)@\n\
         %a@."
        pp_ok a;
      if not quiet then
        match warns with [] -> () | warns -> pf stderr "%a@." pp_errors warns)
  | Error errs, warns -> pf stderr "%a@." pp_errors (errs @ warns)

let sequence r =
  let rec aux = function
    | [] -> ok []
    | ((Ok _, _) as x) :: xs ->
        let* y = x and* ys = aux xs in
        ok (y :: ys)
    | ((Error _, _) as x) :: _ -> x
  in
  aux r

let rec filter_errs = function
  | [] -> ok ()
  | ((k, _) as e) :: es -> (
      match level k with
      | W.Warning ->
          let* _ = warn e in
          filter_errs es
      | W.Error -> error e)

let rec promote = function
  | [] -> ok []
  | ((Ok _, _) as x) :: xs ->
      let* y = x and* ys = promote xs in
      ok (y :: ys)
  | (Error errs, ws) :: xs ->
      let* _ = warns ws and* _ = filter_errs errs in
      promote xs

let promote_opt r =
  match r with
  | (Ok _, _) as x ->
      let* y = x in
      ok (Some y)
  | Error errs, ws ->
      let* _ = warns ws and* _ = filter_errs errs in
      ok None

let rec fold_left (f : 'a -> 'b -> 'a reserr) (acc : 'a) : 'b list -> 'a reserr
    = function
  | [] -> ok acc
  | x :: xs -> (
      match f acc x with
      | (Ok _, _) as acc ->
          let* acc = acc in
          fold_left f acc xs
      | Error errs, ws ->
          let* _ = warns ws and* _ = filter_errs errs in
          fold_left f acc xs)

let of_option ~default = Option.fold ~none:(error default) ~some:ok
let to_option = function Ok x, _ -> Some x | _ -> None
let map f l = List.map f l |> promote
let concat_map f l = fmap List.concat (map f l)
