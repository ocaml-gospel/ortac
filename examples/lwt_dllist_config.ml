open Lwt_dllist_spec

type sut = int t

let init_sut = create ()

module Ty = struct
  type _ ty += Node : 'a ty -> 'a node ty

  let node spec =
    let ty, show = spec in
    (Node ty, fun n -> Printf.sprintf "Node %s" (show (get n)))
end
