type plugins = unit Cmdliner.Cmd.t Queue.t

let plugins = Queue.create ()
let register cmd = Queue.add cmd plugins
let fold = Queue.fold
