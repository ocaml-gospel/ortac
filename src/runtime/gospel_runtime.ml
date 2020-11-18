module Z = struct
  include Z

  let rec forall start stop p =
    if equal start stop then p start else p start && forall (succ start) stop p
end
