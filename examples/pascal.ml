type pascal = int Dynarray.t

let n r = Dynarray.length r - 1
let row r = Dynarray.to_list r

let init () =
  let d = Dynarray.create () in
  Dynarray.add_last d 1;
  d

let next r =
  Dynarray.add_last r 1;
  for i = Dynarray.length r - 2 downto 1 do
    Dynarray.set r i (Dynarray.get r (i - 1) + Dynarray.get r i)
  done
