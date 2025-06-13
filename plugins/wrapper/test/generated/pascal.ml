type pascal = int Array.t ref

let n (r : pascal) = Array.length !r - 1
let row (r : pascal) = Array.to_list !r
let init () : pascal = ref (Array.make 1 1)

let next (r : pascal) =
  r := Array.append !r (Array.make 1 1);
  for i = Array.length !r - 2 downto 1 do
    Array.set !r i (Array.get !r (i - 1) + Array.get !r i)
  done
