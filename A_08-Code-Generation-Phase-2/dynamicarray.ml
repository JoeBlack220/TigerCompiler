module type DYNAMICARRAY = sig
  type 'a dynarray

  val make : int -> 'a -> 'a dynarray
  val default : 'a dynarray -> 'a
  val sub : 'a dynarray -> int -> 'a
  val update : 'a dynarray -> int -> 'a -> unit
  val bound : 'a dynarray -> int
end

module DynamicArray : DYNAMICARRAY = struct
  (* capacity, default element, array *)
  type 'a dynarray = (int * 'a * 'a array) ref

  let make sz el = ref (0, el, Array.make sz el)

  let default { contents = (_, d, _) } = d

  let sub { contents = (sz, d, arr) } i = if i < sz then Array.get arr i else d

  let update dynarrayref i el =
    let (sz, d, arr) = !dynarrayref in
    let sz' = max sz (i+1) in
    let cap = Array.length arr
    in
      if i < cap
      then
        (Array.set arr i el;
        dynarrayref := (sz', d, arr))
      else
        let arr' = Array.init ((i+1) * 2) (fun j -> if j < cap then Array.get arr j else d)
        in
          Array.set arr' i el;
          dynarrayref := (sz', d, arr')

  let bound { contents = (sz, _, _) } = sz
end

