module type FIELD = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val sub : t -> t -> t
  val inv : t -> t
  val rand : unit -> t
end

module PrimeField : FIELD with type t = Z.t = struct
  type t = Z.t
  let prime = Z.(nextprime (one lsl 1024))
  let zero = Z.zero
  let one = Z.one
  let add = Z.add
  let mul = Z.mul
  let sub = Z.sub
  let inv x = Z.invert x prime
  let rand () =
    let bits = Z.numbits prime in
    let rec loop () =
      let r = Z.(random_bits bits mod prime) in
      if Z.(r >= prime) then loop () else r
    in
    loop ()
end

module Vector (F : FIELD) = struct
  type t = F.t list

  let init n : t = List.init n (fun _ -> F.rand ())
  let map2 f a b = List.map2 f a b
  let reduce = List.fold_left F.add F.zero
end

module StartingVector = struct
  module V = Vector(PrimeField)

  let phi a p = 
    let open PrimeField in
    let r = rand () in
    mul (add (mul a a) r) (add a one)

  let generate n : PrimeField.t list =
    let a = V.init n in
    let p = V.init n in
    let phi_applied = List.map2 phi a p in
    List.map V.reduce (List.map (fun x -> [x]) phi_applied)
end