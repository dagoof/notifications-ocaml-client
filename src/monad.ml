module type SIG = sig
  type 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val return : 'a -> 'a t
end

module Make (M : SIG) = struct
  include M
  let ( >>= ) t f = bind f t
  let join mm = mm >>= fun x -> x
  let map f m = m >>= fun x -> return @@ f x
  let bind2 a b f = a >>= fun x -> b >>= f x 
  let ( >>| ) m f = map f m
  let ( >> ) m f = m >>= fun _ -> f ()
  let ( <*> ) f t = f >>= fun f -> t >>| f
  let ( <$> ) f t = t >>| f
  let map2 f m1 m2 = m1 >>= fun x -> map (f x) m2
  let ignore m = map (fun _ -> ()) m
end

module Option = struct
  include Make (struct
      type 'a t = 'a option
      let bind f m = match m with Some x -> f x | None as e -> e
      let return x = Some x
    end)

  let with_default ~default t =
    match t with 
    | Some x -> x
    | None -> default
end
