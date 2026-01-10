module ValueMap = Map.Make (String)

type 'a t = 'a ValueMap.t

let make () = ValueMap.empty
let get environment binding = ValueMap.find_opt binding environment
let bind environment binding value = ValueMap.add binding value environment

let extend ~base ~bindings =
  let prefer_bindings _key _base_val bindings_val = Some bindings_val in
  ValueMap.union prefer_bindings base bindings
;;
