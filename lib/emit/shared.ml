exception EmitterError of string

let read_flag name flags =
  match List.find_opt (fun (f, _) -> f = name) flags with
  | Some (_, v) -> Some v
  | None -> None
