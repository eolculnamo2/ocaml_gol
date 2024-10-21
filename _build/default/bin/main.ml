let height = 40
let width = 55
let total = height * width
let generation_interval = 0.01

type cell =
  { index : int
  ; is_alive : bool
  }

let init_cells : cell array =
  Array.init total (fun i -> { index = i; is_alive = Random.bool () })
;;

let will_live cell cells =
  let neighbor_count =
    [ cell.index - width - 1
    ; cell.index - width
    ; cell.index - width + 1
    ; cell.index - 1
    ; cell.index + 1
    ; cell.index + width - 1
    ; cell.index + width
    ; cell.index + width + 1
    ]
    |> Base.List.fold ~init:0 ~f:(fun agg cur ->
      if cur >= 0 && cur < total
      then (
        let neighbor = Array.get cells cur in
        if neighbor.is_alive then agg + 1 else agg)
      else agg)
  in
  match neighbor_count, cell.is_alive with
  | 2, true | 3, true -> true
  | 3, false -> true
  | _, true -> false
  | _, false -> false
;;

let draw cells =
  let lines =
    cells
    |> Base.Array.foldi ~init:"" ~f:(fun i agg cur ->
      agg
      ^
      match (i + 1) mod width, cur.is_alive with
      | 0, true -> "[\027[31m*\027[0m]\n"
      | 0, false -> "[ ]\n"
      | _, true -> "[\027[31m*\027[0m]"
      | _, false -> "[ ]")
  in
  print_endline (lines ^ "\x1b[1K\r")
;;

(* thread sleep hack to use float *)
let sleep (sec : float) = ignore (Unix.select [] [] [] sec)

let rec generations cells =
  let next_cells =
    cells |> Array.map (fun c -> { c with is_alive = will_live c cells })
  in
  Printf.printf "\027[2J";
  flush stdout;
  draw next_cells;
  sleep generation_interval;
  generations next_cells
;;

let () =
  let ic = init_cells in
  generations ic
;;
