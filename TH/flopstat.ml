open Shuffle ;; open Batteries ;; 

let table = shuffle () |> List.take 2
  
(1 -- 1000) |> Enum.map (fun _ -> table) |> Enum.filter (fun (x, _) -> x > 0) |>
;;
                                           
  

