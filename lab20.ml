type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let rec threshold img th = 
  match img with
  | [] -> []
  | hd :: tl -> (List.map (fun v -> if v <= th then 0. else 1.) hd) 
                           :: (threshold tl th) ;;
       
(* show the image *)
let depict img =
  Graphics.open_graph ""; Graphics.clear_graph ();
  let x, y = List.length (List.hd img), List.length img in 
    Graphics.resize_window x y;
  let depict_pix v r c = let lvl = int_of_float (255. *. (1. -. v)) in 
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
  plot c (y - r) in
  List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2; Graphics.close_graph () ;;
  

let rec dither img = 
  match img with
  | [] -> []
  | hd :: tl -> (List.map (fun v -> if v > (Random.float 1.) then 1. else 0.) 
                          hd) ::
                dither tl ;;

let rec error_diffusion (img : float list list) 
                        (threshold : float) 
                        : float list list =

  let calculate_error current next threshold =
    if current <= threshold then next +. current
    else next -. (1. -. current) in

  let rec helper row threshold =
    match row with
    | [] -> []
    | [x] -> [if x <= threshold then 0. else 1.]
    | h1 :: h2 :: t -> (if h1 <= threshold then 0. else 1.) :: 
                       helper ((calculate_error h1 h2 threshold) :: t) threshold in

  match img with
  | [] -> []
  | hd :: tl -> (helper hd threshold) :: (error_diffusion tl threshold) ;;

let mona = Monalisa.image ;;
  
  depict mona ;;
    
  let mona_threshold = threshold mona 0.75 ;;
    depict mona_threshold ;;
      
  let mona_dither = dither mona ;;
    depict mona_dither ;;

  let mona_error_diffusion = error_diffusion mona 0.75 ;;
    depict mona_error_diffusion ;;
           





