open Midi



type objet_musical = 
  Note of (int * int * int)
|Silence of int
|Sequence of objet_musical list
|Parallel of objet_musical list;;



let exemple = Parallel ([Sequence ([Note(60,100,1000);Note(64,100,500);Note(62,100,500);Silence 1000;Note(67,100,1000)]);
             Sequence([Note(52,100,2000);Note(55,100,1000);Note(55,100,1000)])
            ]);;


let duration m = 
  let rec atom = function
      Silence (d) -> d
    |Note(_,_,d) -> d
    |Sequence(l) ->  sequenceList l
    |Parallel(l) -> parallelList l
    and sequenceList = function
      |[] -> 0
      |h::t -> (atom h) + (sequenceList t)
    and parallelList = function
      |[] -> 0
      |h::t -> max (atom h) (parallelList t)
in
atom m;;



let copy m =
  let rec copyAtom = function 
      Note(a,b,c) -> Note(a,b,c)
    |Silence(d) -> Silence(d)
    |Sequence(l) -> Sequence(copyList l)
    |Parallel(l) -> Parallel(copyList l)
    and
      copyList = function 
    [] -> []
      |h::t -> (copyAtom h)::(copyList t)
  in 
  copyAtom m;;


let note_count m =
  let rec countAtom = function
      Note(_,_,_) -> 1
    |Silence(_) -> 0
    |Sequence(l) -> countList l
    |Parallel(l) -> countList l
    and countList = function
    [] -> 0
      |h::t -> (countAtom h) + countList(t)
  in 
countAtom m;;

let stretch m c = 
  let rec stretchAtom coef = function
      Note(a,b,c) -> Note(a,b,(
                 int_of_float((float_of_int c) *. coef))
             )
     |Silence(d) -> Silence(int_of_float((float_of_int d) *. coef))
     |Sequence(l) -> Sequence(stretchList coef l)
     |Parallel(l) -> Parallel(stretchList coef l)
    and stretchList coef = function
    [] -> []
      |h::t -> (stretchAtom coef h)::(stretchList coef t)
  in
stretchAtom c m;;

let beats tempo m =
  let duration_seconds = (float_of_int (duration m)) /. 1000.
  in
  int_of_float(duration_seconds *. (float_of_int tempo) /. 60.);;

let transpose m n = 
  let rec transposeAtom = function
      Note(a,b,c) -> Note(((a + n) mod 127),b,c)
     |Silence(d) -> Silence(d)
     |Sequence(l) -> Sequence(transposeList l)
     |Parallel(l) -> Parallel(transposeList l)
    and transposeList = function
    [] -> []
      |h::t -> (transposeAtom  h)::(transposeList t)
  in
transposeAtom m;;
  
let retrograde m =
  let reverse l =
    let rec aux l acc = match l with 
    [] -> acc
       |h::t -> aux t (h::acc) in
    aux l [] 
  in
  let rec copyAtom = function 
      Note(a,b,c) -> Note(a,b,c)
    |Silence(d) -> Silence(d)
    |Sequence(l) -> Sequence(copyList (reverse l))
    |Parallel(l) -> Parallel(copyList l)
    and
      copyList = function 
    [] -> []
      |h::t -> (copyAtom h)::(copyList t)
  in 
  copyAtom m;;



let mirror m center = 
  let rec auxAtom = function
      Note(a,b,c) -> Note(abs((2 * center - a) mod 127),b,c)
     |Silence(d) -> Silence(d)
     |Sequence(l) -> Sequence(auxList l)
     |Parallel(l) -> Parallel(auxList l)
    and auxList = function
    [] -> []
      |h::t -> (auxAtom  h)::(auxList t)
  in
auxAtom m;;

let palindrome m = 
  let concat m1 m2 =
    Sequence((copy m1)::(copy m2)::[])
  in
  let aux = function 
      Note(a,b,c) -> Note(a,b,c)
     |Silence(a) -> Silence(a)
     |_ -> concat m (retrograde m)
  in
  aux m;;


palindrome exemple;;

let chordify m l =
  let rec aux xs = match xs with
    [] -> []
    |h::t -> (transpose m h)::(aux t)
  in
  Parallel(aux l);;

let chordifyList m l = 
    let rec chorAtom = function 
      Note(a,b,c) -> (chordify (Note(a,b,c)) l)
    |Silence(d) -> Silence(d)
    |Sequence(l) -> Sequence(chorList l)
    |Parallel(l) -> Parallel(chorList l)
    and
      chorList = function 
    [] -> []
      |h::t -> (chorAtom h)::(chorList t)
  in 
  chorAtom m;;

let sequencify m i =
  let rec aux = function
    |[] -> []
    |[Note(a,b,c)] -> [Note(a,b,c)]
    |h::t -> if (i = 0) then h::(aux t) else (h::Silence(i)::(aux t))
in
match m with 
  Parallel(l) -> Sequence(aux l)
|_ -> failwith"Bad input: expected chord";;


let scrambleOnset m = 
  let shuffle l =
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  let rec copyAtom = function
    Note x -> Note x
    |Silence x -> Silence x
    |Sequence(l) -> Sequence(copyList (shuffle l))
    |Parallel(l) -> Parallel(copyList l)
  and
      copyList = function
    [] -> []
      |h::t -> (copyAtom h)::(copyList t) 
  in
  copyAtom m;;

let scrambleAll m maxDuree = 
  let shuffle l =
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  let rec copyAtom = function
    Note (x,y,z) -> Note (x,y,Random.int maxDuree)
    |Silence _ -> Silence (Random.int maxDuree)
    |Sequence(l) -> Sequence(copyList (shuffle l))
    |Parallel(l) -> Parallel(copyList l)
  and
      copyList = function
    [] -> []
      |h::t -> (copyAtom h)::(copyList t) 
  in
  copyAtom m;;  



let draw m scale start=
  let add cursor duree = 
    (cursor + int_of_float ((float_of_int duree) *. scale))
    in
  let drawItem cursorX hauteur duree = 
    (*Graphics.moveto cursorX hauteur;*)
    let randColor = (Graphics.rgb (Random.int 190)(Random.int 190)(Random.int 190)) in
    Graphics.set_color randColor;
    Graphics.fill_rect cursorX hauteur (add 0 duree) 1
    (*Graphics.lineto (cursorX + (int_of_float ((float_of_int duree) *. scale))) hauteur*)
  in
  let rec atomDraw cursorX = function
      Note(h,_,d) -> drawItem cursorX h d
    |Silence d -> print_string "silence \n"
    |Sequence(l) -> sequenceDraw cursorX l
    |Parallel(l) -> List.iter (atomDraw cursorX) l
    and
      sequenceDraw cursorX = function
	[] -> print_string "endlist \n"
      |Note(h,x,d)::t ->
	  atomDraw cursorX (Note(h,x,d));
	  sequenceDraw (add cursorX d) t;
      |Silence(d)::t -> sequenceDraw (add cursorX d) t
      |h::t -> (
	atomDraw cursorX h;
	sequenceDraw (add cursorX (duration h)) t;
      )
  in
  atomDraw start m;;


let mainDraw m scale startOffset= 
Graphics.open_graph (" " ^ (string_of_int ( startOffset +
int_of_float((float_of_int (duration m)) *. scale)
)) ^ "x200");
draw m scale startOffset;
Graphics.wait_next_event [Graphics.Key_pressed];;



(* Tests unitaires: *)


let exemple = Parallel ([Sequence ([Note(60,100,1000);Note(64,100,500);Note(62,100,500);Silence 1000;Note(67,100,1000)]);
             Sequence([Note(52,100,2000);Note(55,100,1000);Note(55,100,1000)])
            ]);;

let ex_sequence = Sequence ([Note(60,100,1000);Note(64,100,500);Note(62,100,500);Silence 1000;Note(67,100,1000)]);;

let ex_note = Note(50,100,2000);;

duration ex_sequence;;

copy exemple;;

note_count exemple;;

stretch exemple 0.5;;

beats 60 exemple;;
beats 120 exemple;;
beats 15 exemple;;

(* Exercice 2 *)

transpose exemple 12;;

retrograde exemple;;

mirror exemple 30;;

palindrome exemple;;

chordify exemple [0;3;6];;

chordifyList exemple [0;3;6];;

sequencify exemple 10;;

scrambleOnset exemple;;

scrambleAll exemple 5000;;


  (***** SHOW AND PLAY ******)
  mainDraw exemple 0.1 0;;
    mainDraw (transpose exemple 12) 0.1 0;;
    mainDraw (retrograde exemple) 0.1 0;;
    mainDraw (mirror exemple 80) 0.1 0;;


let save_as_midi m division fileName =
  let rec atom = function
    |Silence(d) -> []
    |Note(h,v,d) -> [(0,0,NoteON(h,v));(d,0,NoteOFF(h,v))]
    |Sequence(l) -> sequenceList l
    |Parallel(l) -> sequenceList l
    and sequenceList = function
      |[] -> []
      |h::t -> (atom h) @ sequenceList t
in
Midi.write (division, [(atom m)]) fileName;;

save_as_midi exemple 500 "song.mid";;
