import Text.Printf
import Data.Bits

{-
 * Ported from the ocaml version of this file by the Rust Authors
 * It's almost a direct translation.

 * This module goes near the *bottom* of the dependency DAG, and holds basic
 * types shared across all phases of the compiler.
 -}

main = do { print "Hello" }

type Filename = String
data Pos = Pos Filename Int Int
         deriving (Show, Eq, Ord)

data Span = Span {
      lo :: Pos,
      hi :: Pos
    } deriving (Show, Eq, Ord)
          

type TyParamIdx = Int

data NodeId = Node Int
data TempId = Temp Int
data OpaqueId = Opaque Int
data ConstrId = Constr Int

int_of_node (Node i) = i
int_of_temp (Temp i) = i
int_of_opaque (Opaque i) = i
int_of_constr (Constr i) = i

data Identified a = Indentified { node :: a, id :: NodeId }


bug s = s


{-
exception Semant_err of ((node_id option) * string)
;;

let err (idopt:node_id option) =
  let k s =
    raise (Semant_err (idopt, s))
  in
    Printf.ksprintf k
;;
-}

{- Some ubiquitous low-level types. -}

data Target = Linux_x86_elf
            | Win32_x86_pe
            | MacOS_x86_macho
              deriving (Show, Eq, Ord)

data TyMach = TY_u8
            | TY_u16
            | TY_u32
            | TY_u64
            | TY_i8
            | TY_i16
            | TY_i32
            | TY_i64
            | TY_f32
            | TY_f64
              deriving (Show, Eq, Ord)

string_of_ty_mach tm = drop 3 $ show tm

mach_is_integral :: TyMach -> Bool
mach_is_integral m = m `elem` [ TY_i8, TY_i16, TY_i32, TY_i64
                              , TY_u8, TY_u16, TY_u32, TY_u64 ]

mach_is_signed m = m `elem` [TY_i8, TY_i16, TY_i32, TY_i64]

bytes_of_ty_mach :: TyMach -> Int
bytes_of_ty_mach m = case m of    
                       TY_u8 -> 1
                       TY_u16 -> 2
                       TY_u32 -> 4
                       TY_u64 -> 8
                       TY_i8 -> 1
                       TY_i16 -> 2
                       TY_i32 -> 4
                       TY_i64 -> 8
                       TY_f32 -> 4
                       TY_f64 -> 8

data NabiConv = CONV_rust
              | CONV_cdecl
                deriving (Show, Eq)

data Nabi = Nabi { 
      nabi_indirect :: Bool,
      nabi_convention :: NabiConv
    } deriving (Show, Eq)


string_to_conv s = case s of 
                     "cdecl" -> Just CONV_cdecl
                     "rust" -> Just CONV_rust
                     _ -> Nothing

{- FIXME: remove this when native items go away. -}
string_to_nabi s indirect = 
    case (string_to_conv s) of      
      Nothing -> Nothing
      (Just c) -> Just (Nabi indirect c)

data RequiredLibSpec = RequiredLibSpec {
      required_libname :: String,
      required_prefix :: Int
    } deriving (Show, Eq)


data RequiredLib = REQUIRED_LIB_rustrt
                 | REQUIRED_LIB_crt
                 | REQUIRED_LIB_rust RequiredLibSpec
                 | REQUIRED_LIB_c RequiredLibSpec
                   deriving (Show, Eq)

data Segment = SEG_text
             | SEG_data
               deriving (Show, Eq)

data Fixup = Fixup { 
      fixup_name :: String,
      fixup_file_pos :: Maybe Integer,
      fixup_file_sz :: Maybe Integer,
      fixup_mem_pos :: Maybe Integer,
      fixup_mem_sz :: Maybe Integer
    } deriving (Show, Eq)

new_fixup s = Fixup s Nothing Nothing Nothing Nothing



{-
 * Auxiliary hashtable functions.
 -}



{-

let htab_keys (htab:('a,'b) Hashtbl.t) : ('a list) =
  Hashtbl.fold (fun k _ accum -> k :: accum) htab []
;;


let sorted_htab_keys (tab:('a, 'b) Hashtbl.t) : 'a array =
  let keys = Array.of_list (htab_keys tab) in
    Array.sort compare keys;
    keys
;;

let htab_vals (htab:('a,'b) Hashtbl.t) : ('b list)  =
  Hashtbl.fold (fun _ v accum -> v :: accum) htab []
;;

let htab_pairs (htab:('a,'b) Hashtbl.t) : (('a * 'b) list) =
  Hashtbl.fold (fun k v accum -> (k,v) :: accum) htab []
;;

let htab_search (htab:('a,'b) Hashtbl.t) (k:'a) : ('b option) =
  if Hashtbl.mem htab k
  then Some (Hashtbl.find htab k)
  else None
;;

let htab_search_or_default
    (htab:('a,'b) Hashtbl.t)
    (k:'a)
    (def:unit -> 'b)
    : 'b =
  match htab_search htab k with
      Some v -> v
    | None -> def()
;;

let htab_search_or_add
    (htab:('a,'b) Hashtbl.t)
    (k:'a)
    (mk:unit -> 'b)
    : 'b =
  let def () =
    let v = mk() in
      Hashtbl.add htab k v;
      v
  in
    htab_search_or_default htab k def
;;

let htab_put (htab:('a,'b) Hashtbl.t) (a:'a) (b:'b) : unit =
  assert (not (Hashtbl.mem htab a));
  Hashtbl.add htab a b
;;

{- This is completely ridiculous, but it turns out that ocaml hashtables are
 * order-of-element-addition sensitive when it comes to the built-in
 * polymorphic comparison operator. So you have to canonicalize them after
 * you've stopped adding things to them if you ever want to use them in a
 * term that requires structural comparison to work. Sigh.
 -}

let htab_canonicalize (htab:('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  let n = Hashtbl.create (Hashtbl.length htab) in
    Array.iter
      (fun k -> Hashtbl.add n k (Hashtbl.find htab k))
      (sorted_htab_keys htab);
    n
;;

let htab_map
    (htab:('a,'b) Hashtbl.t)
    (f:'a -> 'b -> ('c * 'd))
    : (('c,'d) Hashtbl.t) =
  let ntab = Hashtbl.create (Hashtbl.length htab) in
  let g a b =
    let (c,d) = f a b in
      htab_put ntab c d
  in
    Hashtbl.iter g htab;
    htab_canonicalize (ntab)
;;

let htab_fold
    (fn:'a -> 'b -> 'c -> 'c)
    (init:'c)
    (h:('a, 'b) Hashtbl.t) : 'c =
  let accum = ref init in
  let f a b = accum := (fn a b (!accum)) in
    Hashtbl.iter f h;
    !accum
;;


let reduce_hash_to_list
    (fn:'a -> 'b -> 'c)
    (h:('a, 'b) Hashtbl.t)
    : ('c list) =
  htab_fold (fun a b ls -> (fn a b) :: ls) [] h
;;

{- 
 * Auxiliary association-array and association-list operations.
 -}
let atab_search (atab:('a * 'b) array) (a:'a) : ('b option) =
  let lim = Array.length atab in
  let rec step i =
    if i = lim
    then None
    else
      let (k,v) = atab.(i) in
        if k = a
        then Some v
        else step (i+1)
  in
    step 0

let atab_find (atab:('a * 'b) array) (a:'a) : 'b =
  match atab_search atab a with
      None -> bug () "atab_find: element not found"
    | Some b -> b

let atab_mem (atab:('a * 'b) array) (a:'a) : bool =
  match atab_search atab a with
      None -> false
    | Some _ -> true

let rec ltab_search (ltab:('a * 'b) list) (a:'a) : ('b option) =
  match ltab with
      [] -> None
    | (k,v)::_ when k = a -> Some v
    | _::lz -> ltab_search lz a

let ltab_put (ltab:('a * 'b) list) (a:'a) (b:'b) : (('a * 'b) list) =
  assert ((ltab_search ltab a) = None);
  (a,b)::ltab

{-
 * Auxiliary list functions.
 -}

let rec list_search (list:'a list) (f:'a -> 'b option) : ('b option) =
  match list with
      [] -> None
    | a::az ->
        match f a with
            Some b -> Some b
          | None -> list_search az f

let rec list_search_ctxt
    (list:'a list)
    (f:'a -> 'b option)
    : ((('a list) * 'b) option) =
  match list with
      [] -> None
    | a::az ->
        match f a with
            Some b -> Some (list, b)
          | None -> list_search_ctxt az f

let rec list_drop n ls =
  if n = 0
  then ls
  else list_drop (n-1) (List.tl ls)
;;


{-
 * Auxiliary option functions.
 -}

let bool_of_option x =
  match x with
      Some _ -> true
    | None -> false

{-
 * Auxiliary stack functions.
 -}

let stk_fold (s:'a Stack.t) (f:'a -> 'b -> 'b) (x:'b) : 'b =
  let r = ref x in
    Stack.iter (fun e -> r := f e (!r)) s;
    !r

let stk_elts_from_bot (s:'a Stack.t) : ('a list) =
  stk_fold s (fun x y -> x::y) []

let stk_elts_from_top (s:'a Stack.t) : ('a list) =
  List.rev (stk_elts_from_bot s)

let stk_search (s:'a Stack.t) (f:'a -> 'b option) : 'b option =
  stk_fold s (fun e accum -> match accum with None -> (f e) | x -> x) None


{-
 * Auxiliary array functions.
 -}

let arr_search (a:'a array) (f:int -> 'a -> 'b option) : 'b option =
  let max = Array.length a in
  let rec iter i =
    if i < max
    then
      let v = a.(i) in
      let r = f i v in
        match r with
            Some _ -> r
          | None -> iter (i+1)
    else
      None
  in
    iter 0
;;

let arr_idx (arr:'a array) (a:'a) : int =
  let find i v = if v = a then Some i else None in
    match arr_search arr find with
        None -> bug () "arr_idx: element not found"
      | Some i -> i
;;

let arr_map_partial (a:'a array) (f:'a -> 'b option) : 'b array =
  let accum a ls =
    match f a with
        None -> ls
      | Some b -> b :: ls
  in
    Array.of_list (Array.fold_right accum a [])
;;

let arr_filter_some (a:'a option array) : 'a array =
  arr_map_partial a (fun x -> x)
;;

let arr_find_dups (a:'a array) : ('a * 'a) option =
  let copy = Array.copy a in
    Array.sort compare copy;
    let lasti = (Array.length copy) - 1 in
    let rec find_dups i =
      if i < lasti then
        let this = copy.(i) in
        let next = copy.(i+1) in
          (if (this = next) then
             Some (this, next)
           else
             find_dups (i+1))
      else
        None
    in
      find_dups 0
;;

let arr_check_dups (a:'a array) (f:'a -> 'a -> unit) : unit =
  match arr_find_dups a with
      Some (x, y) -> f x y
    | None -> ()
;;

let arr_map2 (f:'a -> 'b -> 'c) (a:'a array) (b:'b array) : 'c array =
  assert ((Array.length a) = (Array.length b));
  Array.init (Array.length a) (fun i -> f a.(i) b.(i))
;;

let arr_iter2 (f:'a -> 'b -> unit) (a:'a array) (b:'b array) : unit =
  assert ((Array.length a) = (Array.length b));
  Array.iteri (fun i a_elem -> f a_elem b.(i)) a
;;

let arr_for_all (f:int -> 'a -> bool) (a:'a array) : bool =
  let len = Array.length a in
  let rec loop i =
    (i >= len) || ((f i a.(i)) && (loop (i+1)))
  in
    loop 0
;;

let arr_exists (f:int -> 'a -> bool) (a:'a array) : bool =
  let len = Array.length a in
  let rec loop i =
    (i < len) && ((f i a.(i)) || (loop (i+1)))
  in
    loop 0
;;

{- 
 * Auxiliary queue functions. 
 -}

let queue_to_list (q:'a Queue.t) : 'a list =
  List.rev (Queue.fold (fun ls elt -> elt :: ls)  []  q)
;;

let queue_to_arr (q:'a Queue.t) : 'a array =
  Array.init (Queue.length q) (fun _ -> Queue.take q)
;;

{-
 * Auxiliary int64 functions
 -}

let i64_lt (a:int64) (b:int64) : bool = (Int64.compare a b) < 0
let i64_le (a:int64) (b:int64) : bool = (Int64.compare a b) <= 0
let i64_ge (a:int64) (b:int64) : bool = (Int64.compare a b) >= 0
let i64_gt (a:int64) (b:int64) : bool = (Int64.compare a b) > 0
let i64_max (a:int64) (b:int64) : int64 =
  (if (Int64.compare a b) > 0 then a else b)
let i64_min (a:int64) (b:int64) : int64 =
  (if (Int64.compare a b) < 0 then a else b)
-}
i64_align :: Integer -> Integer -> Integer -- ????
i64_align 0 v = (-666) -- ???
i64_align align v =       
    (complement mask) .&. (v + mask)
    --  Int64.logand (Int64.lognot mask) (Int64.add v mask)
    where 
      mask = align - 1
         
;;
{-
let rec i64_for (lo:int64) (hi:int64) (thunk:int64 -> unit) : unit =
  if i64_lt lo hi then
    begin
      thunk lo;
      i64_for (Int64.add lo 1L) hi thunk;
    end
;;

let rec i64_for_rev (hi:int64) (lo:int64) (thunk:int64 -> unit) : unit =
  if i64_ge hi lo then
    begin
      thunk hi;
      i64_for_rev (Int64.sub hi 1L) lo thunk;
    end
;;


{-
 * Auxiliary int32 functions
 -}

let i32_lt (a:int32) (b:int32) : bool = (Int32.compare a b) < 0
let i32_le (a:int32) (b:int32) : bool = (Int32.compare a b) <= 0
let i32_ge (a:int32) (b:int32) : bool = (Int32.compare a b) >= 0
let i32_gt (a:int32) (b:int32) : bool = (Int32.compare a b) > 0
let i32_max (a:int32) (b:int32) : int32 =
  (if (Int32.compare a b) > 0 then a else b)
let i32_min (a:int32) (b:int32) : int32 =
  (if (Int32.compare a b) < 0 then a else b)
let i32_align (align:int32) (v:int32) : int32 =
  (assert (align <> 0l));
  let mask = Int32.sub align 1l in
    Int32.logand (Int32.lognot mask) (Int32.add v mask)
;;

{-
 * Int-as-unichar functions.
 -}

let bounds lo c hi = (lo <= c) && (c <= hi)
;;

let escaped_char i =
  if bounds 0 i 0x7f
  then Char.escaped (Char.chr i)
  else
    if bounds 0 i 0xffff
    then Printf.sprintf "\\u%4.4X" i
    else Printf.sprintf "\\U%8.8X" i
;;

let char_as_utf8 i =
  let buf = Buffer.create 8 in
  let addb i =
    Buffer.add_char buf (Char.chr (i land 0xff))
  in
  let fini _ =
    Buffer.contents buf
  in
  let rec add_trailing_bytes n i =
    if n = 0
    then fini()
    else
      begin
        addb (0b1000_0000 lor ((i lsr ((n-1) * 6)) land 0b11_1111));
        add_trailing_bytes (n-1) i
      end
  in
    if bounds 0 i 0x7f
    then (addb i; fini())
    else
      if bounds 0x80 i 0x7ff
      then (addb ((0b1100_0000) lor (i lsr 6));
            add_trailing_bytes 1 i)
      else
        if bounds 0x800 i 0xffff
        then (addb ((0b1110_0000) lor (i lsr 12));
              add_trailing_bytes 2 i)
        else
          if bounds 0x1000 i 0x1f_ffff
          then (addb ((0b1111_0000) lor (i lsr 18));
                add_trailing_bytes 3 i)
          else
            if bounds 0x20_0000 i 0x3ff_ffff
            then (addb ((0b1111_1000) lor (i lsr 24));
                  add_trailing_bytes 4 i)
            else
              if bounds 0x400_0000 i 0x7fff_ffff
              then (addb ((0b1111_1100) lor (i lsr 30));
                    add_trailing_bytes 5 i)
              else bug () "bad unicode character 0x%X" i
;;
-}


{-
 * Size-expressions.
 -}


data Size = SIZE_fixed Integer
          | SIZE_fixup_mem_sz Fixup
          | SIZE_fixup_mem_pos Fixup
          | SIZE_param_size TyParamIdx
          | SIZE_param_align TyParamIdx
          | SIZE_rt_neg Size
          | SIZE_rt_add Size Size
          | SIZE_rt_mul Size Size
          | SIZE_rt_max Size Size
          | SIZE_rt_align Size Size
          | SizeUnknown -- added this for handling undefined cases
            deriving (Show, Eq)

string_of_size s =
    case s of
      SIZE_fixed i -> "L" ++ show i
      SIZE_fixup_mem_sz f -> show $ fixup_name f ++ "mem_sz" 
      SIZE_fixup_mem_pos f -> show $ fixup_name f ++ "mem_pos"
      SIZE_param_size i -> "ty" ++ show i ++ ".size" 
      SIZE_param_align i -> "ty" ++ show i ++ ".align"
      SIZE_rt_neg a -> "-(" ++ string_of_size a ++ ")"
      (SIZE_rt_add a b) -> printf "(%s + %s)" 
                           (string_of_size a) (string_of_size b)
      (SIZE_rt_mul a b) -> printf "(%s * %s)" 
                           (string_of_size a) (string_of_size b)
      (SIZE_rt_max a b) -> printf "max(%s,%s)" 
                           (string_of_size a) (string_of_size b)
      (SIZE_rt_align align off) -> printf "align(%s,%s)"
                           (string_of_size align) (string_of_size off)

neg_sz :: Size -> Size
neg_sz a = case a of           
             SIZE_fixed a -> SIZE_fixed $ negate a
             _ -> SIZE_rt_neg a

add_sz :: Size -> Size -> Size
add_sz a b =
    case (a, b) of
      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (a + b)
      ((SIZE_rt_add (SIZE_fixed a) c), SIZE_fixed b) -> SIZE_rt_add (SIZE_fixed (a + b)) c
      ((SIZE_rt_add c (SIZE_fixed a)), SIZE_fixed b) -> SIZE_rt_add (SIZE_fixed (a + b)) c
      (SIZE_fixed a, (SIZE_rt_add (SIZE_fixed b) c)) -> SIZE_rt_add (SIZE_fixed (a + b)) c
      (SIZE_fixed a, (SIZE_rt_add c (SIZE_fixed b))) -> SIZE_rt_add (SIZE_fixed (a + b)) c
      (SIZE_fixed 0, b) -> b
      (a, SIZE_fixed 0) -> a
      (a, SIZE_fixed b) -> SIZE_rt_add (SIZE_fixed b) a
      (a, b) -> SIZE_rt_add a b

mul_sz :: Size -> Size -> Size
mul_sz a b =
    case (a, b) of
      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (a * b)
      (a, SIZE_fixed b) -> SIZE_rt_mul (SIZE_fixed b) a
      (a, b) -> SIZE_rt_mul a b

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

max_sz :: Size -> Size -> Size
max_sz a b =
    case (a, b) of 
      (SIZE_rt_align _ _, SIZE_fixed 1) -> a
      (SIZE_fixed 1, SIZE_rt_align _ _) -> b
      (SIZE_param_align _, SIZE_fixed 1) -> a
      (SIZE_fixed 1, SIZE_param_align _) -> b
      (a, SIZE_rt_max b c) | (a == b) -> max_sz a c
                           | (a == c) -> max_sz a b    

      (SIZE_rt_max b c, a) | (a == b) -> max_sz a c
                           | (a == c) -> max_sz a b

      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (max a b)
      (SIZE_fixed 0, b) | (no_negs b) -> b

      (a, SIZE_fixed 0) | no_negs a -> b
      (a, SIZE_fixed b) -> max_sz (SIZE_fixed b) a
      (a, b) | a == b -> a
      (a, b) -> SIZE_rt_max a b

    where 
      no_negs x =
          case x of 
            SIZE_fixed _ -> True
            SIZE_fixup_mem_sz _ -> True
            SIZE_fixup_mem_pos _ -> True
            SIZE_param_size _ -> True
            SIZE_param_align _ -> True
            SIZE_rt_neg _ -> False

            SIZE_rt_add a b -> (no_negs a) && (no_negs b)
            SIZE_rt_mul a b -> (no_negs a) && (no_negs b)
            SIZE_rt_max a b -> (no_negs a) && (no_negs b)
            SIZE_rt_align a b -> (no_negs a) && (no_negs b)



{- FIXME: audit this carefuly; I am not terribly certain of the
 * algebraic simplification going on here. Sadly, without it
 * the diagnostic output from translation becomes completely
 * illegible.
 -}

align_sz :: Size -> Size -> Size
align_sz a b = 
    case (a, b) of
      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (i64_align a b)
      (SIZE_fixed x, _) | x < 1 -> SizeUnknown --bug () "alignment less than 1"
      (SIZE_fixed 1, b) -> b {- everything is 1-aligned. -}
      (_, SIZE_fixed 0) -> b {- 0 is everything-aligned. -}
      (SIZE_fixed a, b) ->
          let inner_alignment = alignment_of b in
          if (a `rem` inner_alignment) == 0
          then b
          else SIZE_rt_align (SIZE_fixed a) b
      (SIZE_rt_max a (SIZE_fixed 1), b) -> SIZE_rt_align a b
      (SIZE_rt_max (SIZE_fixed 1) a, b) -> SIZE_rt_align a b
      (a, b) -> SIZE_rt_align a b

    where 
      alignment_of s =
          case s of
            SIZE_rt_align (SIZE_fixed n) s ->
                let inner_alignment = alignment_of s in
                if (n `rem` inner_alignment) == 0
                then inner_alignment
                else n

            SIZE_rt_add (SIZE_fixed n) s -> 
                let inner_alignment = alignment_of s in
                if (n `rem` inner_alignment) == 0
                then inner_alignment
                else 1 {- This could be lcd(...) or such. -}

            SIZE_rt_add s (SIZE_fixed n) ->
                let inner_alignment = alignment_of s in
                if (n `rem` inner_alignment) == 0
                then inner_alignment
                else 1 {- This could be lcd(...) or such. -}

            SIZE_rt_max a (SIZE_fixed 1) -> (alignment_of a)
            SIZE_rt_max (SIZE_fixed 1) b -> alignment_of b
            _ -> 1


force_sz :: Size -> Integer
force_sz a =
    case a of
      SIZE_fixed i -> i
    --_ -> bug $ printf "force_sz: forced non-fixed size expression %s" (string_of_size a)

{- HEAD                                 
-}