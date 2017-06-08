

module Chunk =
  struct

    (* Chunk representation

       A chunk is represented by a pair consisting of a weight, w, and
       a payload of items, xs.

         Map [(Int w, Array xs)]

       An item is represented as the disjoint union between a
       leaf-level item, v,

         Int v

       and a chunk-pointer value, p, represented by a valid IPFS
       hash-code link.

         Bytes p

       The binary representation of p is specified in the IPLD
       specification.

         https://github.com/ipld/specs/tree/master/ipld


       IPFS NOTES

       echo "���C" | ipfs dag put --input-enc raw
       zdpuAxKCBsAKQpEw456S49oVDkWJ9PZa44KGRfVBWHiXN3UH8 

     *)

    type weight = int
    
    type item = CBOR.Simple.t
          
    type chunk = CBOR.Simple.t

    type chunk_pointer = string

    let contents_of_chunk c =
      match c with
      | `Map [(`Int w, `Array xs)] ->
          (w, xs)
      | _ -> assert false

    let mk_chunk w xs =
      `Map [(`Int w, `Array xs)]

    let weight_of_chunk : chunk_pointer -> weight = fun p ->
      failwith "todo"

    let weight_of_item x =
      match x with
      | `Int _ -> 1
      | `Bytes p -> weight_of_chunk p
      | _ -> assert false
            
    let create : chunk =
      mk_chunk 0 []

    let push_back c x =
      let (w, xs) = contents_of_chunk c in
      let w' = w + weight_of_item x in
      let xs' = List.append xs [x] in
      mk_chunk w' xs'
        
    let push_front c x =
      let (w, xs) = contents_of_chunk c in
      let w' = w + weight_of_item x in
      let xs' = x :: xs in
      mk_chunk w' xs'
        
    let pop_back c =
      let (w, xs) = contents_of_chunk c in
      let sx = List.rev xs in
      match sx with
      | x :: sx' ->
          let w' = w - weight_of_item x in
          let xs' = List.rev sx' in
          (mk_chunk w' xs', x)
      | _ -> assert false
            
    let pop_front c =
      let (w, xs) = contents_of_chunk c in
      match xs with
      | x :: xs' ->
          let w' = w - weight_of_item x in
          mk_chunk w' xs'
      | _ -> assert false

        
  end

    let _ = Printf.printf "hi\n"
