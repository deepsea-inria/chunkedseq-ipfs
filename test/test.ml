let f0 = CHUNK.Simple.create
let f1 = CHUNK.Simple.push_back f0 (`Int 1234)
let f2 = CHUNK.Simple.push_back f1 (`Int 3322)

(*
to build:
 oasis setup -setup-update dynamic && ./configure --enable-tests && make clean && make  
 *)
                                
let hash = CHUNK.Simple.ipfs_put_cbor f2

let _ = Printf.printf "hash= %s\n" hash

let f3 = CHUNK.Simple.ipfs_get_cbor hash

let _ = Printf.printf "f2 = %s\n" (CBOR.Simple.to_diagnostic f2)
let _ = Printf.printf "f3 = %s\n" (CBOR.Simple.to_diagnostic f3)
