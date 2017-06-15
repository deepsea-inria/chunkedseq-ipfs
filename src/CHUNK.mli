module Simple : sig

type cbor = CBOR.Simple.t
                
type hash = string

type weight = int
    
type item = cbor
              
type chunk = cbor
               
type chunk_pointer = hash

val create : chunk

val push_back : chunk -> item -> chunk
                                   
val pop_back : chunk -> (chunk * item)

val ipfs_put_cbor : cbor -> hash

val ipfs_get_cbor : hash -> cbor

val ipfs_get_nb_bytes_of_cbor : hash -> int
                           
end
