module Simple : sig

type cbor = CBOR.Simple.t
                
type hash = string

type weight = int
    
type item = cbor
              
type chunk = cbor
               
type chunk_pointer = hash

val mk_item : int -> item

val mk_link : hash -> cbor

val create : chunk

val push_back : chunk -> item -> chunk

val push_front : chunk -> item -> chunk
                                   
val pop_back : chunk -> (chunk * item)
                                   
val pop_front : chunk -> (chunk * item)

val concat : (chunk * chunk) -> chunk

val split : (chunk * weight) -> (chunk * item * chunk)

val ipfs_put_cbor : cbor -> hash

val ipfs_get_cbor : hash -> cbor
                           
end
