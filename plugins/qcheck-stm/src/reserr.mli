type error
type 'a reserr

val ok : 'a -> 'a reserr
val error : error -> 'a reserr
val ( let* ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( and* ) : 'a reserr -> 'b reserr -> ('a * 'b) reserr
