module Debug.Extra exposing
  ( log
  )

log : String -> a -> b -> b
log msg x =
  let
    _ = Debug.log msg x
  in
    identity
