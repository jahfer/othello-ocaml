type 'a instruction = Identity | Tail | Append of 'a | Swap of 'a

val step : 'a list -> 'a instruction -> 'a list
