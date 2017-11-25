open Core

type 'a t = Retain of int | Insert of 'a | Delete
