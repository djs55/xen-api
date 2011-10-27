
type value =
	| String of string
	| Int of int
	| List of value list
with rpc

type config = (string * value) list with rpc
