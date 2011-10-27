{
    open Xn_cfg_parser
}
rule token = parse
	  ['a'-'z']['_''0'-'9''a'-'z']* as x { IDENT x }
	| ['0'-'9']['0'-'9''a'-'f''x']* as x { INT (int_of_string x) }
    | '\''([^'\'''\n']|'.')*'\'' as x { STRING x }
    | '"'([^'"''\n']|'.')*'"' as x { STRING x }
	| ','                  { COMMA }
	| '['                  { LBRACKET }
	| ']'                  { RBRACKET }
	| '='                  { EQ }
	| ';'                  { SEMICOLON }
	| '\n'
	| '#'[^'\n']*'\n'      { NEWLINE }
	| [' ' '\t' ]          { token lexbuf }
	| eof                  { EOF }
