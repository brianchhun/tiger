{
  open Parser
  exception Eof
}
  rule token = parse
         "if"  { IF }
       | "\n"  { token lexbuf }
       | eof   { raise Eof }