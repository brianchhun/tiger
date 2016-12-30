let string_of_temp t =
  try
    Temp.Table.find t Frame.temp_map
  with
    Not_found -> Temp.string_of_temp t
                   
let emitproc = function
    Frame.PROC (body, frame) ->
      let stms = body |>
                 Canon.linearize |>
                 Canon.basic_blocks |>
                 Canon.trace_schedule in
      let (prologue, instrs, epilogue) = List.concat (List.map (Codegen.codegen frame) stms) |>
                                       Frame.proc_entry_exit2 frame |>
                                       Frame.proc_entry_exit3 frame in
        print_string prologue;
        List.iter (fun i -> print_endline (Assem.format string_of_temp i)) instrs;
        print_string epilogue


  | Frame.STRING (label, s) ->
      print_string (Frame.string label s)

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let absyn = Parser.program Lexer.token lexbuf in
      Parsing.clear_parser();
      Find_escape.find_escape absyn;
      let frags = Semant.trans_prog absyn in
        List.iter emitproc frags
  with Parsing.Parse_error ->
    Error_msg.parse_error "syntax error"
