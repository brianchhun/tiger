let emitproc oc = function
    Frame.PROC (body, frame) ->
      let instrs = List.concat (List.map (Codegen.codegen frame) 
                                  (body |> Canon.linearize |> Canon.basic_blocks |> Canon.trace_schedule)) in
      let (instrs', allocation) = Reg_alloc.alloc (Frame.proc_entry_exit2 frame instrs) frame in
      let (prologue, instrs'', epilogue) = Frame.proc_entry_exit3 frame instrs' in
      let string_of_temp t = Frame.string_of_register (Temp.Table.find t allocation) in
        output_string oc prologue;
        List.iter (fun i -> output_string oc (Assem.format string_of_temp i ^ "\n")) instrs'';
        output_string oc epilogue

  | Frame.STRING (label, s) ->
      output_string oc (Frame.string label s)

let _ =
  try
    Printexc.record_backtrace true;

    if (Array.length Sys.argv) < 2 then
      begin
        print_endline ("no input files");
        exit 1
      end;

    let filename = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in filename) in
    let absyn = Parser.program Lexer.token lexbuf in
      Parsing.clear_parser();
      Find_escape.find_escape absyn;
      Find_illegal_assign.find_illegal_assign absyn;
      let frags = Semant.trans_prog absyn in
      let oc = open_out (Filename.remove_extension (Filename.basename filename) ^ ".s") in
        List.iter (emitproc oc) frags;
        close_out oc
  with Parsing.Parse_error ->
    Error_msg.parse_error "syntax error"
