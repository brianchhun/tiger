type reg = string
type temp = Temp.temp
type label = Temp.label

type oper_instr = {
  assem: string;
  dst: temp list;
  src: temp list;
  jump: label list option
}

type label_instr = {
  assem: string;
  lab: label
}

type move_instr = {
  assem: string;
  dst: temp;
  src: temp
}

type instr = OPER of oper_instr
           | LABEL of label_instr
           | MOVE of move_instr

let reg_regexp = Str.regexp "'\\([ds]\\)\\([0-9]+\\)"

let format format_temp = function
    OPER {assem; _} when assem = "" -> assem
  | OPER {assem; dst; src; jump} ->
      let s = Str.global_substitute reg_regexp
          (fun s ->
             let temps =
               match Str.matched_group 1 s with "d" -> dst | "s" -> src in
             let i = int_of_string (Str.matched_group 2 s) in
               format_temp (List.nth temps i))
          assem in
        "\t" ^ s
  | LABEL {assem; lab} ->
      assem
  | MOVE {assem; dst; src} ->
      raise (Failure "not done yet")
