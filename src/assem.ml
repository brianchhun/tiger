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

let placeholder_regexp = Str.regexp "'\\([dsj]\\)\\([0-9]+\\)"

let format format_temp instr =
  let fill_placeholders assem src dst jump =
    Str.global_substitute placeholder_regexp
      (fun placeholder ->
         let kind = Str.matched_group 1 placeholder in
         let i = int_of_string (Str.matched_group 2 placeholder) in
           match kind with
             "s" -> format_temp (List.nth src i)
           | "d" -> format_temp (List.nth dst i)
           | "j" -> Temp.string_of_label (List.nth jump i))
      assem in
    match instr with
      OPER {assem; src; dst; jump} ->
        "\t" ^ fill_placeholders assem src dst (Option.default [] jump)
    | LABEL {assem; _} ->
        assem
    | MOVE {assem; src; dst} ->
        "\t" ^ fill_placeholders assem [src] [dst] []
