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

(* TODO  *)
let format f instr = ""
