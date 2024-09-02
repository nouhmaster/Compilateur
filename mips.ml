(*Ecrit en MIPS le langage c*)

type reg =
  | Zero
  | SP
  | RA
  | FP
  | V0  
  | A0
  | A1
  | T0
  | T1
  | T5

type label = string

type loc =
  | Lbl of label
  | Mem of reg * int

type instr =
  | Label of label
  | Li    of reg * int
  | La    of reg * loc
  | Sw    of reg * loc
  | Lw    of reg * loc
  | Sb    of reg * loc
  | Lb    of reg * loc
  | Move  of reg * reg

  | Addi  of reg * reg * int
  | Add   of reg * reg * reg
  | Sub   of reg * reg * reg
  | Mul   of reg * reg * reg
  | Div   of reg * reg * reg

  | Seq   of reg * reg * reg
  | Sne   of reg * reg * reg
  | Slt   of reg * reg * reg
  | Sgt   of reg * reg * reg
  | Sle   of reg * reg * reg
  | Sge   of reg * reg * reg
  | And   of reg * reg * reg
  | Or    of reg * reg * reg
  | Xor   of reg * reg * reg
  | Sll   of reg * reg * reg
  | Srl   of reg * reg * reg
  | Bnez  of reg * label
  | Beqz  of reg * label
  
  | Syscall
  | B     of label
  | Jal   of label
  | Mfhi  of reg
  | Jr    of reg

type directive =
  | Asciiz of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int  = 5
  let read_str  = 8
  let sbrk      = 9
end


let ps = Printf.sprintf (* alias raccourci *)


let fmt_reg = function
  | Zero -> "$zero"
  | SP   -> "$sp"
  | FP   -> "$fp"
  | RA   -> "$ra"
  | V0   -> "$v0"
  | A0   -> "$a0"
  | A1   -> "$a1"
  | T0   -> "$t0"
  | T1   -> "$t1"
  | T5   -> "$t5"

let fmt_loc = function
  | Lbl (l)    -> l
  | Mem (r, o) -> ps "%d(%s)" o (fmt_reg r)

let fmt_instr = function
  | Label (l)        -> ps "%s:" l
  | Li (r, i)        -> ps "  li %s, %d" (fmt_reg r) i
  | La (r, a)        -> ps "  la %s, %s" (fmt_reg r) (fmt_loc a)
  | Sw (r, a)        -> ps "  sw %s, %s" (fmt_reg r) (fmt_loc a)
  | Lw (r, a)        -> ps "  lw %s, %s" (fmt_reg r) (fmt_loc a)
  | Sb (r, a)        -> ps "  sb %s, %s" (fmt_reg r) (fmt_loc a)
  | Lb (r, a)        -> ps "  lb %s, %s" (fmt_reg r) (fmt_loc a)
  | Move (rd, rs)    -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Addi (rd, rs, i) -> ps "  addi %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Add (rd, rs, rt) -> ps "  add %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt) -> ps "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt) -> ps "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Seq  (d, r1, r2) -> ps "  seq  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Sne  (d, r1, r2) -> ps "  sne  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Slt  (d, r1, r2) -> ps "  slt  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Sgt  (d, r1, r2) -> ps "  sgt  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Sle  (d, r1, r2) -> ps "  sle  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Sge  (d, r1, r2) -> ps "  sge  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | And  (d, r1, r2) -> ps "  and  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Or   (d, r1, r2) -> ps "  or   %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Sll  (d, r1, r2) -> ps "  sll  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Srl  (d, r1, r2) -> ps "  srl  %s, %s, %s" (fmt_reg d) (fmt_reg r1) (fmt_reg r2)
  | Bnez (r, l)      -> ps "  bnez %s, %s"     (fmt_reg r) l
  | Syscall          -> ps "  syscall"
  | B (l)            -> ps "  b %s" l
  | Mfhi (d)         -> ps "  mfhi %s" (fmt_reg d)
  | Beqz (r, l)      -> ps "  beqz %s, %s" (fmt_reg r) l
  | Jal (l)          -> ps "  jal %s" l
  | Jr (r)           -> ps "  jr %s" (fmt_reg r)

let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s


let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\n";
  (*List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.label ;*)
  (*Printf.fprintf oc "\nmain:\n";*)
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  Printf.fprintf oc "\n.data\n" ;
  Printf.fprintf oc "nl: .asciiz \"\\n\" \n";
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
