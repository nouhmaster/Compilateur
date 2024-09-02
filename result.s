.text
.globl main
test:
  addi $sp, $sp, -16
  sw $ra, 12($sp)
  sw $fp, 8($sp)
  addi $fp, $sp, 12
  li $v0, 20
  sw $v0, -8($fp)
  li $v0, 10
  sw $v0, -12($fp)
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _lt
  addi $sp, $sp, 8
  beqz $v0, else1
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif1
else1:
  la $v0, str_2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif1:
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _gt
  addi $sp, $sp, 8
  beqz $v0, else2
  la $v0, str_3
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif2
else2:
  la $v0, str_4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif2:
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _lte
  addi $sp, $sp, 8
  beqz $v0, else3
  la $v0, str_5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif3
else3:
  la $v0, str_6
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif3:
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _gt
  addi $sp, $sp, 8
  beqz $v0, else4
  la $v0, str_7
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif4
else4:
  la $v0, str_8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif4:
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _gte
  addi $sp, $sp, 8
  beqz $v0, else5
  la $v0, str_9
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif5
else5:
  la $v0, str_10
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif5:
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _eq
  addi $sp, $sp, 8
  beqz $v0, else6
  la $v0, str_11
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif6
else6:
  la $v0, str_12
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif6:
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _neq
  addi $sp, $sp, 8
  beqz $v0, else7
  la $v0, str_13
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif7
else7:
  la $v0, str_14
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif7:
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 10
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _eq
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 10
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _eq
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _or
  addi $sp, $sp, 8
  beqz $v0, else8
  la $v0, str_15
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif8
else8:
  la $v0, str_16
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif8:
  lw $v0, -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 10
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _eq
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0, -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 10
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _eq
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _and
  addi $sp, $sp, 8
  beqz $v0, else9
  la $v0, str_17
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  b endif9
else9:
  la $v0, str_18
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
endif9:
ret0:
  addi $sp, $sp, 16
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra
main:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 0($sp)
  addi $fp, $sp, 4
  jal test
  addi $sp, $sp, 0
ret10:
  addi $sp, $sp, 8
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra
_add:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  add $v0, $t0, $t1
  jr $ra
_sub:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  sub $v0, $t1, $t0
  jr $ra
_mul:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  mul $v0, $t0, $t1
  jr $ra
_div:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  div $v0, $t1, $t0
  jr $ra
_mod:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  div $v0, $t1, $t0
  mfhi $v0
  jr $ra
puti:
  lw $a0, 0($sp)
  li $v0, 1
  syscall
  jr $ra
putnl:
  la $a0, nl
  li $v0, 4
  syscall
  jr $ra
geti:
  lw $a0, 0($sp)
  li $v0, 5
  syscall
  jr $ra
puts:
  move $a0, $v0
  li $v0, 4
  syscall
  jr $ra
putb:
  move $a0, $v0
  li $v0, 1
  syscall
  jr $ra
_gt:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sgt  $v0, $t0, $t1
  jr $ra
_gte:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sge  $v0, $t0, $t1
  jr $ra
_lt:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  slt  $v0, $t0, $t1
  jr $ra
_lte:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sle  $v0, $t0, $t1
  jr $ra
_eq:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  seq  $v0, $t0, $t1
  jr $ra
_neq:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  sne  $v0, $t0, $t1
  jr $ra
_and:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  and  $v0, $t0, $t1
  jr $ra
_or:
  lw $t0, 4($sp)
  lw $t1, 0($sp)
  or   $v0, $t0, $t1
  jr $ra

.data
nl: .asciiz "\n" 
str_1: .asciiz " x plus petit que y
"
str_2: .asciiz " x plus grand que y
"
str_3: .asciiz " x plus grand que y
"
str_4: .asciiz " x plus petit que y
"
str_5: .asciiz " x plus petit ou egal à y
"
str_6: .asciiz " x plus grand ou à y
"
str_7: .asciiz " x plus grand ou egal à y
"
str_8: .asciiz " x plus petit ou égal à y
"
str_9: .asciiz " x plus grand ou egal à y
"
str_10: .asciiz " x plus petit ou égal à y
"
str_11: .asciiz " x egal à y
"
str_12: .asciiz " x pas égal à y
"
str_13: .asciiz " x different de y
"
str_14: .asciiz " x est egal à y
"
str_15: .asciiz "y est egal a 10 ou x == 10
"
str_16: .asciiz "aucun des deux n'es egal a 10
"
str_17: .asciiz "y est egal a 10 et x == 10
"
str_18: .asciiz "aucun des deux ou un des deux n'es egal a 10
"
