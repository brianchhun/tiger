.text
L10:
	subu $sp, $sp, 48
	sw $fp, 40($sp)
	addiu $fp, $sp, 44
L15:
	sw $s0, -8($fp)
	sw $s1, -12($fp)
	sw $s2, -16($fp)
	sw $s3, -20($fp)
	sw $s4, -24($fp)
	sw $s5, -28($fp)
	sw $s6, -32($fp)
	sw $s7, -36($fp)
	sw $ra, -40($fp)
	sw $a0, 0($fp)
	move $a3, $a1
	li $a0, 0
	beq $a3, $a0, L11
L12:
	lw $a2, 0($fp)
	move $a0, $a2
	li $a0, 1
	sub $a0, $a3, $a0
	move $a1, $a0
	jal L10
	move $a0, $v0
	mulo $a0, $a3, $a0
	move $a0, $a0
L13:
	move $v0, $a0
	lw $a0, -8($fp)
	move $s0, $a0
	lw $a0, -12($fp)
	move $s1, $a0
	lw $a0, -16($fp)
	move $s2, $a0
	lw $a0, -20($fp)
	move $s3, $a0
	lw $a0, -24($fp)
	move $s4, $a0
	lw $a0, -28($fp)
	move $s5, $a0
	lw $a0, -32($fp)
	move $s6, $a0
	lw $a0, -36($fp)
	move $s7, $a0
	lw $a0, -40($fp)
	move $ra, $a0
	j L14
L11:
	li $a0, 1
	move $a0, $a0
	j L13
L14:
	
	lw $fp, 40($sp)
	addiu $sp, $sp, 48
	jr $ra
.text
t_main:
	subu $sp, $sp, 48
	sw $fp, 40($sp)
	addiu $fp, $sp, 44
L17:
	sw $s0, -8($fp)
	sw $s1, -12($fp)
	sw $s2, -16($fp)
	sw $s3, -20($fp)
	sw $s4, -24($fp)
	sw $s5, -28($fp)
	sw $s6, -32($fp)
	sw $s7, -36($fp)
	sw $ra, -40($fp)
	sw $a0, 0($fp)
	move $a0, $fp
	li $a0, 9
	move $a1, $a0
	jal L10
	move $v0, $v0
	lw $a0, -8($fp)
	move $s0, $a0
	lw $a0, -12($fp)
	move $s1, $a0
	lw $a0, -16($fp)
	move $s2, $a0
	lw $a0, -20($fp)
	move $s3, $a0
	lw $a0, -24($fp)
	move $s4, $a0
	lw $a0, -28($fp)
	move $s5, $a0
	lw $a0, -32($fp)
	move $s6, $a0
	lw $a0, -36($fp)
	move $s7, $a0
	lw $a0, -40($fp)
	move $ra, $a0
	j L16
L16:
	
	lw $fp, 40($sp)
	addiu $sp, $sp, 48
	jr $ra
