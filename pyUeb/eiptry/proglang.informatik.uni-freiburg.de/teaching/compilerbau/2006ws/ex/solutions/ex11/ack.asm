        .data
$N:
        .asciiz "n = "
$M:
        .asciiz "m = "
        
        .text
main:   
        subu    $sp,$sp,32    # stack frame size is 32 bytes
        sw      $ra,20($sp)   # save return address
        sw      $fp,16($sp)   # save frame pointer
        addiu   $fp,$sp,28    # set up frame pointer
        # read input n
        la      $a0,$N
        li      $v0,4
        syscall               # print "n = "
        li      $v0,5
        syscall               # read integer into $v0
        move    $t0,$v0       # put n into $t0
        # read input m
        la      $a0,$M
        li      $v0,4
        syscall               # print "m = "
        li      $v0,5
        syscall               # read integer into $v0
        move    $a1,$v0       # put m into $a1
        # call ack
        move    $a0,$t0
        jal     ack
        # print the result
        move    $a0,$v0
        li      $v0,1
        syscall
        # print newline
        li      $a0,10
        li      $v0,11
        syscall
        # exit
        j       $EXIT
        
ack:
        subu    $sp,$sp,32    # stack frame size is 32 bytes
        sw      $ra,20($sp)   # save return address
        sw      $fp,16($sp)   # save frame pointer
        addiu   $fp,$sp,28    # set up frame pointer
        
        blez    $a0,$L1
        blez    $a1,$L2

        # case n > 0 and m > 0     
        # compute a(n, m-1)
        sw      $a0,0($fp)    # save 1th argument
        sw      $a1,-4($fp)    # save 2nd argument
        addiu   $a1,$a1,-1
        jal     ack
        # compute a(n-1,$v0)
        lw      $a0,0($fp)    # restore 1th argument
        addiu   $a0,$a0,-1
        move    $a1,$v0
        jal ack
        j       $EXIT         # exit
        
        
$L1:    # case n <= 0
        addiu   $v0,$a1,1
        j       $EXIT
        
$L2:    # case m <= 0
        # compute a(n-1,1)
        addiu   $a0,$a0,-1
        li      $a1,1
        jal ack
        j       $EXIT
        
$EXIT:    # epilogue
        lw      $ra,20($sp)   # restore return address
        lw      $fp,16($sp)   # restore frame
        addiu   $sp,$sp,32    # pop stack frame
        jr      $ra
