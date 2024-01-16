        .data
prompt:
        .asciiz "Enter your name: "
hello:  
        .asciiz "Hello "
          
        .text
main:   
        la $a0, prompt
        li $v0, 4
        syscall       # print prompt

        li $a0, 64
        li $v0, 9    
        syscall       # allocate 64 bytes, address now in v0
                
        move $a0, $v0
        li $a1, 64
        li $v0, 8    
        syscall       # read string into address $a0
        move $t0, $a0 # save it into $t0
        
        la $a0, hello                      
        li $v0, 4    
        syscall       # print hello

        move $a0, $t0
        syscall       # print the string read from stdin
        
        li $v0, 10   
        syscall       # exit