.text
.type fun, @function
.global fun

# a jest w %rdi
# b jest w %rsi
# c jest w %rdx
# d jest w %rcx

fun:
    mov %rdi, %rax
    push %rax
    
loop:
    cmp $0, (%rsi)
    je ifendloop
    mov (%rsi), %rcx
    mov %rcx,(%rax)
    inc %rax
    inc %rsi
rsiIsZeroAndRdxNot:
    cmp $0, (%rdx)
    je ifendloop1
    mov (%rdx),%rcx
    mov %rcx,(%rax)
    inc %rcx
    inc %rdx
    jmp loop
ifendloop:
    cmp $0, (%rdx)
    je end
    jmp rsiIsZeroAndRdxNot 
    
ifendloop1:
    cmp $0, (%rsi)
    je end
    jmp loop

end:
    mov $0,(%rax)
    pop %rax
    ret