[BITS 64]
    default rel
    global a
    global b
    global f1
    global f2
    global f3
    global df1
    global df2
    global df3

    section .rodata
    a dq 0.000000
    b dq 4.000000
    const2 dq 2.000000
    const3 dq 4.000000
    const4 dq 0.628319
    const5 dq 0.000000
    const6 dq 1.000000

    section .text
f1:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    movsd qword[rsp], xmm0
    mov rax, qword[const2]
    push rax
    push qword[rbp - 8]
    mov rax, qword[const3]
    push rax
    fld qword[rsp + 8]
    fld qword[rsp]
    fdivp
    add rsp, 8
    fstp qword[rsp]
    fld qword[rsp]
    fptan
    fstp st0
    fstp qword[rsp]
    fld qword[rsp + 8]
    fld qword[rsp]
    fsubp
    add rsp, 8
    fstp qword[rsp]
    movsd xmm0, qword[rsp]
    add rsp, 16
    pop rbp
    ret

f2:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    movsd qword[rsp], xmm0
    push qword[rbp - 8]
    movsd xmm0, qword[rsp]
    add rsp, 16
    pop rbp
    ret

f3:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    movsd qword[rsp], xmm0
    mov rax, qword[const4]
    push rax
    movsd xmm0, qword[rsp]
    add rsp, 16
    pop rbp
    ret

df1:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    movsd qword[rsp], xmm0
    mov rax, qword[const5]
    push rax
    mov rax, qword[const6]
    push rax
    push qword[rbp - 8]
    mov rax, qword[const3]
    push rax
    fld qword[rsp + 8]
    fld qword[rsp]
    fdivp
    add rsp, 8
    fstp qword[rsp]
    fld qword[rsp]
    fcos
    fstp qword[rsp]
    push qword[rbp - 8]
    mov rax, qword[const3]
    push rax
    fld qword[rsp + 8]
    fld qword[rsp]
    fdivp
    add rsp, 8
    fstp qword[rsp]
    fld qword[rsp]
    fcos
    fstp qword[rsp]
    fld qword[rsp + 8]
    fld qword[rsp]
    fmulp
    add rsp, 8
    fstp qword[rsp]
    fld qword[rsp + 8]
    fld qword[rsp]
    fdivp
    add rsp, 8
    fstp qword[rsp]
    fld qword[rsp + 8]
    fld qword[rsp]
    fsubp
    add rsp, 8
    fstp qword[rsp]
    movsd xmm0, qword[rsp]
    add rsp, 16
    pop rbp
    ret

df2:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    movsd qword[rsp], xmm0
    mov rax, qword[const6]
    push rax
    movsd xmm0, qword[rsp]
    add rsp, 16
    pop rbp
    ret

df3:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    movsd qword[rsp], xmm0
    mov rax, qword[const5]
    push rax
    movsd xmm0, qword[rsp]
    add rsp, 16
    pop rbp
    ret

