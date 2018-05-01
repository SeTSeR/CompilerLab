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
    const4 dq 0.200000
    const5 dq 3.141593
    const6 dq 0.000000
    const7 dq 1.000000

    section .text
f1:
    movsd qword[rsp - 8], xmm0
    fld qword[const2]
    fld qword[rsp - 8]
    fld qword[const3]
    fdivp
    fptan
    fstp st0
    fsubp
    fstp qword[rsp - 8]
    movsd xmm0, qword[rsp - 8]
    ret

f2:
    movsd qword[rsp - 8], xmm0
    fld qword[rsp - 8]
    fstp qword[rsp - 8]
    movsd xmm0, qword[rsp - 8]
    ret

f3:
    movsd qword[rsp - 8], xmm0
    fld qword[const4]
    fld qword[const5]
    fmulp
    fstp qword[rsp - 8]
    movsd xmm0, qword[rsp - 8]
    ret

df1:
    movsd qword[rsp - 8], xmm0
    fld qword[const6]
    fld qword[const7]
    fld qword[const3]
    fmulp
    fld qword[rsp - 8]
    fld qword[const6]
    fmulp
    fsubp
    fld qword[const3]
    fld qword[const3]
    fmulp
    fdivp
    fld qword[rsp - 8]
    fld qword[const3]
    fdivp
    fcos
    fld qword[rsp - 8]
    fld qword[const3]
    fdivp
    fcos
    fmulp
    fdivp
    fsubp
    fstp qword[rsp - 8]
    movsd xmm0, qword[rsp - 8]
    ret

df2:
    movsd qword[rsp - 8], xmm0
    fld qword[const7]
    fstp qword[rsp - 8]
    movsd xmm0, qword[rsp - 8]
    ret

df3:
    movsd qword[rsp - 8], xmm0
    fld qword[const6]
    fld qword[const5]
    fmulp
    fld qword[const4]
    fld qword[const6]
    fmulp
    faddp
    fstp qword[rsp - 8]
    movsd xmm0, qword[rsp - 8]
    ret

