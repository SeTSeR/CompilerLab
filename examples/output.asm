BITS 64
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
 		   a dq 0.0
 		   b dq 4.0
 		   const1 dq 2.0
 		   const2 dq 4.0
 		   const3 dq 0.2
 		   const4 dq -0.25
 		   
 		   section .text
 		   f1:
 		   push rbp
 		   mov rbp, rsp
 		   movsd qword[rsp - 8], xmm0
 		   fld qword[const1]
 		   fld qword[rsp - 8]
 		   fld qword[const2]
 		   fdivp
 		   fptan
 		   fxch
 		   fstp st1
 		   fsubp
 		   fstp qword[rsp - 8]
 		   movsd xmm0, qword[rsp - 8]
 		   pop rbp
 		   ret
 		   
 		   f2:
 		   push rbp
 		   mov rbp, rsp
 		   movsd qword[rsp - 8], xmm0
 		   fld qword[rsp - 8]
 		   fstp qword[rsp - 8]
 		   movsd xmm0, qword[rsp - 8]
 		   pop rbp
 		   ret
 		   
 		   f3:
 		   push rbp
 		   mov rbp, rsp
 		   movsd qword[rsp - 8], xmm0
 		   fld qword[const3]
 		   fldpi
 		   fmulp
 		   fstp qword[rsp - 8]
 		   movsd xmm0, qword[rsp - 8]
 		   pop rbp
 		   ret
 		   
 		   df1:
 		   push rbp
 		   mov rbp, rsp
 		   movsd qword[rsp - 8], xmm0
 		   fld qword[const4]
 		   fld qword[rsp - 8]
 		   fld qword[const2]
 		   fdivp
 		   fcos
 		   fld st0
 		   fmulp
 		   fld1
 		   fdivrp
 		   fmulp
 		   fstp qword[rsp - 8]
 		   movsd xmm0, qword[rsp - 8]
 		   pop rbp
 		   ret
 		   
 		   df2:
 		   push rbp
 		   mov rbp, rsp
 		   movsd qword[rsp - 8], xmm0
 		   fld1
 		   fstp qword[rsp - 8]
 		   movsd xmm0, qword[rsp - 8]
 		   pop rbp
 		   ret
 		   
 		   df3:
 		   push rbp
 		   mov rbp, rsp
 		   movsd qword[rsp - 8], xmm0
 		   fldz
 		   fstp qword[rsp - 8]
 		   movsd xmm0, qword[rsp - 8]
 		   pop rbp
 		   ret
