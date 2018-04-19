#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
	if(argc < 3) return -1;
	FILE *out = fopen(argv[2], "wt");
	fputs("[BITS 64]\n \
		   default rel\n \
		   global a\n \
		   global b\n \
		   global f1\n \
		   global f2\n \
		   global f3\n \
		   global df1\n \
		   global df2\n \
		   global df3\n \
		   \n \
		   section .rodata\n \
		   a dq -3.0\n \
		   b dq 1.5\n \
		   const1 dq 5.0\n \
		   const2 dq 3.0\n \
		   \n \
		   section .text\n \
		   f1:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld1\n \
		   fld st1\n \
		   fprem\n \
		   f2xm1\n \
		   faddp\n \
		   fscale\n \
		   fstp st1\n \
		   fld1\n \
		   faddp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   f2:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld st0\n \
		   fmulp\n \
		   fld st0\n \
		   fmulp\n \
		   fld qword[rsp - 8]\n \
		   fmulp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   f3:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld1\n \
		   fld qword[rsp - 8]\n \
		   fsubp\n \
		   fld qword[const2]\n \
		   fdivp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   df1:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld1\n \
		   fld st1\n \
		   fprem\n \
		   f2xm1\n \
		   faddp\n \
		   fscale\n \
		   fstp st1\n \
		   fldln2\n \
		   fmulp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   df2:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld qword[rsp - 8]\n \
		   fld st0\n \
		   fmulp\n \
		   fld st0\n \
		   fmulp\n \
		   fld qword[const1]\n \
		   fmulp\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n \
		   \n \
		   df3:\n \
		   push rbp\n \
		   mov rbp, rsp\n \
		   movsd qword[rsp - 8], xmm0\n \
		   fld1\n \
		   fld qword[const2]\n \
		   fdivp\n \
		   fchs\n \
		   fstp qword[rsp - 8]\n \
		   movsd xmm0, qword[rsp - 8]\n \
		   pop rbp\n \
		   ret\n", out);
	fclose(out);
	return 0;
}
