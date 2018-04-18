#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
	if(argc < 3) return -1;
	FILE *out = fopen(argv[2], "wt");
	fclose(out);
	return 0;
}
