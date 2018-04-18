#include <stdio.h>
#include <string.h>

#include "analytic.h"

int main(int argc, char** argv) {
	for(int i = 1; i < argc; ++i) {
		if((strncmp(argv[i], "--help", 6)) || (strncmp(argv[i], "-h", 2))) {
			printf("There is no --help yet\n");
		}
	}
	return 0;
}
