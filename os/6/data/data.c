#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(int argc, char **argv) {
	const size_t size = 2 * 1024 * 1024;
	FILE *io = fopen(argv[1], "a+");
	int c;
	for (size_t i = 0; i < size; i++) {
		fscanf(io, "%i", &c);
		fprintf(io, "%i\n", c);
	}
	fclose(io);
	return 0;
}
