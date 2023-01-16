#include <stdio.h>
#include <time.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(int argc, char **argv) {
	const size_t size = 2 * 1024 * 1024;
	srand(time(NULL));
	FILE *output = fopen(argv[1], "w");
	for (size_t i = 0; i < size; i++) {
		int c = (rand() % 256);
		fprintf(output, "%i\n", c);
	}
	fclose(output);
	return 0;
}
