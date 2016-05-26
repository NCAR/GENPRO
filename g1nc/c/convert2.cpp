
/* Author: Nicholas DeCicco <decicco@ucar.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "gbytes.cpp"

#define HEADER_SIZE 10000
#define DECOMP_SIZE HEADER_SIZE*8/6
#define LINE_LENGTH 100

void swap_uint8(uint8_t *const a, uint8_t *const b);

int main(int argc, char **argv)
{
	FILE *fp;
	char *fn;
	uint8_t header[HEADER_SIZE];
	char header_decomp[HEADER_SIZE*8/6];
	int i;
	char ascii[65] =
		":"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"+-*/()$= ,.#[]%\"_!&'?<>@\\^;";

	if (argc != 2) {
		fprintf(stderr, "Error: Require exactly one argument.\n");
		exit(1);
	}

	fn = argv[1];

	if (!(fp = fopen(fn, "r"))) {
		fprintf(stderr, "Error: Failed to open \"%s\" for reading.\n", fn);
		exit(1);
	}

	if (fread(header, sizeof(uint8_t), HEADER_SIZE, fp) != HEADER_SIZE) {
		fprintf(stderr, "Error: failed to read header.\n");
		exit(1);
	}

	gbytes<uint8_t,char>(header, header_decomp, 0, 6, 0, DECOMP_SIZE);

	for (i = 0; i < DECOMP_SIZE; i++) {
		header_decomp[i] = ascii[(int) header_decomp[i]];
	}
	header_decomp[DECOMP_SIZE] = '\0';

	for (i = 0; i < DECOMP_SIZE; i += LINE_LENGTH) {
		fwrite(header_decomp+i, sizeof(char), 100, stdout);
		fputc('\n', stdout);
	}

	return 0;
}
