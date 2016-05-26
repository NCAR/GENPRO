
/* Author: Nicholas DeCicco <decicco@ucar.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define HEADER_SIZE 10000
#define DECOMP_SIZE HEADER_SIZE*8/6
#define LINE_LENGTH 100

typedef struct {
	int irate;
	float scale;
	float bias;
} Parameter;

int main(int argc, char **argv)
{
	FILE *fp;
	char *fn;
	uint32_t buf[3];
	uint8_t header[HEADER_SIZE];
	char header_decomp[HEADER_SIZE*8/6];
	int i, j;
	Parameter *params = NULL;
	char ascii[65 /* add one for the null terminator */] =
		":"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"+-*/()$= ,.#[]%\"_!&'?<>@\\^;";
	int numParameters, numPerCycle;
	float cycleTime;

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

	// Extract 6 bit fields from this
	for (j = 0, i = 0; i < HEADER_SIZE; i += 12 /* 3*32/8 */) {
		buf[0] = (((uint32_t) header[i+ 0]) << 24) |
		         (((uint32_t) header[i+ 1]) << 16) |
		         (((uint32_t) header[i+ 2]) <<  8) |
		         (((uint32_t) header[i+ 3]) <<  0);
		buf[1] = (((uint32_t) header[i+ 4]) << 24) |
		         (((uint32_t) header[i+ 5]) << 16) |
		         (((uint32_t) header[i+ 6]) <<  8) |
		         (((uint32_t) header[i+ 7]) <<  0);
		buf[2] = (((uint32_t) header[i+ 8]) << 24) |
		         (((uint32_t) header[i+ 9]) << 16) |
		         (((uint32_t) header[i+10]) <<  8) |
		         (((uint32_t) header[i+11]) <<  0);
		header_decomp[j+15] = buf[2] & 0x3F; buf[2] >>= 6;
		header_decomp[j+14] = buf[2] & 0x3F; buf[2] >>= 6;
		header_decomp[j+13] = buf[2] & 0x3F; buf[2] >>= 6;
		header_decomp[j+12] = buf[2] & 0x3F; buf[2] >>= 6;
		header_decomp[j+11] = buf[2] & 0x3F; buf[2] >>= 6;
		header_decomp[j+10] = (buf[2] & 0x3) | ((buf[1] & 0xF) << 2); buf[1] >>= 4;
		header_decomp[j+9] = buf[1] & 0x3F; buf[1] >>= 6;
		header_decomp[j+8] = buf[1] & 0x3F; buf[1] >>= 6;
		header_decomp[j+7] = buf[1] & 0x3F; buf[1] >>= 6;
		header_decomp[j+6] = buf[1] & 0x3F; buf[1] >>= 6;
		header_decomp[j+5] = (buf[1] & 0xF) | ((buf[0] & 0x3) << 4); buf[0] >>= 2;
		header_decomp[j+4] = buf[0] & 0x3F; buf[0] >>= 6;
		header_decomp[j+3] = buf[0] & 0x3F; buf[0] >>= 6;
		header_decomp[j+2] = buf[0] & 0x3F; buf[0] >>= 6;
		header_decomp[j+1] = buf[0] & 0x3F; buf[0] >>= 6;
		header_decomp[j+0] = buf[0] & 0x3F; buf[0] >>= 6;
		j += 16;
	}

	for (i = 0; i < DECOMP_SIZE; i++) {
		header_decomp[i] = ascii[(int) header_decomp[i]];
	}
	header_decomp[DECOMP_SIZE] = '\0';

	sscanf(header_decomp+175, "%3d", &numParameters);
	sscanf(header_decomp+246, "%4d", &numPerCycle);
	sscanf(header_decomp+290, "%f", &cycleTime);

	if (!(params = (Parameter*) malloc(sizeof(Parameter)*numParameters))) {
		goto mallocfail;
	}

	for (i = 0; i < numParameters; i++) {
		sscanf(header_decomp+100*(i+11)+4, "%d", &(params[i].irate));
		sscanf(header_decomp+100*(i+11)+80, "%f", &(params[i].scale));
		sscanf(header_decomp+100*(i+11)+90, "%f", &(params[i].bias));
	}

	for (i = 0; i < DECOMP_SIZE; i += LINE_LENGTH) {
		fwrite(header_decomp+i, sizeof(char), 100, stdout);
		fputc('\n', stdout);
	}

	return 0;

mallocfail:
	return 1;
}
