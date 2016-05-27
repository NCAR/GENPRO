
/* Author: Nicholas DeCicco <decicco@ucar.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <netcdf.h>

#include "gbytes.cpp"

#define HEADER_SIZE 10000
#define DECOMP_SIZE HEADER_SIZE*8/6
#define LINE_LENGTH 100

// Rounds up division.
#define DIV_CEIL(n,d) (((n)-1)/(d)+1)

enum {
	kFalse = 0,
	kTrue = 1
};

typedef struct {
	int rate;         /** Sample rate in samples per "program cycle" (which can
	                      vary). */
	float scale;      /** Scale to be applied to reconstruct original parameter
	                      values. */
	float bias;       /** Offset to be applied to reconstruct original
	                      parameter values. */
	char *label;      /** The label associated with this parameter. */
	int ncVar;        /** The NetCDF variable ID corresponding to this
	                      parameter. */
	float *values;    /** A pointer to an array of values for this
	                      parameter. */
	size_t numValues; /** Total number of samples recorded. This is the length
	                      of the `values' array. */
	char isUnused;    /** Indicates if the variable is unused. */
} Parameter;

int main(int argc, char **argv)
{
	size_t cycleLength, dataStart, numRecords;
	char name[100];
	int cmode = NC_NOCLOBBER;
	FILE *fp;
	char *fn;
	uint32_t buf[3];
	uint8_t header[HEADER_SIZE];
	int data_decomp[HEADER_SIZE*8/20];
	char header_decomp[HEADER_SIZE*8/6];
	int i, j, k;
	int start, curCycle;
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
	memset(params, 0, sizeof(Parameter)*numParameters);

#define PARAMETER(i) header_decomp+100*(i+11)

	for (i = 0; i < numParameters; i++) {
		sscanf(PARAMETER(i)+4, "%d", &(params[i].rate));
		sscanf(PARAMETER(i)+80, "%f", &(params[i].scale));
		sscanf(PARAMETER(i)+90, "%f", &(params[i].bias));
		j = strchr(PARAMETER(i)+56, ' ') - (PARAMETER(i)+56);
		if (!(params[i].label = (char*) malloc(sizeof(char)*(j+1)))) {
			// TODO: free previous allocations
			goto mallocfail;
		}
		strncpy(params[i].label, PARAMETER(i)+56, j);
		params[i].label[j] = '\0';
		// Skip unused parameters
		if (strcmp(params[i].label, "UNUSED") == 0) {
			//numParameters--;
			//free(params[i].label);
			//i--;
			params[i].isUnused = kTrue;
		}
	}

	for (i = 0; i < 100*(11+numParameters); i += LINE_LENGTH) {
		fwrite(header_decomp+i, sizeof(char), LINE_LENGTH, stdout);
		fputc('\n', stdout);
	}

	/*
	 * Determine how many records there are by doing some math.
	 */
	fseek(fp, 0L, SEEK_END);

	// Offset to the start of data.
	dataStart = DIV_CEIL((11+numParameters)*100*6,64)*8;

	// Amount of data to read in with each fread(). (I.e., the amount of data
	// in one cycle's worth of samples.)
	cycleLength = DIV_CEIL(numPerCycle*20/*bits per value*/,64) *
	             8 /*bytes per 64-bit word*/;

	// Number of records in the file.
	numRecords = DIV_CEIL(ftell(fp) - dataStart, cycleLength);

	assert((size_t) ftell(fp) == numRecords*cycleLength+dataStart);

	/*
	 * Get data from the file.
	 */

	// Allocate arrays
	for (i = 0; i < numParameters; i++) {
		if (params[i].isUnused) continue;
		params[i].numValues = params[i].rate * numRecords;
		if (!(params[i].values =
		      (float*) malloc(sizeof(float)*params[i].numValues)))
		{
			fprintf(stderr, "Error: Memory allocation failed, aborting.\n");
			exit(1);
		}
	}

	// Next, read out the records
	fseek(fp, dataStart, SEEK_SET);
	curCycle = 0; // Which cycle are we reading?
	do {
		fread(header, sizeof(uint8_t), cycleLength, fp);
		gbytes<uint8_t,int>(header, data_decomp, 0, 20, 0, numPerCycle);
		k = 0;
		for (i = 0; i < numParameters; i++) {
			if (params[i].isUnused) {
				k += params[i].rate;
				continue;
			}
			start = params[i].rate*curCycle;
			for (j = 0; j < params[i].rate; j++) {
				params[i].values[start+j] = data_decomp[k++];
			}
		}
		curCycle++;
	} while (!feof(fp));

	return 0;

mallocfail:
	fprintf(stderr, "Memory allocation failed, aborting.\n");
	return 1;
}
