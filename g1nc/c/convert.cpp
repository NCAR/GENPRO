
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
	char *desc;       /** Description text associated with this parameter. */
	size_t descLen;   /** Length of description text, not including the null
	                      terminator. */
	int ncVar;        /** The NetCDF variable ID corresponding to this
	                      parameter. */
	float *values;    /** A pointer to an array of values for this
	                      parameter. */
	size_t numValues; /** Total number of samples recorded. This is the length
	                      of the `values' array. */
	char isUnused;    /** Indicates if the variable is unused. */
	int ncDimId;      /** Handle to the NetCDF dimension for this array. */
} Parameter;

int main(int argc, char **argv)
{
	int status;
	int ncid; /* Handle on the NetCDF file */
	size_t cycleLength, dataStart, numRecords;
	char name[100];
	int cmode = NC_NOCLOBBER;
	FILE *fp;
	char *inFileName, *outFileName;
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

	if (argc != 3) {
		fprintf(stderr, "Error: Require exactly two arguments.\n");
		printf("Usage:\n"
		       "\n"
		       "    convert INFILE OUTFILE\n");
		exit(1);
	}

	inFileName = argv[1];
	outFileName = argv[2];

	if (!(fp = fopen(inFileName, "r"))) {
		fprintf(stderr, "Error: Failed to open \"%s\" for reading.\n", inFileName);
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
			params[i].isUnused = kTrue;
		}
		// Compute the length of the description text.
		for (j = 42; *(PARAMETER(i)+13+j) == ' '; j--) {}
		j++;
		// Copy the description
		if (!(params[i].desc = (char*) malloc(sizeof(char)*(j+1)))) {
			// TODO: free previous allocations
			goto mallocfail;
		}
		strncpy(params[i].desc, PARAMETER(i)+13, j);
		params[i].descLen = j;
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

	// Restore original values by scaling/biasing.
	for (i = 0; i < numParameters; i++) {
		if (params[i].isUnused) continue;
		for (j = 0; j < (int) params[i].numValues; j++) {
			params[i].values[j] = ((float) params[i].values[j])/params[i].scale -
			                      params[i].bias;
		}
	}

	/*
	 * Save the data to NetCDF.
	 */

	if ((status = nc_create(outFileName, cmode, &ncid)) != NC_NOERR) {
		fprintf(stderr, "Fatal: failed to create NetCDF file.\n");
		goto ncerr;
	}

	// Define dimensions and variables.
	for (i = 0; i < numParameters; i++) {
		if (params[i].isUnused) continue;
		sprintf(name, "%s_LENGTH", params[i].label);
		if ((status = nc_def_dim(ncid, name, params[i].numValues,
		                         &(params[i].ncDimId))) != NC_NOERR)
		{
			fprintf(stderr, "Fatal: failed to define dimension for %s\n",
			        params[i].label);
			goto ncerr;
		}
		if ((status = nc_def_var(ncid, params[i].label, NC_FLOAT, 1L,
		                         &(params[i].ncDimId),
		                         &(params[i].ncVar))) != NC_NOERR)
		{
			fprintf(stderr, "NetCDF variable definition failed, aborting.\n");
			goto ncerr;
		}

		if ((status = nc_put_att_text(ncid, params[i].ncVar, "DESCRIPTION",
		                              params[i].descLen,
		                              params[i].desc)) != NC_NOERR)
		{
			goto ncerr;
		}

		if ((status = nc_put_att_int(ncid, params[i].ncVar, "SAMPLE_RATE",
		                             NC_INT, 1,
		                             &(params[i].rate))) != NC_NOERR)
		{
			goto ncerr;
		}

	}

	if ((status = nc_put_att_float(ncid, NC_GLOBAL, "cycle_time", NC_FLOAT, 1,
	                               &cycleTime)) != NC_NOERR)
	{
		goto ncerr;
	}

	nc_enddef(ncid);

	for (i = 0; i < numParameters; i++) {
		if (params[i].isUnused) continue;
		if (nc_put_var_float(ncid, params[i].ncVar,
		                     params[i].values) != NC_NOERR)
		{
			exit(1);
		}
	}

	// Clean up.
	for (i = 0; i < numParameters; i++) {
		if (params[i].values) {
			free(params[i].values);
		}
		if (params[i].label) {
			free(params[i].label);
		}
	}
	free(params);

	nc_close(ncid);

	return 0;

mallocfail:
	fprintf(stderr, "Memory allocation failed, aborting.\n");
	return 1;

ncerr:
	fprintf(stderr, "Error string was: %s\n", nc_strerror(status));
	return 1;
}
