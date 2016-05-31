
/* Author: Nicholas DeCicco <decicco@ucar.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <netcdf.h>

#include "gbytes.cpp"

// Rounds up division.
#define DIV_CEIL(n,d) (((n)-1)/(d)+1)

#define LINE_LENGTH 100
#define HEADER_LINES 11

#define GET_COMP_HEADER_SIZE(l) (DIV_CEIL((l)*LINE_LENGTH*6,8))
#define GET_DECOMP_HEADER_SIZE(l) ((l)*LINE_LENGTH)

// The size of the header, in bytes, when compressed (i.e., when encoded).
#define COMP_HEADER_SIZE GET_COMP_HEADER_SIZE(HEADER_LINES)
#define DECOMP_HEADER_SIZE GET_DECOMP_HEADER_SIZE(HEADER_LINES)
#define PARAMETER_HEADER_START COMP_HEADER_SIZE

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

int read_header_chunk(FILE *fp, uint8_t **in_buffer, char **header_decomp, int numLines);

int main(int argc, char **argv)
{
	int status;
	int ncid; /* Handle on the NetCDF file */
	size_t cycleLength, dataStart, numRecords, amtRead;
	char name[100];
	int cmode = NC_NOCLOBBER;
	FILE *fp;
	char *inFileName, *outFileName;
	uint8_t *in_buffer = NULL;
	int *data_decomp = NULL;
	char *header_decomp = NULL;
	int i, j, k;
	int start, curCycle;
	Parameter *params = NULL;
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

	if (!read_header_chunk(fp, &in_buffer, &header_decomp, HEADER_LINES)) {
		fclose(fp);
		exit(1);
	}

	sscanf(header_decomp+175, "%3d", &numParameters);
	sscanf(header_decomp+246, "%4d", &numPerCycle);
	sscanf(header_decomp+290, "%f", &cycleTime);

	if (!(params = (Parameter*) malloc(sizeof(Parameter)*numParameters))) {
		free(in_buffer);
		free(header_decomp);
		fclose(fp);
		goto mallocfail;
	}
	memset(params, 0, sizeof(Parameter)*numParameters);

	/* Now that we know precisely how many parameters there are, we can
	 * decompress the parameter description text.
	 */
	fseek(fp, COMP_HEADER_SIZE, SEEK_SET);
	if (!read_header_chunk(fp, &in_buffer, &header_decomp, numParameters)) {
		fclose(fp);
		exit(1);
	}

	/*
	 * Parse parameters' parameters.
	 */

#define PARAMETER(i) (header_decomp+LINE_LENGTH*(i))

	for (i = 0; i < numParameters; i++) {
		sscanf(PARAMETER(i)+4, "%d", &(params[i].rate));
		sscanf(PARAMETER(i)+80, "%f", &(params[i].scale));
		sscanf(PARAMETER(i)+90, "%f", &(params[i].bias));
		j = strchr(PARAMETER(i)+56, ' ') - (PARAMETER(i)+56);
		if (!(params[i].label = (char*) malloc(sizeof(char)*(j+1)))) {
			for (j = 0; j < i; j++) {
				if (params[i].label) free(params[i].label);
				if (params[i].desc) free(params[i].desc);
				fclose(fp);
				free(in_buffer);
				free(header_decomp);
			}
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
			for (j = 0; j < i; j++) {
				if (params[i].label) free(params[i].label);
				if (params[i].desc) free(params[i].desc);
				fclose(fp);
				free(in_buffer);
				free(header_decomp);
			}
			goto mallocfail;
		}
		strncpy(params[i].desc, PARAMETER(i)+13, j);
		params[i].descLen = j;
	}

	free(header_decomp);
	header_decomp = NULL;

	/*
	 * Determine how many records there are by doing some math.
	 */
	fseek(fp, 0L, SEEK_END);

	// Offset to the start of data.
	dataStart = DIV_CEIL((HEADER_LINES+numParameters)*LINE_LENGTH*6,64)*8;

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

	if (!(in_buffer = (uint8_t*) realloc(in_buffer, sizeof(uint8_t)*cycleLength))) {
		goto mallocfail;
	}
	if (!(data_decomp = (int*) realloc(data_decomp, sizeof(int)*numPerCycle))) {
		goto mallocfail;
	}

	// Allocate arrays
	for (i = 0; i < numParameters; i++) {
		if (params[i].isUnused) continue;
		params[i].numValues = params[i].rate * numRecords;
		if (!(params[i].values =
		      (float*) malloc(sizeof(float)*params[i].numValues)))
		{
			goto mallocfail;
		}
	}

	// Next, read out the records
	fseek(fp, dataStart, SEEK_SET);
	curCycle = 0; // Which cycle are we reading?
	do {
		amtRead = fread(in_buffer, sizeof(uint8_t), cycleLength, fp);
		if (amtRead < cycleLength) {
			fprintf(stderr, "Warning: premature EOF encountered while reading"
			        " data\n");
			break;
		}
		gbytes<uint8_t,int>(in_buffer, data_decomp, 0, 20, 0, numPerCycle);
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

	free(in_buffer); in_buffer = NULL;
	free(data_decomp); data_decomp = NULL;
	fclose(fp);

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

	nc_close(ncid);

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

	return 0;

mallocfail:
	fprintf(stderr, "Memory allocation failed, aborting.\n");
	return 1;

ncerr:
	fprintf(stderr, "Error string was: %s\n", nc_strerror(status));
	return 1;
}

int read_header_chunk(FILE *fp, uint8_t **in_buffer, char **header_decomp, int numLines)
{
	size_t i;
	size_t compSize = GET_COMP_HEADER_SIZE(numLines);
	size_t decompSize = GET_DECOMP_HEADER_SIZE(numLines);
	char ascii[65 /* add one for the null terminator */] =
		":"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"+-*/()$= ,.#[]%\"_!&'?<>@\\^;";

	if (!(*in_buffer = (uint8_t*) realloc(*in_buffer,
	                                      sizeof(uint8_t)*compSize)))
	{
		return 0;
	}

	if (fread(*in_buffer, sizeof(uint8_t), compSize, fp) != compSize) {
		fprintf(stderr, "Error: failed to read header.\n");
		free(in_buffer);
		return 0;
	}

	if (!(*header_decomp = (char*) realloc(*header_decomp,
	                                       sizeof(char)*(decompSize+1))))
	{
		free(in_buffer);
		return 0;
	}

	// Decompress the header.
	gbytes<uint8_t,char>(*in_buffer, *header_decomp, 0, 6, 0, decompSize);

	for (i = 0; i < decompSize; i++) {
		(*header_decomp)[i] = ascii[(int) (*header_decomp)[i]];
	}
	(*header_decomp)[decompSize] = '\0';

	for (i = 0; i < decompSize; i += LINE_LENGTH) {
		fwrite((*header_decomp)+i, sizeof(char), LINE_LENGTH, stdout);
		fputc('\n', stdout);
	}

	return 1;
}
