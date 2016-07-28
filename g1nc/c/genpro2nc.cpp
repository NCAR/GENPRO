
/* Copyright (c) 2016, University Corporation for Atmospheric Research
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *   contributors may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @file
 * @author Nicholas DeCicco <decicco@ucar.edu>
 *                          <nsd.cicco@gmail.com>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <netcdf.h>
#include <regex.h>

#include "gbytes.cpp"
#include "gpfile.hpp"
#include "rules.hpp"

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

#define max(a,b) ((a) > (b) ? (a) : (b))

enum {
	kFalse = 0,
	kTrue = 1
};

int gp1_read(GP1File *const gp, FILE *fp);
int gp1_write_nc(GP1File const*const gp,
                 char const*const outFileName,
                 int cmode);
void gp1_free(GP1File *const gp);
int read_header_chunk(FILE *fp, uint8_t **in_buffer, char **header_decomp, int numLines);
int get_text(char *const in_buf,
             const int offset,
             const int maxLength,
             char **out_buf,
             size_t *const out_length);
int gp1_write_nc(GP1File const*const gp,
                 char const*const outFileName,
                 int cmode);
int gp1_addAttrs(int ncid, int ncVar, Attribute *attrs, int numAttrs);
int *get_unique_sample_rates(GP1File const*const gp, int *numUnique, int **lookupTable);

extern Rule rules[];

int main(int argc, char **argv)
{
	int cmode = NC_NOCLOBBER;
	FILE *fp;
	char *inFileName, *outFileName;
	GP1File gp;

	if (argc != 3) {
		fprintf(stderr, "Error: Require exactly two arguments.\n");
		printf("Usage:\n"
		       "\n"
		       "    genpro2nc INFILE OUTFILE\n");
		exit(1);
	}

	inFileName = argv[1];
	outFileName = argv[2];

	if (!(fp = fopen(inFileName, "r"))) {
		fprintf(stderr, "Error: Failed to open \"%s\" for reading.\n", inFileName);
		exit(1);
	}

	memset(&gp, 0, sizeof(GP1File));

	if (!gp1_read(&gp, fp)) return 1;

	fclose(fp);

	rule_applyAll(rules, &gp);

	if (!gp1_write_nc(&gp, outFileName, cmode)) return 1;

	gp1_free(&gp);

	return 0;
}

/**
 * Reads a GENPRO-1 file.
 *
 * @param gp
 * @param fp
 */
int gp1_read(GP1File *const gp, FILE *fp)
{
	size_t amtRead;
	uint8_t *in_buffer = NULL;
	int *data_decomp = NULL;
	char *header_decomp = NULL;
	int i, j, k, m;
	int start, curCycle;
	regex_t re, reNoUnits;
	regmatch_t match[4];
	char reStr[] = "^ *([^ ]+.+[^ ]+)  +([^ ]+.+[^ ]+|[^ ]{1,2})  +([^ ]+.+[^ ]+|[^ ]{1,2}) *$";
	char reNoUnitsStr[] = "^ *([^ ]+.+[^ ]+)  +([^ ]+.+[^ ]+|[^ ]{1,2}) *$";
	int haveUnits;
	char tmp[100];
	long fileLen;

	if (!read_header_chunk(fp, &in_buffer, &header_decomp, HEADER_LINES)) {
		return 0;
	}

	// Get the file description.
	if (!get_text(header_decomp, 0, 31, &(gp->fileDesc), &(gp->fileDescLen))) {
		return 0;
	}

	sscanf(header_decomp+175, "%3d", &(gp->numParameters));
	sscanf(header_decomp+246, "%4d", &(gp->samplesPerCycle));
	sscanf(header_decomp+290, "%f", &(gp->cycleTime));
	sscanf(header_decomp+304, "%d", &(gp->cyclesPerBlock));

	if (!(gp->params = (Parameter*) malloc(sizeof(Parameter)*gp->numParameters))) {
		free(in_buffer);
		free(header_decomp);
		fclose(fp);
		goto mallocfail;
	}
	memset(gp->params, 0, sizeof(Parameter)*gp->numParameters);

	/* Now that we know precisely how many parameters there are, we can
	 * decompress the parameter description text.
	 */
	fseek(fp, COMP_HEADER_SIZE, SEEK_SET);
	if (!read_header_chunk(fp, &in_buffer, &header_decomp, gp->numParameters)) {
		fclose(fp);
		exit(1);
	}

	/*
	 * Parse parameters' parameters.
	 */

#define PARAMETER(i) (header_decomp+LINE_LENGTH*(i))

	assert(!regcomp(&re, reStr, REG_EXTENDED|REG_ICASE));
	assert(!regcomp(&reNoUnits, reNoUnitsStr, REG_EXTENDED|REG_ICASE));

	for (i = 0; i < gp->numParameters; i++) {
		sscanf(PARAMETER(i)+4, "%d", &(gp->params[i].rate));
		sscanf(PARAMETER(i)+80, "%f", &(gp->params[i].scale));
		sscanf(PARAMETER(i)+90, "%f", &(gp->params[i].bias));

		// Skip unused parameters
		if (memmem(header_decomp+LINE_LENGTH*i, 100, "UNUSED", 6)) {
			gp->params[i].isUnused = kTrue;
			continue;
		}

		/* Are the units missing? */
		haveUnits = 0;
		for (j = 0; j < 7; j++) {
			if (header_decomp[LINE_LENGTH*i+66+j] != ' ') {
				haveUnits = 1;
				break;
			}
		}

		strncpy(tmp, header_decomp+LINE_LENGTH*i+13, 60);
		tmp[60] = '\0';
		if (haveUnits && !(regexec(&re, tmp, 4, match, 0))) {
			gp->params[i].descLen = match[1].rm_eo-match[1].rm_so;
			gp->params[i].unitsLen = match[3].rm_eo-match[3].rm_so;
			if (!(gp->params[i].desc = (char*) malloc(sizeof(char)*gp->params[i].descLen))) {
				goto param_malloc_fail;
			}
			if (!(gp->params[i].label = (char*) malloc(sizeof(char)*(1+match[2].rm_eo-match[2].rm_so)))) {
				goto param_malloc_fail;
			}
			if (!(gp->params[i].units = (char*) malloc(sizeof(char)*gp->params[i].unitsLen))) {
				goto param_malloc_fail;
			}
			strncpy(gp->params[i].desc,  tmp+match[1].rm_so, gp->params[i].descLen);
			strncpy(gp->params[i].label, tmp+match[2].rm_so, match[2].rm_eo-match[2].rm_so);
			strncpy(gp->params[i].units, tmp+match[3].rm_so, gp->params[i].unitsLen);
			gp->params[i].label[match[2].rm_eo-match[2].rm_so] = '\0';
		} else if (!(regexec(&reNoUnits, tmp, 3, match, 0))) {
			/* Units might be missing */
			gp->params[i].descLen = match[1].rm_eo-match[1].rm_so;
			if (!(gp->params[i].desc = (char*) malloc(sizeof(char)*gp->params[i].descLen))) {
				goto param_malloc_fail;
			}
			if (!(gp->params[i].label = (char*) malloc(sizeof(char)*(match[2].rm_eo-match[2].rm_so)))) {
				goto param_malloc_fail;
			}
			gp->params[i].units = NULL;
			gp->params[i].unitsLen = 0;
			strncpy(gp->params[i].desc,  tmp+match[1].rm_so, gp->params[i].descLen);
			strncpy(gp->params[i].label, tmp+match[2].rm_so, match[2].rm_eo-match[2].rm_so);
		} else {
			fprintf(stderr, "fatal: failed to parse parameter. Line was:\n%.*s\n",
			        100, header_decomp+LINE_LENGTH*i);
			return 0;
		}

		fprintf(stderr, "found parameter: "
		                "desc = \"%.*s\", units = \"%.*s\", label = \"%s\"\n",
		        (int) gp->params[i].descLen, gp->params[i].desc,
		        (int) gp->params[i].unitsLen, gp->params[i].units, gp->params[i].label);
	}

	free(header_decomp);
	header_decomp = NULL;

	/*
	 * Determine how many records there are by doing some math.
	 */
	fseek(fp, 0L, SEEK_END);

	// Offset to the start of data.
	gp->dataStart = DIV_CEIL((HEADER_LINES+gp->numParameters)*LINE_LENGTH*6,64)*8;

	// Amount of data to read in with each fread(). (I.e., the amount of data
	// in one cycle's worth of samples.)
	gp->blockLength = DIV_CEIL(gp->cyclesPerBlock*gp->samplesPerCycle*20/*bits per value*/,64) *
	             8 /*bytes per 64-bit word*/;

	// It would seem that if cyclesPerBlock*samplesPerCycle*20%64 == 0 (i.e., the
	// amount of space needed for a block is exactly divisible by 64 such that
	// the data would run right up to the edge with no zero padding), a word
	// of zero padding is added. (So every block is separated by 8 zero bytes.)
	if (gp->cyclesPerBlock*gp->samplesPerCycle*20/*bits per value*/ % 64 == 0) {
		gp->blockLength += 8;
	}

	// Number of records in the file.
	fileLen = ftell(fp);
	gp->numBlocks = DIV_CEIL(fileLen - gp->dataStart, gp->blockLength);

	if ((size_t) fileLen != gp->numBlocks*gp->blockLength+gp->dataStart) {
		/* The file size doesn't match what we computed the file size should
		 * be assuming that data starts on a 64-bit word boundary. See if the
		 * file size works out for a non-64-bit word boundary.
		 */
		gp->dataStart = (HEADER_LINES+gp->numParameters)*LINE_LENGTH*6/8;
		if ((size_t) fileLen != gp->numBlocks*gp->blockLength+gp->dataStart) {
			fprintf(stderr, "Error: file length does not match predicted "
			                "length\n");
			return 0;
		}
	}

	/*
	 * Get data from the file.
	 */

	if (!(in_buffer = (uint8_t*) realloc(in_buffer, sizeof(uint8_t)*gp->blockLength))) {
		goto mallocfail;
	}
	if (!(data_decomp = (int*) realloc(data_decomp, sizeof(int)*gp->cyclesPerBlock*gp->samplesPerCycle))) {
		goto mallocfail;
	}

	// Allocate arrays
	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		gp->params[i].numValues = gp->params[i].rate * gp->numBlocks * gp->cyclesPerBlock;
		if (!(gp->params[i].values =
		      (int*) malloc(sizeof(int)*gp->params[i].numValues)))
		{
			goto mallocfail;
		}
	}

	// Next, read out the records
	fseek(fp, gp->dataStart, SEEK_SET);
	curCycle = 0; // Which cycle are we reading?
	do {
		amtRead = fread(in_buffer, sizeof(uint8_t), gp->blockLength, fp);
		if (amtRead < gp->blockLength) {
			if (amtRead > 0) {
				fprintf(stderr, "Warning: premature EOF encountered while "
				        "reading data\n");
			}
			break;
		}
		gbytes<uint8_t,int>(in_buffer, data_decomp, 0, 20, 0, gp->cyclesPerBlock*gp->samplesPerCycle);
		k = 0; // Index into data_decomp
		for (m = 0; m < gp->cyclesPerBlock; m++) {
			for (i = 0; i < gp->numParameters; i++) {
				if (gp->params[i].isUnused) {
					k += gp->params[i].rate;
					continue;
				}
				start = gp->params[i].rate*curCycle;
				for (j = 0; j < gp->params[i].rate; j++) {
					gp->params[i].values[start+j] = data_decomp[k++];
				}
			}
			curCycle++;
		}
	} while (!feof(fp));

	free(in_buffer); in_buffer = NULL;
	free(data_decomp); data_decomp = NULL;

	return 1;

param_malloc_fail: // Memory allocation failed while reading parameter info
	for (j = 0; j < i; j++) {
		if (gp->params[i].label) free(gp->params[i].label);
		if (gp->params[i].desc)  free(gp->params[i].desc);
		if (gp->params[i].units) free(gp->params[i].units);
	}
	fclose(fp);
	if (in_buffer) free(in_buffer);
	if (header_decomp) free(header_decomp);
	free(gp->params);
	gp->params = NULL;
	// fall through

mallocfail:
	fprintf(stderr, "Memory allocation failed, aborting.\n");
	return 1;
}

int compare_rates(const void *a, const void *b)
{
	if (*((int*) a) < *((int*) b)) return -1;
	else if (*((int*) a) == *((int*) b)) return 0;
	else return 1;
}

int *get_unique_sample_rates(GP1File const*const gp, int *numUnique, int **lookupTable)
{
	int i, j;
	int *redundantRates;
	int *rates;
	int numUsed;

	// Sort sampling rates
	for (i = 0, numUsed = 0; i < gp->numParameters; i++) {
		if (!gp->params[i].isUnused) numUsed++;
	}
	if (!(rates = (int*) malloc(sizeof(int)*4*numUsed))) {
		return NULL;
	}
	if (!(*lookupTable = (int*) malloc(sizeof(int)*gp->numParameters))) {
		free(rates);
		return NULL;
	}
	redundantRates = rates+numUsed;
	for (i = 0, j = 0; i < gp->numParameters; i++) {
		if (!gp->params[i].isUnused) {
			redundantRates[j] = gp->params[i].rate;
			redundantRates[j+1] = i;
			j += 2;
		}
	}
	qsort(redundantRates, numUsed, 2*sizeof(int), compare_rates);

	// Remove redundant sampling rates
	(*lookupTable)[redundantRates[1]] = 0;
	rates[0] = redundantRates[0];
	for (i = 1, j = 0; i < numUsed; i++) {
		if (redundantRates[2*(i-1)] != redundantRates[2*i]) {
			rates[++j] = redundantRates[2*i];
		}
		(*lookupTable)[redundantRates[2*i+1]] = j;
	}
	// j now contains how many unique rates we have
	*numUnique = j+1;

	return rates;
}

/*
 * Saves a GENPRO-1 file to NetCDF.
 */
int gp1_write_nc(GP1File const*const gp,
                 char const*const outFileName,
                 int cmode)
{
	int ncid; /* Handle on the NetCDF file */
	char name[100];
	int status;
	int i, j;
	int timeDimId;
	int numUniqueRates;
	int *uniqueRates;
	int *rateDimIds;
	int *lookupTable;
	int dimIds[2];
	long numDims;
	size_t start[2] = { 0, 0 };
	size_t count[2];
	void *values;
	size_t maxNumValues;

	if ((status = nc_create(outFileName, cmode, &ncid)) != NC_NOERR) {
		fprintf(stderr, "Fatal: failed to create NetCDF file.\n");
		goto ncerr;
	}

	if ((status = nc_def_dim(ncid, "Time", gp->params[0].numValues/
	                                       gp->params[0].rate,
	                         &timeDimId)) != NC_NOERR)
	{
		goto ncerr;
	}
	dimIds[0] = timeDimId;

	if (!(uniqueRates = get_unique_sample_rates(gp, &numUniqueRates, &lookupTable))) {
		nc_close(ncid);
		return 0;
	}

	if (!(rateDimIds = (int*) malloc(sizeof(int) * numUniqueRates))) {
		free(uniqueRates);
		nc_close(ncid);
		return 0;
	}

	for (i = 0; i < numUniqueRates; i++) {
		// We don't need to define a dimension for a sample rate of 1.
		if (uniqueRates[i] == 1) continue;

		sprintf(name, "sps%d", uniqueRates[i]);
		if ((status = nc_def_dim(ncid, name, uniqueRates[i],
		                         rateDimIds+i)) != NC_NOERR)
		{
			fprintf(stderr, "Fatal: failed to define dimension for %s\n",
			        gp->params[i].label);
			goto ncerr;
		}
	}

	// Define dimensions and variables.
	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;

		if (gp->params[i].rate == 1) {
			numDims = 1L;
		} else {
			numDims = 2L;
			dimIds[1] = rateDimIds[lookupTable[i]];
		}

		if ((status = nc_def_var(ncid, gp->params[i].label,
		                         gp->params[i].preferredType,
		                         numDims, dimIds,
		                         &(gp->params[i].ncVar))) != NC_NOERR)
		{
			fprintf(stderr, "NetCDF variable definition failed, aborting.\n"
			                "(desc = \"%.*s\", units = \"%.*s\", label = \"%s\")\n",
			        (int) gp->params[i].descLen, gp->params[i].desc,
			        (int) gp->params[i].unitsLen, gp->params[i].units, gp->params[i].label);
			goto ncerr;
		}

		if ((status = nc_put_att_text(ncid, gp->params[i].ncVar, "long_name",
		                              gp->params[i].descLen,
		                              gp->params[i].desc)) != NC_NOERR)
		{
			goto ncerr;
		}

		if ((status = nc_put_att_text(ncid, gp->params[i].ncVar, "units",
		                              gp->params[i].unitsLen,
		                              gp->params[i].units)) != NC_NOERR)
		{
			goto ncerr;
		}

		// Arbitrary attributes which may have been added by the rule set.
		gp1_addAttrs(ncid, gp->params[i].ncVar, gp->params[i].attrs, gp->params[i].numAttrs);
	}

	// Add global attributes which may have been added by the rule set.
	gp1_addAttrs(ncid, NC_GLOBAL, gp->attrs, gp->numAttrs);

	free(lookupTable);

	if ((status = nc_put_att_text(ncid, NC_GLOBAL, "DESCRIPTION",
	                              gp->fileDescLen, gp->fileDesc)) != NC_NOERR)
	{
		goto ncerr;
	}

	if ((status = nc_put_att_float(ncid, NC_GLOBAL, "CYCLE_TIME", NC_FLOAT, 1,
	                               &gp->cycleTime)) != NC_NOERR)
	{
		goto ncerr;
	}

	nc_enddef(ncid);


	// Restore original values by scaling/biasing.

	// Allocate an array big enough to hold the largest array
	maxNumValues = uniqueRates[numUniqueRates-1]*gp->numBlocks*gp->cyclesPerBlock;
	if (!(values = malloc(max(sizeof(float),sizeof(int))*maxNumValues))) {
		return 0;
	}

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;

		count[0] = gp->params[i].numValues / gp->params[i].rate;
		count[1] = gp->params[i].rate;

		switch (gp->params[i].preferredType) {
			case NC_FLOAT:
				for (j = 0; j < (int) gp->params[i].numValues; j++) {
					((float*) values)[j] = ((float) gp->params[i].values[j])/
					                       gp->params[i].scale -
					                       gp->params[i].bias;
				}
				if (nc_put_vara_float(ncid, gp->params[i].ncVar, start, count,
				                      (float*) values) != NC_NOERR)
				{
					goto ncerr;
				}
				break;

			case NC_INT:
				for (j = 0; j < (int) gp->params[i].numValues; j++) {
					((int*) values)[j] = gp->params[i].values[j]/
					                     gp->params[i].scale -
					                     gp->params[i].bias;
				}
				if (nc_put_vara_int(ncid, gp->params[i].ncVar, start, count,
				                    (int*) values) != NC_NOERR)
				{
					goto ncerr;
				}
				break;
		}
	}

	free(values);
	free(uniqueRates);

	nc_close(ncid);

	return 1;

ncerr:
	fprintf(stderr, "Error string was: %s\n", nc_strerror(status));
	return 0;
}

/**
 * Adds attributes which have been added by a rule set to a variable in the
 * NetCDF output.
 * @param ncid Handle to the NetCDF output file, as returned by nc_open().
 * @return 1 on success, 0 on failure
 */
int gp1_addAttrs(int ncid, int ncVar, Attribute *attrs, int numAttrs)
{
	int i;
	int status;
	Attribute *attr;

	for (i = 0; i < numAttrs; i++) {
		attr = attrs+i;
		switch (attr->type) {
			case kAttrTypeText:
				if ((status = nc_put_att_text(ncid, ncVar, attr->name,
				                              strlen((char*) attr->data),
				                              (char*) attr->data)) != NC_NOERR)
				{
					goto put_fail;
				}
				break;
			case kAttrTypeFloat:
				if ((status = nc_put_att_float(ncid, ncVar, attr->name,
				                               NC_FLOAT, attr->len,
				                               (float*) attr->data)) != NC_NOERR)
				{
					goto put_fail;
				}
				break;
			case kAttrTypeInt:
				if ((status = nc_put_att_int(ncid, ncVar, attr->name,
				                             NC_INT, attr->len,
				                             (int*) attr->data)) != NC_NOERR)
				{
					goto put_fail;
				}
				break;
			// TODO: implement others
		}
	}
	return 1;

put_fail:
	fprintf(stderr, "Error string was: %s\n", nc_strerror(status));
	return 0;
}

void gp1_free(GP1File *const gp)
{
	int i;

	if (gp->fileDesc) { free(gp->fileDesc); gp->fileDesc = NULL; }

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].values) { free(gp->params[i].values); gp->params[i].values = NULL; }
		if (gp->params[i].label)  { free(gp->params[i].label);  gp->params[i].label  = NULL; }
		if (gp->params[i].desc)   { free(gp->params[i].desc);   gp->params[i].desc   = NULL; }
		if (gp->params[i].units)  { free(gp->params[i].units);  gp->params[i].units  = NULL; }
	}
	free(gp->params);
	gp->params = NULL;
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

int get_text(char *const in_buf,
             const int offset,
             const int maxLength,
             char **out_buf,
             size_t *const out_length)
{
	int i;

	for (i = maxLength; in_buf[offset+i] == ' ' && i > -1; i--) {}
	i++;
	if (!(*out_buf = (char*) malloc(sizeof(char)*(i+1)))) {
		return 0;
	}
	strncpy(*out_buf, in_buf+offset, i);
	(*out_buf)[i] = '\0';
	if (out_length) *out_length = i;
	return 1;
}
