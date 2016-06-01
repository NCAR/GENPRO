
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
	char *units;      /** Units text associated with this parameter. */
	size_t unitsLen;  /** Length of units text, not including the null
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

typedef struct {
	size_t blockLength; /** The size, in 8-bit bytes, of a block. */
	size_t dataStart;   /** The offset to the start of the data. */
	size_t numBlocks;   /** The number of blocks in a file. */
	Parameter *params;  /** Parameters (variables) contained in this file. */
	int numParameters;  /** The number of parameters. */
	int samplesPerCycle;    /** The number of samples per cycle. */
	int cyclesPerBlock; /** The number of cycles found in each block. */
	float cycleTime;    /** Period between batches of samples. */
	char *fileDesc;     /** File description text. */
	size_t fileDescLen; /** Length of the file description text. */
} GP1File;

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
		       "    convert INFILE OUTFILE\n");
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

	if (!read_header_chunk(fp, &in_buffer, &header_decomp, HEADER_LINES)) {
		return 0;
	}

	// Get the file description.
	if (!get_text(header_decomp, 0, 23, &(gp->fileDesc), &(gp->fileDescLen))) {
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

	for (i = 0; i < gp->numParameters; i++) {
		sscanf(PARAMETER(i)+4, "%d", &(gp->params[i].rate));
		sscanf(PARAMETER(i)+80, "%f", &(gp->params[i].scale));
		sscanf(PARAMETER(i)+90, "%f", &(gp->params[i].bias));

		// Get the parameter (variable) name.
		if (!get_text(header_decomp, LINE_LENGTH*i+56, 9,
		              &(gp->params[i].label), NULL))
		{
			goto param_malloc_fail;
		}

		// Skip unused parameters
		if (strcmp(gp->params[i].label, "UNUSED") == 0) {
			gp->params[i].isUnused = kTrue;
		}

		// Get the description text.
		if (!get_text(header_decomp, LINE_LENGTH*i+13, 42,
		              &(gp->params[i].desc), &(gp->params[i].descLen)))
		{
			goto param_malloc_fail;
		}

		// Get the units text.
		if (!get_text(header_decomp, LINE_LENGTH*i+66, 7,
		              &(gp->params[i].units), &(gp->params[i].unitsLen)))
		{
			goto param_malloc_fail;
		}
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
	gp->numBlocks = DIV_CEIL(ftell(fp) - gp->dataStart, gp->blockLength);

	assert((size_t) ftell(fp) == gp->numBlocks*gp->blockLength+gp->dataStart);

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
		      (float*) malloc(sizeof(float)*gp->params[i].numValues)))
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

	// Restore original values by scaling/biasing.
	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		for (j = 0; j < (int) gp->params[i].numValues; j++) {
			gp->params[i].values[j] = ((float) gp->params[i].values[j])/
			                          gp->params[i].scale -
			                          gp->params[i].bias;
		}
	}

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

typedef struct {
	char *name;
	char *value;
} NCTextAttr;

NCTextAttr globalAttrs[] = {
#define GLOBAL_ATTR_CREATION_DATE 0
	/* 0 */ { (char*) "date_created" },
#if 0
#define GLOBAL_ATTR_LAT 1
	/* 1 */ { (char*) "latitude_coordinate" },
#define GLOBAL_ATTR_LON 2
	/* 2 */ { (char*) "longitude_coordinate" },
#endif

// Global attributes with predetermined values
	{
		(char*) "institution",
		(char*) "NCAR Research Aviation Facility"
	}, {
		(char*) "Address",
		(char*) "P.O. Box 3000, Boulder, CO 80307-3000"
	}, {
		(char*) "creator_url",
		(char*) "http://www.eol.ucar.edu"
	}, {
		(char*) "Conventions",
		(char*) "NCAR-RAF/nimbus"
	}, {
		(char*) "ConventionsURL",
		(char*) "http://www.eol.ucar.edu/raf/Software/netCDF.html"
	}
};

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
	int i;
	time_t t;

	if ((status = nc_create(outFileName, cmode, &ncid)) != NC_NOERR) {
		fprintf(stderr, "Fatal: failed to create NetCDF file.\n");
		goto ncerr;
	}

	// Define dimensions and variables.
	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		sprintf(name, "%s_LENGTH", gp->params[i].label);
		if ((status = nc_def_dim(ncid, name, gp->params[i].numValues,
		                         &(gp->params[i].ncDimId))) != NC_NOERR)
		{
			fprintf(stderr, "Fatal: failed to define dimension for %s\n",
			        gp->params[i].label);
			goto ncerr;
		}
		if ((status = nc_def_var(ncid, gp->params[i].label, NC_FLOAT, 1L,
		                         &(gp->params[i].ncDimId),
		                         &(gp->params[i].ncVar))) != NC_NOERR)
		{
			fprintf(stderr, "NetCDF variable definition failed, aborting.\n");
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

		if ((status = nc_put_att_int(ncid, gp->params[i].ncVar, "SampledRate",
		                             NC_INT, 1,
		                             &(gp->params[i].rate))) != NC_NOERR)
		{
			goto ncerr;
		}
	}


	time(&t);
	globalAttrs[GLOBAL_ATTR_CREATION_DATE].value = ctime(&t);

	for (i = 0; i < (int) (sizeof(globalAttrs)/sizeof(NCTextAttr)); i++) {
		if ((status = nc_put_att_text(ncid, NC_GLOBAL, globalAttrs[i].name,
		                              strlen(globalAttrs[i].value),
		                              globalAttrs[i].value)) != NC_NOERR)
		{
			goto ncerr;
		}
	}

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

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		if (nc_put_var_float(ncid, gp->params[i].ncVar,
		                     gp->params[i].values) != NC_NOERR)
		{
			return 0;
		}
	}

	nc_close(ncid);

	return 1;

ncerr:
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
