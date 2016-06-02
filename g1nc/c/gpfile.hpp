
/* Author: Nicholas DeCicco <decicco@ucar.edu>
 */

typedef enum {
	kAttrTypeText,
	kAttrTypeFloat
} AttrType;

typedef struct {
	char *name;
	AttrType type;
	void *data;
} Attribute;

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

// From rules
	Attribute *attrs;
	int numAttrs;
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

// From rules
	Attribute *attrs;
	int numAttrs;
} GP1File;
