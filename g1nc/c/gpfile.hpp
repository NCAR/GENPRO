
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

#ifndef GPFILE_HPP
#define GPFILE_HPP

typedef enum {
	kAttrTypeText,
	kAttrTypeFloat,
	kAttrTypeInt
} AttrType;

typedef struct {
	char *name;
	AttrType type;
	void *data;
	size_t len; /** Not used for kAttrTypeText. */
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
	int preferredType; /** Preferred output type. */
	int *values;      /** A pointer to an array of raw (i.e., unscaled and
	                      unbiased) values for this parameter. */
	size_t numValues; /** Total number of samples recorded. This is the length
	                      of the `values' array. */
	char isUnused;    /** Indicates if the variable is unused. */
	int ncDimId;      /** Handle to the NetCDF dimension for this array. */

// From rules
	Attribute *attrs;
	int numAttrs;
} Parameter;

typedef struct {
	size_t blockLength; /** The size, in bits, of a block. */
	size_t dataStart;   /** The offset, in bits, to the start of the
	                        data. */
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

Parameter *gp1_findParam(GP1File const*const gpfile, const char paramName[]);

#endif
