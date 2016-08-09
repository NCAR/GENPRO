
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
 *
 * Rules which transform GENPRO-1 files into RAF NetCDF files.
 */

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <regex.h>
#include <time.h>
#include <netcdf.h>
#include "gpfile.hpp"
#include "rules.hpp"

ParamRegexChangeRule changeTimeRule = {
	/* .matchReStr        = */ (char*) "^time$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

Attribute strptime_format = {
	(char*) "strptime_format",
	kAttrTypeText,
	(char*) "seconds since %F %T %z"
};

Attribute time_standard_name = {
	(char*) "standard_name",
	kAttrTypeText,
	(char*) "time"
};

RuleApplicatorData changeTimeRuleApplicators[] = {
	{ rule_setVariableName, (char*) "Time" },
	{ rule_addAttr, &strptime_format },
	{ rule_setDesc, (char*) "time of measurement" },
	{ rule_addAttr, &time_standard_name },
	{ rule_setTimeUnits, NULL },
	{ rule_setPreferredType, (void*) NC_INT }
};

///////////////////////////////////////////////////////////////////////////////

ParamRegexChangeRule findWSPDRule = {
	/* .matchReStr        = */ (char*) "^wspd1\\{0,1\\}$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

ParamRegexChangeRule findWIRule = {
	/* .matchReStr        = */ (char*) "^wi$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

ParamRegexChangeRule findWDRCTNRule = {
	/* .matchReStr        = */ (char*) "^wdrctn1\\{0,1\\}$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

Attribute windFieldGlobalAttr_WSPD_WDRCTN_WI = {
	(char*) "wind_field",
	kAttrTypeText,
	(char*) "WSPD WDRCTN WI"
};

RuleApplicatorData addWindFieldGlobalAttr_WSPD_WDRCTN_WI_Applicators[] = {
	{ rule_addGlobalAttr, &windFieldGlobalAttr_WSPD_WDRCTN_WI }
};

Attribute windFieldGlobalAttr_WSPD_WDRCTN = {
	(char*) "wind_field",
	kAttrTypeText,
	(char*) "WSPD WDRCTN"
};

RuleApplicatorData addWindFieldGlobalAttr_WSPD_WDRCTN_Applicators[] = {
	{ rule_addGlobalAttr, &windFieldGlobalAttr_WSPD_WDRCTN }
};

///////////////////////////////////////////////////////////////////////////////

ParamRegexChangeRule addZAxisCoordinateRule = {
	/* .matchReStr        = */ (char*) "^hp1\\{0,1\\}$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

Attribute zAxisCoordinateGlobalAttr = {
	(char*) "zaxis_coordinate",
	kAttrTypeText,
	(char*) "HP"
};

CopyStrRule copyVertCoordUnitsRule = {
	rule_getUnits,
	rule_makeAttrFromStr,
	rule_addGlobalAttr,
	(char*) "geospatial_vertical_units"
};

RuleApplicatorData addZAxisCoordinateApplicators[] = {
	{ rule_addGlobalAttr, &zAxisCoordinateGlobalAttr },
	{ rule_copyStr, &copyVertCoordUnitsRule },
	{ rule_addGlobalMinMax, (char*) "geospatial_vertical_%s" }
};

///////////////////////////////////////////////////////////////////////////////

// Also used by ALAT rules
Attribute position_category = {
	(char*) "Category",
	kAttrTypeText,
	(char*) "Position"
};

ParamRegexChangeRule ALAT_renameRule = {
	/* .matchReStr        = */ (char*) "^alat1\\{0,1\\}$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

Attribute ALAT_standard_name = {
	(char*) "standard_name",
	kAttrTypeText,
	(char*) "latitude"
};

float ALAT_valid_range[2] = { -90.0f, 90.0f };

Attribute ALAT_valid_range_attr = {
	(char*) "valid_range",
	kAttrTypeFloat,
	ALAT_valid_range, 2
};

Attribute latGlobalAttr = {
	(char*) "latitude_coordinate",
	kAttrTypeText,
	(char*) "LAT"
};

RuleApplicatorData ALAT_renameRuleApplicators[] = {
	{ rule_setVariableName, (char*) "LAT" },
	{ rule_addAttr, &position_category, },
	{ rule_setUnits, (char*) "degree_N" },
//	{ rule_setDesc, (char*) "" },
	{ rule_addAttr, &ALAT_standard_name },
	{ rule_addGlobalMinMax, (char*) "geospatial_lat_%s" },
	{ rule_addAttr, &ALAT_valid_range_attr },
	{ rule_addGlobalAttr, &latGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

ParamRegexChangeRule ALON_renameRule = {
	/* .matchReStr        = */ (char*) "^along$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

Attribute ALON_standard_name = {
	(char*) "standard_name",
	kAttrTypeText,
	(char*) "longitude"
};

float ALON_valid_range[2] = { -180.0f, 180.0f };

Attribute ALON_valid_range_attr = {
	(char*) "valid_range",
	kAttrTypeFloat,
	ALON_valid_range, 2
};

Attribute lonGlobalAttr = {
	(char*) "longitude_coordinate",
	kAttrTypeText,
	(char*) "LON"
};

RuleApplicatorData ALON_renameRuleApplicators[] = {
	{ rule_setVariableName, (char*) "LON" },
	{ rule_addAttr, &position_category, },
	{ rule_setUnits, (char*) "degree_E" },
//	{ rule_setDesc, (char*) "" },
	{ rule_addAttr, &ALON_standard_name },
	{ rule_addGlobalMinMax, (char*) "geospatial_lon_%s" },
	{ rule_addAttr, &ALON_valid_range_attr },
	{ rule_addGlobalAttr, &lonGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

ParamRegexChangeRule actualRangeRule = {
	/* .matchReStr        = */ (char*) "^time$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 1
};

float fill_value[] = { -32767.0f };

Attribute fill_value_attr = {
	(char*) "_FillValue",
	kAttrTypeFloat,
	fill_value, 1
};

RuleApplicatorData actualRangeRuleApplicators[] = {
	{ rule_addMinMaxAttr, (char*) "actual_range" },
	{ rule_setPreferredType, (void*) NC_FLOAT },
	{ rule_addSampleRate, NULL },
	{ rule_addAttr, &fill_value_attr }
};

///////////////////////////////////////////////////////////////////////////////

Attribute institutionGlobalAttr = {
	(char*) "institution",
	kAttrTypeText,
	(char*) "NCAR Research Aviation Facility"
};

Attribute addressGlobalAttr = {
	(char*) "Address",
	kAttrTypeText,
	(char*) "P.O. Box 3000, Boulder, CO 80307-3000"
};

Attribute creatorURLGlobalAttr = {
	(char*) "creator_url",
	kAttrTypeText,
	(char*) "http://www.eol.ucar.edu"
};

Attribute conventionsGlobalAttr = {
	(char*) "Conventions",
	kAttrTypeText,
	(char*) "NCAR-RAF/nimbus"
};

Attribute conventionsURLGlobalAttr = {
	(char*) "ConventionsURL",
	kAttrTypeText,
	(char*) "http://www.eol.ucar.edu/raf/Software/netCDF.html"
};

Attribute conventionsVersionGlobalAttr = {
	(char*) "ConventionsVersion",
	kAttrTypeText,
	(char*) "1.3"
};

Attribute metadataConventionsGlobalAttr = {
	(char*) "Metadata_Conventions",
	kAttrTypeText,
	(char*) "Unidata Dataset Discovery v1.0"
};

Attribute standardNameVocabularyGlobalAttr = {
	(char*) "standard_name_vocabulary",
	kAttrTypeText,
	(char*) "CF-1.0"
};

Attribute timeCoordinateGlobalAttr = {
	(char*) "time_coordinate",
	kAttrTypeText,
	(char*) "Time"
};

Attribute phoneNumberGlobalAttr = {
	(char*) "Phone",
	kAttrTypeText,
	(char*) "(303) 497-1030"
};

Attribute processorURL = {
	(char*) "ProcessorURL",
	kAttrTypeText,
	(char*) "https://github.com/ncareol/GENPRO"
};

Attribute processorRevision = {
	(char*) "ProcessorRevision",
	kAttrTypeText,
	(char*) PROCESSOR_REVISION
};

Attribute categoriesGlobalAttr = {
	(char*) "Categories",
	kAttrTypeText,
	(char*) "Position,Thermodynamic,Aircraft State,Atmos. State"
};

Attribute geospatialVerticalPositiveGlobalAttr = {
	(char*) "geospatial_vertical_positive",
	kAttrTypeText,
	(char*) "up"
};

TrimThresholdRule aircraftSpeedTrimRule = {
	/* varName */ (char*) "TASG",
	/* condition */ kTrimIfValueLess,
	/* threshold */ 25.0f
};

RuleApplicatorData constantGlobalAttrs[] = {
	{ rule_trimData,      NULL },
	{ rule_addGlobalAttr, &institutionGlobalAttr },
	{ rule_addGlobalAttr, &addressGlobalAttr },
	{ rule_addGlobalAttr, &creatorURLGlobalAttr },
	{ rule_addGlobalAttr, &conventionsGlobalAttr },
	{ rule_addGlobalAttr, &conventionsURLGlobalAttr },
	{ rule_addGlobalAttr, &conventionsVersionGlobalAttr },
	{ rule_addGlobalAttr, &metadataConventionsGlobalAttr },
	{ rule_addGlobalAttr, &standardNameVocabularyGlobalAttr },
	{ rule_addGlobalAttr, &timeCoordinateGlobalAttr },
	{ rule_addGlobalAttr, &phoneNumberGlobalAttr },
	{ rule_addGlobalAttr, &processorURL },
	{ rule_addGlobalAttr, &processorRevision },
	{ rule_addGlobalAttr, &categoriesGlobalAttr },
	{ rule_addGlobalAttr, &geospatialVerticalPositiveGlobalAttr },
	{ rule_addCreationDate, NULL },
	{ rule_setFlightInfo, NULL }
};

///////////////////////////////////////////////////////////////////////////////

/* Match "TASF", "TASG", "GS", "GSF", or "GSF1" */
ParamRegexChangeRule trimAirspeedRule = {
	/* .matchReStr        = */ (char*) "^\\(tas[fg]\\|gs\\(f1\\{0,1\\}\\)\\{0,1\\}\\)$",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags      = */ REG_ICASE,
	/* .getText           = */ rule_getVariableName,
	/* .invert            = */ 0
};

RuleApplicatorData trimAirspeedRuleApplicators[] = {
	{ rule_trimThreshold, &aircraftSpeedTrimRule },
};

///////////////////////////////////////////////////////////////////////////////

RuleApplicatorData sanitizeParamNamesRuleApplicators[] = {
	{ rule_sanitizeParamName, NULL }
};

///////////////////////////////////////////////////////////////////////////////

RuleApplicatorData makeUnitsCFCompliantRuleApplicators[] = {
	{ rule_makeUnitsCFCompliant, NULL }
};

///////////////////////////////////////////////////////////////////////////////

Rule rules[] = {
	// Trim data according to airspeed
	{
		&trimAirspeedRule,
		rule_paramRegexChange,
		trimAirspeedRuleApplicators, 1,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Add global attributes which should always be present
	{
		NULL,
		rule_alwaysApplyGlobal,
		constantGlobalAttrs, 17,
		kContinueOnSuccess | kAbortOnFailure
	},
	// Make units CF compliant
	{
		NULL,
		rule_applyToAllParams,
		makeUnitsCFCompliantRuleApplicators, 1,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Sanitize variable names
	{
		NULL,
		rule_applyToAllParams,
		sanitizeParamNamesRuleApplicators, 1,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Look for the WI variable, and set 'wind_field' to 'WSPD WDRCTN WI'
	// if present; otherwise, set 'wind_field' to 'WSPD WDRCTN'.
	{
		&findWIRule,
		rule_paramRegexChange,
		addWindFieldGlobalAttr_WSPD_WDRCTN_WI_Applicators, 1,
		kJumpOnSuccess | kContinueOnFailure, 1, 0
	},
	{
		NULL,
		rule_alwaysApplyGlobal,
		addWindFieldGlobalAttr_WSPD_WDRCTN_Applicators, 1,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Look for the WSPD variable (this is just to throw warnings)
	{
		&findWSPDRule,
		rule_paramRegexChange,
		NULL,
		/* .numApplicators = */ 0,
		kContinueOnSuccess | kAbortOnFailure
	},
	// Look for the WDRCTN variable (this is just to throw warnings)
	{
		&findWDRCTNRule,
		rule_paramRegexChange,
		NULL,
		/* .numApplicators = */ 0,
		kContinueOnSuccess | kAbortOnFailure
	},
	// Change a variable named TIME
	{
		&changeTimeRule,
		rule_paramRegexChange,
		changeTimeRuleApplicators,
		/* .numApplicators = */ 6,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Change "ALAT" ("RAW INS LATITUDE") into "LAT"
	{
		&ALAT_renameRule,
		rule_paramRegexChange,
		ALAT_renameRuleApplicators, 7,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Change "ALON" ("RAW INS LONGITUDE") into "LON"
	{
		&ALON_renameRule,
		rule_paramRegexChange,
		ALON_renameRuleApplicators, 7,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Add "actual_range" to every variable *except* time
	{
		&actualRangeRule,
		rule_paramRegexChange,
		actualRangeRuleApplicators, 4,
		kContinueOnSuccess | kContinueOnFailure
	},
	// Add the 'zaxis_coordinate' global attribute
	{
		&addZAxisCoordinateRule,
		rule_paramRegexChange,
		addZAxisCoordinateApplicators, 3,
		kContinueOnSuccess | kContinueOnFailure
	},

	// End of rule set marker
	{ NULL, NULL, NULL, 0 }
};

static void get_hms(int value, int *const h, int *const m, int *const s);

int set_str(char **dest, size_t *len, char *src)
{
	size_t newLen = strlen(src);
	if (!*dest || !((len && newLen <= *len) || newLen <= strlen(*dest))) {
		if (!(*dest = (char*) realloc(*dest, sizeof(char)*(newLen+1)))) {
			return 0;
		}
	}
	strncpy(*dest, src, newLen);
	(*dest)[newLen] = '\0';
	if (len) *len = newLen;
	return 1;
}

/**
 * Removes zero values at the end of a file.
 */
int rule_trimData(void *applicatorData, void *extData, GP1File *const gp)
{
	int i, trimAmount = 0;
	Parameter *time;

	/* Find the 'Time' variable */
	if (!(time = gp1_findParam(gp, "TIME"))) {
		fprintf(stderr, "rule_trimData: warning: failed to find `Time' "
		                "variable\n");
		return 0;
	}

	/* Search backwards in 'Time' for nonzero values. */
	i = (int) time->numValues-1;
	while (i > 0) {
		if (time->values[i] == 0) {
			i--;
			trimAmount++;
		} else {
			break;
		}
	}

	fprintf(stderr, "rule_trimData: trimming %d points\n", trimAmount);

	/* Trim variables. */
	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		gp->params[i].numValues -= trimAmount*gp->params[i].rate;
	}

	return 1;
}

int rule_trimThreshold_trimIfLess(const float value, const float threshold)
{
	return value < threshold;
}

int rule_trimThreshold_trimIfGreater(const float value, const float threshold)
{
	return value > threshold;
}

enum {
	kTrimStart,
	kTrimEnd
};

/**
 * Trims a multiple of each parameter's respective sampling rates worth of
 * points from either the beginning or end of each parameter.
 *
 * @param trimAmount Not the number of points to trim, but a multiple of each
 *        parameter's rate worth of values to trim.
 * @param trimDirection One of either kTrimStart or kTrimEnd
 */
static void ResizeParam(GP1File *const gp, const int trimAmount,
                        const int trimDirection)
{
	int i;

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		gp->params[i].numValues -= trimAmount*gp->params[i].rate;
	}

	/* We could resize the arrays, but instead we'll just cheat and shift
	 * the data in memory. (We could also just advance the parameter value
	 * pointers forward, but that would mess up freeing memory.)
	 */
	if (trimDirection == kTrimStart) {
		for (i = 0; i < gp->numParameters; i++) {
			if (gp->params[i].isUnused) continue;
			memmove(gp->params[i].values,
			        gp->params[i].values + trimAmount*gp->params[i].rate,
			        gp->params[i].numValues * sizeof(int));
		}
	}
}

/**
 * Trims data from either end of every variable corresponding to values in
 * a variable specified in a TrimThresholdRule structure whose values are
 * either above or below (as indicated) a threshold, both also specified in
 * said structure.
 *
 * @param applicatorData A pointer to a TrimThresholdRule structure.
 * @param extData Unused.
 * @param gp
 */
int rule_trimThreshold(void *applicatorData, void *extData, GP1File *const gp)
{
	TrimThresholdRule *rule = (TrimThresholdRule*) applicatorData;
	Parameter *var;
	float value;
	int i, j, k;
	int stop;
	int (*cond[])(const float, const float) = {
		rule_trimThreshold_trimIfLess,
		rule_trimThreshold_trimIfGreater,
	};

	if (extData) {
		var = (Parameter*) extData;
	} else {
		if (!(var = gp1_findParam(gp, rule->varName))) {
			fprintf(stderr, "rule_trimThreshold: could not find specified "
			                "variable \"%s\"\n", rule->varName);
			return 0;
		}
	}

	/* Trim values from the start of the array. */
	for (i = 0, j = 0; j < (int) var->numValues; i++, j += var->rate)
	{
		/* Trim entire "blocks" of data (length equal to the sampling rate at
		 * a time
		 */
		for (stop = 0, k = 0; k < var->rate; k++) {
			value = var->values[j+k]/var->scale - var->bias;
			if (!cond[rule->condition](value, rule->threshold)) {
				stop = 1;
				break;
			}
		}
		if (stop) {
			ResizeParam(gp, i, kTrimStart);
			break;
		}
	}

	for (i = var->numValues/var->rate - 1, j = var->numValues - 1;
	     j >= 0; i--, j -= var->rate)
	{
		/* Trim entire "blocks" of data (length equal to the sampling rate at
		 * a time
		 */
		for (stop = 0, k = 0; k < var->rate; k++) {
			value = var->values[j-k]/var->scale - var->bias;
			if (!cond[rule->condition](value, rule->threshold)) {
				stop = 1;
				break;
			}
		}
		if (stop) {
			ResizeParam(gp, var->numValues/var->rate - 1 - i, kTrimEnd);
			break;
		}
	}

	return 1;
}

#define UNITS_EQUALS(str) param->unitsLen == strlen((char*) str) && \
                          !strncmp(param->units, (char*) str, param->unitsLen)
#define SET_UNITS(str) set_str(&(param->units), &(param->unitsLen), (char*) str)

int rule_makeUnitsCFCompliant(void *applicatorData,
                              void *extData,
                              GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;

	/* degrees */
	     if (UNITS_EQUALS("DEG"))      SET_UNITS("degree");

	/* millibar */
	else if (UNITS_EQUALS("MB"))       SET_UNITS("hPa");

	/* volts */
	else if (UNITS_EQUALS("V"))        SET_UNITS("V");

	/* meters per second */
	else if (UNITS_EQUALS("M/S"))      SET_UNITS("m/s");

	/* seconds */
	else if (UNITS_EQUALS("SEC"))      SET_UNITS("s");

	/* meters per second squared */
	else if (UNITS_EQUALS("M/S2"))     SET_UNITS("m/s2");

	/* kilometers */
	else if (UNITS_EQUALS("KM"))       SET_UNITS("km");

	/* volts DC */
	else if (UNITS_EQUALS("VDC"))      SET_UNITS("Vdc");

	/* meters */
	else if (UNITS_EQUALS("M"))        SET_UNITS("m");

	/* grams per cubic meter */
	else if (UNITS_EQUALS("G/M3"))     SET_UNITS("gram/m3");

	/* parts per billion */
	else if (UNITS_EQUALS("PPB"))      SET_UNITS("1/10e9");

	/* parts per billion (per volume?) */
	else if (UNITS_EQUALS("PPBV"))     SET_UNITS("ppbv");

	/* grams per kilogram */
	else if (UNITS_EQUALS("G/KG"))     SET_UNITS("g/kg");

	/* watts per square meter */
	else if (UNITS_EQUALS("WATTS/M2")) SET_UNITS("W/m2");

	/* watts per square meter */
	else if (UNITS_EQUALS("(W/M2)"))   SET_UNITS("W/m2");

	/* number per cubic centimeter (?) */
	else if (UNITS_EQUALS("N/CC"))     SET_UNITS("1/cm3");

	/* degrees Kelvin */
	else if (UNITS_EQUALS("K"))        SET_UNITS("deg_K");

	/* degrees Celsius */
	else if (UNITS_EQUALS("C"))        SET_UNITS("deg_C");

	/* percent */
	else if (UNITS_EQUALS("PER CENT")) SET_UNITS("%");

	/* refractive index */
	else if (strncmp(param->desc, "REFRACTIVE INDEX", param->descLen) &&
	         UNITS_EQUALS("N"))        SET_UNITS("none");

	/* unknown */
	else if (UNITS_EQUALS(""))         SET_UNITS("unk");

	return 1;
}

int rule_sanitizeParamName(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	char *src, *dst;

	// Strip whitespace and replace slashes ('/') with underscores ('_')
	src = dst = param->label;
	while (*src != '\0') {
		if (*src != ' ') {
			if (*src == '/') {
				*dst++ = '_';
			} else {
				*dst++ = *src;
			}
		}
		src++;
	}
	*dst = '\0';

	return 1;
}

int rule_setUnits(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	char *const newUnits = (char*) applicatorData;

	set_str(&(param->units), &(param->unitsLen), newUnits);
	return 1;
}

int rule_setDesc(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	char *const newDesc = (char*) applicatorData;

	set_str(&(param->desc), &(param->descLen), newDesc);
	return 1;
}

int rule_setVariableName(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	char *const newName = (char*) applicatorData;

	set_str(&(param->label), NULL, newName);
	return 1;
}

char *rule_getUnits(Parameter *const param)
{
	char *str;

	if (!(str = (char*) malloc(sizeof(char)*(param->unitsLen+1)))) {
		return 0;
	}
	return param->units;
}

char *rule_getVariableName(Parameter *const param)
{
	return param->label;
}

/**
 * Adds an attribute to a variable containing its min and max values.
 */
#define BUF_SIZE 100
int rule_addMinMaxAttr(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	char *const name = (char*) applicatorData;
	float *minmax;
	int minmax_i[2];
	size_t i;

	if (!(minmax = (float*) malloc(sizeof(float)*2))) return 0;

	// Compute min/max values
	minmax_i[0] = minmax_i[1] = param->values[0];
	for (i = 0; i < param->numValues; i++) {
		if (minmax_i[0] > param->values[i]) {
			minmax_i[0] = param->values[i];
		}
		if (minmax_i[1] < param->values[i]) {
			minmax_i[1] = param->values[i];
		}
	}

	minmax[0] = ((float) minmax_i[0])/param->scale - param->bias;
	minmax[1] = ((float) minmax_i[1])/param->scale - param->bias;

	Attribute attr = { name, kAttrTypeFloat, minmax, 2 };

	return rule_addAttr(&attr, param, gp);
}
/**
 * Adds global attributes for the min and max of a variable.
 */
#define BUF_SIZE 100
int rule_addGlobalMinMax(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	char *const formatStr = (char*) applicatorData;
	char *minName, *maxName;
	float *min, *max;
	int min_i, max_i;
	size_t i;

	if (!(minName = (char*) malloc(sizeof(char)*BUF_SIZE))) return 0;
	if (!(maxName = (char*) malloc(sizeof(char)*BUF_SIZE))) return 0;

	if (!(min = (float*) malloc(sizeof(float)))) return 0;
	if (!(max = (float*) malloc(sizeof(float)))) return 0;

	snprintf(minName, BUF_SIZE, formatStr, "min");
	snprintf(maxName, BUF_SIZE, formatStr, "max");

	// Compute min/max values
	min_i = max_i = param->values[0];
	for (i = 0; i < param->numValues; i++) {
		if (min_i > param->values[i]) {
			min_i = param->values[i];
		}
		if (max_i < param->values[i]) {
			max_i = param->values[i];
		}
	}

	*min = ((float) min_i)/param->scale - param->bias;
	*max = ((float) max_i)/param->scale - param->bias;

	Attribute minAttr = { minName, kAttrTypeFloat, min, 1 };
	Attribute maxAttr = { maxName, kAttrTypeFloat, max, 1 };

	return rule_addGlobalAttr(&minAttr, NULL, gp) &&
	       rule_addGlobalAttr(&maxAttr, NULL, gp);
}

/**
 * Adds a global attribute.
 */
int rule_addGlobalAttr(void *applicatorData, void *extData, GP1File *const gp)
{
	Attribute *attr = (Attribute*) applicatorData;

	if (!(gp->attrs = (Attribute*)
	      realloc(gp->attrs, sizeof(Attribute)*(++gp->numAttrs))))
	{
		return 0;
	}

	memcpy(gp->attrs+(gp->numAttrs-1), attr, sizeof(Attribute));

	return 1;
}

/**
 * Adds an attribute to a particular parameter (variable).
 */
int rule_addAttr(void *applicatorData, void *extData, GP1File *const gp)
{
	Attribute *attr = (Attribute*) applicatorData;
	Parameter *const param = (Parameter*) extData;

	if (!(param->attrs = (Attribute*)
	      realloc(param->attrs, sizeof(Attribute)*(++param->numAttrs))))
	{
		return 0;
	}

	memcpy(param->attrs+(param->numAttrs-1), attr, sizeof(Attribute));

	return 1;
}

static char date_created_attr[] = "date_created";
static char date_created[BUF_SIZE];
int rule_addCreationDate(void *applicatorData, void *extData, GP1File *const gp)
{
	time_t tt;
	struct tm tmt;
	time(&tt);
	gmtime_r(&tt, &tmt);
	strftime(date_created, BUF_SIZE, "%FT%T +0000", &tmt);

	Attribute attr = {
		date_created_attr,
		kAttrTypeText,
		(char*) date_created
	};

	return rule_addGlobalAttr(&attr, NULL, gp);
}

static void get_hms(int value, int *const h, int *const m, int *const s)
{
	*s = value % 60;
	value /= 60;
	*m = value % 60;
	*h = value / 60;
}

int rule_setPreferredType(void *applicatorData, void *extData, GP1File *const gp)
{
	// g++ won't let us cast from void* to int in one go, so we have to resort
	// to this nonsense
	int preferredType = (int) ((long) applicatorData);
	Parameter *const param = (Parameter*) extData;

	param->preferredType = preferredType;

	return 1;
}

static char time_interval[] = "TimeInterval";
static char time_coverage_start[] = "time_coverage_start";
static char time_coverage_end[]   = "time_coverage_end";
static char flight_date[] = "FlightDate";
static char timeUnits[BUF_SIZE];
static char timeCoverageStart[BUF_SIZE];
static char timeCoverageEnd[BUF_SIZE];
static char timeIntervalValue[BUF_SIZE];
static char flightDate[BUF_SIZE];
int rule_setTimeUnits(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	int year, month, day;
	regmatch_t match[5];
	regex_t matchRe, altRe;
	int found = 0;
	int i;
	int startH, startM, startS, endH, endM, endS;
	int regexDayInd, regexMonthInd, regexYearInd;
#define MONTH_RE "\\(JAN\\|FEB\\|MAR\\|APR\\|MAY\\|JUN\\|JUL\\|AUG\\|SEP\\|OCT\\|NOV\\|DEC\\)[A-Z]*"
#define DAY_RE "\\([0-9]\\{1,2\\}\\)"
#define YEAR_RE "\\(19\\)\\{0,1\\}\\([0-9]\\{2\\}\\)"
#define SPACE_RE " \\{0,1\\}"
	const char matchReStr[] = DAY_RE SPACE_RE MONTH_RE SPACE_RE YEAR_RE;

	/* Alternate regular expression for dates of the form "MONTH DAY YEAR" */
	const char altReStr[] = MONTH_RE SPACE_RE DAY_RE ",\\{0,1\\} \\{0,2\\}" YEAR_RE;

	struct {
		char *name;
	} monthNames[] = {
		{ (char*) "JAN" }, { (char*) "FEB" }, { (char*) "MAR" },
		{ (char*) "APR" }, { (char*) "MAY" }, { (char*) "JUN" },
		{ (char*) "JUL" }, { (char*) "AUG" }, { (char*) "SEP" },
		{ (char*) "OCT" }, { (char*) "NOV" }, { (char*) "DEC" }
	};

	assert(!regcomp(&matchRe, matchReStr, REG_ICASE));
	assert(!regcomp(&altRe, altReStr, REG_ICASE));
	if (!regexec(&matchRe, gp->fileDesc, 5, match, REG_EXTENDED)) {
		regexDayInd = 1;
		regexMonthInd = 2;
		regexYearInd = 4;
	} else if (!regexec(&altRe, gp->fileDesc, 5, match, REG_EXTENDED)) {
		regexDayInd = 2;
		regexMonthInd = 1;
		regexYearInd = 4;
	} else {
		fprintf(stderr, "rule_setTimeUnits: warning: couldn't find date\n");
		return 0;
	}

	/*
	 * Extract fields from the date regex.
	 */
	regfree(&matchRe);
	// use timeUnits as a temporary buffer
	strncpy(timeUnits, gp->fileDesc+match[regexDayInd].rm_so,
	        match[regexDayInd].rm_eo-match[regexDayInd].rm_so);
	day = atoi(timeUnits);
	for (i = 0; i < 12; i++) {
		if (!strncmp(monthNames[i].name, gp->fileDesc+match[regexMonthInd].rm_so, 3)) {
			found = 1;
			break;
		}
	}
	assert(found);
	month = i+1;
	// use timeUnits as a temporary buffer
	strncpy(timeUnits, gp->fileDesc+match[regexYearInd].rm_so,
	        match[regexYearInd].rm_eo-match[regexYearInd].rm_so);
	year = 1900 + atoi(timeUnits);

	snprintf(timeUnits, BUF_SIZE, "seconds since %4d-%02d-%02d 00:00:00 +0000",
	         year, month, day);

	if (!rule_setUnits(timeUnits, param, gp)) return 0;

	Attribute start = {
		time_coverage_start,
		kAttrTypeText,
		timeCoverageStart
	};

	Attribute end = {
		time_coverage_end,
		kAttrTypeText,
		timeCoverageEnd
	};

	Attribute flightDateAttr = {
		flight_date,
		kAttrTypeText,
		flightDate
	};

	// This is what ncplot uses to determine the number of seconds of data
	// in the file (in GetTimeInterval() in dataIO.c). If we don't define this,
	// ncplot will crash due to division by zero.
	Attribute timeInterval = {
		time_interval,
		kAttrTypeText,
		timeIntervalValue
	};

	// note that we assume time to be monotonically increasing (it should be)
	get_hms(param->values[0], &startH, &startM, &startS);
	snprintf(timeCoverageStart, BUF_SIZE, "%4d-%02d-%02dT%02d:%02d:%02d +0000",
	         year, month, day, startH, startM, startS);

	snprintf(flightDate, BUF_SIZE, "%02d/%02d/%04d",
	         month, day, year);

	get_hms(param->values[param->numValues-1], &endH, &endM, &endS);
	snprintf(timeCoverageEnd, BUF_SIZE, "%4d-%02d-%02dT%02d:%02d:%02d +0000",
	         year, month, day, endH, endM, endS);

	snprintf(timeIntervalValue, BUF_SIZE, "%02d:%02d:%02d-%02d:%02d:%02d",
	         startH, startM, startS, endH, endM, endS);

	return rule_addGlobalAttr(&start, NULL, gp) &&
	       rule_addGlobalAttr(&end, NULL, gp) &&
	       rule_addGlobalAttr(&timeInterval, NULL, gp) &&
	       rule_addGlobalAttr(&flightDateAttr, NULL, gp);
}

// From http://www.eol.ucar.edu/node/906
//
// 1 = N130AR, C-130Q Hercules
// 2 = N312D, B200T KingAir (one N2UW, Wyoming KingAir project in 1985)
// 3 = Naval Research Lab (NRL) P-3
// 4 = N304D, QueenAir, A-80
// 5 = N595KR, L-188 Electra (1973 through 1977)
// 6 = N306D, QueenAir, B-80
// 7 = N307D, Sabreliner, Model 60
// 8 = N308D, L-188 Electra (1978 through 2000)
// 9 = N9929J, Schweitzer 2-32 Sailplane
static struct {
	char* name;
} flightPlatforms[] = {
	/* 1 */ { (char*) "N130AR" },
	/* 2 */ { (char*) "N312D" },
	/* 3 */ { (char*) "Naval Research Lab (NRL) P-3"  },
	/* 4 */ { (char*) "N304D" },
	/* 5 */ { (char*) "N595KR" },
	/* 6 */ { (char*) "N306D" },
	/* 7 */ { (char*) "N307D" },
	/* 8 */ { (char*) "N308D" },
	/* 9 */ { (char*) "N9929J" }
};
static char flight_platform[] = "Platform";
static char ncar_electra_platform[] = "N595KR";
int rule_setFlightInfo(void *applicatorData, void *extData, GP1File *const gp)
{
	int i;
	int platformNum;
	for (i = 0; gp->fileDesc[i] == ' '; i++) {} // Skip leading whitespace
	platformNum = gp->fileDesc[i];
	const char ncar_electra[] = "NCAR ELECTRA";
	Attribute platform = {
		flight_platform,
		kAttrTypeText
	};
	if (platformNum < '1' || platformNum > '9') {
		if (!strncmp(gp->fileDesc, ncar_electra, strlen(ncar_electra))) {
			platform.data = ncar_electra_platform;
		} else {
			fprintf(stderr, "rule_setFlightInfo: warning: invalid flight number\n");
			return 0;
		}
	} else {
		platformNum -= '1';
		platform.data = flightPlatforms[platformNum].name;
	}

	return rule_addGlobalAttr(&platform, NULL, gp);
}

static char SampledRate[] = "SampledRate";
int rule_addSampleRate(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	int *rate;

	if (!(rate = (int*) malloc(sizeof(int)))) return 0;

	*rate = param->rate;

	Attribute sampleRate = {
		SampledRate,
		kAttrTypeInt,
		rate, 1
	};

	return rule_addAttr(&sampleRate, param, gp);
}

void* rule_makeAttrFromStr(char *const str, void *const data)
{
	Attribute *attr;

	if (!(attr = (Attribute*) malloc(sizeof(Attribute)))) {
		return NULL;
	}

	attr->name = (char*) data;
	attr->type = kAttrTypeText;
	attr->data = str;
	attr->len = strlen(str);

	return attr;
}

int rule_copyStr(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	CopyStrRule *rule = (CopyStrRule*) applicatorData;
	void *data;
	char *str;

	if (!(str = rule->getText(param))) {
		return 0;
	}

	if (!(data = rule->transformData(str, rule->data))) {
		return 0;
	}
	return rule->apply(data, extData, gp);
}

int rule_applyAll(Rule const*const rules, GP1File *const gp)
{
	size_t i;
	int retval;

	for (i = 0; rules[i].match; i++) {
		retval = rules[i].match(rules+i, gp);
		if (!retval) {
			switch (RULE_POSTACTION_FAILURE(rules[i].nextAction)) {
				case kJumpOnFailure:
					fprintf(stderr, "Info: rule failed, skipping following "
					        "%d rules.\n", rules[i].failureJumpAmount);
					i += rules[i].failureJumpAmount;
					break;
				case kAbortOnFailure:
					fprintf(stderr, "Error: Critical rule did not succeed, "
					                "aborting.\n");
					return 0;
				case kContinueOnFailure:
					break;
				default:
					break;
			}
		} else {
			switch (RULE_POSTACTION_SUCCESS(rules[i].nextAction)) {
				case kJumpOnSuccess:
					fprintf(stderr, "Info: rule succeeded, skipping following"
					        " %d rules.\n", rules[i].successJumpAmount);
					i += rules[i].successJumpAmount;
					break;
				case kAbortOnSuccess:
					fprintf(stderr, "Error: Rule succeded which requires "
					                "abort on success.\n");
					return 0;
				case kContinueOnSuccess:
					break;
				default:
					break;
			}
		}
	}
	return 1;
}

/**
 * Applies individual rules. Called after a match was found.
 */
int rule_apply(Rule const*const rule, void *data, GP1File *const gp)
{
	int i;

	for (i = 0; i < rule->numApplicators; i++) {
		if (!rule->applicators[i].apply(rule->applicators[i].data, data, gp)) {
			return 0;
		}
	}
	return 1;
}

/**
 * Changes some part of a GENPRO1 file if some part of a parameter matches a
 * regex.
 *
 * @return -1 on failure, 0 if not applied (rule was not relevant), 1 if applied
 */
int rule_paramRegexChange(Rule const*const rule, GP1File *const gp)
{
	int i;
	int doesntMatch;
	regmatch_t match;
	ParamRegexChangeRule *data = (ParamRegexChangeRule*) rule->data;
	int found = 0;

	if (!data->didCompileMatchRe) {
		assert(!regcomp(&(data->matchRe), data->matchReStr, data->matchReFlags));
		data->didCompileMatchRe = 1;
	}

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		doesntMatch = regexec(&(data->matchRe), data->getText(gp->params+i),
		                      1, &match, 0);
		// ^ (regexec returns 0 on success)
		if ((doesntMatch && data->invert) || (!doesntMatch && !data->invert)) {
			found++;
			if (!rule_apply(rule, gp->params+i, gp)) return 0;
		}
	}

	if (found == 0) {
		fprintf(stderr, "warning: no parameter found matching regex \"%s\"\n",
		        data->matchReStr);
		return 0;
	}

	return 1;
}

/**
 * Applies the rule to all parameters.
 */
int rule_applyToAllParams(Rule const*const rule, GP1File *const gp)
{
	int i;

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		if (!rule_apply(rule, gp->params+i, gp)) return 0;
	}

	return 1;
}

/**
 * Always applies the rule. Does not iterate over parameters.
 */
int rule_alwaysApplyGlobal(Rule const*const rule, GP1File *const gp)
{
	rule_apply(rule, NULL, gp);

	return 1;
}
