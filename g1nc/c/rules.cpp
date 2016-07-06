
/**
 * Rules which transform GENPRO-1 files into RAF NetCDF files.
 * Author: Nicholas DeCicco
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

// Also used by ALAT rules
Attribute position_category = {
	(char*) "Category",
	kAttrTypeText,
	(char*) "Position"
};

ParamRegexChangeRule ALAT_renameRule = {
	/* .matchReStr        = */ (char*) "^alat$",
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

RuleApplicatorData ALAT_renameRuleApplicators[] = {
	{ rule_setVariableName, (char*) "LAT" },
	{ rule_addAttr, &position_category, },
	{ rule_setUnits, (char*) "degree_N" },
//	{ rule_setDesc, (char*) "" },
	{ rule_addAttr, &ALAT_standard_name },
	{ rule_addGlobalMinMax, (char*) "geospatial_lat_%s" },
	{ rule_addAttr, &ALAT_valid_range_attr }
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

RuleApplicatorData ALON_renameRuleApplicators[] = {
	{ rule_setVariableName, (char*) "LON" },
	{ rule_addAttr, &position_category, },
	{ rule_setUnits, (char*) "degree_E" },
//	{ rule_setDesc, (char*) "" },
	{ rule_addAttr, &ALON_standard_name },
	{ rule_addGlobalMinMax, (char*) "geospatial_lon_%s" },
	{ rule_addAttr, &ALON_valid_range_attr }
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
	{ rule_addCreationDate, NULL },
	{ rule_setFlightInfo, NULL }
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
	// Add global attributes which should always be present
	{
		NULL,
		rule_alwaysApplyGlobal,
		constantGlobalAttrs, 13
	},
	// Make units CF compliant
	{
		NULL,
		rule_applyToAllParams,
		makeUnitsCFCompliantRuleApplicators, 1
	},
	// Sanitize variable names
	{
		NULL,
		rule_applyToAllParams,
		sanitizeParamNamesRuleApplicators, 1
	},
	// Change a variable named TIME
	{
		&changeTimeRule,
		rule_paramRegexChange,
		changeTimeRuleApplicators,
		/* .numApplicators = */ 6
	},
	// Change "ALAT" ("RAW INS LATITUDE") into "LAT"
	{
		&ALAT_renameRule,
		rule_paramRegexChange,
		ALAT_renameRuleApplicators, 6
	},
	// Change "ALON" ("RAW INS LONGITUDE") into "LON"
	{
		&ALON_renameRule,
		rule_paramRegexChange,
		ALON_renameRuleApplicators, 6
	},
	// Add "actual_range" to every variable *except* time
	{
		&actualRangeRule,
		rule_paramRegexChange,
		actualRangeRuleApplicators, 4
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
	int i, time = -1, trimAmount = 0;

	/* Find the 'Time' variable */
	for (i = 0; i < gp->numParameters; i++) {
		if (strcmp(gp->params[i].label, "TIME") == 0) {
			time = i;
			break;
		}
	}

	if (time < 0) {
		fprintf(stderr, "rule_trimData: warning: failed to find `Time' "
		                "variable\n");
		return 0;
	}

	/* Search backwards in 'Time' for nonzero values. */
	i = (int) gp->params[time].numValues-1;
	while (i > 0) {
		if (gp->params[time].values[i] == 0) {
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
		if (minmax_i[0] < param->values[i]) {
			minmax_i[0] = param->values[i];
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
	regmatch_t match[4];
	regex_t matchRe;
	int found = 0;
	int i;
	int startH, startM, startS, endH, endM, endS;
	const char matchReStr[] =
		"\\([0-9]\\{1,2\\}\\) \\{0,1\\}"
		"\\(JAN\\|FEB\\|MAR\\|APR\\|MAY\\|JUN\\|JUL\\|AUG\\|SEP\\|OCT\\|NOV\\|DEC\\)"
		" \\{0,1\\}\\([0-9]\\{2\\}\\)";

	struct {
		char *name;
	} monthNames[] = {
		{ (char*) "JAN" }, { (char*) "FEB" }, { (char*) "MAR" },
		{ (char*) "APR" }, { (char*) "MAY" }, { (char*) "JUN" },
		{ (char*) "JUL" }, { (char*) "AUG" }, { (char*) "SEP" },
		{ (char*) "OCT" }, { (char*) "NOV" }, { (char*) "DEC" }
	};

	assert(!regcomp(&matchRe, matchReStr, REG_ICASE));
	if (!regexec(&matchRe, gp->fileDesc, 4, match, REG_EXTENDED)) {
		regfree(&matchRe);
		// use timeUnits as a temporary buffer
		strncpy(timeUnits, gp->fileDesc+match[1].rm_so,
		        match[1].rm_eo-match[1].rm_so);
		day = atoi(timeUnits);
		for (i = 0; i < 12; i++) {
			if (!strncmp(monthNames[i].name, gp->fileDesc+match[2].rm_so, 3)) {
				found = 1;
				break;
			}
		}
		assert(found);
		month = i+1;
		// use timeUnits as a temporary buffer
		strncpy(timeUnits, gp->fileDesc+match[3].rm_so,
		        match[3].rm_eo-match[3].rm_so);
		year = 1900 + atoi(timeUnits);
	} else {
		fprintf(stderr, "rule_setTimeUnits: warning: couldn't find date\n");
	}

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
// 5 = N677F, Gulfstream V (from 2005)
// 5 = N595KR, L-188 Electra (1973 through 1977)
// 6 = N306D, QueenAir, B-80
// 7 = N307D, Sabreliner, Model 60
// 8 = N308D, L-188 Electra (1978 through 2000)
// 9 = N9929J, Schweitzer 2-32 Sailplane
static struct {
	char* name;
} flightPlatforms[] = {
	{ (char*) "N130AR" },
	{ (char*) "N312D" },
	{ (char*) "Naval Research Lab (NRL) P-3"  },
	{ (char*) "N304D" },
	{ (char*) "N677F" },
	{ (char*) "N595KR" },
	{ (char*) "N306D" },
	{ (char*) "N307D" },
	{ (char*) "N308D" },
	{ (char*) "N9929J" }
};
static char flight_platform[] = "Platform";
int rule_setFlightInfo(void *applicatorData, void *extData, GP1File *const gp)
{
	// FIXME: we're making a lot of assumptions here...
	// should probably at least trim leading whitespace
	int platformNum = gp->fileDesc[0];
	if (platformNum < '1' || platformNum > '9') {
		fprintf(stderr, "rule_setFlightInfo: warning: invalid flight number\n");
		return 0;
	}
	platformNum -= '1';

	Attribute platform = {
		flight_platform,
		kAttrTypeText,
		flightPlatforms[platformNum].name
	};

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

int rule_applyAll(Rule const*const rules, GP1File *const gp)
{
	size_t i;

	for (i = 0; rules[i].match; i++) {
		if (!rules[i].match(rules+i, gp)) {
			return 0;
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
			if (!rule_apply(rule, gp->params+i, gp)) return 0;
		}
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
