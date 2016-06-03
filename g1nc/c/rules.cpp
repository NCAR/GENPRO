
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
	{ rule_setTimeUnits, NULL }
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

RuleApplicatorData actualRangeRuleApplicators[] = {
	{ rule_addMinMaxAttr, (char*) "actual_range" }
};

///////////////////////////////////////////////////////////////////////////////

Attribute institutionGlobalAttr = {
	(char*) "institution",
	kAttrTypeText,
	(char*) "NCAR Research Aviation Facility"
};

RuleApplicatorData addGlobalAttrInstitutionRuleApplicators[] = {
	{ rule_addGlobalAttr, &institutionGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

Attribute addressGlobalAttr = {
	(char*) "Address",
	kAttrTypeText,
	(char*) "P.O. Box 3000, Boulder, CO 80307-3000"
};

RuleApplicatorData addGlobalAttrAddressRuleApplicators[] = {
	{ rule_addGlobalAttr, &addressGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

Attribute creatorURLGlobalAttr = {
	(char*) "creator_url",
	kAttrTypeText,
	(char*) "http://www.eol.ucar.edu"
};

RuleApplicatorData addGlobalAttrCreatorURLRuleApplicators[] = {
	{ rule_addGlobalAttr, &creatorURLGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

Attribute conventionsGlobalAttr = {
	(char*) "ConventionsURL",
	kAttrTypeText,
	(char*) "NCAR-RAF/nimbus"
};

RuleApplicatorData addGlobalAttrConventionsRuleApplicators[] = {
	{ rule_addGlobalAttr, &conventionsGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

Attribute conventionsURLGlobalAttr = {
	(char*) "ConventionsURL",
	kAttrTypeText,
	(char*) "http://www.eol.ucar.edu/raf/Software/netCDF.html"
};

RuleApplicatorData addGlobalAttrConventionsURLRuleApplicators[] = {
	{ rule_addGlobalAttr, &conventionsURLGlobalAttr }
};

///////////////////////////////////////////////////////////////////////////////

Rule rules[] = {
	// Add a global attribute "institution"
	{
		NULL,
		rule_alwaysApplyGlobal,
		addGlobalAttrInstitutionRuleApplicators, 1
	},
	// Add a global attribute "Address"
	{
		NULL,
		rule_alwaysApplyGlobal,
		addGlobalAttrAddressRuleApplicators, 1
	},
	// Add a global attribute "creator_url"
	{
		NULL,
		rule_alwaysApplyGlobal,
		addGlobalAttrCreatorURLRuleApplicators, 1
	},
	// Add a global attribute "Conventions"
	{
		NULL,
		rule_alwaysApplyGlobal,
		addGlobalAttrConventionsRuleApplicators, 1
	},
	// Add a global attribute "ConventionsURL"
	{
		NULL,
		rule_alwaysApplyGlobal,
		addGlobalAttrConventionsURLRuleApplicators, 1
	},
	// Change a variable named TIME
	{
		&changeTimeRule,
		rule_paramRegexChange,
		changeTimeRuleApplicators,
		/* .numApplicators = */ 5
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
		actualRangeRuleApplicators, 1
	}
};

int set_str(char **dest, size_t *len, char *src)
{
	size_t newLen = strlen(src);
	if (!((len && newLen <= *len) || newLen <= strlen(*dest))) {
		if (!(*dest = (char*) malloc(sizeof(char)*(newLen+1)))) {
			return 0;
		}
	}
	strncpy(*dest, src, newLen);
	(*dest)[newLen] = '\0';
	if (len) *len = newLen;
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
	size_t i;

	if (!(minmax = (float*) malloc(sizeof(float)*2))) return 0;

	// Compute min/max values
	minmax[0] = minmax[1] = param->values[0];
	for (i = 0; i < param->numValues; i++) {
		if (minmax[0] > param->values[i]) {
			minmax[0] = param->values[i];
		}
		if (minmax[0] < param->values[i]) {
			minmax[0] = param->values[i];
		}
	}

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
	size_t i;

	if (!(minName = (char*) malloc(sizeof(char)*BUF_SIZE))) return 0;
	if (!(maxName = (char*) malloc(sizeof(char)*BUF_SIZE))) return 0;

	if (!(min = (float*) malloc(sizeof(float)))) return 0;
	if (!(max = (float*) malloc(sizeof(float)))) return 0;

	snprintf(minName, BUF_SIZE, formatStr, "min");
	snprintf(maxName, BUF_SIZE, formatStr, "max");

	// Compute min/max values
	*min = *max = param->values[0];
	for (i = 0; i < param->numValues; i++) {
		if (*min > param->values[i]) {
			*min = param->values[i];
		}
		if (*max < param->values[i]) {
			*max = param->values[i];
		}
	}

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

	if (!(gp->attrs = (Attribute*) realloc(gp->attrs, sizeof(Attribute)*(++gp->numAttrs)))) {
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

	if (!(param->attrs = (Attribute*) realloc(param->attrs, sizeof(Attribute)*(++param->numAttrs)))) {
		return 0;
	}

	memcpy(param->attrs+(param->numAttrs-1), attr, sizeof(Attribute));

	return 1;
}

static char timeUnits[BUF_SIZE];
int rule_setTimeUnits(void *applicatorData, void *extData, GP1File *const gp)
{
	Parameter *const param = (Parameter*) extData;
	int year, month, day;
	regmatch_t match[4];
	regex_t matchRe;
	int found = 0;
	int i;
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
		// use timeUnits as a temporary buffer
		strncpy(timeUnits, gp->fileDesc+match[1].rm_so, match[1].rm_eo-match[1].rm_so);
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
		strncpy(timeUnits, gp->fileDesc+match[3].rm_so, match[3].rm_eo-match[3].rm_so);
		year = 1900 + atoi(timeUnits);
	} else {
		fprintf(stderr, "rule_setTimeUnits: warning: couldn't find date\n");
	}

	snprintf(timeUnits, BUF_SIZE, "seconds since %4d-%02d-%02d 00:00:00 +0000", year, month, day);

	return rule_setUnits(timeUnits, param, gp);
}

int rule_applyAll(Rule const*const rules, size_t numRules, GP1File *const gp)
{
	size_t i;

	for (i = 0; i < numRules; i++) {
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
		doesntMatch = regexec(&(data->matchRe), data->getText(gp->params+i), 1, &match, 0);
		// ^ (regexec returns 0 on success)
		if ((doesntMatch && data->invert) || (!doesntMatch && !data->invert)) {
			if (!rule_apply(rule, gp->params+i, gp)) return 0;
		}
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
