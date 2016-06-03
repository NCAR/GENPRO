
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
	/* .matchReStr = */ (char*) "time",
	/* .didCompileMatchRe = */ 0,
	/* .matchReFlags = */ REG_ICASE,
	rule_getVariableName
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
	{ rule_addAttr, &time_standard_name }
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
		/* .numApplicators = */ 4
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
	regmatch_t match;
	ParamRegexChangeRule *data = (ParamRegexChangeRule*) rule->data;

	if (!data->didCompileMatchRe) {
		assert(!regcomp(&(data->matchRe), data->matchReStr, data->matchReFlags));
		data->didCompileMatchRe = 1;
	}

	for (i = 0; i < gp->numParameters; i++) {
		if (!regexec(&(data->matchRe), data->getText(gp->params+i), 1, &match, 0)) {
			// ^ (regexec returns 0 on success)
			return rule_apply(rule, gp->params+i, gp);
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
