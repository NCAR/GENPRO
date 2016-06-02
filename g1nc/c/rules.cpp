
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

Rule rules[] = {
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
		if (!(*dest = (char*) malloc(sizeof(char)*newLen))) {
			return 0;
		}
	}
	strncpy(*dest, src, newLen);
	if (len) *len = newLen;
	return 1;
}

int rule_setDesc(void *applicatorData, void *extData)
{
	Parameter *const param = (Parameter*) extData;
	char *const newDesc = (char*) applicatorData;

	set_str(&(param->desc), &(param->descLen), newDesc);
	return 1;
}

int rule_setVariableName(void *applicatorData, void *extData)
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

int rule_addAttr(void *applicatorData, void *extData)
{
	Attribute *attr = (Attribute*) applicatorData;
	Parameter *const param = (Parameter*) extData;

	if (!(param->attrs = (Attribute*) realloc(param->attrs, ++param->numAttrs))) {
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
int rule_apply(Rule const*const rule, void *data)
{
	int i;

	for (i = 0; i < rule->numApplicators; i++) {
		if (!rule->applicators[i].apply(rule->applicators[i].data, data)) {
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
			rule_apply(rule, gp->params+i);
			return 1;
		}
	}

	return 0;
}
