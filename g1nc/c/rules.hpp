
/*
 * Author: Nicholas DeCicco
 */

#ifndef RULES_HPP
#define RULES_HPP

struct Rule_s;
typedef struct Rule_s Rule;

typedef struct {
	int (*apply)(void *applicatorData, void *extData, GP1File *const gp);
	void *data;
} RuleApplicatorData;

struct Rule_s {
	void *data;
	int (*match)(Rule const*const rule, GP1File *const gp);
	RuleApplicatorData *applicators;
	int numApplicators;
};

/**
 * A ParamRegexChangeRule is used to change some part of a parameter (variable)
 * if a regular expression is found to match.
 */
typedef struct {
	char *matchReStr;
	int didCompileMatchRe;
	int matchReFlags;
	char* (*getText)(Parameter *const param);
	regex_t matchRe;
} ParamRegexChangeRule;

//
// External API
//
int rule_applyAll(Rule const*const rules, size_t numRules, GP1File *const gp);

//
// Applicators
//
int rule_setUnits(void *applicatorData, void *extData, GP1File *const gp);
int rule_setDesc(void *applicatorData, void *extData, GP1File *const gp);
int rule_setVariableName(void *applicatorData, void *extData, GP1File *const gp);
int rule_addAttr(void *applicatorData, void *extData, GP1File *const gp);
int rule_addGlobalAttr(void *applicatorData, void *extData, GP1File *const gp);
int rule_addGlobalMinMax(void *applicatorData, void *extData, GP1File *const gp);
int rule_addMinMaxAttr(void *applicatorData, void *extData, GP1File *const gp);
int rule_setTimeUnits(void *applicatorData, void *extData, GP1File *const gp);

//
// Rules
//
int rule_paramRegexChange(Rule const*const rule, GP1File *const gp);
int rule_alwaysApplyGlobal(Rule const*const rule, GP1File *const gp);

//
// Helpers for rule_paramRegexChange
//
char *rule_getVariableName(Parameter *const param);

//
// Helper functions
//
int set_str(char **dest, size_t *len, char *src);
int rule_apply(Rule const*const rule, void *data, GP1File *const gp);

#endif
