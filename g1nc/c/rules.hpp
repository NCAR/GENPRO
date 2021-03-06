
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

#ifndef RULES_HPP
#define RULES_HPP

struct Rule_s;
typedef struct Rule_s Rule;

typedef struct {
	int (*apply)(void *applicatorData, void *extData, GP1File *const gp);
	void *data;
} RuleApplicatorData;

/** Values for the 'nextAction' property of 'Rule'. */
enum {
	kAbortOnFailure     = 1 << 0,
	kJumpOnFailure      = 1 << 1,
	kContinueOnFailure  = 1 << 2,
	kAbortOnSuccess     = 1 << 3,
	kJumpOnSuccess      = 1 << 4,
	kContinueOnSuccess  = 1 << 5
};

#define RULE_POSTACTION_FAILURE(action) ((action) & 0x07)
#define RULE_POSTACTION_SUCCESS(action) ((action) & 0x38)

struct Rule_s {
	void *data;
	int (*match)(Rule const*const rule, GP1File *const gp);
	RuleApplicatorData *applicators;
	int numApplicators;
	int nextAction;
	int successJumpAmount;
	int failureJumpAmount;
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
	int invert;
	regex_t matchRe;
} ParamRegexChangeRule;

/**
 * Trim conditions for rule_trimThreshold().
 */
enum {
	kTrimIfValueLess = 0,
	kTrimIfValueGreater
};

/**
 * Used in conjunction with rule_trimThreshold().
 */
typedef struct {
	char *varName;
	int condition;
	float threshold;
} TrimThresholdRule;

typedef struct {
	char* (*getText)(Parameter *const param);
	/**
	 * Acts as an intermediary between the callback which obtains the string
	 * to copy and the callback which uses the data generated by this callback.
	 */
	void* (*transformData)(char *const str, void *const data);
	int (*apply)(void *applicatorData, void *extData, GP1File *const gp);
	void *data;
} CopyStrRule;

//
// External API
//
int rule_applyAll(Rule const*const rules, GP1File *const gp);

//
// Applicators
//
int rule_makeUnitsCFCompliant(void *applicatorData,
                              void *extData,
                              GP1File *const gp);
int rule_sanitizeParamName(void *applicatorData, void *extData, GP1File *const gp);
int rule_setUnits(void *applicatorData, void *extData, GP1File *const gp);
int rule_setDesc(void *applicatorData, void *extData, GP1File *const gp);
int rule_setVariableName(void *applicatorData, void *extData, GP1File *const gp);
int rule_addAttr(void *applicatorData, void *extData, GP1File *const gp);
int rule_addGlobalAttr(void *applicatorData, void *extData, GP1File *const gp);
int rule_addGlobalMinMax(void *applicatorData, void *extData, GP1File *const gp);
int rule_addMinMaxAttr(void *applicatorData, void *extData, GP1File *const gp);
int rule_setTimeUnits(void *applicatorData, void *extData, GP1File *const gp);
int rule_setPreferredType(void *applicatorData, void *extData, GP1File *const gp);
int rule_setFlightInfo(void *applicatorData, void *extData, GP1File *const gp);
int rule_addSampleRate(void *applicatorData, void *extData, GP1File *const gp);
int rule_trimData(void *applicatorData, void *extData, GP1File *const gp);
int rule_trimThreshold(void *applicatorData, void *extData, GP1File *const gp);
int rule_addCreationDate(void *applicatorData, void *extData, GP1File *const gp);
int rule_copyStr(void *applicatorData, void *extData, GP1File *const gp);

//
// Rules
//
int rule_paramRegexChange(Rule const*const rule, GP1File *const gp);
int rule_alwaysApplyGlobal(Rule const*const rule, GP1File *const gp);
int rule_applyToAllParams(Rule const*const rule, GP1File *const gp);

//
// Helpers for rule_paramRegexChange
//
char *rule_getVariableName(Parameter *const param);
char *rule_getUnits(Parameter *const param);

//
// Helpers for rule_copyStr
//
void* rule_makeAttrFromStr(char *const str, void *const data);

//
// Helper functions
//
int set_str(char **dest, size_t *len, char *src);
int rule_apply(Rule const*const rule, void *data, GP1File *const gp);

#endif
