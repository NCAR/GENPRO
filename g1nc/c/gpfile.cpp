
/**
 * @author Nicholas DeCicco
 */

#include <stdlib.h>
#include <string.h>
#include "gpfile.hpp"

/**
 * Searches for the index of a parameter in a GENPRO-1 file.
 *
 * @param gpfile A pointer to the GENPRO-1 file in which to look for the
 *        specified parameter.
 * @param paramName The name of the parameter to search for.
 *
 * @
 */

Parameter *gp1_findParam(GP1File const*const gp, const char paramName[])
{
	int param = -1;
	int i;

	for (i = 0; i < gp->numParameters; i++) {
		if (gp->params[i].isUnused) continue;
		if (strcmp(gp->params[i].label, paramName) == 0) {
			param = i;
			break;
		}
	}

	return (param < 0) ? NULL : gp->params+param;
}
