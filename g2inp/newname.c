/*      Copyright University Corporation for Atmospheric Research, 1995   */
/* #include <Xan_general.h> */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#define TRUE 1

#if(0)			/* use version from arc.c instead */
int
hpad(buf)
    char           *buf;

{
    int             i, j;

    for (i = 0; i < 11; i++) {
	if (*(buf + i) == ' ' || *(buf + i) == '\0') {
	    for (j = i; j < 11; j++) {
		*(buf + j) = ' ';
	    }
	    break;
	}
    }
    return;
}
#endif

void
newname_(name)

char *name;

{

    static int	first = TRUE;
    static FILE *name_file;
    static char oldnames[1000][9];
    static char newnames[1000][12];
    static number = 0;
    int i, j, k, nb=16;
    static char buffer[22];
    char testname[9];

    if(first) {
	first = 0;
	if ( (name_file = fopen("./genpro.names", "r")) == NULL ) 
	    return;
	while( number < 1000 && fgets(buffer, nb, name_file) != NULL) {
	    i = 0;
	    j = -1;
	    do {
		if (j >= 0) {
		    newnames[number][j] = buffer[i];
		    if (j > 10 || buffer[i] == ' ') {
			for (k=j; k<11; k++) newnames[number][k] = ' ';
			newnames[number][11] = '\0';
			i = 21;
		    }
		    j++;
		} else {
		    if ((buffer[i] == '=') || (i > 7)) {
			for (k=i; k<8; k++) oldnames[number][k] = ' ';
			oldnames[number][8] = '\0';
			j = 0;
		    } else oldnames[number][i] = buffer[i];
		}
	    } while (++i < 21);
	    number++;
	}
	fclose(name_file);
    }
    if (number <= 0) return;
    strncpy(testname, name, 8);
    i = 0;
    do {
	if (testname[i] == ' ' || testname[i] == '\0') 
	    while (i < 8) testname[i++] = ' ';
    } while (++i < 8);
    for (i=0;i<number;i++) {
        if (!strncmp(oldnames[i], testname, 8)) {
	    sprintf(buffer, "        ->      ");
	    bcopy(testname, buffer, 8);
	    bcopy(newnames[i], buffer+10, 8);
	    printf("replacement %s\n", buffer);
            strncpy(name, newnames[i], 11);
	    hpad(name);
	    return;
	}
    }
    return;
}

