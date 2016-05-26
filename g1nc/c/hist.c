
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
	char ascii[64] =
		":"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"+-*/()$= ,.#[]%\"_!&'?<>@\\^;";
	char rev_ascii[256];
	int i, c;
	int histogram[64];
	FILE *fp;

	if (argc == 1) {
		fp = stdin;
	} else if (argc == 2) {
		if (!(fp = fopen(argv[1], "r"))) {
			fprintf(stderr, "Failed to open \"%s\" for reading.\n", argv[1]);
			exit(1);
		}
	} else {
		fprintf(stderr, "Error: unexpected arguments.\n");
	}

	memset(rev_ascii, 0, sizeof(char)*256);
	for (i = 0; i < 64; i++) {
		rev_ascii[(int) ascii[i]] = i;
	}

	memset(histogram, 0, sizeof(int)*64);

	while ((c = getc(fp)) != EOF) {
		histogram[(int) rev_ascii[c]]++;
	}

	for (i = 0; i < 64; i++) {
		printf("%d %d\n", i+1, histogram[i]);
	}

	return 0;
}
