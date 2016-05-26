
#include <stdio.h>

int main()
{
	char ascii[64] =
		":"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"+-*/()$= ,.#[]%\"_!&'?<>@\\^;";
	int i;
	for (i = 0; i < 64; i++) {
		printf("\"%c\" %d, ", ascii[i], i+1);
	}
}
