#include <stdio.h>
#include <stdlib.h>

void load_flt(float *x, char *path) {
	FILE *fp;
	int len = 60;
	float val;
	char *line = (char *) malloc(sizeof(char) * len);

	int i = 0;

	fp = fopen(path, "r");
	if (fp == NULL) {
		printf("File not found\n");
		exit(EXIT_FAILURE);
	}

	while(fgets(line, len, fp) != NULL) {
		val = atof(line);
		x[i++] = (float) val;
	}

	fclose(fp);
	if (line)
		free(line);
}

void load_uchar(unsigned char *x, char *path) {
	FILE *fp;
	int len = 60;
	unsigned char val;
	char *line = (char *) malloc(sizeof(char) * len);

	int i = 0;

	fp = fopen(path, "r");
	if (fp == NULL) {
		printf("File not found\n");
		exit(EXIT_FAILURE);
	}

	while(fgets(line, len, fp) != NULL) {
		val = atoi(line);
		x[i++] = (unsigned char) val;
	}

	fclose(fp);
	if (line)
		free(line);
}

void load_ushort(unsigned short int *x, char *path) {
	FILE *fp;
	int len = 60;
	int val;
	char *line = (char *) malloc(sizeof(char) * len);

	int i = 0;

	fp = fopen(path, "r");
	if (fp == NULL) {
		printf("File not found\n");
		exit(EXIT_FAILURE);
	}

	while(fgets(line, len, fp) != NULL) {
		val = atoi(line);
		x[i++] = (unsigned short int) val;
	}

	fclose(fp);
	if (line)
		free(line);
}
