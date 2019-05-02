#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "dSAX.h"
#include "load.h"
#include "exact_search.h"

char *path_dataset;
char *path_res;
char *path_seas;
char *path_repr;
char *path_es;

void (*es_ptr)(float *, unsigned short int *, distance (*dist_ptr)(void *, void *, float *, float *), float *, float *, int, es *);
distance (*dist_ptr)(void *, void *, float *, float *);

int main(int argc, char **argv) {

	struct timespec time1, time2;
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

	path_dataset = argv[1];
	path_res = argv[2];
	path_seas = argv[3];
	path_repr = argv[4];
	path_es = argv[5];

	float *dataset = (float *) malloc(I * T * sizeof(float));
	load_flt(dataset, path_dataset);

	float *res = NULL;
	float *seas = NULL;
	unsigned short int *repr = NULL;

	if (strcmp(argv[6], "ED") == 0) {
		es_ptr = &exact_search_runtime_ed;
		dist_ptr = &d_ed;
	} else if(strcmp(argv[6], "SAX") == 0) {
		es_ptr = &exact_search_runtime;
		dist_ptr = &d_sax;

		res = (float *) malloc(A * A * sizeof(float));
		load_flt(res, path_res);

		repr = (unsigned short int *) malloc(I * W * sizeof(unsigned short int));
		load_ushort(repr, path_repr);
	} else if (strcmp(argv[6], "sSAX") == 0){
		es_ptr = &exact_search_runtime;
		dist_ptr = &d_ssax;

		res = (float *) malloc(A_res * A_res * sizeof(float));
		load_flt(res, path_res);

		seas = (float *) malloc(A_seas * A_seas * sizeof(float));
		load_flt(seas, path_seas);

		repr = (unsigned short int *) malloc(I * (W + L) * sizeof(unsigned short int));
		load_ushort(repr, path_repr);
	} else {
		printf("Unknown representation: %s\n", argv[6]);
		return EXIT_FAILURE;
	}

//	printf("First repr: %u .. %u\n", repr[0], repr[W - 1]);
//	printf("Last repr: %u .. %u\n", repr[(I - 1) * W], repr[I * W - 1]);

	exact_search_runtime_run(dataset, repr, dist_ptr, res, seas, path_es, es_ptr);

	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

	double elapsed = (double) (diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec) / 1000000.0;
	printf("Time elapsed [ms]: %f\n", elapsed);

	return EXIT_SUCCESS;
}
