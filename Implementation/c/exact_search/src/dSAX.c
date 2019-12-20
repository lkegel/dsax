#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "dSAX.h"
#include "load.h"
#include "exact_search.h"

int main(int argc, char **argv) {

	struct timespec time1, time2;
	clock_gettime(CLOCK_REALTIME, &time1);

	char *path = argv[1];
	char *fn_dataset = argv[2];
	char *fn_res = argv[3];
	char *fn_det = argv[4];
	char *fn_repr = argv[5];
	char *fn_es = argv[6];
	char *dist = argv[7];
	int I = atoi(argv[8]);
	int T = atoi(argv[9]);
	int A_res = atoi(argv[10]);
	int A_det = atoi(argv[11]);
	int	W = atoi(argv[12]);
	int L = atoi(argv[13]);
	int Q = (argc - 14);

	void (*es_ptr)(char *, unsigned short int *, distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double), double *, double *, int, es *, int, int, int, int, int, int);
	distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double);

	char dataset[1024], fp_res[1024], fp_det[1024], fp_repr[1024];
	file_path(dataset, path, fn_dataset);
	file_path(fp_res, path, fn_res);
	file_path(fp_det, path, fn_det);
	file_path(fp_repr, path, fn_repr);

	// FILE *dataset = open_bin(path, fn_dataset);

	double *res = NULL;
	double *det = NULL;
	unsigned short int *repr = NULL;

	if (strcmp(dist, "ED") == 0) {
		es_ptr = &exact_search_ed;
		dist_ptr = &d_ed;
	} else if (strcmp(dist, "SAX") == 0) {
		es_ptr = &exact_search_sax;
		dist_ptr = &d_sax;

		res = (double *) malloc(A_res * A_res * sizeof(double));
		read_float8(fp_res, A_res * A_res, 0, res);

		repr = (unsigned short int *) malloc(I * W * sizeof(unsigned short int));
		read_uint2(fp_repr, I * W, 0, repr);
	} else if (strcmp(dist, "sSAX") == 0){
		es_ptr = &exact_search_sax;
		dist_ptr = &d_ssax;

		res = (double *) malloc(A_res * A_res * sizeof(double));
		read_float8(fp_res, A_res * A_res, 0, res);
//		for (int i = 0; i < A_res * A_res; i++) {
//			res[i] = -1 * res[i];
//		}

		det = (double *) malloc(A_det * A_det * sizeof(double));
		read_float8(fp_det, A_det * A_det, 0, det);

		repr = (unsigned short int *) malloc(I * (W + L) * sizeof(unsigned short int));
		read_uint2(fp_repr, I * (W + L), 0, repr);
	} else if (strcmp(dist, "SAXhard") == 0) {
		es_ptr = &exact_search_sax_hard;
		dist_ptr = &d_sax;

		res = (double *) malloc(A_res * A_res * sizeof(double));
		read_float8(fp_res, A_res * A_res, 0, res);

		repr = (unsigned short int *) malloc(I * W * sizeof(unsigned short int));
		read_uint2(fp_repr, I * W, 0, repr);
	} else {
		printf("Unknown representation: %s\n", argv[6]);
		return EXIT_FAILURE;
	}

//	printf("First repr: %u .. %u\n", repr[0], repr[W - 1]);
//	printf("Last repr: %u .. %u\n", repr[(I - 1) * W], repr[I * W - 1]);

	int *Qs = (int*) malloc(sizeof(int) * Q);
	int q;
	for (int q = 0; q < Q; q++) {
		Qs[q] = atoi(argv[14 + q]);
	}
	es *result = (es *) malloc(I * sizeof(es));
	for (int i = 0; i < Q; i++) {
		printf(".");
		fflush(stdout);
		q = Qs[i];
		(*es_ptr)(dataset, repr, dist_ptr, res, det, q, result + i, I, T, A_res, A_det, W, L);
	}
	exact_search_store(result, path, fn_es, Q);
	clock_gettime(CLOCK_REALTIME, &time2);

	double elapsed = (double) (diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec) / 1000000.0;
	printf("Time elapsed [ms]: %f\n", elapsed);

	// fclose(dataset);

	return EXIT_SUCCESS;
}
