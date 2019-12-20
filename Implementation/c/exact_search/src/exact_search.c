#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "dSAX.h"
#include "load.h"
#include "exact_search.h"

int cmp(const void *a, const void *b) {
	idx_val *a1 = (idx_val *) a;
	idx_val *a2 = (idx_val *) b;
    if (a1->value < a2->value)
        return -1;
    else if (a1->value > a2->value)
        return 1;
    else
        return 0;
}

struct timespec diff(struct timespec start, struct timespec end) {
  struct timespec temp;
  if ((end.tv_nsec-start.tv_nsec)<0) {
    temp.tv_sec = end.tv_sec-start.tv_sec-1;
    temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
  } else {
    temp.tv_sec = end.tv_sec-start.tv_sec;
    temp.tv_nsec = end.tv_nsec-start.tv_nsec;
  }
  return temp;
}

distance d_ed(void * x, void * y, double * res, double * seas, int A_res, int A_det, int T, int W, int L, double bsf) {
	double * x_flt = (double *) x;
	double * y_flt = (double *) y;
	double d = 0.0;
	// struct timespec time1, time2;

	// clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

	for (int t = 0; t < T; t++) {
		d += (x_flt[t] - y_flt[t]) * (x_flt[t] - y_flt[t]);
//		if (d > bsf)
//			break; // slows down ED
	}
	// d = sqrt(d);

	// clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

	distance ret;
	ret.distance = d;
	ret.elapsed = 0; //diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

	return(ret);
}

distance d_sax(void * x, void * y, double * res, double * seas, int A, int A_seas, int T, int W, int L, double bsf) {
  unsigned short int * x_uint1 = (unsigned short int *) x;
  unsigned short int * y_uint1 = (unsigned short int *) y;
  double result = 0.0;
  // struct timespec time1, time2;

  // clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int w = 0; w < W; w++) {
	double tmp = res[x_uint1[w] * A + y_uint1[w]];
    result += tmp * tmp;
  }

  result *= T / W;
  // result = sqrt(result);

  // clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

  distance ret;
  ret.distance = result;
  ret.elapsed = 0; //diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

  return ret;
}

distance d_ssax(void * x, void * y, double * res, double * seas, int A_res, int A_seas, int T, int W, int L, double bsf) {
	unsigned short int * x_ush = (unsigned short int *) x;
    unsigned short int * y_ush = (unsigned short int *) y;
	double result = 0.0;
	// struct timespec time1, time2;

    // clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

    double c2_seas, c2_res, c2_seas_inv, c2_res_inv, case1, case2;

    for (int l = 0; l < L; l++) {
    	c2_seas = seas[x_ush[l] * A_seas + y_ush[l]];
    	for (int w = 0; w < W; w++) {
    		c2_res = res[x_ush[w + L] * A_res + y_ush[w + L]];
    		if (c2_seas > -1 * c2_res) {
    			case1 = c2_seas + c2_res;
    			result += case1 * case1;
    		} else {
    			c2_seas_inv = seas[y_ush[l] * A_seas + x_ush[l]];
    			c2_res_inv = res[y_ush[w + L] * A_res + x_ush[w + L]];
				if (c2_seas_inv > -1 * c2_res_inv) {
					case2 = c2_seas_inv + c2_res_inv;
					result += case2 * case2;
				}
    		}
		}
    }

    result *= T / (W * L);
    // result = sqrt(result);

    // clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

    distance ret;
    ret.distance = result;
    ret.elapsed = 0; //diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

    return ret;
}

distance d_ssax_opt(void * x, void * y, double * res, double * seas, int A_res, int A_seas, int T, int W, int L, double bsf) {
	unsigned short int * x_ush = (unsigned short int *) x;
    unsigned short int * y_ush = (unsigned short int *) y;
	double result = 0.0;
	// struct timespec time1, time2;

    // clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

    double c2_seas, c2_res, c2_seas_inv, c2_res_inv;//, case1, case2;
    for (int l = 0; l < L; l++) {
    	c2_seas = seas[x_ush[l] * A_seas + y_ush[l]];
    	for (int w = 0; w < W; w++) {
    		c2_res = res[x_ush[w + L] * A_res + y_ush[w + L]];
    		if (c2_seas > c2_res) {
    			//case1 = ;
    			result += (c2_seas - c2_res) * (c2_seas - c2_res); //case1 * case1;
    		} else {
    			c2_seas_inv = seas[y_ush[l] * A_seas + x_ush[l]];
    			c2_res_inv = res[y_ush[w + L] * A_res + x_ush[w + L]];
				if (c2_seas_inv > c2_res_inv) {
					//case2 = ;
					result += (c2_seas_inv - c2_res_inv) * (c2_seas_inv - c2_res_inv);
				}
    		}
		}
    }

    result *= T / (W * L);
    // result = sqrt(result);

    // clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

    distance ret;
    ret.distance = result;
    ret.elapsed = 0; //diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

    return ret;
}

void exact_search_sax(
		char * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double),
		double * res,
		double * det,
		int query_name,
		es * result,
		int I, int T, int A_res, int A_det, int W, int L) {

	struct timespec time1, time2, time3;
	clock_gettime(CLOCK_REALTIME, &time1);

	void * query_repr = (void *) (repr + query_name * (W + L));

	idx_val * dist_repr = (idx_val *) malloc(I * sizeof(idx_val));
	dist_repr[query_name].index = query_name;
	dist_repr[query_name].value = 0.0;

	distance temp;
	for (int j = 0; j < I; j++) {
		if (query_name == j)
			continue;
		dist_repr[j].index = j;
		temp = (*dist_ptr)(query_repr, (void *) (repr + j * (W + L)), res, det, A_res, A_det, T, W, L, 0.0);
		dist_repr[j].value = temp.distance;
		// el_repr += temp.elapsed;
	}


	qsort(dist_repr, I, sizeof(idx_val), cmp);

	clock_gettime(CLOCK_REALTIME, &time2);

	// Check for identical dist_repr
	if (dist_repr[1].value == dist_repr[2].value) {
		printf("Identical distance\r\n");
	}
	return;

	int nn_name = -1;
	double nn_dist = INFINITY;
	double nn_repr = INFINITY;
	int n_raw = 0;

	double * query_raw = (double *) malloc(sizeof(double) * T);
	double * cand_raw = (double *) malloc(sizeof(double) * T);
	read_series(dataset, query_name, T, query_raw);
	int el_raw = 0;
	for (int j = 1; j < I; j++) {
		int cand_name = dist_repr[j].index;
		double cand_repr = dist_repr[j].value;
		if(cand_repr > nn_dist) {
			break;
		}
		read_series(dataset, cand_name, T, cand_raw);
		distance res = d_ed(query_raw, cand_raw, NULL, NULL, 0, 0, T, 0, 0, nn_dist);

		el_raw += res.elapsed;
		double cand_dist = res.distance;
		n_raw++;

		if (cand_dist < nn_dist) {
		      nn_name = cand_name;
		      nn_dist = cand_dist;
		      nn_repr = cand_repr;
		}
		// printf("%i;%i;%f;%f;%f\n", j, cand_name, cand_repr, cand_dist, nn_dist);
	}
	free(query_raw);
	free(cand_raw);
	free(dist_repr);

	clock_gettime(CLOCK_REALTIME, &time3);

	result->name = nn_name;
	result->raw = nn_dist;
	result->repr = nn_repr;
	result->n_raw = n_raw;
	result->n_repr = I - 1;
	result->el_repr = diff(time1, time2).tv_sec * 1000 + diff(time1, time2).tv_nsec / 1000000;
	result->el_raw = diff(time2, time3).tv_sec * 1000 + diff(time2, time3).tv_nsec / 1000000;

	return;
}

void exact_search_sax_hard(
		char * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double),
		double * res,
		double * det,
		int query_name,
		es * result,
		int I, int T, int A_res, int A_det, int W, int L) {

	struct timespec time1, time2, time3;
	clock_gettime(CLOCK_REALTIME, &time1);

	void * query_repr = (void *) (repr + query_name * (W + L));

	idx_val * dist_repr = (idx_val *) malloc(I * sizeof(idx_val));
	dist_repr[query_name].index = query_name;
	dist_repr[query_name].value = 0.0;

	distance temp;
	for (int j = 0; j < I; j++) {
		if (query_name == j)
			continue;
		dist_repr[j].index = j;
		temp = (*dist_ptr)(query_repr, (void *) (repr + j * (W + L)), res, det, A_res, A_det, T, W, L, 0.0);
		dist_repr[j].value = temp.distance;
		// el_repr += temp.elapsed;
	}


	qsort(dist_repr, I, sizeof(idx_val), cmp);

	clock_gettime(CLOCK_REALTIME, &time2);

	int nn_name = -1;
	double nn_dist = INFINITY;
	double nn_repr = INFINITY;
	int n_raw = 0;

	double * query_raw = (double *) malloc(sizeof(double) * T);
	double * cand_raw = (double *) malloc(sizeof(double) * T);
	read_series(dataset, query_name, T, query_raw);
	int el_raw = 0;
	for (int j = 1; j < I; j++) {
		int cand_name = dist_repr[j].index;
		double cand_repr = dist_repr[j].value;
		if(cand_repr > nn_dist) {
			break;
		}
		read_series(dataset, cand_name, T, cand_raw);
		distance res = d_ed(query_raw, cand_raw, NULL, NULL, 0, 0, T, 0, 0, nn_dist);

		el_raw += res.elapsed;
		double cand_dist = res.distance;
		n_raw++;

		if (cand_dist < nn_dist) {
		      nn_name = cand_name;
		      nn_dist = cand_dist;
		      nn_repr = cand_repr;
		}
		// printf("%i;%i;%f;%f;%f\n", j, cand_name, cand_repr, cand_dist, nn_dist);
	}

	clock_gettime(CLOCK_REALTIME, &time3);

	double maxdist = nn_dist * 1.1;
	int hard = 0;
	for (int j = 1; j < I; j++) {
		if (j % 100000 == 0) printf(".");
		int cand_name = dist_repr[j].index;
		double cand_repr = dist_repr[j].value;
		if(cand_repr > maxdist) {
			break;
		}
		read_series(dataset, cand_name, T, cand_raw);
		distance res = d_ed(query_raw, cand_raw, NULL, NULL, 0, 0, T, 0, 0, nn_dist);
		if (res.distance < maxdist) {
			hard++;
		}
	}

	free(query_raw);
	free(cand_raw);
	free(dist_repr);

	result->name = nn_name;
	result->raw = nn_dist;
	result->repr = nn_repr;
	result->n_raw = hard;
	result->n_repr = I - 1;
	result->el_repr = diff(time1, time2).tv_sec * 1000 + diff(time1, time2).tv_nsec / 1000000;
	result->el_raw = diff(time2, time3).tv_sec * 1000 + diff(time2, time3).tv_nsec / 1000000;

	return;
}

void exact_search_ed(
		char * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double),
		double * res,
		double * det,
		int query_name,
		es * result,
		int I, int T, int A_res, int A_det, int W, int L) {

	struct timespec time1, time2;
	clock_gettime(CLOCK_REALTIME, &time1);

	int nn_name = -1;
	double nn_dist = INFINITY;
	int n_raw = 0;

	double * query_raw = (double *) malloc(sizeof(double) * T);
	double * cand_raw = (double *) malloc(sizeof(double) * T);
	read_series(dataset, query_name, T, query_raw);
	for (int cand_name = 0; cand_name < I; cand_name++) {
		if (cand_name == query_name)
			continue;

		read_series(dataset, cand_name, T, cand_raw);
		distance res = (*dist_ptr)(query_raw, cand_raw, NULL, NULL, 0, 0, T, 0, 0, nn_dist);
		// el_raw += res.elapsed;
		double cand_dist = res.distance;
		n_raw++;

		if (cand_dist < nn_dist) {
		      nn_name = cand_name;
		      nn_dist = cand_dist;
		}
	}

	free(query_raw);
	free(cand_raw);

	clock_gettime(CLOCK_REALTIME, &time2);

	result->name = nn_name;
	result->raw = nn_dist;
	result->repr = 0;
	result->n_raw = n_raw;
	result->n_repr = 0;
	result->el_repr = 0;
	result->el_raw = diff(time1, time2).tv_sec * 1000 + diff(time1, time2).tv_nsec / 1000000;

	return;
}


void exact_search_store(es * result, char * path, char * fn, int I) {
	FILE *fd;
	char fp[1024];

	file_path(fp, path, fn);

	fd = fopen(fp, "w");
	fprintf(fd, "name;raw;repr;n_raw;n_repr;el_repr;el_raw\n");
	for (int i = 0; i < I; i++) {
		fprintf(fd, "%i;%f;%f;%i;%i;%ld;%ld\n",
				result[i].name,
				result[i].raw,
				result[i].repr,
				result[i].n_raw,
				result[i].n_repr,
				result[i].el_repr,
				result[i].el_raw);
	}

	fclose(fd);
}

int exact_search_load(es * result, char * path, char * fn, int I) {
	FILE *fd;
	char fp[1024];

	char puffer[1024];

	file_path(fp, path, fn);

	fd = fopen(fp, "r");

	boolean header = TRUE;
	int i = 0;
	while(fgets(puffer, 1024, fd)) {
		if (header) {
			header = FALSE;
			continue;
		}

		sscanf(puffer, "%i;%lf;%lf;%i;%i;%ld;%ld\n",
				&result[i].name,
				&result[i].raw,
				&result[i].repr,
				&result[i].n_raw,
				&result[i].n_repr,
				&result[i].el_repr,
				&result[i].el_raw);

		i++;
	}

	fclose(fd);

	return i;
}

void exact_search_store_dist(idx_val *id, char * path, char * fn, int I) {
	FILE *fd;
	char fp[1024];

	file_path(fp, path, fn);

	fd = fopen(fp, "w");
	fprintf(fd, "name;value\n");
	for (int i = 0; i < I; i++) {
		fprintf(fd, "%i;%f\n",
				id[i].index,
				id[i].value);
	}

	fclose(fd);
}
