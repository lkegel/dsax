#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "dSAX.h"
#include "exact_search.h"

void exact_search_runtime_run(
		float * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, float *, float *),
		float * res, float * seas, char * path_es,
		void (*es_ptr)(float *, unsigned short int *, distance (*dist_ptr)(void *, void *, float *, float *), float *, float *, int, es *)) {
	es *result = (es *) malloc(I * sizeof(es));

	for (int i = 0; i < I; i++) {
		(*es_ptr)(dataset, repr, dist_ptr, res, seas, i, result + i);
	}

	exact_search_runtime_store(result, path_es);

	free(result);

	return;
}

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

distance d_ed(void * x, void * y, float * res, float * seas) {
	float * x_flt = (float *) x;
	float * y_flt = (float *) y;
	float d = 0.0;
	struct timespec time1, time2;

	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

	for (int t = 0; t < T; t++) {
		d += (x_flt[t] - y_flt[t]) * (x_flt[t] - y_flt[t]);
	}
	d = sqrt(d);

	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

	distance ret;
	ret.distance = d;
	ret.elapsed = diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

	return(ret);
}

distance d_sax(void * x, void * y, float * res, float * seas) {
  unsigned short int * x_ush = (unsigned short int *) x;
  unsigned short int * y_ush = (unsigned short int *) y;
  float result = 0.0;
  struct timespec time1, time2;

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

  for (int w = 0; w < W; w++) {
	float tmp = res[x_ush[w] * A + y_ush[w]];
    result += tmp * tmp;
  }

  result *= T / W;
  result = sqrt(result);

  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

  distance ret;
  ret.distance = result;
  ret.elapsed = diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

  return ret;
}

distance d_ssax(void * x, void * y, float * res, float * seas) {
	unsigned short int * x_ush = (unsigned short int *) x;
    unsigned short int * y_ush = (unsigned short int *) y;
	float result = 0.0;
	struct timespec time1, time2;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time1);

    float c2_seas, c2_res, c2_seas_inv, c2_res_inv, case1, case2;

    for (int l = 0; l < L; l++) {
    	c2_seas = seas[x_ush[l] * A_seas + y_ush[l]];
    	c2_seas_inv = seas[y_ush[l] * A_seas + x_ush[l]];
    	for (int w = 0; w < W; w++) {
    		c2_res = res[x_ush[w + L] * A_res + y_ush[w + L]];
    		if (c2_seas > -1 * c2_res) {
    			case1 = c2_seas + c2_res;
    			result += case1 * case1;
    		} else {
    			c2_res_inv = res[y_ush[w + L] * A_res + x_ush[w + L]];
				if (c2_seas_inv > -1 * c2_res_inv) {
					case2 = c2_seas_inv + c2_res_inv;
					result += case2 * case2;
				}
    		}
		}
    }

    result = sqrt(result);

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time2);

    distance ret;
    ret.distance = result;
    ret.elapsed = diff(time1, time2).tv_sec * 1000000000 + diff(time1, time2).tv_nsec;

    return ret;
}

void exact_search_runtime(
		float * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, float *, float *),
		float * res,
		float * seas,
		int query_name,
		es * result) {
	printf("Query %i\n", query_name);

	void * query_repr = (void *) (repr + query_name * WL);
	int el_repr =  0;

	idx_val * dist_repr = (idx_val *) malloc(I * sizeof(idx_val));
	dist_repr[query_name].index = query_name;
	dist_repr[query_name].value = 0.0;

	distance temp;
	for (int j = 0; j < I; j++) {
		if (query_name == j)
			continue;
		dist_repr[j].index = j;
		temp = (*dist_ptr)(query_repr, (void *) (repr + j * WL), res, seas);
		dist_repr[j].value = temp.distance;
		el_repr += temp.elapsed;
	}

	qsort(dist_repr, I, sizeof(idx_val), cmp);

	int nn_name;
	float nn_dist = INFINITY;
	float nn_repr;
	int n_raw = 0;

	int el_raw = 0;
	for (int j = 1; j < I; j++) {
		int cand_name = dist_repr[j].index;
		float cand_repr = dist_repr[j].value;
		if(cand_repr > nn_dist) {
			break;
		}

		void * query_raw = dataset + query_name * T;
		void * cand_raw = dataset + cand_name * T;
		distance res = d_ed(query_raw, cand_raw, NULL, NULL);
		el_raw += res.elapsed;
		float cand_dist = res.distance;
		n_raw++;

		if (cand_dist < nn_dist) {
		      nn_name = cand_name;
		      nn_dist = cand_dist;
		      nn_repr = cand_repr;
		}
	}

	free(dist_repr);

	result->name = nn_name;
	result->raw = nn_dist;
	result->repr = nn_repr;
	result->n_raw = n_raw;
	result->n_repr = I - 1;
	result->el_repr = el_repr;
	result->el_raw = el_raw;

	return;
}

void exact_search_runtime_ed(
		float * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, float *, float *),
		float * res,
		float * seas,
		int query_name,
		es * result) {
	printf("Query %i\n", query_name);

	int nn_name;
	float nn_dist = INFINITY;
	int n_raw = 0;

	void * query_raw = (void *) (dataset + query_name * T);
	int el_raw = 0;
	for (int cand_name = 0; cand_name < I; cand_name++) {
		if (cand_name== query_name)
			continue;

		void * cand_raw = (void *) (dataset + cand_name * T);
		distance res = (*dist_ptr)(query_raw, cand_raw, NULL, NULL);
		el_raw += res.elapsed;
		float cand_dist = res.distance;
		n_raw++;

		if (cand_dist < nn_dist) {
		      nn_name = cand_name;
		      nn_dist = cand_dist;
		}
	}

	result->name = nn_name;
	result->raw = nn_dist;
	result->repr = 0;
	result->n_raw = n_raw;
	result->n_repr = 0;
	result->el_repr = 0;
	result->el_raw = el_raw;

	return;
}


void exact_search_runtime_store(es * result, char * path) {
	FILE *fp;

	fp = fopen(path, "w");
	fprintf(fp, "name;raw;repr;n_raw;n_repr;el_repr;el_raw\n");
	for (int i = 0; i < I; i++) {
		fprintf(fp, "%i;%f;%f;%i;%i;%i;%i\n",
				result[i].name,
				result[i].raw,
				result[i].repr,
				result[i].n_raw,
				result[i].n_repr,
				result[i].el_repr,
				result[i].el_raw);
	}

	fclose(fp);
}
