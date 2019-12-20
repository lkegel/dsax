#ifndef EXACT_SEARCH_H_
#define EXACT_SEARCH_H_

typedef struct es_struct {
   int name;
   double raw;
   double repr;
   int n_repr;
   int n_raw;
   long el_repr;
   long el_raw;
} es;

typedef struct idx_val
{
    int index;
    double value;
} idx_val;

typedef struct distance {
	double distance;
	long elapsed;
} distance;

struct timespec diff(struct timespec start, struct timespec end);

distance d_ed(void * x, void * y, double * res, double * det, int A_res, int A_det, int T, int W, int L, double bsf);
distance d_sax(void * x, void * y, double * res, double * det, int A, int A_seas, int T, int W, int L, double bsf);
distance d_ssax(void * x, void * y, double * res, double * det, int A_res, int A_seas, int T, int W, int L, double bsf);

void exact_search_sax(char * dataset, unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double),
		double * res, double * det, int query_name, es * result,
		int I, int T, int A_res, int A_det, int W, int L);
void exact_search_sax_hard(char * dataset, unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double),
		double * res, double * det, int query_name, es * result,
		int I, int T, int A_res, int A_det, int W, int L);
void exact_search_ed(
		char * dataset,
		unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, double *, double *, int, int, int, int, int, double),
		double * res,
		double * det,
		int query_name,
		es * result,
		int I, int T, int A_res, int A_det, int W, int L);
void exact_search_store(es * result, char * path, char * fn, int I);
int exact_search_load(es * result, char * path, char * fn, int I);
void exact_search_store_dist(idx_val *id, char * path, char * fn, int I);
#endif /* EXACT_SEARCH_H_ */
