#ifndef EXACT_SEARCH_H_
#define EXACT_SEARCH_H_

typedef struct es_struct {
   int name;
   float raw;
   float repr;
   int n_repr;
   int n_raw;
   int el_repr;
   int el_raw;
} es;

typedef struct idx_val
{
    int index;
    float value;
} idx_val;

typedef struct distance {
	float distance;
	int elapsed;
} distance;

struct timespec diff(struct timespec start, struct timespec end);

distance d_ed(void * x, void * y, float * res, float * seas);
distance d_sax(void * x, void * y, float * res, float * seas);
distance d_ssax(void * x, void * y, float * res, float * seas);

void exact_search_runtime_run(float * dataset, unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, float *, float *),
		float * res, float * seas, char * path_es,
		void (*es_ptr)(float *, unsigned short int *, distance (*dist_ptr)(void *, void *, float *, float *), float *, float *, int, es *));
void exact_search_runtime(float * dataset, unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, float *, float *),
		float * res, float * seas, int query_name, es * result);
void exact_search_runtime_ed(float * dataset, unsigned short int * repr,
		distance (*dist_ptr)(void *, void *, float *, float *),
		float * res, float * seas, int query_name, es * result);
void exact_search_runtime_store(es * result, char * path);

#endif /* EXACT_SEARCH_H_ */
