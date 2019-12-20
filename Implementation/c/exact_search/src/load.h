/*
 * load.h
 *
 *  Created on: 13.04.2019
 *      Author: Lars
 */

#ifndef LOAD_H_
#define LOAD_H_
#include <windows.h>

void load_flt(double *dataset, char *path);
void load_uchar(unsigned char *x, char *path);
void load_ushort(unsigned char *x, char *path);
FILE *open_bin(char *path, char *file_name);
void file_path(char *file_path, char *path, char *file);
void read_series(char *dataset, int i, int T, double *series);
__int64 myFileSeek(HANDLE hf, __int64 distance, DWORD MoveMethod);
void file_path(char *file_path, char *path, char *file);

void read_float8(const char *fp, int size, long pos, double *x);
//void read_uint1(const char *fp, int size, long pos, unsigned char *x);
//void write_uint1(unsigned char *x, const char *fp, int size, const char *mod);
void read_uint2(const char *fp, int size, long pos, unsigned short int *x);
//void write_uint2(unsigned short int *x, const char *fp, int size, const char *mod);

#endif /* LOAD_H_ */
