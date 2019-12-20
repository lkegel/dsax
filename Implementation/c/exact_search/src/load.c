#include <stdio.h>
#include <stdlib.h>

#include "load.h"

//void load_flt(double *x, char *path) {
//	FILE *fp;
//	int len = 60;
//	double val;
//	char *line = (char *) malloc(sizeof(char) * len);
//
//	int i = 0;
//
//	fp = fopen(path, "r");
//	if (fp == NULL) {
//		printf("File not found\n");
//		exit(EXIT_FAILURE);
//	}
//
//	while(fgets(line, len, fp) != NULL) {
//		val = atof(line);
//		x[i++] = (double) val;
//	}
//
//	fclose(fp);
//	if (line)
//		free(line);
//}

void read_float8(const char *fp, int size, long pos, double *x) {
	FILE *fd;

	fd = fopen(fp, "rb");
	if (fp == NULL) {
		printf("File not found\n");
		exit(EXIT_FAILURE);
	}

	if (pos > 0) {
		long offset = pos * sizeof(double);
		fseek(fd, offset, SEEK_SET);
	}
	fread(x, sizeof(double), size, fd);
	fclose(fd);
}

//void read_uint1(const char *fp, int size, long pos, unsigned char *x) {
//	FILE *fd;
//
//	fd = fopen(fp, "rb");
//	if (fd == NULL) {
//			printf("File not found\n");
//			exit(EXIT_FAILURE);
//		}
//
//	if (pos > 0) {
//		long offset = pos * sizeof(unsigned char);
//		fseek(fd, offset, SEEK_SET);
//	}
//	fread(x, sizeof(unsigned char), size, fd);
//	fclose(fd);
//}
//
//void write_uint1(unsigned char *x, const char *fp, int size, const char *mod) {
//  FILE *fd = fopen(fp, mod);
//  if (fp == NULL) {
//	  printf("File not found\n");
//	  exit(EXIT_FAILURE);
//  }
//
//  fwrite(x, sizeof(unsigned char), size, fd);
//  fclose(fd);
//}

void read_uint2(const char *fp, int size, long pos, unsigned short int *x) {
	FILE *fd;

	fd = fopen(fp, "rb");
	if (fd == NULL) {
			printf("File not found\n");
			exit(EXIT_FAILURE);
		}

	if (pos > 0) {
		long offset = pos * sizeof(unsigned short int);
		fseek(fd, offset, SEEK_SET);
	}
	fread(x, sizeof(unsigned short int), size, fd);
	fclose(fd);
}

//void write_uint2(unsigned short int *x, const char *fp, int size, const char *mod) {
//  FILE *fd = fopen(fp, mod);
//  if (fp == NULL) {
//	  printf("File not found\n");
//	  exit(EXIT_FAILURE);
//  }
//
//  fwrite(x, sizeof(unsigned short int), size, fd);
//  fclose(fd);
//}

//void load_ushort(unsigned char *x, char *path) {
//	FILE *fp;
//	int len = 60;
//	int val;
//	char *line = (char *) malloc(sizeof(char) * len);
//
//	int i = 0;
//
//	fp = fopen(path, "r");
//	if (fp == NULL) {
//		printf("File not found\n");
//		exit(EXIT_FAILURE);
//	}
//
//	while(fgets(line, len, fp) != NULL) {
//		val = atoi(line);
//		x[i++] = (unsigned char) val;
//	}
//
//	fclose(fp);
//	if (line)
//		free(line);
//}

FILE *open_bin(char *path, char *filename) {
    char fp[1024];
    file_path(fp, path, filename);
    FILE *fd = fopen(fp, "rb");

    if (fd == NULL) {
    	printf("File not found: %s\n", fp);
    	exit(EXIT_FAILURE);
    }

    return(fd);
}

//void read_series_old(char *dataset, int i, int T, double *series) {
//	FILE *fd = fopen(dataset, "rb");
//
//	__int64 offset = (__int64) i * (__int64) T * sizeof(double);
//	int ret = _fseeki64(fd, offset, SEEK_SET);
//	if (ret != 0) {
//		perror("fseek was not successful");
//		exit(EXIT_FAILURE);
//	}
//	fread(series, sizeof(double), T, fd);
//
//	fclose(fd);
//}

void read_series(char *dataset, int i, int T, double *series) {
	HANDLE hFile = CreateFile(
			dataset,                // name of the write
            GENERIC_READ,           // open for writing
            0,                      // do not share
            NULL,                   // default security
			OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_NO_BUFFERING,  // normal file
            NULL);                  // no attr. template

	__int64 offset = (__int64) i * (__int64) T * sizeof(double);
	myFileSeek(hFile, offset, FILE_BEGIN);

	ReadFile(
				hFile,
				series,
				sizeof(double) * T,
				NULL,
				NULL);

	CloseHandle(hFile);
}

__int64 myFileSeek(HANDLE hf, __int64 distance, DWORD MoveMethod)
{
   LARGE_INTEGER li;

   li.QuadPart = distance;

   li.LowPart = SetFilePointer (hf,
                                li.LowPart,
                                &li.HighPart,
                                MoveMethod);

   if (li.LowPart == INVALID_SET_FILE_POINTER && GetLastError()
       != NO_ERROR)
   {
      li.QuadPart = -1;
   }

   return li.QuadPart;
}

void file_path(char *file_path, char *path, char *file) {
#ifdef __linux__
    char sep = '/';
#elif _WIN32
    char sep = '\\';
#endif

    sprintf(file_path, "%s%c%s", path, sep, file);
}
