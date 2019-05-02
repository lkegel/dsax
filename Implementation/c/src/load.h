/*
 * load.h
 *
 *  Created on: 13.04.2019
 *      Author: Lars
 */

#ifndef LOAD_H_
#define LOAD_H_

void load_flt(float *dataset, char *path);
void load_uchar(unsigned char *x, char *path);
void load_ushort(unsigned short int *x, char *path);

#endif /* LOAD_H_ */
