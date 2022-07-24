#include "return_codes.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int check(const void* p);

void message(const char[], const char[]);
void message_error_memory();
void message_error_file();
void message_error_png();
void message_error_writer();
void message_error_spec();
void message_error_corrupted();

void free_2ptr(unsigned char* ptr_1, unsigned char* ptr_2);
void free_3ptr(unsigned char* ptr_1, unsigned char* ptr_2, unsigned char* ptr_3);
void free_pix(size_t h, void** arr);

int next(FILE* file, unsigned char* buf);
size_t shift(size_t num, unsigned char, int i);
int compare(const unsigned char* arr, const unsigned char* buf);
int pgm_start(FILE* file, size_t width, size_t height);
int skip(unsigned char* buffer, size_t chunk_size, FILE* in);
int average(int AVERAGE, int RAW, int PRIOR);
int paeth_predictor(int a, int b, int c);
