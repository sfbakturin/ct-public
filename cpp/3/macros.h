#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define MALL(TYPE)                                   \
	if (TYPE == 0)                                   \
	{                                                \
		data = new (std::nothrow) int[size]();       \
	}                                                \
	else if (TYPE == 1)                              \
	{                                                \
		data = new (std::nothrow) float[size]();     \
	}                                                \
	else                                             \
	{                                                \
		data = new (std::nothrow) phonebook[size](); \
	}

#define READ(TYPE)                                                                                                  \
	if (TYPE == 0)                                                                                                  \
	{                                                                                                               \
		int *temp = static_cast< int * >(data);                                                                     \
		std::fscanf(fin, "%i", &temp[i]);                                                                           \
	}                                                                                                               \
	else if (TYPE == 1)                                                                                             \
	{                                                                                                               \
		float *temp = static_cast< float * >(data);                                                                 \
		std::fscanf(fin, "%f", &temp[i]);                                                                           \
	}                                                                                                               \
	else                                                                                                            \
	{                                                                                                               \
		phonebook *temp = static_cast< phonebook * >(data);                                                         \
		std::fscanf(fin, "%s %s %s %zu", &(*temp[i].name1), &(*temp[i].name2), &(*temp[i].name3), &(temp[i].numb)); \
	}

#define WRITE(TYPE)                                                                                     \
	if (TYPE == 0)                                                                                      \
	{                                                                                                   \
		int *temp = static_cast< int * >(data);                                                         \
		std::fprintf(fou, "%i\n", temp[i]);                                                             \
	}                                                                                                   \
	else if (TYPE == 1)                                                                                 \
	{                                                                                                   \
		float *temp = static_cast< float * >(data);                                                     \
		std::fprintf(fou, "%g\n", temp[i]);                                                             \
	}                                                                                                   \
	else                                                                                                \
	{                                                                                                   \
		phonebook *temp = static_cast< phonebook * >(data);                                             \
		std::fprintf(fou, "%s %s %s %zu\n", temp[i].name1, temp[i].name2, temp[i].name3, temp[i].numb); \
	}

#define SORT(TYPE)                                                                                     \
	if (TYPE == 0)                                                                                     \
	{                                                                                                  \
		int *temp = static_cast< int * >(data);                                                        \
		(flag ? quicksort< int, true >(temp, size) : quicksort< int, false >(temp, size));             \
	}                                                                                                  \
	else if (TYPE == 1)                                                                                \
	{                                                                                                  \
		float *temp = static_cast< float * >(data);                                                    \
		(flag ? quicksort< float, true >(temp, size) : quicksort< float, false >(temp, size));         \
	}                                                                                                  \
	else                                                                                               \
	{                                                                                                  \
		phonebook *temp = static_cast< phonebook * >(data);                                            \
		(flag ? quicksort< phonebook, true >(temp, size) : quicksort< phonebook, false >(temp, size)); \
	}

#define DELETE(TYPE)                                        \
	if (TYPE == 0)                                          \
	{                                                       \
		int *temp = static_cast< int * >(data);             \
		delete[] temp;                                      \
		data = nullptr;                                     \
	}                                                       \
	else if (TYPE == 1)                                     \
	{                                                       \
		float *temp = static_cast< float * >(data);         \
		delete[] temp;                                      \
		data = nullptr;                                     \
	}                                                       \
	else                                                    \
	{                                                       \
		phonebook *temp = static_cast< phonebook * >(data); \
		delete[] temp;                                      \
		data = nullptr;                                     \
	}