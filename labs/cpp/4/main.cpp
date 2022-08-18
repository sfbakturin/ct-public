#include "LN.h"
#include "error_message.h"
#include "functions.h"
#include "macros.h"
#include "return_codes.h"
#include "return_errors.h"

#include <fstream>
#include <iostream>
#include <map>
#include <stack>
#include <string>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(int const argc, char const **argv)
{
	std::stack< LN > expression;
	std::FILE *fou;
	std::map< std::string, void (*)(std::stack< LN > &) > function{
		{ "+", &add },	   { "-", &sub },		   { "*", &mul },	  { "/", &div },
		{ "+=", &uadd },   { "-=", &usub },		   { "*=", &umul },	  { "/=", &udiv },
		{ "<", &less },	   { "<=", &less_equals }, { "==", &equals }, { ">=", &greater_equals },
		{ ">", &greater }, { "!=", &not_equals },  { "_", &minus },	  { "~", &sqrt },
		{ "%", &mod }
	};
	if (argc != 3)
	{
		error_arguments_count();
		return ERROR_INVALID_PARAMETER;
	}
	std::ifstream fin(argv[1]);
	if (!fin.is_open())
	{
		error_file_not_found();
		return ERROR_FILE_NOT_FOUND;
	}
	std::string line;
	while (std::getline(fin, line))
	{
		try
		{
			if (EXIST(line))
			{
				function[line](expression);
			}
			else
			{
				expression.emplace(line);
			}
		} catch (int const err)
		{
			switch (err)
			{
			case ERROR_LONG_LONG:
			{
				error_long_long();
				fin.close();
				return ERROR_INVALID_DATA;
			}
			case ERROR_MEMORY_LEAK:
			{
				error_not_enough_memory();
				fin.close();
				return ERROR_NOT_ENOUGH_MEMORY;
			}
			default:
			{
				error_unknown();
				fin.close();
				return ERROR_UNKNOWN;
			}
			}
		} catch (...)
		{
			error_unknown();
			fin.close();
			return ERROR_UNKNOWN;
		}
	}
	fin.close();
	fou = std::fopen(argv[2], "w");
	if (!fou)
	{
		error_file_exists();
		return ERROR_FILE_EXISTS;
	}
	while (!expression.empty())
	{
		UNARY;
		unary.print(fou);
	}
	std::fclose(fou);
}
