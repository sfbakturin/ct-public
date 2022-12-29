#pragma once

#include <cstdio>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

void error(char const *text)
{
	std::fprintf(stderr, "ERROR: %s\n", text);
}

void error_file_not_found()
{
	error("The system cannot find the file specified.");
}

void error_not_enough_memory()
{
	error("Not enough memory resources are available to process this command.");
}

void error_arguments_count()
{
	error("The parameter is incorrect.");
}

void error_file_exists()
{
	error("Cannot create a file when that file already exists.");
}

void error_no_such_implementation()
{
	error("This function is not implemented.");
}