#pragma once

#include <string_view>

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <stack>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

void message(const char[]);
void message_error_args();
void message_error_file();
void message_error_memo();
void message_error_unkn();
void message_error_long();
bool check(const void *);
bool isNumeric(const char *);
