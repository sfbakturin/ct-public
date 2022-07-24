#include "LN.h"
#include "error.h"
#include "functions.h"
#include "return_codes.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(const int argc, const char **argv)
{
	if (argc != 3)
	{
		message_error_args();
		return ERROR_INVALID_PARAMETER;
	}

	FILE *const in = fopen(argv[1], "r");
	if (in == NULL)
	{
		message_error_file();
		return ERROR_FILE_NOT_FOUND;
	}

	std::stack< LN > expression;
	try
	{
		while (true)
		{
			size_t len = 0, max = 8;
			char *string = (char *)malloc(sizeof(char) * max);
			if (check(string))
			{
				fclose(in);
				throw EXCEPTION_MALLOC;
			}
			while (true)
			{
				const int chr = std::fgetc(in);
				if (chr == EOF || chr == '\n')
				{
					break;
				}
				if (chr != '\r')
				{
					if (len >= max)
					{
						const size_t newSize = max * 2 * sizeof(char);
						char *newString = (char *)realloc(string, sizeof(char) * newSize);
						if (check(newString))
						{
							fclose(in);
							free(string);
							throw EXCEPTION_MALLOC;
						}
						max = newSize;
						string = newString;
					}
					string[len++] = (char)(chr);
				}
			}

			if (len == 0)
			{
				free(string);
				break;
			}

			string[len] = '\0';

			if (isNumeric(string))
			{
				expression.push(LN(string));
				free(string);
			}
			else
			{
				if (std::strcmp("+", string) == 0)
				{
					const LN left(expression.top());
					expression.pop();
					const LN right(expression.top());
					expression.pop();
					const LN answer = right + left;
					expression.push(answer);
					left.~LN();
					right.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("-", string) == 0)
				{
					const LN left(expression.top());
					expression.pop();
					const LN right(expression.top());
					expression.pop();
					const LN answer = right - left;
					expression.push(answer);
					left.~LN();
					right.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("*", string) == 0)
				{
					const LN left(expression.top());
					expression.pop();
					const LN right(expression.top());
					expression.pop();
					const LN answer = right * left;
					expression.push(answer);
					left.~LN();
					right.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("/", string) == 0)
				{
					const LN left(expression.top());
					expression.pop();
					const LN right(expression.top());
					expression.pop();
					const LN answer = right / left;
					expression.push(answer);
					left.~LN();
					right.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("%", string) == 0)
				{
					const LN left(expression.top());
					expression.pop();
					const LN right(expression.top());
					expression.pop();
					const LN answer = right % left;
					expression.push(answer);
					left.~LN();
					right.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("~", string) == 0)
				{
					const LN middle(expression.top());
					expression.pop();
					const LN answer = ~middle;
					expression.push(answer);
					middle.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("_", string) == 0)
				{
					const LN middle(expression.top());
					expression.pop();
					const LN answer = -middle;
					expression.push(answer);
					middle.~LN();
					answer.~LN();
					free(string);
				}
				else if (std::strcmp("+=", string) == 0)
				{
					LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					right += left;
					expression.push(right);
					left.~LN();
					free(string);
				}
				else if (std::strcmp("-=", string) == 0)
				{
					LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					right -= left;
					expression.push(right);
					left.~LN();
					free(string);
				}
				else if (std::strcmp("*=", string) == 0)
				{
					LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					right *= left;
					expression.push(right);
					left.~LN();
					free(string);
				}
				else if (std::strcmp("/=", string) == 0)
				{
					LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					right /= left;
					expression.push(right);
					left.~LN();
					free(string);
				}
				else if (std::strcmp("<", string) == 0)
				{
					const LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					if (left < right)
					{
						expression.push(LN("1"));
					}
					else
					{
						expression.push(LN("0"));
					}
					left.~LN();
					right.~LN();
					free(string);
				}
				else if (std::strcmp("<=", string) == 0)
				{
					const LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					if (left <= right)
					{
						expression.push(LN("1"));
					}
					else
					{
						expression.push(LN("0"));
					}
					left.~LN();
					right.~LN();
					free(string);
				}
				else if (std::strcmp(">", string) == 0)
				{
					const LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					if (left > right)
					{
						expression.push(LN("1"));
					}
					else
					{
						expression.push(LN("0"));
					}
					left.~LN();
					right.~LN();
					free(string);
				}
				else if (std::strcmp(">=", string) == 0)
				{
					const LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					if (left >= right)
					{
						expression.push(LN("1"));
					}
					else
					{
						expression.push(LN("0"));
					}
					left.~LN();
					right.~LN();
					free(string);
				}
				else if (std::strcmp("==", string) == 0)
				{
					const LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					if (left == right)
					{
						expression.push(LN("1"));
					}
					else
					{
						expression.push(LN("0"));
					}
					left.~LN();
					right.~LN();
					free(string);
				}
				else if (std::strcmp("!=", string) == 0)
				{
					const LN right(expression.top());
					expression.pop();
					const LN left(expression.top());
					expression.pop();
					if (left != right)
					{
						expression.push(LN("1"));
					}
					else
					{
						expression.push(LN("0"));
					}
					left.~LN();
					right.~LN();
					free(string);
				}
				else
				{
					free(string);
					break;
				}
			}
		}
	} catch (const int x)
	{
		switch (x)
		{
		case EXCEPTION_MALLOC:
		{
			message_error_memo();
			return ERROR_NOT_ENOUGH_MEMORY;
		}
		case EXCEPTION_LONG_LONG:
		{
			message_error_long();
			return ERROR_UNKNOWN;
		}
		}
	} catch (...)
	{
		message_error_unkn();
		return ERROR_UNKNOWN;
	}

	fclose(in);

	FILE *const out = fopen(argv[2], "w");
	if (check(out))
	{
		message_error_memo();
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	const size_t it = expression.size();
	for (size_t i = 0; i < it; i++)
	{
		const LN elem(expression.top());
		/*expression.top().~LN();*/
		expression.pop();
		elem.toString(out);
	}

	fclose(out);

	return ERROR_SUCCESS;
}
