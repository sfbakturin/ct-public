#include <stdio.h>
#include <stdint.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int c[2001];
uint64_t coef[2001], numt[2001];

int main(void)
{
	int k, m, modulus = 1000000000 + 7;
	scanf("%i %i", &k, &m);
	for (int i = 0; i < k; i++)
	{
		scanf("%i", &c[i]);
	}
	coef[0] = numt[0] = 1;
	for (int i = 1; i <= m; i++)
	{
		for (int j = 0; j < k; j++)
		{
			numt[i] = (numt[i] + ((i - c[j]) >= 0 ? coef[i - c[j]] : 0)) % modulus;
		}
		for (int j = 0; j <= i; j++)
		{
			coef[i] = (coef[i] + (numt[j] * (numt[i - j])) % modulus) % modulus;
		}
	}
	for (int i = 1; i <= m; i++)
	{
		printf("%lu ", numt[i]);
	}
}
