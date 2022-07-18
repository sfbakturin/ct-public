#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

int compare(const int *, const int *);

int main(void)
{
    int n;
    int *array;
    struct input *data;
    scanf("%i", &n);
    array = malloc(sizeof(int) * n);
    for (int i = 0; i < n; i++)
    {
        scanf("%i", &array[i]);
    }
    qsort(array, n, sizeof(int), (int(*) (const void *, const void *)) compare);
    printf("%i\n", n);
    for (int i = 0; i < n - 1; i++)
    {
        printf("%i -1 %i\n", array[i], (i + 2));
    }
    printf("%i -1 -1\n1\n", array[n - 1]);
    free(array);
}

int compare(const int *a, const int *b)
{
    return *a - *b;
}
