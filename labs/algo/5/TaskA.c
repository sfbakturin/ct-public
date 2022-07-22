#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

int flag = 1;

struct input
{
    int value;
    int left;
    int right;
};

void check(const struct input *, const int);

int main(void)
{
    struct input *data;
    int n, root;
    scanf("%i", &n);
    data = malloc(sizeof(struct input) * (n + 1));
    for (int i = 1; i <= n; i++)
    {
        int v, l, r;
        scanf("%i %i %i", &v, &l, &r);
        struct input input = {.value = v, .left = l, .right = r};
        data[i] = input;
    }
    scanf("%i", &root);
    check(data, root);
    if (flag)
    {
        printf("YES\n");
    }
    else
    {
        printf("NO\n");
    }
    free(data);
}

void check(const struct input *array, const int root)
{
    if (array[root].left == -1 && array[root].right == -1)
    {
        return;
    }

    if (array[root].left == -1)
    {
        if (array[root].value < array[array[root].right].value)
        {
            check(array, array[root].right);
            return;
        }
        else
        {
            flag = 0;
            return;
        }
    }

    if (array[root].right == -1)
    {
        if (array[array[root].left].value < array[root].value)
        {
            check(array, array[root].left);
            return;
        }
        else
        {
            flag = 0;
            return;
        }
    }

    if (array[array[root].left].value < array[root].value &&
    array[root].value < array[array[root].right].value)
    {
        check(array, array[root].left);
        check(array, array[root].right);
        return;
    }
    else
    {
        flag = 0;
        return;
    }
}
