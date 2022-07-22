#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

struct Point
{
	struct Point *left, *right;
	int x;
};

struct Point *insert(struct Point *, const int, int *);
struct Point *delete_point(struct Point *, const int);
struct Point *get_min(struct Point *);
struct Point *exists(struct Point *, const int);
struct Point *next(struct Point *, const int);
struct Point *prev(struct Point *, const int);

int main(void)
{
	struct Point *bst = NULL;
	char *op;
	op = malloc(sizeof(char) * 6);
	int size = 0;
	while (scanf("%s", op) != EOF)
	{
		int v;
		scanf("%i", &v);
		if (strcmp(op, "insert") == 0)
		{
			bst = insert(bst, v, &size);
			continue;
		}
		if (strcmp(op, "delete") == 0)
		{
			bst = delete_point(bst, v);
			size--;
			continue;
		}
		if (strcmp(op, "exists") == 0)
		{
			if (exists(bst, v) != NULL)
			{
				printf("true\n");
			}
			else
			{
				printf("false\n");
			}
			continue;
		}
		if (strcmp(op, "next") == 0)
		{
			if (next(bst, v) != NULL)
			{
				printf("%i\n", next(bst, v)->x);
			}
			else
			{
				printf("none\n");
			}
			continue;
		}
		if (strcmp(op, "prev") == 0)
		{
			if (prev(bst, v) != NULL)
			{
				printf("%i\n", prev(bst, v)->x);
			}
			else
			{
				printf("none\n");
			}
			continue;
		}
	}
	free(bst);
}

struct Point *insert(struct Point *root, const int x, int *size)
{
	if (root != NULL)
	{
		if (root->x > x)
		{
			root->left = insert(root->left, x, size);
		}
		else
		{
			if (root->x < x)
			{
				root->right = insert(root->right, x, size);
			}
		}
	}
	else
	{
		root = malloc(sizeof(struct Point));
		root->left = NULL;
		root->right = NULL;
		root->x = x;
	}
	return root;
}

struct Point *delete_point(struct Point *root, const int x)
{
	if (root != NULL)
	{
		if (root->x > x)
		{
			root->left = delete_point(root->left, x);
		}
		else
		{
			if (root->x < x)
			{
				root->right = delete_point(root->right, x);
			}
			else
			{
				if (root->left != NULL && root->right != NULL)
				{
					root->x = get_min(root->right)->x;
					root->right = delete_point(root->right, root->x);
				}
				else
				{
					if (root->left != NULL)
					{
						root = root->left;
					}
					else
					{
						if (root->right != NULL)
						{
							root = root->right;
						}
						else
						{
							root = NULL;
						}
					}
				}
			}
		}
	}
	return root;
}

struct Point *get_min(struct Point *p)
{
	if (p->left == NULL)
	{
		return p;
	}
	else
	{
		return get_min(p->left);
	}
}

struct Point *exists(struct Point *root, const int x)
{
	if (root != NULL && root->x != x)
	{
		if (root->x > x)
		{
			return exists(root->left, x);
		}
		else
		{
			return exists(root->right, x);
		}
	}
	else
	{
		return root;
	}
}

struct Point *next(struct Point *root, const int x)
{
	struct Point *prev, *find;
	prev = root;
	find = NULL;
	while (prev != NULL)
	{
		if (prev->x > x)
		{
			find = prev;
			prev = prev->left;
		}
		else
		{
			prev = prev->right;
		}
	}
	return find;
}

struct Point *prev(struct Point *root, const int x)
{
	struct Point *prev, *find;
	prev = root;
	find = NULL;
	while (prev != NULL)
	{
		if (prev->x < x)
		{
			find = prev;
			prev = prev->right;
		}
		else
		{
			prev = prev->left;
		}
	}
	return find;
}
