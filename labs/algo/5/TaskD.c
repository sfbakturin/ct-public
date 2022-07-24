#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct Point
{
	struct Point *left, *right;
	int x, h, hl, hr;
};

struct Point *insert(struct Point *, const int);
struct Point *delete_point(struct Point *, const int);
struct Point *get_min(struct Point *);
struct Point *exists(struct Point *, const int);
struct Point *next(struct Point *, const int);
struct Point *prev(struct Point *, const int);
struct Point *rotateLeft(struct Point *);
struct Point *rotateRight(struct Point *);
struct Point *bigRotateLeft(struct Point *);
struct Point *bigRotateRight(struct Point *);
int recalcHeight(const struct Point *root);
int max(const int, const int);
void printTree(const struct Point *);

int main(void)
{
	struct Point *bst = NULL;
	char *op;
	op = malloc(sizeof(char) * 6);
	while (scanf("%s", op) != EOF)
	{
		int v;
		scanf("%i", &v);
		if (strcmp(op, "print") == 0)
		{
			printTree(bst);
			continue;
		}
		if (strcmp(op, "head") == 0)
		{
			printf("ROOT HEAD is %i\n", bst->x);
			continue;
		}
		if (strcmp(op, "height") == 0)
		{
			printf("ROOT HEIGHT is %i\n", bst->h);
			continue;
		}
		if (strcmp(op, "insert") == 0)
		{
			bst = insert(bst, v);
			continue;
		}
		if (strcmp(op, "delete") == 0)
		{
			bst = delete_point(bst, v);
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

struct Point *insert(struct Point *root, const int x)
{
	if (root != NULL)
	{
		if (root->x > x)
		{
			root->left = insert(root->left, x);

			root->hl = recalcHeight(root->left);
			root->hr = recalcHeight(root->right);
			root->h = max(root->hl, root->hr);

			if (root->hr >= root->hl + 2)
			{
				if (root->right->right != NULL)
				{
					root = rotateLeft(root);
				}
				else
				{
					if (root->right->left != NULL)
					{
						root = bigRotateLeft(root);
					}
				}
			}

			if (root->hl >= root->hr + 2)
			{
				if (root->left->left != NULL)
				{
					root = rotateRight(root);
				}
				else
				{
					if (root->left->right != NULL)
					{
						root = bigRotateRight(root);
					}
				}
			}
		}
		else
		{
			if (root->x < x)
			{
				root->right = insert(root->right, x);

				root->hl = recalcHeight(root->left);
				root->hr = recalcHeight(root->right);
				root->h = max(root->hl, root->hr);

				if (root->hr >= root->hl + 2)
				{
					if (root->right->right != NULL)
					{
						root = rotateLeft(root);
					}
					else
					{
						if (root->right->left != NULL)
						{
							root = bigRotateLeft(root);
						}
					}
				}

				if (root->hl >= root->hr + 2)
				{
					if (root->left->left != NULL)
					{
						root = rotateRight(root);
					}
					else
					{
						if (root->left->right != NULL)
						{
							root = bigRotateRight(root);
						}
					}
				}
			}
		}
	}
	else
	{
		root = malloc(sizeof(struct Point));
		root->left = NULL;
		root->right = NULL;
		root->x = x;
		root->hl = 0;
		root->hr = 0;
		root->h = 0;
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

			root->hl = recalcHeight(root->left);
			root->hr = recalcHeight(root->right);
			root->h = max(root->hl, root->hr);

			if (root->hr >= root->hl + 2)
			{
				if (root->right->right != NULL)
				{
					root = rotateLeft(root);
				}
				else
				{
					if (root->right->left != NULL)
					{
						root = bigRotateLeft(root);
					}
				}
			}

			if (root->hl >= root->hr + 2)
			{
				if (root->left->left != NULL)
				{
					root = rotateRight(root);
				}
				else
				{
					if (root->left->right != NULL)
					{
						root = bigRotateRight(root);
					}
				}
			}
		}
		else
		{
			if (root->x < x)
			{
				root->right = delete_point(root->right, x);

				root->hl = recalcHeight(root->left);
				root->hr = recalcHeight(root->right);
				root->h = max(root->hl, root->hr);

				if (root->hr >= root->hl + 2)
				{
					if (root->right->right != NULL)
					{
						root = rotateLeft(root);
					}
					else
					{
						if (root->right->left != NULL)
						{
							root = bigRotateLeft(root);
						}
					}
				}

				if (root->hl >= root->hr + 2)
				{
					if (root->left->left != NULL)
					{
						root = rotateRight(root);
					}
					else
					{
						if (root->left->right != NULL)
						{
							root = bigRotateRight(root);
						}
					}
				}
			}
			else
			{
				if (root->left != NULL && root->right != NULL)
				{
					root->x = get_min(root->right)->x;
					root->right = delete_point(root->right, root->x);

					root->hl = recalcHeight(root->left);
					root->hr = recalcHeight(root->right);
					root->h = max(root->hl, root->hr);

					if (root->hr >= root->hl + 2)
					{
						if (root->right->right != NULL)
						{
							root = rotateLeft(root);
						}
						else
						{
							if (root->right->left != NULL)
							{
								root = bigRotateLeft(root);
							}
						}
					}

					if (root->hl >= root->hr + 2)
					{
						if (root->left->left != NULL)
						{
							root = rotateRight(root);
						}
						else
						{
							if (root->left->right != NULL)
							{
								root = bigRotateRight(root);
							}
						}
					}
				}
				else
				{
					if (root->left != NULL)
					{
						root = root->left;

						root->hl = recalcHeight(root->left);
						root->hr = recalcHeight(root->right);
						root->h = max(root->hl, root->hr);

						if (root->hr >= root->hl + 2)
						{
							if (root->right->right != NULL)
							{
								root = rotateLeft(root);
							}
							else
							{
								if (root->right->left != NULL)
								{
									root = bigRotateLeft(root);
								}
							}
						}

						if (root->hl >= root->hr + 2)
						{
							if (root->left->left != NULL)
							{
								root = rotateRight(root);
							}
							else
							{
								if (root->left->right != NULL)
								{
									root = bigRotateRight(root);
								}
							}
						}
					}
					else
					{
						if (root->right != NULL)
						{
							root = root->right;

							root->hl = recalcHeight(root->left);
							root->hr = recalcHeight(root->right);
							root->h = max(root->hl, root->hr);

							if (root->hr >= root->hl + 2)
							{
								if (root->right->right != NULL)
								{
									root = rotateLeft(root);
								}
								else
								{
									if (root->right->left != NULL)
									{
										root = bigRotateLeft(root);
									}
								}
							}

							if (root->hl >= root->hr + 2)
							{
								if (root->left->left != NULL)
								{
									root = rotateRight(root);
								}
								else
								{
									if (root->left->right != NULL)
									{
										root = bigRotateRight(root);
									}
								}
							}
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

struct Point *rotateLeft(struct Point *root)
{
	struct Point *temp = root->right;
	root->right = temp->left;
	temp->left = root;
	return temp;
}

struct Point *rotateRight(struct Point *root)
{
	struct Point *temp = root->left;
	root->left = temp->right;
	temp->right = root;
	return temp;
}

struct Point *bigRotateLeft(struct Point *root)
{
	root->right = rotateRight(root->right);
	return rotateLeft(root);
}

struct Point *bigRotateRight(struct Point *root)
{
	root->left = rotateLeft(root->left);
	return rotateRight(root);
}

int recalcHeight(const struct Point *root)
{
	if (root == NULL)
	{
		return 0;
	}

	if (root->left == NULL && root->right == NULL)
	{
		return 1;
	}

	if (root->right == NULL && root->left != NULL)
	{
		return root->left->h + 1;
	}

	if (root->right != NULL && root->left == NULL)
	{
		return root->right->h + 1;
	}

	return max(root->right->h, root->left->h) + 1;
}

int max(const int a, const int b)
{
	return (a < b ? b : a);
}

void printTree(const struct Point *root)
{
	if (root != NULL)
	{
		printTree(root->left);
		printf("%i ", root->x);
		printTree(root->right);
	}
}
