#include <stdio.h>

void escape(const char *s, int size)
{
    for (int i = 0; i < size; i++)
    {
        char c = s[i];
        switch (c)
        {
        case '\'':
            printf("\\\'");
            break;
        case '\"':
            printf("\\\"");
            break;
        case '\t':
            printf("\\t");
            break;
        case '\r':
            printf("\\r");
            break;
        case '\n':
            printf("\\n");
            break;
        case '%':
            printf("%%");
            break;
        case '\\':
            printf("\\\\");
            break;
        default:
            printf("%c", c);
        }
    }
}

void strsub(char *s, int from, int to)
{
    for (int j = from; j < to; j++)
    {
        printf("%c", s[j]);
    }
}

int main(void)
{
    char src[] = "#include <stdio.h>\n\nvoid escape(const char *s, int size)\n{\n    for (int i = 0; i < size; i++)\n    {\n        char c = s[i];\n        switch (c)\n        {\n        case \'\\\'\':\n            printf(\"\\\\\\\'\");\n            break;\n        case \'\\\"\':\n            printf(\"\\\\\\\"\");\n            break;\n        case \'\\t\':\n            printf(\"\\\\t\");\n            break;\n        case \'\\r\':\n            printf(\"\\\\r\");\n            break;\n        case \'\\n\':\n            printf(\"\\\\n\");\n            break;\n        case \'%\':\n            printf(\"%%\");\n            break;\n        case \'\\\\\':\n            printf(\"\\\\\\\\\");\n            break;\n        default:\n            printf(\"%c\", c);\n        }\n    }\n}\n\nvoid strsub(char *s, int from, int to)\n{\n    for (int j = from; j < to; j++)\n    {\n        printf(\"%c\", s[j]);\n    }\n}\n\nint main()\n{\n    char src[] = \"?\";\n    int size = sizeof(src) / sizeof(src[0]);\n    for (int i = 0; i < size; i++)\n    {\n        if (src[i] == \'?\')\n        {\n            strsub(src, 0, i);\n            escape(src, size);\n            strsub(src, i + 1, size);\n            break;\n        }\n    }\n    return 0;\n}\n";
    int size = sizeof(src) / sizeof(src[0]);
    for (int i = 0; i < size; i++)
    {
        if (src[i] == '?')
        {
            strsub(src, 0, i);
            escape(src, size);
            strsub(src, i + 1, size);
            break;
        }
    }
    return 0;
}
