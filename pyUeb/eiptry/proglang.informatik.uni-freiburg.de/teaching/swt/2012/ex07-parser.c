#include "stdio.h"

int main(int argc, char** argv)
{
	FILE* fp;
	char ch;
    int balance = 0;
    int stage = 0;

    if (argc == 1)
    {
        printf("usage parser <filename>\n");
        return 0;
    }
    
    fp = fopen(argv[1], "r");
    if (fp == NULL)
    {
        printf("unable to open file \"%s\"\n", argv[1]);
        return 0;
    }
    
    while ((ch = fgetc(fp)) != EOF)
    {
        if (stage != 3)
        {
            if (ch == 'Z')
            {
                stage = 1;
            }
            else if (stage == 1 && ch == 'M')
            {
                stage = 2;
            }
            else if (stage == 2 && ch == 'F')
            {
                stage = 3;
            }
            else
            {
                stage = 0;
            }
        }
        
        if (ch == '<')
        {
            balance++;
        }
        else if (ch == '>')
        {
            balance--;
        }
    }
    
    fclose(fp);
    
    if (balance != 0)
    {
        printf("parse error!\n");
    }
    else if (stage == 3)
    {
        printf("segmentation fault!\n");
    }
    else
    {
        printf("sucessfully finished.\n");
    }
    
    return 0;
}
