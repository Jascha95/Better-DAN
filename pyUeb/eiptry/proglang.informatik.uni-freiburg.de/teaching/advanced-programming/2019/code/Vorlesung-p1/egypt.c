#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <locale.h>

#define NUM_EGYPT 7

struct number
{
	unsigned int val;
	const char *str;
};

static struct number numbers[NUM_EGYPT] =
{
{1000000, "𓁨"},
{100000, "𓆐"},
{10000, "𓂭"},
{1000, "𓆼"},
{100, "𓍢"},
{10, "𓎆"},
{1, "𓏺 "}
};

int egypt(char *str, unsigned int number, size_t n)
{
	for(unsigned int i = 0; i < NUM_EGYPT; i++)
	{
		while(number >= numbers[i].val)
		{
			if (n <= strlen(numbers[i].str))
			{
				return -1;
			}
			strcpy(str, numbers[i].str);
			str += strlen(numbers[i].str);
			n -= strlen(numbers[i].str);
			number -= numbers[i].val;
		}
	}
	return 0;
}

#define BUFLEN 10000

int main(void)
{
	char buffer[BUFLEN];
	if(!setlocale(LC_ALL, "C.UTF-8"))
		return -1;

	egypt(buffer, 12345, BUFLEN);

	puts(buffer);

	return 0;
}

