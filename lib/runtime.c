#include <stdio.h>
#include <stdlib.h>

void printInt(void* env, int n)
{
    printf("%d\n", n);
}

char* allocMemory(int size)
{
    return (char*) malloc(size * sizeof(char));
}
