#include <stdio.h>
#include <stdlib.h>

void printInt(int n)
{
    printf("%d\n", n);
}

char* allocMemory(int size)
{
    return (char*) malloc(size * sizeof(char));
}

void freeMemory(void* ptr)
{
    free(ptr);
}
