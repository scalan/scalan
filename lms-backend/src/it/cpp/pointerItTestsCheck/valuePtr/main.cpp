#include <stdio.h>
#include <mpi.h>
#include <utility>

typedef int int32_t;
#include "valuePtr.cxx"

int main()
{
    int in = 1;

    int *res = apply_valuePtr(in);

    char name[] = "valuePtr";
    auto flag = *res == 2;
    if (flag)
        printf("test %s passed\n", name);
    else
        printf("test %s failed!!!\n", name);
    return 0;
}
