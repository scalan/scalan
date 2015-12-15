#include <stdio.h>
#include <mpi.h>
#include <utility>

typedef int int32_t;
#include "intPtr.cxx"

int main()
{
    int in = 2;

    int *res = apply_intPtr(in);

    char name[] = "intPtr";
    auto flag = *res == 2;
    if (flag)
        printf("test %s passed\n", name);
    else
        printf("test %s failed!!!\n", name);
    return 0;
}
