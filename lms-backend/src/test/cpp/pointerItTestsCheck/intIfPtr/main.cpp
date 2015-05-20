#include <stdio.h>
#include <mpi.h>
#include <utility>

typedef int int32_t;
#include "intIfPtr.cxx"

int main()
{
    char name[] = "intIfPtr";
    int flag = 0;

    int in1 = 1;
    int *res1 = apply_intIfPtr(in1);
    flag += (*res1 == 2) ? 1 : 0;

    int in2 = 0;
    int *res2 = apply_intIfPtr(in2);
    flag += (*res2 == 0) ? 1 : 0;

    if (flag == 2)
        printf("test %s passed\n", name);
    else
        printf("test %s failed!!!\n", name);
    return 0;
}
