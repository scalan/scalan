#include <stdio.h>
#include <mpi.h>
#include <utility>

typedef int int32_t;
#include "pairPtrIntDoubleSame.cxx"

int main()
{
    int in = 2;

    std::pair<int *, double *> res = apply_pairPtrIntDoubleSame(in);

    char name[] = "pairPtrIntDoubleSame";
    auto flag = ((*res.first == 0) && (*res.second == 0.0)) ? 1 : 0;
    if (flag)
        printf("test %s passed\n", name);
    else
        printf("test %s failed!!!\n", name);
    return 0;
}
