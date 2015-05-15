#include <stdio.h>
#include <mpi.h>
#include <utility>

typedef int int32_t;
#include "pairScalarIntDoubleSame.cxx"

int main()
{
    int in = 2;

    std::pair<int, double> res = apply_pairScalarIntDoubleSame(in);

    char name[] = "pairScalarIntDoubleSame";
    auto flag = ((res.first == 0) && (res.second == 0.0)) ? 1 : 0;
    if (flag)
        printf("test %s passed\n", name);
    else
        printf("test %s failed!!!\n", name);
    return 0;
}
