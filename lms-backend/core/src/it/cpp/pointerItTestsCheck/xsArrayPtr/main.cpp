#include <stdio.h>
#include <mpi.h>
#include <utility>

typedef int int32_t;
#include "xsArrayPtr.cxx"

#include <iostream>
using std::ostream;
using std::cout;

int main()
{
    auto xs = std::make_shared<boost::container::vector<int32_t>>(std::initializer_list<int32_t>{1, 2, 4});
    auto in = xs;

    int *res = apply_xsArrayPtr(in);

    char name[] = "xsArrayPtr";
    int expected[] = {1, 2, 4};
    int flag = 0;
    flag += res[0] == expected[0] ? 1 : 0;
    flag += res[1] == expected[1] ? 1 : 0;
    flag += res[2] == expected[2] ? 1 : 0;
    if (flag == 3)
        printf("test %s passed\n", name);
    else
        printf("test %s failed!!!\n", name);
    return 0;
}
