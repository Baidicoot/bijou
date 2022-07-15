#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr arith_add_0(Ptr arith_x_0,Ptr arith_y_0);
Ptr arith_start_0(Ptr arith_x_1);

void main(int main_arg) {
    arith_start_0(main_arg);
}
Ptr arith_add_0(Ptr arith_x_0,Ptr arith_y_0) {
    Ptr v_9;
    v_9 = (int)arith_x_0 + (int)arith_y_0;
    return v_9;
}
Ptr arith_start_0(Ptr arith_x_1) {
    Ptr v_10;
    v_10 = arith_add_0(9,13);
    Ptr v_11;
    v_11 = printf("%i\n",v_10);
    return v_11;
}
