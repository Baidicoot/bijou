#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr add__0(Ptr x__0,Ptr y__0);
Ptr start__0(Ptr x__1);

void main(int main_arg) {
    start__0(main_arg);
}
Ptr add__0(Ptr x__0,Ptr y__0) {
    Ptr v_0;
    v_0 = (int)x__0 + (int)y__0;
    return v_0;
}
Ptr start__0(Ptr x__1) {
    Ptr v_1;
    v_1 = add__0(9,13);
    Ptr v_2;
    v_2 = printf("%i\n",v_1);
    return v_2;
}
