#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr v_23(Ptr v_24,Ptr v_25);
Ptr test_putInt_0(Ptr test_x_0);
Ptr test_app_0(Ptr test_f_0,Ptr test_x_1);
Ptr test_add_0(Ptr test_x_2,Ptr test_y_0);
Ptr test_start_0(Ptr test_x_3);

void main(int main_arg) {
    test_start_0(main_arg);
}
Ptr v_23(Ptr v_24,Ptr v_25) {
    Ptr v_26;
    v_26 = ((Ptr*)(v_24))[1];
    Ptr v_27;
    v_27 = test_add_0(v_26,v_25);
    return v_27;
}
Ptr test_putInt_0(Ptr test_x_0) {
    Ptr v_28;
    v_28 = printf("%i\n",test_x_0);
    return v_28;
}
Ptr test_app_0(Ptr test_f_0,Ptr test_x_1) {
    Ptr v_21;
    v_21 = test_f_0;
    Ptr v_22;
    v_22 = ((Ptr*)(v_21))[0];
    Ptr v_29;
    v_29 = ((Ptr (*)(Ptr,Ptr))(v_22))(v_21,test_x_1);
    return v_29;
}
Ptr test_add_0(Ptr test_x_2,Ptr test_y_0) {
    Ptr v_30;
    v_30 = (int)test_x_2 + (int)test_y_0;
    return v_30;
}
Ptr test_start_0(Ptr test_x_3) {
    Ptr v_31;
    v_31 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_31))[0] = v_23;
    ((Ptr*)(v_31))[1] = 4;
    Ptr v_32;
    v_32 = test_app_0(v_31,3);
    Ptr v_33;
    v_33 = test_putInt_0(v_32);
    return v_33;
}
