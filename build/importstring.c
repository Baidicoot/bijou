#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
#include "string.h"
Ptr importstring_app_0(Ptr importstring_x_0,Ptr importstring_y_0);
Ptr importstring_unit_0(Ptr importstring_s_0);
Ptr importstring_start_0(Ptr importstring_x_1);

void main(int main_arg) {
    importstring_start_0(main_arg);
}
Ptr importstring_app_0(Ptr importstring_x_0,Ptr importstring_y_0) {
    Ptr v_11;
    v_11 = string_append_0(importstring_x_0,importstring_y_0);
    return v_11;
}
Ptr importstring_unit_0(Ptr importstring_s_0) {
    Ptr v_12;
    v_12 = string_Top();
    return v_12;
}
Ptr importstring_start_0(Ptr importstring_x_1) {
    Ptr v_14;
    v_14 = string_unpack_0("bees");
    Ptr v_13;
    v_13 = string_unpack_0("test");
    Ptr v_15;
    v_15 = string_append_0(v_13,v_14);
    Ptr v_16;
    v_16 = string_putString_0(v_15);
    return v_16;
}
