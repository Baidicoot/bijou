#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
#include "string.h"
Ptr importstring_app_0(Ptr importstring_x_0,Ptr importstring_y_0);
Ptr importstring_start_0(Ptr importstring_x_1);

void main(int main_arg) {
    importstring_start_0(main_arg);
}
Ptr importstring_app_0(Ptr importstring_x_0,Ptr importstring_y_0) {
    Ptr v_10;
    v_10 = string_append_0(importstring_x_0,importstring_y_0);
    return v_10;
}
Ptr importstring_start_0(Ptr importstring_x_1) {
    Ptr v_12;
    v_12 = string_unpack_0("bees");
    Ptr v_11;
    v_11 = string_unpack_0("test");
    Ptr v_13;
    v_13 = string_append_0(v_11,v_12);
    Ptr v_14;
    v_14 = string_putString_0(v_13);
    return v_14;
}
