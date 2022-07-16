#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr v_14(Ptr v_15,Ptr v_16);
Ptr v_17(Ptr v_18,Ptr v_19);
Ptr typesys_test_0(Ptr typesys_f_0,Ptr typesys_x_0);
Ptr typesys_id_0(Ptr typesys_x_1);
Ptr typesys_whatever_0(Ptr typesys_x_2);

Ptr v_14(Ptr v_15,Ptr v_16) {
    Ptr v_21;
    v_21 = typesys_id_0(v_16);
    return v_21;
}
Ptr v_17(Ptr v_18,Ptr v_19) {
    Ptr v_20;
    v_20 = ((Ptr*)(v_18))[1];
    Ptr v_22;
    v_22 = typesys_test_0(v_20,v_19);
    return v_22;
}
Ptr typesys_test_0(Ptr typesys_f_0,Ptr typesys_x_0) {
    Ptr v_23;
    v_23 = (((Ptr (**)(Ptr,Ptr))(typesys_f_0))[0])(typesys_f_0,typesys_x_0);
    return v_23;
}
Ptr typesys_id_0(Ptr typesys_x_1) {
    return typesys_x_1;
}
Ptr typesys_whatever_0(Ptr typesys_x_2) {
    Ptr v_24;
    v_24 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_24))[0] = v_14;
    Ptr v_25;
    v_25 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_25))[0] = v_17;
    ((Ptr*)(v_25))[1] = v_24;
    Ptr typesys___0;
    typesys___0 = v_25;
    return typesys_x_2;
}
