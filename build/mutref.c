#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr mutref_newref_0(Ptr mutref_x_0);
Ptr mutref_getref_0(Ptr mutref_r_0);
Ptr mutref_putref_0(Ptr mutref_r_1,Ptr mutref_x_1);
Ptr newref_internal(Ptr v) {
    Ptr* loc = malloc(sizeof(Ptr));
    *loc = v;
    return loc;
}

Ptr getref_internal(Ptr loc) {
    return *((Ptr*)loc);
}

Ptr putref_internal(Ptr loc,Ptr v) {
    *((Ptr*)loc) = v;
    return 0;
}

Ptr mutref_newref_0(Ptr mutref_x_0) {
    Ptr v_20;
    v_20 = newref_internal(mutref_x_0);
    return v_20;
}
Ptr mutref_getref_0(Ptr mutref_r_0) {
    Ptr v_21;
    v_21 = getref_internal(mutref_r_0);
    return v_21;
}
Ptr mutref_putref_0(Ptr mutref_r_1,Ptr mutref_x_1) {
    Ptr v_22;
    v_22 = putref_internal(mutref_r_1,mutref_x_1);
    return v_22;
}
