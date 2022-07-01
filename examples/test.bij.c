#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr addThing(Ptr x,Ptr y);
Ptr testThingy(Ptr x);
Ptr main(Ptr x);
Ptr addThing(Ptr x,Ptr y) {
    Ptr v_0;
    v_0 = (int)x + (int)y;
    return v_0;
}
Ptr testThingy(Ptr x) {
    Ptr v_1;
    v_1 = addThing(x,2);
    Ptr v_2;
    v_2 = printf("%i\n",v_1);
    return v_2;
}
Ptr main(Ptr x) {
    Ptr v_3;
    v_3 = testThingy(4);
    return v_3;
}
