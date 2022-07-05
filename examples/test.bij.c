#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr v_0(Ptr v_1,Ptr v_2);
Ptr tmp__1(Ptr x__2,Ptr y__1);
Ptr add__0(Ptr x__0,Ptr y__0);
Ptr putInt__0(Ptr i__0);
Ptr doubleClosure__0(Ptr x__1);
Ptr doublePartial__0(Ptr x__2);
Ptr main(Ptr x__3);
Ptr v_0(Ptr v_1,Ptr v_2) {
    Ptr v_3;
    v_3 = ((Ptr*)(v_1))[1];
    Ptr v_4;
    v_4 = add__0(v_3,v_2);
    return v_4;
}
Ptr tmp__1(Ptr x__2,Ptr y__1) {
    Ptr v_5;
    v_5 = add__0(x__2,y__1);
    return v_5;
}
Ptr add__0(Ptr x__0,Ptr y__0) {
    Ptr v_6;
    v_6 = (int)x__0 + (int)y__0;
    return v_6;
}
Ptr putInt__0(Ptr i__0) {
    Ptr v_7;
    v_7 = printf("%i\n",i__0);
    return v_7;
}
Ptr doubleClosure__0(Ptr x__1) {
    Ptr v_9;
    v_9 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_9))[0] = v_0;
    ((Ptr*)(v_9))[1] = x__1;
    Ptr tmp__0;
    tmp__0 = v_9;
    Ptr v_8;
    v_8 = (((Ptr (**)(Ptr,Ptr))(tmp__0))[0])(tmp__0,x__1);
    return v_8;
}
Ptr doublePartial__0(Ptr x__2) {
    Ptr v_10;
    v_10 = tmp__1(x__2,x__2);
    return v_10;
}
Ptr main(Ptr x__3) {
    Ptr v_11;
    v_11 = doubleClosure__0(5);
    Ptr v_12;
    v_12 = putInt__0(v_11);
    return v_12;
}
