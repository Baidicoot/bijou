#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr v_0(Ptr v_1,Ptr v_2);
Ptr tmp__0(Ptr x__1,Ptr y__0);
Ptr start(Ptr x__0);
Ptr doublePartial__0(Ptr x__1);
Ptr doubleClosure__0(Ptr x__2);
Ptr putInt__0(Ptr i__0);
Ptr add__0(Ptr x__3,Ptr y__1);
void main(int main_arg) {
    start(main_arg);
}
Ptr v_0(Ptr v_1,Ptr v_2) {
    Ptr v_3;
    v_3 = ((Ptr*)(v_1))[1];
    Ptr v_4;
    v_4 = add__0(v_3,v_2);
    return v_4;
}
Ptr tmp__0(Ptr x__1,Ptr y__0) {
    Ptr v_5;
    v_5 = add__0(x__1,y__0);
    return v_5;
}
Ptr start(Ptr x__0) {
    Ptr v_6;
    v_6 = doubleClosure__0(5);
    Ptr v_7;
    v_7 = putInt__0(v_6);
    return v_7;
}
Ptr doublePartial__0(Ptr x__1) {
    Ptr v_8;
    v_8 = tmp__0(x__1,x__1);
    return v_8;
}
Ptr doubleClosure__0(Ptr x__2) {
    Ptr v_10;
    v_10 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_10))[0] = v_0;
    ((Ptr*)(v_10))[1] = x__2;
    Ptr tmp__1;
    tmp__1 = v_10;
    Ptr v_9;
    v_9 = (((Ptr (**)(Ptr,Ptr))(tmp__1))[0])(tmp__1,x__2);
    return v_9;
}
Ptr putInt__0(Ptr i__0) {
    Ptr v_11;
    v_11 = printf("%i\n",i__0);
    return v_11;
}
Ptr add__0(Ptr x__3,Ptr y__1) {
    Ptr v_12;
    v_12 = (int)x__3 + (int)y__1;
    return v_12;
}
