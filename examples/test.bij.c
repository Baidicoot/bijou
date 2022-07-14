#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr v_0(Ptr v_1,Ptr v_2);
Ptr add__0(Ptr x__0,Ptr y__0);
Ptr putInt__0(Ptr i__0);
Ptr doubleCls__0(Ptr x__1);
Ptr start__0(Ptr x__2);
void main(int main_arg) {
    start__0(main_arg);
}
Ptr v_0(Ptr v_1,Ptr v_2) {
    Ptr v_3;
    v_3 = ((Ptr*)(v_1))[1];
    Ptr v_4;
    v_4 = add__0(v_3,v_2);
    return v_4;
}
Ptr add__0(Ptr x__0,Ptr y__0) {
    Ptr v_5;
    v_5 = (int)x__0 + (int)y__0;
    return v_5;
}
Ptr putInt__0(Ptr i__0) {
    Ptr v_6;
    v_6 = printf("%i\n",i__0);
    return v_6;
}
Ptr doubleCls__0(Ptr x__1) {
    Ptr v_8;
    v_8 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_8))[0] = v_0;
    ((Ptr*)(v_8))[1] = x__1;
    Ptr tmp__0;
    tmp__0 = v_8;
    Ptr v_7;
    v_7 = (((Ptr (**)(Ptr,Ptr))(tmp__0))[0])(tmp__0,x__1);
    return v_7;
}
Ptr start__0(Ptr x__2) {
    Ptr v_9;
    v_9 = doubleCls__0(5);
    Ptr v_10;
    v_10 = putInt__0(v_9);
    return v_10;
}
