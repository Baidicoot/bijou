#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr Cons__0(Ptr v_4,Ptr v_5);
Ptr Nil__0();
Ptr start(Ptr x__0);
Ptr showIntList__0(Ptr ls__0);
Ptr map__0(Ptr f__0,Ptr ls__1);
Ptr fac__0(Ptr x__1);
Ptr div__0(Ptr x__2,Ptr y__0);
Ptr mul__0(Ptr x__3,Ptr y__1);
Ptr sub__0(Ptr x__4,Ptr y__2);
Ptr add__0(Ptr x__5,Ptr y__3);
Ptr putInt(Ptr i__0);
void main(int main_arg) {
    start(main_arg);
}
Ptr Cons__0(Ptr v_4,Ptr v_5) {
    Ptr v_6;
    v_6 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_6))[0] = 0;
    ((Ptr*)(v_6))[1] = v_4;
    ((Ptr*)(v_6))[2] = v_5;
    return v_6;
}
Ptr Nil__0() {
    Ptr v_7;
    v_7 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_7))[0] = 1;
    return v_7;
}
Ptr start(Ptr x__0) {
    Ptr v_8;
    v_8 = Nil__0();
    Ptr v_9;
    v_9 = Cons__0(3,v_8);
    Ptr v_10;
    v_10 = Cons__0(1,v_9);
    Ptr v_11;
    v_11 = showIntList__0(v_10);
    return v_11;
}
Ptr showIntList__0(Ptr ls__0) {
    if (((Ptr*)(ls__0))[0] == 1) {
        return 0;
    } else if (((Ptr*)(ls__0))[0] == 0) {
        Ptr v_0;
        v_0 = ((Ptr*)(ls__0))[1];
        Ptr v_1;
        v_1 = ((Ptr*)(ls__0))[2];
        Ptr as__0;
        as__0 = v_1;
        Ptr a__0;
        a__0 = v_0;
        Ptr v_13;
        v_13 = putInt(a__0);
        Ptr h__0;
        h__0 = v_13;
        Ptr v_12;
        v_12 = showIntList__0(as__0);
        return v_12;
    } else {
        return printf("%s","match error");
    }
}
Ptr map__0(Ptr f__0,Ptr ls__1) {
    if (((Ptr*)(ls__1))[0] == 1) {
        Ptr v_14;
        v_14 = Nil__0();
        return v_14;
    } else if (((Ptr*)(ls__1))[0] == 0) {
        Ptr v_2;
        v_2 = ((Ptr*)(ls__1))[1];
        Ptr v_3;
        v_3 = ((Ptr*)(ls__1))[2];
        Ptr as__0;
        as__0 = v_3;
        Ptr a__0;
        a__0 = v_2;
        Ptr v_16;
        v_16 = map__0(f__0,as__0);
        Ptr v_15;
        v_15 = (((Ptr (**)(Ptr,Ptr))(f__0))[0])(f__0,a__0);
        Ptr v_17;
        v_17 = Cons__0(v_15,v_16);
        return v_17;
    } else {
        return printf("%s","match error");
    }
}
Ptr fac__0(Ptr x__1) {
    if (x__1 == 0) {
        return 1;
    } else {
        Ptr y__0;
        y__0 = x__1;
        Ptr v_18;
        v_18 = sub__0(x__1,1);
        Ptr v_19;
        v_19 = fac__0(v_18);
        Ptr v_20;
        v_20 = mul__0(x__1,v_19);
        return v_20;
    }
}
Ptr div__0(Ptr x__2,Ptr y__0) {
    Ptr v_21;
    v_21 = (int)x__2 / (int)y__0;
    return v_21;
}
Ptr mul__0(Ptr x__3,Ptr y__1) {
    Ptr v_22;
    v_22 = (int)x__3 * (int)y__1;
    return v_22;
}
Ptr sub__0(Ptr x__4,Ptr y__2) {
    Ptr v_23;
    v_23 = (int)x__4 - (int)y__2;
    return v_23;
}
Ptr add__0(Ptr x__5,Ptr y__3) {
    Ptr v_24;
    v_24 = (int)x__5 + (int)y__3;
    return v_24;
}
Ptr putInt(Ptr i__0) {
    Ptr v_25;
    v_25 = printf("%i\n",i__0);
    return v_25;
}
