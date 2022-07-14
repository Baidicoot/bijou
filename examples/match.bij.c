#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr Cons__0(Ptr v_2,Ptr v_3);
Ptr Nil__0();
Ptr v_4(Ptr v_5,Ptr v_6);
Ptr putIntLn__0(Ptr i__0);
Ptr putStrLn__0(Ptr s__0);
Ptr add__0(Ptr x__0,Ptr y__0);
Ptr sub__0(Ptr x__1,Ptr y__1);
Ptr mul__0(Ptr x__2,Ptr y__2);
Ptr div__0(Ptr x__3,Ptr y__3);
Ptr fac__0(Ptr x__4);
Ptr id__0(Ptr x__5);
Ptr map__0(Ptr f__0,Ptr ls__0);
Ptr showList__0(Ptr showElem__0,Ptr ls__1);
Ptr start__0(Ptr x__7);

void main(int main_arg) {
    start__0(main_arg);
}
Ptr Cons__0(Ptr v_2,Ptr v_3) {
    Ptr v_8;
    v_8 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_8))[0] = 0;
    ((Ptr*)(v_8))[1] = v_2;
    ((Ptr*)(v_8))[2] = v_3;
    return v_8;
}
Ptr Nil__0() {
    Ptr v_9;
    v_9 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_9))[0] = 1;
    return v_9;
}
Ptr v_4(Ptr v_5,Ptr v_6) {
    Ptr v_7;
    v_7 = ((Ptr*)(v_5))[1];
    Ptr v_10;
    v_10 = printf(v_7,v_6);
    return v_10;
}
Ptr putIntLn__0(Ptr i__0) {
    Ptr v_11;
    v_11 = printf("%i\n",i__0);
    return v_11;
}
Ptr putStrLn__0(Ptr s__0) {
    Ptr v_12;
    v_12 = printf("%s\n",s__0);
    return v_12;
}
Ptr add__0(Ptr x__0,Ptr y__0) {
    Ptr v_13;
    v_13 = (int)x__0 + (int)y__0;
    return v_13;
}
Ptr sub__0(Ptr x__1,Ptr y__1) {
    Ptr v_14;
    v_14 = (int)x__1 - (int)y__1;
    return v_14;
}
Ptr mul__0(Ptr x__2,Ptr y__2) {
    Ptr v_15;
    v_15 = (int)x__2 * (int)y__2;
    return v_15;
}
Ptr div__0(Ptr x__3,Ptr y__3) {
    Ptr v_16;
    v_16 = (int)x__3 / (int)y__3;
    return v_16;
}
Ptr fac__0(Ptr x__4) {
    if (x__4 == 0) {
        return 1;
    } else {
        Ptr y__4;
        y__4 = x__4;
        Ptr v_17;
        v_17 = sub__0(x__4,1);
        Ptr v_18;
        v_18 = fac__0(v_17);
        Ptr v_19;
        v_19 = mul__0(x__4,v_18);
        return v_19;
    }
}
Ptr id__0(Ptr x__5) {
    return x__5;
}
Ptr map__0(Ptr f__0,Ptr ls__0) {
    if (((Ptr*)(ls__0))[0] == 1) {
        Ptr v_20;
        v_20 = Nil__0();
        return v_20;
    } else if (((Ptr*)(ls__0))[0] == 0) {
        Ptr v_0;
        v_0 = ((Ptr*)(ls__0))[1];
        Ptr v_1;
        v_1 = ((Ptr*)(ls__0))[2];
        Ptr as__0;
        as__0 = v_1;
        Ptr a__5;
        a__5 = v_0;
        Ptr v_22;
        v_22 = map__0(f__0,as__0);
        Ptr v_21;
        v_21 = (((Ptr (**)(Ptr,Ptr))(f__0))[0])(f__0,a__5);
        Ptr v_23;
        v_23 = Cons__0(v_21,v_22);
        return v_23;
    } else {
        return printf("%s","no match");
    }
}
Ptr showList__0(Ptr showElem__0,Ptr ls__1) {
    Ptr v_24;
    v_24 = map__0(showElem__0,ls__1);
    Ptr x__6;
    x__6 = v_24;
    return 0;
}
Ptr start__0(Ptr x__7) {
    Ptr v_26;
    v_26 = Nil__0();
    Ptr v_27;
    v_27 = Cons__0("hello,",v_26);
    Ptr v_28;
    v_28 = Cons__0("world!",v_27);
    Ptr v_25;
    v_25 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_25))[0] = v_4;
    ((Ptr*)(v_25))[1] = "%s ";
    Ptr v_29;
    v_29 = map__0(v_25,v_28);
    Ptr _underscore___0;
    _underscore___0 = v_29;
    return 0;
}
