#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr Just__0(Ptr v_2);
Ptr Nothing__0();
Ptr Top__0();
Ptr Unfix__0(Ptr v_3);
Ptr add__0(Ptr x__0,Ptr y__0);
Ptr sub__0(Ptr x__1,Ptr y__1);
Ptr putInt__0(Ptr x__2);
Ptr count__0(Ptr f__1);
Ptr uncount__0(Ptr i__0);
Ptr start__0(Ptr x__4);
void main(int main_arg) {
    start__0(main_arg);
}
Ptr Just__0(Ptr v_2) {
    Ptr v_4;
    v_4 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_4))[0] = 0;
    ((Ptr*)(v_4))[1] = v_2;
    return v_4;
}
Ptr Nothing__0() {
    Ptr v_5;
    v_5 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_5))[0] = 1;
    return v_5;
}
Ptr Top__0() {
    Ptr v_6;
    v_6 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_6))[0] = 0;
    return v_6;
}
Ptr Unfix__0(Ptr v_3) {
    Ptr v_7;
    v_7 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_7))[0] = 0;
    ((Ptr*)(v_7))[1] = v_3;
    return v_7;
}
Ptr add__0(Ptr x__0,Ptr y__0) {
    Ptr v_8;
    v_8 = (int)x__0 + (int)y__0;
    return v_8;
}
Ptr sub__0(Ptr x__1,Ptr y__1) {
    Ptr v_9;
    v_9 = (int)x__1 - (int)y__1;
    return v_9;
}
Ptr putInt__0(Ptr x__2) {
    Ptr v_10;
    v_10 = printf("%i\n",x__2);
    return v_10;
}
Ptr count__0(Ptr f__1) {
    if (((Ptr*)(f__1))[0] == 0) {
        Ptr v_0;
        v_0 = ((Ptr*)(f__1))[1];
        if (((Ptr*)(v_0))[0] == 0) {
            Ptr v_1;
            v_1 = ((Ptr*)(v_0))[1];
            Ptr g__0;
            g__0 = v_1;
            Ptr v_11;
            v_11 = count__0(g__0);
            Ptr v_12;
            v_12 = add__0(1,v_11);
            return v_12;
        } else if (((Ptr*)(v_0))[0] == 1) {
            return 0;
        } else {
            return printf("%s","no match");
        }
    } else {
        return printf("%s","no match");
    }
}
Ptr uncount__0(Ptr i__0) {
    if (i__0 == 0) {
        Ptr v_17;
        v_17 = Nothing__0();
        Ptr v_18;
        v_18 = Unfix__0(v_17);
        return v_18;
    } else {
        Ptr x__3;
        x__3 = i__0;
        Ptr v_13;
        v_13 = sub__0(x__3,1);
        Ptr v_14;
        v_14 = uncount__0(v_13);
        Ptr v_15;
        v_15 = Just__0(v_14);
        Ptr v_16;
        v_16 = Unfix__0(v_15);
        return v_16;
    }
}
Ptr start__0(Ptr x__4) {
    Ptr v_19;
    v_19 = uncount__0(70);
    Ptr v_20;
    v_20 = count__0(v_19);
    Ptr v_21;
    v_21 = putInt__0(v_20);
    if (((Ptr*)(v_21))[0] == 0) {
        Ptr v_22;
        v_22 = Top__0();
        return v_22;
    } else {
        return printf("%s","no match");
    }
}
