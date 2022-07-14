#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr SCons__0(Ptr v_6,Ptr v_7);
Ptr SNil__0();
Ptr Top__0();
Ptr add__0(Ptr x__0,Ptr y__0);
Ptr sub__0(Ptr x__1,Ptr y__1);
Ptr fuseStr__0(Ptr x__2,Ptr y__2);
Ptr putStrLn__0(Ptr s__0);
Ptr go__0(Ptr x__3,Ptr l__0);
Ptr stringLen__0(Ptr xs__0);
Ptr unpack__0(Ptr x__4);
Ptr appendToPtr__0(Ptr ptr__0,Ptr x__6);
Ptr pack__0(Ptr x__5);
Ptr append__0(Ptr x__7,Ptr y__3);
Ptr start__0(Ptr x__8);
#include<string.h>

void main(int main_arg) {
    start__0(main_arg);
}
Ptr SCons__0(Ptr v_6,Ptr v_7) {
    Ptr v_8;
    v_8 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_8))[0] = 0;
    ((Ptr*)(v_8))[1] = v_6;
    ((Ptr*)(v_8))[2] = v_7;
    return v_8;
}
Ptr SNil__0() {
    Ptr v_9;
    v_9 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_9))[0] = 1;
    return v_9;
}
Ptr Top__0() {
    Ptr v_10;
    v_10 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_10))[0] = 0;
    return v_10;
}
Ptr add__0(Ptr x__0,Ptr y__0) {
    Ptr v_11;
    v_11 = (int)x__0 + (int)y__0;
    return v_11;
}
Ptr sub__0(Ptr x__1,Ptr y__1) {
    Ptr v_12;
    v_12 = (int)x__1 - (int)y__1;
    return v_12;
}
Ptr fuseStr__0(Ptr x__2,Ptr y__2) {
    Ptr v_17;
    v_17 = strlen(y__2);
    Ptr v_16;
    v_16 = strlen(x__2);
    Ptr v_18;
    v_18 = add__0(v_16,v_17);
    Ptr v_19;
    v_19 = add__0(1,v_18);
    Ptr len__0;
    len__0 = v_19;
    Ptr v_15;
    v_15 = malloc(len__0);
    Ptr out__0;
    out__0 = v_15;
    Ptr v_14;
    v_14 = strcpy(out__0,x__2);
    Ptr hole_underscore_0__0;
    hole_underscore_0__0 = v_14;
    Ptr v_13;
    v_13 = strcat(out__0,y__2);
    Ptr hole_underscore_1__0;
    hole_underscore_1__0 = v_13;
    return out__0;
}
Ptr putStrLn__0(Ptr s__0) {
    Ptr v_20;
    v_20 = printf("%s\n",s__0);
    return v_20;
}
Ptr go__0(Ptr x__3,Ptr l__0) {
    if (((Ptr*)(x__3))[0] == 1) {
        return l__0;
    } else if (((Ptr*)(x__3))[0] == 0) {
        Ptr v_0;
        v_0 = ((Ptr*)(x__3))[1];
        Ptr v_1;
        v_1 = ((Ptr*)(x__3))[2];
        Ptr xs__1;
        xs__1 = v_1;
        Ptr s__1;
        s__1 = v_0;
        Ptr v_21;
        v_21 = strlen(s__1);
        Ptr v_22;
        v_22 = add__0(l__0,v_21);
        Ptr v_23;
        v_23 = go__0(xs__1,v_22);
        return v_23;
    } else {
        return printf("%s","no match");
    }
}
Ptr stringLen__0(Ptr xs__0) {
    Ptr v_24;
    v_24 = go__0(xs__0,0);
    return v_24;
}
Ptr unpack__0(Ptr x__4) {
    Ptr v_25;
    v_25 = SNil__0();
    Ptr v_26;
    v_26 = SCons__0(x__4,v_25);
    return v_26;
}
Ptr appendToPtr__0(Ptr ptr__0,Ptr x__6) {
    if (((Ptr*)(x__6))[0] == 1) {
        Ptr v_27;
        v_27 = Top__0();
        return v_27;
    } else if (((Ptr*)(x__6))[0] == 0) {
        Ptr v_2;
        v_2 = ((Ptr*)(x__6))[1];
        Ptr v_3;
        v_3 = ((Ptr*)(x__6))[2];
        Ptr xs__2;
        xs__2 = v_3;
        Ptr s__2;
        s__2 = v_2;
        Ptr v_29;
        v_29 = strcat(ptr__0,s__2);
        Ptr _underscore___2;
        _underscore___2 = v_29;
        Ptr v_28;
        v_28 = appendToPtr__0(ptr__0,xs__2);
        return v_28;
    } else {
        return printf("%s","no match");
    }
}
Ptr pack__0(Ptr x__5) {
    Ptr v_34;
    v_34 = stringLen__0(x__5);
    Ptr len__1;
    len__1 = v_34;
    Ptr v_32;
    v_32 = add__0(len__1,1);
    Ptr v_33;
    v_33 = malloc(v_32);
    Ptr ptr__0;
    ptr__0 = v_33;
    Ptr v_31;
    v_31 = strcpy(ptr__0,"");
    Ptr _underscore___0;
    _underscore___0 = v_31;
    Ptr v_30;
    v_30 = appendToPtr__0(ptr__0,x__5);
    Ptr _underscore___1;
    _underscore___1 = v_30;
    return ptr__0;
}
Ptr append__0(Ptr x__7,Ptr y__3) {
    if (((Ptr*)(x__7))[0] == 1) {
        return y__3;
    } else if (((Ptr*)(x__7))[0] == 0) {
        Ptr v_4;
        v_4 = ((Ptr*)(x__7))[1];
        Ptr v_5;
        v_5 = ((Ptr*)(x__7))[2];
        Ptr xs__3;
        xs__3 = v_5;
        Ptr s__3;
        s__3 = v_4;
        Ptr v_35;
        v_35 = append__0(xs__3,y__3);
        Ptr v_36;
        v_36 = SCons__0(s__3,v_35);
        return v_36;
    } else {
        return printf("%s","no match");
    }
}
Ptr start__0(Ptr x__8) {
    Ptr v_37;
    v_37 = SNil__0();
    Ptr v_38;
    v_38 = SCons__0("iii",v_37);
    Ptr v_39;
    v_39 = SCons__0("hhh",v_38);
    Ptr v_40;
    v_40 = pack__0(v_39);
    Ptr v_41;
    v_41 = putStrLn__0(v_40);
    return v_41;
}
