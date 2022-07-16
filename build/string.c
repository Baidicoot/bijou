#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr string_SCons_0(Ptr v_69,Ptr v_70);
Ptr string_SNil_0();
Ptr string_Top_0();
Ptr string_add_0(Ptr string_x_0,Ptr string_y_0);
Ptr string_sub_0(Ptr string_x_1,Ptr string_y_1);
Ptr string_fuseStr_0(Ptr string_x_2,Ptr string_y_2);
Ptr string_putStr_0(Ptr string_s_0);
Ptr string_putStrLn_0(Ptr string_s_1);
Ptr string_go_0(Ptr string_x_3,Ptr string_l_0);
Ptr string_stringLen_0(Ptr string_xs_0);
Ptr string_unpack_0(Ptr string_x_4);
Ptr string_appendToPtr_0(Ptr string_ptr_0,Ptr string_x_6);
Ptr string_pack_0(Ptr string_x_5);
Ptr string_append_0(Ptr string_x_7,Ptr string_y_3);
Ptr string_putString_0(Ptr string_s_5);
Ptr string_putStringLn_0(Ptr string_s_6);
#include<string.h>

Ptr string_SCons_0(Ptr v_69,Ptr v_70) {
    Ptr v_71;
    v_71 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_71))[0] = 0;
    ((Ptr*)(v_71))[1] = v_69;
    ((Ptr*)(v_71))[2] = v_70;
    return v_71;
}
Ptr string_SNil_0() {
    Ptr v_72;
    v_72 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_72))[0] = 1;
    return v_72;
}
Ptr string_Top_0() {
    Ptr v_73;
    v_73 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_73))[0] = 0;
    return v_73;
}
Ptr string_add_0(Ptr string_x_0,Ptr string_y_0) {
    Ptr v_74;
    v_74 = (int)string_x_0 + (int)string_y_0;
    return v_74;
}
Ptr string_sub_0(Ptr string_x_1,Ptr string_y_1) {
    Ptr v_75;
    v_75 = (int)string_x_1 - (int)string_y_1;
    return v_75;
}
Ptr string_fuseStr_0(Ptr string_x_2,Ptr string_y_2) {
    Ptr v_80;
    v_80 = strlen(string_y_2);
    Ptr v_79;
    v_79 = strlen(string_x_2);
    Ptr v_81;
    v_81 = string_add_0(v_79,v_80);
    Ptr v_82;
    v_82 = string_add_0(1,v_81);
    Ptr string_len_0;
    string_len_0 = v_82;
    Ptr v_78;
    v_78 = malloc(string_len_0);
    Ptr string_out_0;
    string_out_0 = v_78;
    Ptr v_77;
    v_77 = strcpy(string_out_0,string_x_2);
    Ptr string_hole_0_0;
    string_hole_0_0 = v_77;
    Ptr v_76;
    v_76 = strcat(string_out_0,string_y_2);
    Ptr string_hole_1_0;
    string_hole_1_0 = v_76;
    return string_out_0;
}
Ptr string_putStr_0(Ptr string_s_0) {
    Ptr v_83;
    v_83 = printf(string_s_0);
    return v_83;
}
Ptr string_putStrLn_0(Ptr string_s_1) {
    Ptr v_84;
    v_84 = printf("%s\n",string_s_1);
    return v_84;
}
Ptr string_go_0(Ptr string_x_3,Ptr string_l_0) {
    if (((Ptr*)(string_x_3))[0] == 1) {
        return string_l_0;
    } else if (((Ptr*)(string_x_3))[0] == 0) {
        Ptr v_63;
        v_63 = ((Ptr*)(string_x_3))[1];
        Ptr v_64;
        v_64 = ((Ptr*)(string_x_3))[2];
        Ptr string_xs_1;
        string_xs_1 = v_64;
        Ptr string_s_2;
        string_s_2 = v_63;
        Ptr v_85;
        v_85 = strlen(string_s_2);
        Ptr v_86;
        v_86 = string_add_0(string_l_0,v_85);
        Ptr v_87;
        v_87 = string_go_0(string_xs_1,v_86);
        return v_87;
    } else {
        return printf("%s","no match");
    }
}
Ptr string_stringLen_0(Ptr string_xs_0) {
    Ptr v_88;
    v_88 = string_go_0(string_xs_0,0);
    return v_88;
}
Ptr string_unpack_0(Ptr string_x_4) {
    Ptr v_89;
    v_89 = string_SNil_0();
    Ptr v_90;
    v_90 = string_SCons_0(string_x_4,v_89);
    return v_90;
}
Ptr string_appendToPtr_0(Ptr string_ptr_0,Ptr string_x_6) {
    if (((Ptr*)(string_x_6))[0] == 1) {
        Ptr v_91;
        v_91 = string_Top_0();
        return v_91;
    } else if (((Ptr*)(string_x_6))[0] == 0) {
        Ptr v_65;
        v_65 = ((Ptr*)(string_x_6))[1];
        Ptr v_66;
        v_66 = ((Ptr*)(string_x_6))[2];
        Ptr string_xs_2;
        string_xs_2 = v_66;
        Ptr string_s_3;
        string_s_3 = v_65;
        Ptr v_93;
        v_93 = strcat(string_ptr_0,string_s_3);
        Ptr string___2;
        string___2 = v_93;
        Ptr v_92;
        v_92 = string_appendToPtr_0(string_ptr_0,string_xs_2);
        return v_92;
    } else {
        return printf("%s","no match");
    }
}
Ptr string_pack_0(Ptr string_x_5) {
    Ptr v_98;
    v_98 = string_stringLen_0(string_x_5);
    Ptr string_len_1;
    string_len_1 = v_98;
    Ptr v_96;
    v_96 = string_add_0(string_len_1,1);
    Ptr v_97;
    v_97 = malloc(v_96);
    Ptr string_ptr_0;
    string_ptr_0 = v_97;
    Ptr v_95;
    v_95 = strcpy(string_ptr_0,"");
    Ptr string___0;
    string___0 = v_95;
    Ptr v_94;
    v_94 = string_appendToPtr_0(string_ptr_0,string_x_5);
    Ptr string___1;
    string___1 = v_94;
    return string_ptr_0;
}
Ptr string_append_0(Ptr string_x_7,Ptr string_y_3) {
    if (((Ptr*)(string_x_7))[0] == 1) {
        return string_y_3;
    } else if (((Ptr*)(string_x_7))[0] == 0) {
        Ptr v_67;
        v_67 = ((Ptr*)(string_x_7))[1];
        Ptr v_68;
        v_68 = ((Ptr*)(string_x_7))[2];
        Ptr string_xs_3;
        string_xs_3 = v_68;
        Ptr string_s_4;
        string_s_4 = v_67;
        Ptr v_99;
        v_99 = string_append_0(string_xs_3,string_y_3);
        Ptr v_100;
        v_100 = string_SCons_0(string_s_4,v_99);
        return v_100;
    } else {
        return printf("%s","no match");
    }
}
Ptr string_putString_0(Ptr string_s_5) {
    Ptr v_101;
    v_101 = string_pack_0(string_s_5);
    Ptr v_102;
    v_102 = printf(v_101);
    return v_102;
}
Ptr string_putStringLn_0(Ptr string_s_6) {
    Ptr v_103;
    v_103 = string_pack_0(string_s_6);
    Ptr v_104;
    v_104 = printf("%s\n",v_103);
    return v_104;
}
