#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr newtype_Cons_0(Ptr v_74,Ptr v_75);
Ptr newtype_NewNat_0(Ptr v_76,Ptr v_77);
Ptr newtype_Nil_0;
Ptr v_88(Ptr v_89,Ptr v_90);
Ptr v_91(Ptr v_92,Ptr v_93);
Ptr v_95(Ptr v_96,Ptr v_97);
Ptr v_99(Ptr v_100,Ptr v_101);
Ptr newtype_fromInt_0(Ptr newtype_x_0);
Ptr newtype_toInt_0(Ptr newtype_x_1);
Ptr newtype_liftInt_0(Ptr newtype_f_0,Ptr newtype_x_2);
Ptr newtype_branch_0(Ptr newtype_t_0,Ptr newtype_f_1,Ptr newtype_b_1);
Ptr newtype_map_0(Ptr newtype_f_2,Ptr newtype_ls_0);
Ptr newtype_putInt_0(Ptr newtype_x_4);
Ptr newtype_add_0(Ptr newtype_x_5,Ptr newtype_y_0);
Ptr newtype_matchUnit_0(Ptr newtype_x_6);
Ptr newtype_start_0(Ptr newtype_x_7);

void main(int main_arg) {
    newtype_start_0(main_arg);
}
Ptr newtype_Cons_0(Ptr v_74,Ptr v_75) {
    Ptr v_102;
    v_102 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_102))[0] = 0;
    ((Ptr*)(v_102))[1] = v_74;
    ((Ptr*)(v_102))[2] = v_75;
    return v_102;
}
Ptr newtype_NewNat_0(Ptr v_76,Ptr v_77) {
    return v_76;
}
Ptr newtype_Nil_0 = 1;
Ptr v_88(Ptr v_89,Ptr v_90) {
    Ptr v_103;
    v_103 = newtype_fromInt_0(v_90);
    return v_103;
}
Ptr v_91(Ptr v_92,Ptr v_93) {
    Ptr v_94;
    v_94 = ((Ptr*)(v_92))[1];
    Ptr v_104;
    v_104 = newtype_add_0(v_94,v_93);
    return v_104;
}
Ptr v_95(Ptr v_96,Ptr v_97) {
    Ptr v_98;
    v_98 = ((Ptr*)(v_96))[1];
    Ptr v_105;
    v_105 = newtype_liftInt_0(v_98,v_97);
    return v_105;
}
Ptr v_99(Ptr v_100,Ptr v_101) {
    Ptr v_106;
    v_106 = newtype_putInt_0(v_101);
    return v_106;
}
Ptr newtype_fromInt_0(Ptr newtype_x_0) {
    return newtype_x_0;
}
Ptr newtype_toInt_0(Ptr newtype_x_1) {
    Ptr v_70;
    v_70 = newtype_x_1;
    Ptr newtype_i_0;
    newtype_i_0 = v_70;
    return newtype_i_0;
}
Ptr newtype_liftInt_0(Ptr newtype_f_0,Ptr newtype_x_2) {
    Ptr v_71;
    v_71 = newtype_x_2;
    Ptr newtype_i_1;
    newtype_i_1 = v_71;
    Ptr v_78;
    v_78 = newtype_f_0;
    Ptr v_79;
    v_79 = ((Ptr*)(v_78))[0];
    Ptr v_107;
    v_107 = ((Ptr (*)(Ptr,Ptr))(v_79))(v_78,newtype_i_1);
    return v_107;
}
Ptr newtype_branch_0(Ptr newtype_t_0,Ptr newtype_f_1,Ptr newtype_b_1) {
    if (newtype_b_1 == 0) {
        Ptr v_80;
        v_80 = newtype_f_1;
        Ptr v_81;
        v_81 = ((Ptr*)(v_80))[0];
        Ptr v_108;
        v_108 = ((Ptr (*)(Ptr,Ptr))(v_81))(v_80,0);
        return v_108;
    } else if (newtype_b_1 == 1) {
        Ptr v_82;
        v_82 = newtype_t_0;
        Ptr v_83;
        v_83 = ((Ptr*)(v_82))[0];
        Ptr v_109;
        v_109 = ((Ptr (*)(Ptr,Ptr))(v_83))(v_82,1);
        return v_109;
    } else {
        return printf("%s","no match");
    }
}
Ptr newtype_map_0(Ptr newtype_f_2,Ptr newtype_ls_0) {
    Ptr v_84;
    v_84 = newtype_ls_0;
    Ptr v_87;
    v_87 = ((Ptr*)(v_84))[0];
    if (v_87 == 1) {
        return &newtype_Nil_0;
    } else if (v_87 == 0) {
        Ptr v_72;
        v_72 = ((Ptr*)(v_84))[1];
        Ptr v_73;
        v_73 = ((Ptr*)(v_84))[2];
        Ptr newtype_xs_0;
        newtype_xs_0 = v_73;
        Ptr newtype_x_3;
        newtype_x_3 = v_72;
        Ptr v_111;
        v_111 = newtype_map_0(newtype_f_2,newtype_xs_0);
        Ptr v_85;
        v_85 = newtype_f_2;
        Ptr v_86;
        v_86 = ((Ptr*)(v_85))[0];
        Ptr v_110;
        v_110 = ((Ptr (*)(Ptr,Ptr))(v_86))(v_85,newtype_x_3);
        Ptr v_112;
        v_112 = malloc(sizeof(Ptr)*3);
        ((Ptr*)(v_112))[0] = 0;
        ((Ptr*)(v_112))[1] = v_110;
        ((Ptr*)(v_112))[2] = v_111;
        return v_112;
    } else {
        return printf("%s","no match");
    }
}
Ptr newtype_putInt_0(Ptr newtype_x_4) {
    Ptr v_113;
    v_113 = printf("%i\n",newtype_x_4);
    return v_113;
}
Ptr newtype_add_0(Ptr newtype_x_5,Ptr newtype_y_0) {
    Ptr v_114;
    v_114 = (int)newtype_x_5 + (int)newtype_y_0;
    return v_114;
}
Ptr newtype_matchUnit_0(Ptr newtype_x_6) {
    if (newtype_x_6 == 0) {
        return 0;
    } else {
        return printf("%s","no match");
    }
}
Ptr newtype_start_0(Ptr newtype_x_7) {
    Ptr v_122;
    v_122 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_122))[0] = 0;
    ((Ptr*)(v_122))[1] = 2;
    ((Ptr*)(v_122))[2] = &newtype_Nil_0;
    Ptr v_123;
    v_123 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_123))[0] = 0;
    ((Ptr*)(v_123))[1] = 1;
    ((Ptr*)(v_123))[2] = v_122;
    Ptr newtype_is_0;
    newtype_is_0 = v_123;
    Ptr v_120;
    v_120 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_120))[0] = v_88;
    Ptr v_121;
    v_121 = newtype_map_0(v_120,newtype_is_0);
    Ptr newtype_ns_0;
    newtype_ns_0 = v_121;
    Ptr v_117;
    v_117 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_117))[0] = v_91;
    ((Ptr*)(v_117))[1] = 4;
    Ptr v_118;
    v_118 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_118))[0] = v_95;
    ((Ptr*)(v_118))[1] = v_117;
    Ptr v_119;
    v_119 = newtype_map_0(v_118,newtype_ns_0);
    Ptr newtype_js_0;
    newtype_js_0 = v_119;
    Ptr v_115;
    v_115 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_115))[0] = v_99;
    Ptr v_116;
    v_116 = newtype_map_0(v_115,newtype_js_0);
    Ptr newtype____0;
    newtype____0 = v_116;
    return 0;
}
