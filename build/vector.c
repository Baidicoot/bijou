#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
#include "mutref.h"
Ptr vector_MVector_0(Ptr v_126,Ptr v_127);
Ptr vector_newMVec_0(Ptr vector_s_0);
Ptr vector_lenMVec_0(Ptr vector_vec_0);
Ptr vector_writeMVec_0(Ptr vector_vec_1,Ptr vector_idx_0,Ptr vector_x_0);
Ptr vector_indexMVec_0(Ptr vector_vec_2,Ptr vector_idx_1);
Ptr vector_growMVec_0(Ptr vector_vec_3,Ptr vector_growth_0);
Ptr vector_go_0(Ptr vector_s_2,Ptr vector_n_0);
Ptr vector_putMString_0(Ptr vector_s_1);
Ptr vector_start_0(Ptr vector_x_2);
#include<string.h>

Ptr ptrs_alloc(Ptr n) {
    return malloc(sizeof(Ptr)*(int)n);
}

Ptr ptrs_cpy(Ptr dest, Ptr src, Ptr n) {
    memcpy(dest,src,sizeof(Ptr)*(int)n);
    return 0;
}

Ptr ptrs_write(Ptr a,Ptr i,Ptr v) {
    ((Ptr*)a)[(int)i] = v;
    return 0;
}

Ptr ptrs_read(Ptr a,Ptr i) {
    return ((Ptr*)a)[(int)i];
}

void main(int main_arg) {
    vector_start_0(main_arg);
}
Ptr vector_MVector_0(Ptr v_126,Ptr v_127) {
    Ptr v_132;
    v_132 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_132))[0] = v_126;
    ((Ptr*)(v_132))[1] = v_127;
    return v_132;
}
Ptr vector_newMVec_0(Ptr vector_s_0) {
    Ptr v_134;
    v_134 = ptrs_alloc(vector_s_0);
    Ptr v_135;
    v_135 = mutref_newref_0(v_134);
    Ptr v_133;
    v_133 = mutref_newref_0(vector_s_0);
    Ptr v_136;
    v_136 = malloc(sizeof(Ptr)*2);
    ((Ptr*)(v_136))[0] = v_133;
    ((Ptr*)(v_136))[1] = v_135;
    return v_136;
}
Ptr vector_lenMVec_0(Ptr vector_vec_0) {
    Ptr v_128;
    v_128 = vector_vec_0;
    Ptr v_118;
    v_118 = ((Ptr*)(v_128))[0];
    Ptr v_119;
    v_119 = ((Ptr*)(v_128))[1];
    Ptr vector_ptr_0;
    vector_ptr_0 = v_119;
    Ptr vector_len_0;
    vector_len_0 = v_118;
    Ptr v_137;
    v_137 = mutref_getref_0(vector_len_0);
    return v_137;
}
Ptr vector_writeMVec_0(Ptr vector_vec_1,Ptr vector_idx_0,Ptr vector_x_0) {
    Ptr v_129;
    v_129 = vector_vec_1;
    Ptr v_120;
    v_120 = ((Ptr*)(v_129))[0];
    Ptr v_121;
    v_121 = ((Ptr*)(v_129))[1];
    Ptr vector_ptr_1;
    vector_ptr_1 = v_121;
    Ptr vector_len_1;
    vector_len_1 = v_120;
    Ptr v_138;
    v_138 = mutref_getref_0(vector_ptr_1);
    Ptr v_139;
    v_139 = ptrs_write(v_138,vector_idx_0,vector_x_0);
    Ptr vector____0;
    vector____0 = v_139;
    return 0;
}
Ptr vector_indexMVec_0(Ptr vector_vec_2,Ptr vector_idx_1) {
    Ptr v_130;
    v_130 = vector_vec_2;
    Ptr v_122;
    v_122 = ((Ptr*)(v_130))[0];
    Ptr v_123;
    v_123 = ((Ptr*)(v_130))[1];
    Ptr vector_ptr_2;
    vector_ptr_2 = v_123;
    Ptr vector_len_2;
    vector_len_2 = v_122;
    Ptr v_140;
    v_140 = mutref_getref_0(vector_ptr_2);
    Ptr v_141;
    v_141 = ptrs_read(v_140,vector_idx_1);
    return v_141;
}
Ptr vector_growMVec_0(Ptr vector_vec_3,Ptr vector_growth_0) {
    Ptr v_131;
    v_131 = vector_vec_3;
    Ptr v_124;
    v_124 = ((Ptr*)(v_131))[0];
    Ptr v_125;
    v_125 = ((Ptr*)(v_131))[1];
    Ptr vector_ptr_3;
    vector_ptr_3 = v_125;
    Ptr vector_len_3;
    vector_len_3 = v_124;
    Ptr v_148;
    v_148 = mutref_getref_0(vector_len_3);
    Ptr v_149;
    v_149 = (int)v_148 + (int)vector_growth_0;
    Ptr vector_len_prime_0;
    vector_len_prime_0 = v_149;
    Ptr v_147;
    v_147 = ptrs_alloc(vector_len_prime_0);
    Ptr vector_ptr_prime_0;
    vector_ptr_prime_0 = v_147;
    Ptr v_145;
    v_145 = mutref_getref_0(vector_len_3);
    Ptr v_144;
    v_144 = mutref_getref_0(vector_ptr_3);
    Ptr v_146;
    v_146 = ptrs_cpy(vector_ptr_prime_0,v_144,v_145);
    Ptr vector____1;
    vector____1 = v_146;
    Ptr v_143;
    v_143 = mutref_putref_0(vector_ptr_3,vector_ptr_prime_0);
    Ptr vector____2;
    vector____2 = v_143;
    Ptr v_142;
    v_142 = mutref_putref_0(vector_len_3,vector_len_prime_0);
    Ptr vector____3;
    vector____3 = v_142;
    return vector_len_prime_0;
}
Ptr vector_go_0(Ptr vector_s_2,Ptr vector_n_0) {
    Ptr v_150;
    v_150 = vector_lenMVec_0(vector_s_2);
    Ptr v_151;
    v_151 = (int)v_150 - (int)vector_n_0;
    if (v_151 == 0) {
        return 0;
    } else {
        Ptr v_156;
        v_156 = vector_lenMVec_0(vector_s_2);
        Ptr v_157;
        v_157 = (int)v_156 - (int)vector_n_0;
        Ptr vector_x_1;
        vector_x_1 = v_157;
        Ptr v_154;
        v_154 = vector_indexMVec_0(vector_s_2,vector_n_0);
        Ptr v_155;
        v_155 = printf(v_154);
        Ptr vector____4;
        vector____4 = v_155;
        Ptr v_152;
        v_152 = (int)vector_n_0 + (int)1;
        Ptr v_153;
        v_153 = vector_go_0(vector_s_2,v_152);
        return v_153;
    }
}
Ptr vector_putMString_0(Ptr vector_s_1) {
    Ptr v_158;
    v_158 = vector_go_0(vector_s_1,0);
    return v_158;
}
Ptr vector_start_0(Ptr vector_x_2) {
    Ptr v_166;
    v_166 = vector_newMVec_0(2);
    Ptr vector_vec_4;
    vector_vec_4 = v_166;
    Ptr v_165;
    v_165 = vector_writeMVec_0(vector_vec_4,0,"hello, ");
    Ptr vector____5;
    vector____5 = v_165;
    Ptr v_164;
    v_164 = vector_writeMVec_0(vector_vec_4,1,"world!\n");
    Ptr vector____6;
    vector____6 = v_164;
    Ptr v_163;
    v_163 = vector_putMString_0(vector_vec_4);
    Ptr vector____7;
    vector____7 = v_163;
    Ptr v_162;
    v_162 = vector_growMVec_0(vector_vec_4,1);
    Ptr vector____8;
    vector____8 = v_162;
    Ptr v_161;
    v_161 = vector_writeMVec_0(vector_vec_4,1,"to ");
    Ptr vector____9;
    vector____9 = v_161;
    Ptr v_160;
    v_160 = vector_writeMVec_0(vector_vec_4,2,"you!\n");
    Ptr vector____10;
    vector____10 = v_160;
    Ptr v_159;
    v_159 = vector_putMString_0(vector_vec_4);
    Ptr vector____11;
    vector____11 = v_159;
    return 0;
}
