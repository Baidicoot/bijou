#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr vector_MVector(Ptr v_64,Ptr v_65);
Ptr vector_Top();
Ptr vector_MVec_0(Ptr vector_s_0);
Ptr vector_writeMVec_0(Ptr vector_vec_0,Ptr vector_idx_0,Ptr vector_x_0);
Ptr vector_indexMVec_0(Ptr vector_vec_1,Ptr vector_idx_1);
Ptr vector_putStrLn_0(Ptr vector_x_1);
Ptr vector_start_0(Ptr vector_x_2);
#include<string.h>

Ptr ptrs_alloc(Ptr n) {
    return malloc(sizeof(Ptr)*(int)n);
}

Ptr ptrs_copy(Ptr dest, Ptr src, Ptr n) {
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
Ptr vector_MVector(Ptr v_64,Ptr v_65) {
    Ptr v_66;
    v_66 = malloc(sizeof(Ptr)*3);
    ((Ptr*)(v_66))[0] = 0;
    ((Ptr*)(v_66))[1] = v_64;
    ((Ptr*)(v_66))[2] = v_65;
    return v_66;
}
Ptr vector_Top() {
    Ptr v_67;
    v_67 = malloc(sizeof(Ptr)*1);
    ((Ptr*)(v_67))[0] = 0;
    return v_67;
}
Ptr vector_MVec_0(Ptr vector_s_0) {
    Ptr v_68;
    v_68 = ptrs_alloc(vector_s_0);
    Ptr v_69;
    v_69 = vector_MVector(vector_s_0,v_68);
    return v_69;
}
Ptr vector_writeMVec_0(Ptr vector_vec_0,Ptr vector_idx_0,Ptr vector_x_0) {
    if (((Ptr*)(vector_vec_0))[0] == 0) {
        Ptr v_60;
        v_60 = ((Ptr*)(vector_vec_0))[1];
        Ptr v_61;
        v_61 = ((Ptr*)(vector_vec_0))[2];
        Ptr vector_ptr_0;
        vector_ptr_0 = v_61;
        Ptr vector_len_0;
        vector_len_0 = v_60;
        Ptr v_71;
        v_71 = ptrs_write(vector_ptr_0,vector_idx_0,vector_x_0);
        Ptr vector___0;
        vector___0 = v_71;
        Ptr v_70;
        v_70 = vector_Top();
        return v_70;
    } else {
        return printf("%s","no match");
    }
}
Ptr vector_indexMVec_0(Ptr vector_vec_1,Ptr vector_idx_1) {
    if (((Ptr*)(vector_vec_1))[0] == 0) {
        Ptr v_62;
        v_62 = ((Ptr*)(vector_vec_1))[1];
        Ptr v_63;
        v_63 = ((Ptr*)(vector_vec_1))[2];
        Ptr vector_ptr_1;
        vector_ptr_1 = v_63;
        Ptr vector_len_1;
        vector_len_1 = v_62;
        Ptr v_72;
        v_72 = ptrs_read(vector_ptr_1,vector_idx_1);
        return v_72;
    } else {
        return printf("%s","no match");
    }
}
Ptr vector_putStrLn_0(Ptr vector_x_1) {
    Ptr v_73;
    v_73 = printf("%s\n",vector_x_1);
    return v_73;
}
Ptr vector_start_0(Ptr vector_x_2) {
    Ptr v_84;
    v_84 = vector_MVec_0(10);
    Ptr vector_v_0;
    vector_v_0 = v_84;
    Ptr v_83;
    v_83 = vector_writeMVec_0(vector_v_0,0,"test");
    Ptr vector___1;
    vector___1 = v_83;
    Ptr v_82;
    v_82 = vector_writeMVec_0(vector_v_0,1,"hello");
    Ptr vector___2;
    vector___2 = v_82;
    Ptr v_81;
    v_81 = vector_writeMVec_0(vector_v_0,2,"world");
    Ptr vector___3;
    vector___3 = v_81;
    Ptr v_79;
    v_79 = vector_indexMVec_0(vector_v_0,0);
    Ptr v_80;
    v_80 = vector_putStrLn_0(v_79);
    Ptr vector___4;
    vector___4 = v_80;
    Ptr v_77;
    v_77 = vector_indexMVec_0(vector_v_0,1);
    Ptr v_78;
    v_78 = vector_putStrLn_0(v_77);
    Ptr vector___5;
    vector___5 = v_78;
    Ptr v_75;
    v_75 = vector_indexMVec_0(vector_v_0,2);
    Ptr v_76;
    v_76 = vector_putStrLn_0(v_75);
    Ptr vector___6;
    vector___6 = v_76;
    Ptr v_74;
    v_74 = vector_Top();
    return v_74;
}
