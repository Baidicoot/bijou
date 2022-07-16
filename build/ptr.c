#include <stdlib.h>
#include <stdint.h>
typedef void* Ptr;
Ptr ptr_MVector__internal_0(Ptr v_33,Ptr v_34);
Ptr ptr_newMVec_0(Ptr ptr_len_0);
Ptr ptr_length_0(Ptr ptr_vec_0);
Ptr ptr_unsafe__index_0(Ptr ptr_vec_1,Ptr ptr_idx_0);
Ptr ptr_alloc(int n) {
    return malloc(sizeof(Ptr)*n);
}

Ptr ptr_offset(Ptr* a, int n) {
    return a + n*sizeof(Ptr);
}

Ptr ptr_ref(Ptr a) {
    return &a;
}

Ptr ptr_deref(Ptr* a) {
    return *a;
}

int ptr_assign(Ptr* a,Ptr v) {
    *a = v;
    return 0;
}

Ptr* ptr_cast(Ptr* a) {
    return a;
}

Ptr ref_getref(Ptr* a) {
    return *a;
}

int ref_putref(Ptr* a, Ptr v) {
    *a = v;
    return 0;
}

Ptr ptr_MVector__internal_0(Ptr v_33,Ptr v_34) {
    return v_33;
}
Ptr ptr_newMVec_0(Ptr ptr_len_0) {
    Ptr v_36;
    v_36 = (int)ptr_len_0 + (int)1;
    Ptr v_37;
    v_37 = ptr_alloc(v_36);
    Ptr ptr_ptr_0;
    ptr_ptr_0 = v_37;
    Ptr v_35;
    v_35 = ptr_assign(ptr_ptr_0,ptr_len_0);
    Ptr ptr____0;
    ptr____0 = v_35;
    return ptr_ptr_0;
}
Ptr ptr_length_0(Ptr ptr_vec_0) {
    Ptr v_31;
    v_31 = ptr_vec_0;
    Ptr ptr_ptr_1;
    ptr_ptr_1 = v_31;
    Ptr v_38;
    v_38 = ptr_deref(ptr_ptr_1);
    return v_38;
}
Ptr ptr_unsafe__index_0(Ptr ptr_vec_1,Ptr ptr_idx_0) {
    Ptr v_32;
    v_32 = ptr_vec_1;
    Ptr ptr_ptr_2;
    ptr_ptr_2 = v_32;
    Ptr v_39;
    v_39 = (int)ptr_idx_0 + (int)1;
    Ptr v_40;
    v_40 = ptr_offset(ptr_ptr_2,v_39);
    Ptr v_41;
    v_41 = ptr_deref(v_40);
    return v_41;
}
