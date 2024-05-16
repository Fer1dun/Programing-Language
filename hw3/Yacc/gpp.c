#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"


Valuef valuef_convert(char* str) {
    Valuef v;

    char* f_position = strchr(str, 'b');
    int num_len = f_position - str;
    int denom_len = strlen(f_position + 1);

    char num_str[num_len + 1];
    char denom_str[denom_len + 1];

    strncpy(num_str, str, num_len);
    num_str[num_len] = '\0';

    strcpy(denom_str, f_position + 1);

    v.num = strtol(num_str, NULL, 10);
    v.denom = strtol(denom_str, NULL, 10);
    return v;
}

Valuef valuef_create(int num, int denom) {
    Valuef v;
    v.num = num;
    v.denom = denom;
    return v;
}

int gcd(int a, int b) {
    return a == 0 ? b : gcd(b % a, a);
}

void simplify(Valuef* v) {
    int div = gcd(abs(v->num), abs(v->denom));
    v->num = (v->num / div) * ((v->num < 0) ? -1 : 1);  // İşareti koruyarak güncelle
    v->denom = v->denom / div;
}


Valuef valuef_add(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.denom + v2.num * v1.denom;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef valuef_sub(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.denom - v2.num * v1.denom;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef valuef_mult(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.num;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef valuef_div(Valuef v1, Valuef v2) {
    Valuef r;
    printf("%d %d %d  %d\n",v1.num,v1.denom,v2.num,v2.denom );
    r.num = v1.num * v2.denom;
    r.denom = v1.denom * v2.num;
    simplify(&r);
    return r;
}
