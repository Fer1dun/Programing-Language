#ifndef GPP_H
#define GPP_H
typedef struct Valuef {
    int num;    /* numerator */
    int denom;  /* denominator */
} Valuef;
Valuef valuef_convert(char* str);
Valuef valuef_create(int num, int denom);
int gcd(int a, int b);
void simplify(Valuef* v);
Valuef valuef_add(Valuef v1, Valuef v2);
Valuef valuef_sub(Valuef v1, Valuef v2);
Valuef valuef_div(Valuef v1, Valuef v2);
Valuef valuef_mult(Valuef v1, Valuef v2);

#endif
