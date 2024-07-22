#include <stdio.h>
#include <immintrin.h>

// Declare the external function
void add_ps(__m256* result, const __m256* a, const __m256* b);

int main() {
    __m256 a = _mm256_set_ps(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
    __m256 b = _mm256_set_ps(8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0);
    __m256 result;

    add_ps(&result, &a, &b);

    float* res = (float*)&result;
    for (int i = 0; i < 8; i++) {
        printf("%f ", res[i]);
    }
    printf("\n");

    return 0;
}
