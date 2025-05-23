#include <VFMUL.h>
#include "common.h"

int main(int argc, char* argv[]) {
    if(argc != 2){
        printf("usage: %s <rounding-mode>\n", argv[0]);
        return -1;
    }

    int rm = get_str_index(argv[1], rounding_modes, 5);
    if(rm == -1){
        printf("unknown rounding mode: %s\n", argv[1]);
        return -1;
    }

    VFMUL module;

    for(int i = 0; i<10; i++){
        module.reset = 1;
        module.clock = 0;
        module.eval();
        module.clock = 1;
        module.eval();
    }
    module.reset = 0;
    module.clock =0;
    module.eval();
    module.clock = 1;
    module.eval();

    uint64_t a, b, ref_result, ref_fflags;
    uint64_t dut_result, dut_fflags;

    uint64_t cnt = 0;
    uint64_t error = 0;

    module.io_roundingMode = rm;
    while(scanf("%lx %lx %lx %lx", &a, &b, &ref_result, &ref_fflags) != EOF){
        module.io_a = a;
        module.io_b = b;
        module.clock = 0;
        module.eval();
        module.clock = 1;
        module.eval();
        dut_result = module.io_result;
        dut_fflags = module.io_fflags;
        if( (dut_result != ref_result || dut_fflags != ref_fflags) ){
            printf("[%ld] input: %lx %lx\n", cnt, a, b);
            printf("[%ld] dut_sum: %lx dut_fflags: %lx\n", cnt, dut_result, dut_fflags);
            printf("[%ld] ref_sum: %lx ref_fflags: %lx\n", cnt, ref_result, ref_fflags);
            error++;
            return -1;
        }
        cnt++;
    }
    printf("cnt = %ld error=%ld\n", cnt, error);
}
