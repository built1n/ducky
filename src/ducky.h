#include <stdbool.h>
#include <stdint.h>

void ducky_main(int fd, bool verbose);
void ducky_compile(int fd, bool verbose, int out_fd);

typedef int32_t imm_t;
typedef uint8_t instr_t;
typedef uint16_t varid_t;
