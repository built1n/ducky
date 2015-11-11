#include <stdbool.h>
#include <stdint.h>

#define DUCKY_MAGIC 0x4475634B /* DucK */
#define VARFORMAT "%lld"

#define ARRAYLEN(x) (sizeof(x)/sizeof(x[0]))

#define MIN(x,y) ((x<y)?(x):(y))

void ducky_main(int fd, bool verbose);
void ducky_compile(int fd, bool verbose, int out_fd);
void ducky_vm(int fd);

typedef int32_t imm_t;
typedef uint8_t instr_t;
typedef uint16_t varid_t;
typedef imm_t vartype;
