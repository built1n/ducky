#include <stdbool.h>
#include <stdint.h>

#define DUCKY_MAGIC 0x4475634B /* DucK */
#define VARFORMAT "%d"
#define VARNAME_MAX 24
#define MAXOPSTACK 64

/* define for rockbox cross-compile */
#define DUCKY_ROCKBOX

#define ARRAYLEN(x) (sizeof(x)/sizeof(x[0]))

#define MIN(x,y) ((x<y)?(x):(y))

#define OK 0

int ducky_interp(int fd, bool verbose);
int ducky_compile(int fd, bool verbose, int out_fd);
int ducky_vm(int fd);
int ducky_to_c(int fd, int out_fd);

typedef int32_t imm_t;
typedef uint8_t instr_t;
typedef uint16_t varid_t;
typedef imm_t vartype;
