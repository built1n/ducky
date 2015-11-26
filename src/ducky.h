#include <stdbool.h>
#include <stdint.h>
#include "platform.h"

#define DUCKY_MAGIC 0x4475634B /* DucK */
#define VARFORMAT "%d"
#define VARNAME_MAX 24

#define DEFAULT_DELAY 0
#define STRING_DELAY 0
#define TOKEN_IS(str) (strcmp(tok, str) == 0)
#define MAX_LINE_LEN 512

#define MAXOPSTACK 64
#define MAXNUMSTACK 64
#define CALL_STACK_SZ 64
#define VARMAP_SIZE 256

/* define for rockbox cross-compile */
#define DUCKY_ROCKBOX

#define ARRAYLEN(x) (sizeof(x)/sizeof(x[0]))

#define MIN(x,y) ((x<y)?(x):(y))

#define OK 0

enum special_id { SPECIAL_NULL = 0, SPECIAL_RAND, SPECIAL_TIME };

typedef int32_t imm_t;
typedef uint8_t instr_t;
typedef uint16_t varid_t;
typedef imm_t vartype;

int ducky_interp(int fd, bool verbose);
int ducky_compile(int fd, bool verbose, int out_fd);
int ducky_vm(int fd);
int ducky_to_c(int fd, int out_fd);

int read_line(int fd, char *buf, size_t sz);

off_t *index_lines(int fd, unsigned *numlines, bool labels, bool (*isValidVariable)(const char*), void (*setConst)(const char*, bool), void (*setVariable)(const char*, vartype));

void vid_logf(const char *fmt, ...) __attribute__((format(printf,1,2)));

void error(jmp_buf exit_point, int current_line, const char *fmt, ...) __attribute__((noreturn,format(printf,3,4)));

#define ERROR(fmt, ...) (error(exit_point, current_line, fmt, ##__VA_ARGS__))
void warning_real(int current_line, const char *fmt, ...) __attribute__((format(printf,2,3)));
#define WARNING(fmt, ...) (warning_real(current_line, fmt, ##__VA_ARGS__))
