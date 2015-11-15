#include <platform.h>
#include <ducky.h>

#include "opcodes.h"

#define STACK_SZ 1024
#define CALLSTACK_SZ 64
#define MAX_VARS 65536

imm_t callstack[CALLSTACK_SZ];
unsigned callstack_pointer;

imm_t stack[STACK_SZ];
unsigned stack_pointer;

struct var_t {
    vartype val;
    bool constant;
};

struct var_t vars[MAX_VARS];
int file_des;
unsigned current_line, num_lines;

off_t *line_offset;

bool want_quit;
int repeats_left;
imm_t repeat_line;

static void error(const char *fmt, ...) __attribute__((noreturn,format(print,1,2)));
static void vid_write(const char *str);
static void vid_logf(const char *fmt, ...) __attribute__((format(printf,1,2)));

instr_t read_instr(void)
{
    instr_t ret;
    if(read(file_des, &ret, sizeof(ret)) != sizeof(ret))
        want_quit = true;
    return ret;
}

imm_t read_imm(void)
{
    imm_t ret;
    if(read(file_des, &ret, sizeof(ret)) != sizeof(ret))
        want_quit = true;
    return ret;
}

imm_t read_varid(void)
{
    varid_t ret;
    if(read(file_des, &ret, sizeof(ret)) != sizeof(ret))
        want_quit = true;
    return ret;
}

imm_t read_byte(void)
{
    unsigned char ret;
    if(read(file_des, &ret, sizeof(ret)) != sizeof(ret))
        want_quit = true;
    return ret;
}

static void vid_write(const char *str)
{
    printf("%s", str);
}

static void __attribute__((format(printf,1,2))) vid_writef(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    vid_write(fmtbuf);
    va_end(ap);
}

static void __attribute__((noreturn,format(printf,1,2))) error(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    if(current_line)
        vid_writef("Line %d: ", current_line);
    vid_writef("ERROR: %s\n",  fmtbuf);
    va_end(ap);

    exit(EXIT_FAILURE);
}

static void __attribute__((format(printf,1,2))) warning(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    vid_writef("Line %d: WARNING: %s\n", current_line, fmtbuf);
    va_end(ap);
}

static void init_globals(void)
{
    current_line = 0;
    stack_pointer = 0;
    callstack_pointer = 0;
    want_quit = false;
    repeats_left = 0;
    repeat_line = 0;
}

static inline void push(imm_t n)
{
    if(stack_pointer < STACK_SZ)
        stack[stack_pointer++] = n;
    else
        error("stack overflow");
}

static inline imm_t pop(void)
{
    if(stack_pointer > 0)
        return stack[--stack_pointer];
    else
        error("stack underflow");
}

static inline vartype getvar(varid_t varid)
{
    if(varid < ARRAYLEN(vars))
        return vars[varid].val;
}

static inline void setvar(varid_t varid, vartype val)
{
    if(varid < ARRAYLEN(vars) && !vars[varid].constant)
        vars[varid].val = val;
    else
        error("cannot modify variable");
}

static inline void mkconst(varid_t varid)
{
    if(varid < ARRAYLEN(vars))
        vars[varid].constant = true;
}

static inline void jump(imm_t line)
{
    if(1 <= line && line <= num_lines)
    {
        lseek(file_des, line_offset[line], SEEK_SET);
        current_line = line;
    }
    else
        error("jump target out of bounds");
}

void pushimm_handler(void)
{
    push(read_imm());
}

void pushvar_handler(void)
{
    push(getvar(read_varid()));
}

void pop_handler(void)
{
    setvar(read_varid(), pop());
}

void mkconst_handler(void)
{
    mkconst(read_varid());
}

void incvar_handler(void)
{
    varid_t varid = read_varid();
    if(varid < ARRAYLEN(vars))
        ++vars[varid].val;
}

void decvar_handler(void)
{
    varid_t varid = read_varid();
    if(varid < ARRAYLEN(vars))
        --vars[varid].val;
}

void writestr_handler(void)
{
    while(1)
    {
        char c = read_byte();
        if(c)
            vid_writef("%c", c);
        else
            break;
    }
}

void repeat_handler(void)
{
    if(repeats_left > 0)
    {
        if(repeat_line + 1 != current_line)
            error("nested REPEAT");
        --repeats_left;
        if(repeats_left > 0)
        {
            jump(repeat_line);
        }
    }
    else
    {
        repeat_line = pop();
        repeats_left = pop() - 1;
        jump(repeat_line);
    }
}

void jump_handler(void)
{
    jump(pop());
}

void subcall_handler(void)
{
    if(callstack_pointer < CALLSTACK_SZ)
    {
        callstack[callstack_pointer++] = current_line + 1;
        jump(pop());
    }
    else
        error("call stack overflow");
}

void subret_handler(void)
{
    if(callstack_pointer > 0)
    {
        jump(callstack[--callstack_pointer]);
    }
    else
        error("call stack underflow");
}

void if_handler(void)
{
    imm_t line = pop();
    if(!pop())
        jump(line);
}

void delay_handler(void)
{
    imm_t ms = pop();

    struct timespec t;
    t.tv_sec = ms / 1000;
    t.tv_nsec = (ms % 1000) * 1000000;
    nanosleep(&t, NULL);
}

void logvar_handler(void)
{
    vid_writef(VARFORMAT, pop());
}

void quit_handler(void)
{
    want_quit = true;
}

void logascii_handler(void)
{
    vid_writef("%c", pop());
}

void neg_handler(void)
{
    stack[stack_pointer - 1] = -stack[stack_pointer - 1];
}

static vartype eval_exp(vartype a1, vartype a2)
{
    return a2<0 ? 0 : (a2==0?1:a1*eval_exp(a1, a2-1));
}

void pow_handler(void)
{
    imm_t pow = pop();
    imm_t base = pop();
    push(eval_exp(base, pow));
}

void mul_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a*b);
}

void div_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a/b);
}

void mod_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a%b);
}

void add_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a+b);
}

void sub_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a-b);
}

void eq_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a==b);
}

void neq_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a!=b);
}

void leq_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a<=b);
}

void geq_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a>=b);
}

void lt_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a<b);
}

void gt_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a>b);
}

void lognot_handler(void)
{
    push(!pop());
}

void logand_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a&&b);
}

void logor_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a||b);
}

void bitand_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a&b);
}

void bitor_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a|b);
}

void bitxor_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a^b);
}

void bitcomp_handler(void)
{
    push(~pop());
}

void lsh_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a<<b);
}

void rsh_handler(void)
{
    imm_t b = pop();
    imm_t a = pop();
    push(a>>b);
}

void sqrt_handler(void)
{
    push(sqrt(pop()));
}

void decl_const(void)
{
    /* no checking, only the compiler can output this instruction */
    varid_t varid = read_varid();
    vars[varid].val = read_imm();
    vars[varid].constant = true;
}

void inc_line_pointer(void)
{
    ++current_line;

    vars[0].val = current_line;
}

static void (*instr_tab[0x100])(void) = {
    pushimm_handler,   /*  0x0   */
    pushvar_handler,   /*  0x1   */
    pop_handler,       /*  0x2   */
    mkconst_handler,   /*  0x3   */
    incvar_handler,    /*  0x4   */
    decvar_handler,    /*  0x5   */
    writestr_handler,  /*  0x6   */
    repeat_handler,    /*  0x7   */
    jump_handler,      /*  0x8   */
    subcall_handler,   /*  0x9   */
    subret_handler,    /*  0xa   */
    if_handler,        /*  0xb   */
    delay_handler,     /*  0xc   */
    logvar_handler,    /*  0xd   */
    quit_handler,      /*  0xe   */
    logascii_handler,  /*  0xf   */
    neg_handler,       /*  0x10  */
    pow_handler,       /*  0x11  */
    mul_handler,       /*  0x12  */
    div_handler,       /*  0x13  */
    mod_handler,       /*  0x14  */
    add_handler,       /*  0x15  */
    sub_handler,       /*  0x16  */
    eq_handler,        /*  0x17  */
    neq_handler,       /*  0x18  */
    leq_handler,       /*  0x19  */
    geq_handler,       /*  0x1a  */
    lt_handler,        /*  0x1b  */
    gt_handler,        /*  0x1c  */
    lognot_handler,    /*  0x1d  */
    logand_handler,    /*  0x1e  */
    logor_handler,     /*  0x1f  */
    bitand_handler,    /*  0x20  */
    bitor_handler,     /*  0x21  */
    bitxor_handler,    /*  0x22  */
    bitcomp_handler,   /*  0x23  */
    lsh_handler,       /*  0x24  */
    rsh_handler,       /*  0x25  */
    sqrt_handler,      /*  0x26  */
    decl_const,        /*  0x27  */
    NULL,              /*  0x28  */
    NULL,              /*  0x29  */
    NULL,              /*  0x2a  */
    NULL,              /*  0x2b  */
    NULL,              /*  0x2c  */
    NULL,              /*  0x2d  */
    NULL,              /*  0x2e  */
    NULL,              /*  0x2f  */
    NULL,              /*  0x30  */
    NULL,              /*  0x31  */
    NULL,              /*  0x32  */
    NULL,              /*  0x33  */
    NULL,              /*  0x34  */
    NULL,              /*  0x35  */
    NULL,              /*  0x36  */
    NULL,              /*  0x37  */
    NULL,              /*  0x38  */
    NULL,              /*  0x39  */
    NULL,              /*  0x3a  */
    NULL,              /*  0x3b  */
    NULL,              /*  0x3c  */
    NULL,              /*  0x3d  */
    NULL,              /*  0x3e  */
    NULL,              /*  0x3f  */
    NULL,              /*  0x40  */
    NULL,              /*  0x41  */
    NULL,              /*  0x42  */
    NULL,              /*  0x43  */
    NULL,              /*  0x44  */
    NULL,              /*  0x45  */
    NULL,              /*  0x46  */
    NULL,              /*  0x47  */
    NULL,              /*  0x48  */
    NULL,              /*  0x49  */
    NULL,              /*  0x4a  */
    NULL,              /*  0x4b  */
    NULL,              /*  0x4c  */
    NULL,              /*  0x4d  */
    NULL,              /*  0x4e  */
    NULL,              /*  0x4f  */
    NULL,              /*  0x50  */
    NULL,              /*  0x51  */
    NULL,              /*  0x52  */
    NULL,              /*  0x53  */
    NULL,              /*  0x54  */
    NULL,              /*  0x55  */
    NULL,              /*  0x56  */
    NULL,              /*  0x57  */
    NULL,              /*  0x58  */
    NULL,              /*  0x59  */
    NULL,              /*  0x5a  */
    NULL,              /*  0x5b  */
    NULL,              /*  0x5c  */
    NULL,              /*  0x5d  */
    NULL,              /*  0x5e  */
    NULL,              /*  0x5f  */
    NULL,              /*  0x60  */
    NULL,              /*  0x61  */
    NULL,              /*  0x62  */
    NULL,              /*  0x63  */
    NULL,              /*  0x64  */
    NULL,              /*  0x65  */
    NULL,              /*  0x66  */
    NULL,              /*  0x67  */
    NULL,              /*  0x68  */
    NULL,              /*  0x69  */
    NULL,              /*  0x6a  */
    NULL,              /*  0x6b  */
    NULL,              /*  0x6c  */
    NULL,              /*  0x6d  */
    NULL,              /*  0x6e  */
    NULL,              /*  0x6f  */
    NULL,              /*  0x70  */
    NULL,              /*  0x71  */
    NULL,              /*  0x72  */
    NULL,              /*  0x73  */
    NULL,              /*  0x74  */
    NULL,              /*  0x75  */
    NULL,              /*  0x76  */
    NULL,              /*  0x77  */
    NULL,              /*  0x78  */
    NULL,              /*  0x79  */
    NULL,              /*  0x7a  */
    NULL,              /*  0x7b  */
    NULL,              /*  0x7c  */
    NULL,              /*  0x7d  */
    NULL,              /*  0x7e  */
    NULL,              /*  0x7f  */
    NULL,              /*  0x80  */
    NULL,              /*  0x81  */
    NULL,              /*  0x82  */
    NULL,              /*  0x83  */
    NULL,              /*  0x84  */
    NULL,              /*  0x85  */
    NULL,              /*  0x86  */
    NULL,              /*  0x87  */
    NULL,              /*  0x88  */
    NULL,              /*  0x89  */
    NULL,              /*  0x8a  */
    NULL,              /*  0x8b  */
    NULL,              /*  0x8c  */
    NULL,              /*  0x8d  */
    NULL,              /*  0x8e  */
    NULL,              /*  0x8f  */
    NULL,              /*  0x90  */
    NULL,              /*  0x91  */
    NULL,              /*  0x92  */
    NULL,              /*  0x93  */
    NULL,              /*  0x94  */
    NULL,              /*  0x95  */
    NULL,              /*  0x96  */
    NULL,              /*  0x97  */
    NULL,              /*  0x98  */
    NULL,              /*  0x99  */
    NULL,              /*  0x9a  */
    NULL,              /*  0x9b  */
    NULL,              /*  0x9c  */
    NULL,              /*  0x9d  */
    NULL,              /*  0x9e  */
    NULL,              /*  0x9f  */
    NULL,              /*  0xa0  */
    NULL,              /*  0xa1  */
    NULL,              /*  0xa2  */
    NULL,              /*  0xa3  */
    NULL,              /*  0xa4  */
    NULL,              /*  0xa5  */
    NULL,              /*  0xa6  */
    NULL,              /*  0xa7  */
    NULL,              /*  0xa8  */
    NULL,              /*  0xa9  */
    NULL,              /*  0xaa  */
    NULL,              /*  0xab  */
    NULL,              /*  0xac  */
    NULL,              /*  0xad  */
    NULL,              /*  0xae  */
    NULL,              /*  0xaf  */
    NULL,              /*  0xb0  */
    NULL,              /*  0xb1  */
    NULL,              /*  0xb2  */
    NULL,              /*  0xb3  */
    NULL,              /*  0xb4  */
    NULL,              /*  0xb5  */
    NULL,              /*  0xb6  */
    NULL,              /*  0xb7  */
    NULL,              /*  0xb8  */
    NULL,              /*  0xb9  */
    NULL,              /*  0xba  */
    NULL,              /*  0xbb  */
    NULL,              /*  0xbc  */
    NULL,              /*  0xbd  */
    NULL,              /*  0xbe  */
    NULL,              /*  0xbf  */
    NULL,              /*  0xc0  */
    NULL,              /*  0xc1  */
    NULL,              /*  0xc2  */
    NULL,              /*  0xc3  */
    NULL,              /*  0xc4  */
    NULL,              /*  0xc5  */
    NULL,              /*  0xc6  */
    NULL,              /*  0xc7  */
    NULL,              /*  0xc8  */
    NULL,              /*  0xc9  */
    NULL,              /*  0xca  */
    NULL,              /*  0xcb  */
    NULL,              /*  0xcc  */
    NULL,              /*  0xcd  */
    NULL,              /*  0xce  */
    NULL,              /*  0xcf  */
    NULL,              /*  0xd0  */
    NULL,              /*  0xd1  */
    NULL,              /*  0xd2  */
    NULL,              /*  0xd3  */
    NULL,              /*  0xd4  */
    NULL,              /*  0xd5  */
    NULL,              /*  0xd6  */
    NULL,              /*  0xd7  */
    NULL,              /*  0xd8  */
    NULL,              /*  0xd9  */
    NULL,              /*  0xda  */
    NULL,              /*  0xdb  */
    NULL,              /*  0xdc  */
    NULL,              /*  0xdd  */
    NULL,              /*  0xde  */
    NULL,              /*  0xdf  */
    NULL,              /*  0xe0  */
    NULL,              /*  0xe1  */
    NULL,              /*  0xe2  */
    NULL,              /*  0xe3  */
    NULL,              /*  0xe4  */
    NULL,              /*  0xe5  */
    NULL,              /*  0xe6  */
    NULL,              /*  0xe7  */
    NULL,              /*  0xe8  */
    NULL,              /*  0xe9  */
    NULL,              /*  0xea  */
    NULL,              /*  0xeb  */
    NULL,              /*  0xec  */
    NULL,              /*  0xed  */
    NULL,              /*  0xee  */
    NULL,              /*  0xef  */
    NULL,              /*  0xf0  */
    NULL,              /*  0xf1  */
    NULL,              /*  0xf2  */
    NULL,              /*  0xf3  */
    NULL,              /*  0xf4  */
    NULL,              /*  0xf5  */
    NULL,              /*  0xf6  */
    NULL,              /*  0xf7  */
    NULL,              /*  0xf8  */
    NULL,              /*  0xf9  */
    NULL,              /*  0xfa  */
    NULL,              /*  0xfb  */
    NULL,              /*  0xfc  */
    NULL,              /*  0xfd  */
    NULL,              /*  0xfe  */
    inc_line_pointer,  /*  0xff  */
};

void ducky_vm(int fd)
{
    file_des = fd;

    init_globals();

    if(read_imm() != DUCKY_MAGIC)
        error("unknown format");

    num_lines = read_imm();
    line_offset = malloc(num_lines + 1);
    for(unsigned int i = 1; i <= num_lines; ++i)
    {
        line_offset[i] = read_imm();
    }

    /* and... execute! */
    while(!want_quit)
    {
        instr_t instr = read_instr();
        if(want_quit)
            break;
        void (*handler)(void) = instr_tab[instr];
        if(handler)
            handler();
        else
            error("invalid instruction");
    }
}
