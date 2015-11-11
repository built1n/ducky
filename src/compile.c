#include <ducky.h>
#include <platform.h>

#include "opcodes.h"

/*******************************************************************************
 * Bytecode format (all numbers are little-endian):
 *
 * +0x00: 'DucK' signature (4 bytes)
 * +0x04: number of lines (4 bytes)
 * +0x08: line offsets (4 * num lines)
 * +    : compiler-generated init code
 * +(0x08): start of compiled code
 */

/*** Defines ***/

#define DEFAULT_DELAY 0
#define STRING_DELAY 0
#define TOKEN_IS(str) (strcmp(tok, str) == 0)
#define MAX_LINE_LEN 512

#define MAXOPSTACK 64
#define MAXNUMSTACK 64
#define CALL_STACK_SZ 64
#define VARMAP_SIZE 256

#define VARFORMAT "%lld"
#define VARNAME_MAX 24

#define ARRAYLEN(x) (sizeof(x)/sizeof(x[0]))

#define MIN(x,y) ((x<y)?(x):(y))

/*** Globals ***/

static off_t *line_offset = NULL;

static unsigned lines_executed = 0, current_line = 0, num_lines;

static unsigned call_stack[CALL_STACK_SZ];
static unsigned stack_frame = 0;

static int file_des = -1, out_fd = -1;

struct var_t {
    char name[VARNAME_MAX];
    bool constant;
};

static void error(const char *fmt, ...) __attribute__((noreturn,format(print,1,2)));
static void vid_write(const char *str);
static void vid_logf(const char *fmt, ...) __attribute__((format(printf,1,2)));
static void debug(const char *fmt, ...) __attribute__((format(printf,1,2)));
static bool isValidVariable(const char *c);

/* variables are stored in a chained hash map */
/* collisions are manageable, but should be minimized */

#define MAX_VARS 65336
static struct var_t vars[MAX_VARS];
unsigned bytes_written;

void write_instr(instr_t ins)
{
    write(out_fd, &ins, sizeof(ins));
    vid_logf("writing instruction 0x%x", ins);
    bytes_written += sizeof(ins);
}

void write_imm(imm_t imm)
{
    write(out_fd, &imm, sizeof(imm));
    vid_logf("writing immediate 0x%x", imm);
    bytes_written += sizeof(imm);
}

void write_byte(unsigned char c)
{
    write(out_fd, &c, 1);
    vid_logf("writing byte '%c'", c);
    bytes_written += 1;
}

void write_varid(varid_t varid)
{
    write(out_fd, &varid, sizeof(varid));
    vid_logf("writing varid %d which is %s", varid, vars[varid].name);
    bytes_written += sizeof(varid);
}

varid_t get_varid(const char *name)
{
    /* VERY VERY SLOW ALGORITHM, but it works */
    static int last_assigned_var = 0;
    for(int i = 0; i < last_assigned_var; ++i)
        if(strcmp(name, vars[i].name) == 0)
            return i;
    strlcpy(vars[last_assigned_var].name, name, VARNAME_MAX);
    ++last_assigned_var;
    return last_assigned_var - 1;
}

static void setVariable(const char *name, vartype val)
{
    write_instr(PUSHIMM);
    write_imm(val);
    write_instr(POP);
    write_varid(get_varid(name));
}

static void setConst(const char *name, bool c)
{
    if(c)
    {
        write_instr(MKCONST);
        write_varid(get_varid(name));
    }
}

static void incVar(const char *name)
{
    write_instr(INCVAR);
    write_varid(get_varid(name));
}

static void decVar(const char *name)
{
    write_instr(DECVAR);
    write_varid(get_varid(name));
}

/*** Utility functions ***/

static void exit_handler(void)
{
    if(file_des >= 0)
        close(file_des);
    if(line_offset)
        free(line_offset);
}

static void vid_write(const char *str)
{
    write_instr(WRITE_STR);
    while(*str)
    {
        write_byte(*str++);
    }
    write_byte('\0');
}

static void __attribute__((format(printf,1,2))) vid_logf(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    printf("%s\n", fmtbuf);
    va_end(ap);
}

static void __attribute__((noreturn,format(printf,1,2))) error(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    if(current_line)
        vid_logf("Line %d: ", current_line);
    vid_logf("ERROR: %s\n",  fmtbuf);
    va_end(ap);

    exit(EXIT_FAILURE);
}

static void __attribute__((format(printf,1,2))) warning(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    vid_logf("Line %d: WARNING: %s\n", current_line, fmtbuf);
    va_end(ap);
}

/* grabs a line from a file, -1 on error, returns # bytes read otherwise */
static int read_line(int fd, char *buf, size_t sz)
{
    unsigned i = 0;
    int bytes_read = 0;
    int status = 1;
    while(i < sz)
    {
        char c;
        status = read(fd, &c, 1);
        if(status != 1)
            break;

        ++bytes_read;

        if(c == '\r')
            continue;
        if(c == '\n' || c == EOF)
        {
            break;
        }

        buf[i++] = c;
    }
    buf[MIN(i, sz - 1)] = '\0';

    return (status <= 0)?-1:bytes_read;
}

/* depends on index_lines, indexes labels in the file */
static void index_labels(int fd)
{
    for(unsigned i = 1; i <= num_lines; ++i)
    {
        lseek(fd, line_offset[i], SEEK_SET);
        char buf[MAX_LINE_LEN];
        int status = read_line(fd, buf, sizeof(buf));

        /* exit early if failed or too short for a label */
        if(status < strlen("LBL"))
            break;

        char *save = NULL;
        char *tok = strtok_r(buf, " \t", &save);
        if(tok && (strcmp(tok, "LABEL") == 0 || strcmp("LBL", tok) == 0))
        {
            tok = strtok_r(NULL, " \t", &save);

            vid_logf("found label %s val %d", tok, i);
            if(tok && isValidVariable(tok))
            {
                setVariable(tok, i);
                setConst(tok, true);
            }
        }
    }
    lseek(fd, 0, SEEK_SET);
}

/* index_lines() precalculates the offset of each line for faster jumping */
/* the line data is only used by index_labels */

static off_t *index_lines(int fd, unsigned *numlines)
{
    size_t sz = sizeof(off_t);
    off_t *data = malloc(sz);

    /* this uses 1-indexed line numbers, so the first indice is wasted */
    unsigned idx = 1;

    while(1)
    {
        sz += sizeof(off_t);
        data = realloc(data, sz);
        data[idx] = lseek(fd, 0, SEEK_CUR);

        char buf[MAX_LINE_LEN];

        if(read_line(fd, buf, sizeof(buf)) < 0)
            break;

        ++idx;
    }

    lseek(fd, 0, SEEK_SET);

    *numlines = idx - 1;

    return data;
}

static void jump_line(int fd, unsigned where)
{
    if(1 <= where && where <= num_lines)
    {
        lseek(fd, line_offset[where], SEEK_SET);
    }
    else
        error("JUMP target out of range (%u)", where);
    current_line = where - 1;
}

static void sub_call(int fd, unsigned where)
{
    if(stack_frame < ARRAYLEN(call_stack))
    {
        call_stack[stack_frame] = current_line + 1;
        ++stack_frame;
        jump_line(fd, where);
    }
    else
        error("call stack overflow");
}

static void sub_return(int fd)
{
    if(stack_frame > 0)
    {
        --stack_frame;
        jump_line(fd, call_stack[stack_frame]);
    }
}

/** Expression Parsing **/

/* based on http://en.literateprograms.org/Shunting_yard_algorithm_%28C%29 */

static void eval_uminus(void)
{
    write_instr(NEG);
}
static void eval_exp(void)
{
    write_instr(POW);
}
static void eval_mul(void)
{
    write_instr(MULT);
}
static void eval_div(void)
{
    write_instr(DIV);
}
static void eval_mod(void)
{
    write_instr(MOD);
}
static void eval_add(void)
{
    write_instr(ADD);
}
static void eval_sub(void)
{
    write_instr(SUB);
}
static void eval_eq(void)
{
    write_instr(EQ);
}
static void eval_neq(void)
{
    write_instr(NEQ);
}
static void eval_leq(void)
{
    write_instr(LEQ);
}
static void eval_geq(void)
{
    write_instr(GEQ);
}
static void eval_lt(void)
{
    write_instr(LT);
}
static void eval_gt(void)
{
    write_instr(GT);
}
static void eval_log_neg(void)
{
    write_instr(LOGNOT);
}
static void eval_log_and(void)
{
    write_instr(LOGAND);
}
static void eval_log_or(void)
{
    write_instr(LOGOR);
}
static void eval_bit_and(void)
{
    write_instr(BITAND);
}
static void eval_bit_xor(void)
{
    write_instr(BITXOR);
}
static void eval_bit_or(void)
{
    write_instr(BITOR);
}
static void eval_bit_comp(void)
{
    write_instr(BITCOMP);
}
static void eval_lsh(void)
{
    write_instr(LSH);
}
static void eval_rsh(void)
{
    write_instr(RSH);
}
static void eval_sqrt(void)
{
    write_instr(SQRT);
}

enum {ASSOC_NONE=0, ASSOC_LEFT, ASSOC_RIGHT};

/* order matters in this table, because operators can share prefixes */
/* apart from that, they should be ordered by frequency of use */
/* operator precedence is based on that of C */
/* frequency is based off a crude analysis of the rockbox source tree: */

/* 99639 * */
/* 48282 -  */
/* 46639 +  */
/* 27678 & */
/* 24542 <  */
/* 21862 /  */
/* 20000 | */
/* 19138 == */
/* 12694 %  */
/* 11619 >  */
/* 11087 !  */
/* 8230 << */
/* 7339 && */
/* 7180 != */
/* 6010 >> */
/* 5575 || */
/* 3121 ~ */
/* 1311 ^ */

/* arrays are implemented as UNARY OPERATORS */

static struct op_s {
    const char *op;
    int prec;
    int assoc;
    int unary;
    void (*eval)(void);
    unsigned int len;
} ops[] = {
    {"+",     20,  ASSOC_LEFT,   0,  eval_add,       -1},
    {"-",     20,  ASSOC_LEFT,   0,  eval_sub,       -1},
    {"**",    40,  ASSOC_RIGHT,  0,  eval_exp,       -1},
    {"*",     30,  ASSOC_LEFT,   0,  eval_mul,       -1},
    {"&&",    8,   ASSOC_LEFT,   0,  eval_log_and,   -1},
    {"&",     11,  ASSOC_LEFT,   0,  eval_bit_and,   -1},
    {"<<",    15,  ASSOC_LEFT,   0,  eval_lsh,       -1},
    {">>",    15,  ASSOC_LEFT,   0,  eval_rsh,       -1},
    {"<=",    14,  ASSOC_LEFT,   0,  eval_leq,       -1},
    {">=",    14,  ASSOC_LEFT,   0,  eval_geq,       -1},
    {"<",     14,  ASSOC_LEFT,   0,  eval_lt,        -1},
    {">",     14,  ASSOC_LEFT,   0,  eval_gt,        -1},
    {"/",     30,  ASSOC_LEFT,   0,  eval_div,       -1},
    {"||",    7,   ASSOC_LEFT,   0,  eval_log_or,    -1},
    {"|",     9,   ASSOC_LEFT,   0,  eval_bit_or,    -1},
    {"==",    12,  ASSOC_LEFT,   0,  eval_eq,        -1},
    {"!=",    12,  ASSOC_LEFT,   0,  eval_neq,       -1},
    {"%",     30,  ASSOC_LEFT,   0,  eval_mod,       -1},
    {"!",     50,  ASSOC_LEFT,   1,  eval_log_neg,   -1},
    {"~",     50,  ASSOC_LEFT,   1,  eval_bit_comp,  -1},
    {"^",     10,  ASSOC_LEFT,   0,  eval_bit_xor,   -1},
    {"(",     0,   ASSOC_NONE,   0,  NULL,           -1},
    {")",     0,   ASSOC_NONE,   0,  NULL,           -1},
    {"sqrt",  1,   ASSOC_LEFT,   1,  eval_sqrt,      -1},
};

#define OPMAP_SIZE 25

static void op_hash_round(char c, uint32_t *hash)
{
    *hash *= 70;
    *hash ^= c;
}

static uint32_t op_hash(const char *c)
{
    uint32_t hash = 4412;
    while(1)
    {
        if(!*c)
            return hash;
        op_hash_round(*c, &hash);
        ++c;
    }
}

/* optimized hash map for fast lookup of operators */
static struct op_s op_map[OPMAP_SIZE];
static size_t longest_op = 0;

static void opmap_insert(struct op_s *op)
{
    if(op->len > longest_op)
        longest_op = op->len;

    uint32_t hash = op_hash(op->op) % OPMAP_SIZE;

    if(op_map[hash].op)
        error("hash map collision %lu: %s VS %s", hash, op->op, op_map[hash].op);
    memcpy(op_map+hash, op, sizeof(*op));
}

static void init_optable(void)
{
    memset(op_map, 0, sizeof(op_map));
    for(unsigned int i = 0; i < ARRAYLEN(ops); ++i)
    {
        ops[i].len = strlen(ops[i].op);
        opmap_insert(ops+i);
    }
}

static const struct op_s *getop(const char *ch, int *len)
{
    unsigned int i = 0;
    uint32_t hash = 4412;
    const struct op_s *poss = NULL;
    do {
        op_hash_round(ch[i], &hash);
        uint32_t modhash = hash % OPMAP_SIZE;

        if(op_map[modhash].op && strncmp(ch, op_map[modhash].op, op_map[modhash].len) == 0)
        {
            *len = op_map[modhash].len;
            poss = op_map + modhash;
        }
    } while(ch[i++] && i < longest_op);
    return poss;
}

static const struct op_s *opstack[MAXOPSTACK];
static int nopstack;

static void push_opstack(const struct op_s *op)
{
    if(nopstack>MAXOPSTACK - 1) {
        error("operator stack overflow");
    }
    opstack[nopstack++] = op;
}

static const struct op_s *pop_opstack(void)
{
    if(!nopstack) {
        error("operator stack empty");
    }
    return opstack[--nopstack];
}

static bool isDigit(char c)
{
    return '0' <= c && c <= '9';
}

static bool isValidNumber(char *str)
{
    //vid_logf("isValidNumber %s", str);
    if(str && (isDigit(*str) || *str == '-'))
    {
        while(1)
        {
            char c = *str++;
            if(!c)
                break;
            if(!isDigit(c))
                return false;
        }
        return true;
    }
    return false;
}

static bool isSpace(char c)
{
    //vid_logf("isSpace '%c'", c);
    return (c == ' ') || (c == '\t');
}

static bool isValidVariable(const char *c)
{
    //vid_logf("isValidVariable %s", c);
    if(!isDigit(*c) && !getop(c, NULL) && !isSpace(*c))
    {
        return true;
    }
    return false;
}

static bool isValidNumberOrVariable(const char *c)
{
    //vid_logf("isValidNumberOrVariable %s", c);
    if(isDigit(*c) || isValidVariable(c))
    {
        return true;
    }
    return false;
}

static void shunt_op(const struct op_s *op)
{
    const struct op_s *pop;
    vartype n1, n2;
    if(strcmp(op->op, "(") == 0)
    {
        push_opstack(op);
        return;
    }
    else if(strcmp(op->op, ")") == 0)
    {
        while(nopstack > 0 && strcmp(opstack[nopstack-1]->op, "(") != 0)
        {
            pop = pop_opstack();
            pop->eval();
        }

        if(!(pop = pop_opstack()) || strcmp(pop->op,"(") != 0)
        {
            error("mismatched parentheses");
        }
        return;
    }

    if(op->assoc == ASSOC_LEFT)
    {
        while(nopstack && op->prec <= opstack[nopstack - 1]->prec)
        {
            pop = pop_opstack();
            pop->eval();
        }
    }
    else
    {
        while(nopstack && op->prec<opstack[nopstack - 1]->prec)
        {
            pop = pop_opstack();
            pop->eval();
        }
    }

    push_opstack(op);
}

static vartype eval_expr(char *str)
{
    //vid_write("**************** EVAL EXPR ***************");

    /* token start */
    char *tstart = NULL;

    /* hard-code some operators that are for internal use only */
    const struct op_s startop = {"startop_", 0, ASSOC_NONE, 0, NULL, strlen("startop_")};
    const struct op_s unaryminus = {"-", 50, ASSOC_RIGHT, 1, eval_uminus, strlen("-")};

    const struct op_s *op = NULL;
    vartype n1, n2;
    const struct op_s *lastop = &startop;

    int len;
    char *expr;
    for(expr = str; *expr; expr += len)
    {
        //vid_write("****** PARSING A CHARACTER ******");
        len = 1;
        if(!tstart)
        {
            if((op = getop(expr, &len)))
            {
                if(lastop && (lastop == &startop || strcmp(lastop->op, ")") != 0))
                {
                    if(strcmp(op->op, "-") == 0)
                    {
                        op = &unaryminus;
                        len = 1;
                    }
                    else if(strcmp(op->op, "(") != 0 && !op->unary)
                    {
                        error("illegal use of binary operator (%s)", op->op);
                    }
                }
                shunt_op(op);
                lastop = op;
            }
            else if(isValidNumberOrVariable(expr))
                tstart = expr;
            else if(!isSpace(*expr))
            {
                error("syntax error");
            }
        }
        else
        {
            if(isSpace(*expr))
            {
                //push_numstack(getValue(tstart, expr));

                if(isValidVariable(tstart))
                {
                    write_instr(PUSHVAR);

                    /* isolate the variable name into a buffer */
                    char varname[VARNAME_MAX + 1] = { 0 };
                    memcpy(varname, tstart, expr - tstart);
                    write_varid(get_varid(varname));
                }
                else
                {
                    write_instr(PUSHIMM);
                    write_imm(strtol(tstart, NULL, 0));
                }
                tstart = NULL;
                lastop = NULL;
            }
            else if((op = getop(expr, &len)))
            {
                //push_numstack(getValue(tstart, expr));
                if(isValidVariable(tstart))
                {
                    write_instr(PUSHVAR);

                    /* isolate the variable name into a buffer */
                    char varname[VARNAME_MAX + 1] = { 0 };
                    memcpy(varname, tstart, expr - tstart);
                    write_varid(get_varid(varname));
                }
                else
                {
                    write_instr(PUSHIMM);
                    write_imm(strtol(tstart, NULL, 0));
                }

                tstart = NULL;
                shunt_op(op);
                lastop = op;
            }
            else if(!isValidNumberOrVariable(expr))
            {
                error("syntax error");
            }
        }
    }

    if(tstart)
    {
        //push_numstack(getValue(tstart, expr));
        if(isValidVariable(tstart))
        {
            write_instr(PUSHVAR);

            /* isolate the variable name into a buffer */
            char varname[VARNAME_MAX + 1] = { 0 };
            memcpy(varname, tstart, expr - tstart);
            write_varid(get_varid(varname));
        }
        else
        {
            write_instr(PUSHIMM);
            write_imm(strtol(tstart, NULL, 0));
        }
    }

    while(nopstack) {
        op = pop_opstack();
        op->eval();
    }
}

#define OK 0
#define DONE 1
#define NEXT 2
#define BREAK 3

static int let_handler(char **save)
{
    (void) save;
    char *varname = strtok_r(NULL, "= \t", save);

    if(varname && isValidVariable(varname))
    {
        int varid = get_varid(varname);
        char *tok = strtok_r(NULL, "=;", save);
        if(tok)
            eval_expr(tok);
        else
            error("exprected valid expression after LET");

        write_instr(POP);
        write_varid(varid);
    }
    else
    {
        error("invalid variable name for LET");
    }
    return OK;
}

static int repeat_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        eval_expr(tok);
        write_instr(PUSHIMM);
        write_imm(current_line - 1);
        write_instr(REPEAT);
        return OK;
    }
    else
        error("expected valid expression after REPEAT");
}

static int goto_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        eval_expr(tok);
        write_instr(JUMP);
        return OK;
    }
    else
        error("expected valid expression after GOTO");
}

static int call_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, "", save);
    if(tok)
    {
        eval_expr(tok);
        write_instr(SUBCALL);
        //sub_call(file_des, eval_expr(tok));
        return OK;
    }
    else
        error("expected destination for CALL");
}

static int ret_handler(char **save)
{
    (void) save;
    write_instr(SUBRET);
    //sub_return(file_des);
    return NEXT;
}

static int inc_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, " \t", save);

    if(isValidVariable(tok))
        incVar(tok);
    return OK;
}

static int dec_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, " \t", save);

    if(isValidVariable(tok))
        decVar(tok);
    return OK;
}

static int if_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, ";", save);

    if(!tok)
        error("expected conditional after IF");

    eval_expr(tok);
    write_instr(PUSHIMM);
    write_imm(current_line + 1);
    write_instr(IF);

    return OK;
}

static int delay_handler(char **save)
{
    (void) save;
    /* delay N 1000ths of a sec */
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        eval_expr(tok);
        write_instr(DELAY);
    }
    else
        error("expected valid expression after DELAY");

    return OK;
}

static int log_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, "", save);

    vid_write(tok);
    return OK;
}

static int logvar_handler(char **save)
{
    (void) save;
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        eval_expr(tok);
        write_instr(LOGVAR);
        return OK;
    }
    else
        error("expected expression after LOGVAR");
}

static int rem_handler(char **save)
{
    (void) save;
    vid_logf("REM, skipping line");
    return BREAK;
}

static int quit_handler(char **save)
{
    (void) save;
    write_instr(QUIT);
    return OK;
}

static int newline_handler(char **save)
{
    (void) save;
    vid_write("\n");
    return OK;
}

static int logchar_handler(char **save)
{
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        eval_expr(tok);
        write_instr(LOGASCII);
        return OK;
    }
    else
        error("expected expression after LOGCHAR");

}

static struct token_t {
    const char *tok;
    int (*func)(char **save);
} tokens[] = {
    { "LET",     let_handler,     },
    { "REPEAT",  repeat_handler,  },
    { "JUMP",    goto_handler,    },
    { "GOTO",    goto_handler,    },
    { "CALL",    call_handler,    },
    { "GOSUB",   call_handler,    },
    { "RET",     ret_handler,     },
    { "RETURN",  ret_handler,     },
    { "INC",     inc_handler,     },
    { "DEC",     dec_handler,     },
    { "IF",      if_handler,      },
    { "DELAY",   delay_handler,   },
    { "LOG",     log_handler,     },
    { "LOGVAR",  logvar_handler,  },
    { "LOGCHAR", logchar_handler  },
    { "NEWLINE", newline_handler, },
    { "REM",     rem_handler,     },
    { "//",      rem_handler,     },
    { "LABEL",   rem_handler,     },
    { "LBL",     rem_handler,     },
    { "QUIT",    quit_handler,    },
    { "EXIT",    quit_handler,    },
};

/* once again, this lookup table is implemented with a perfect hash map */

#define TOKMAP_SIZE ARRAYLEN(tokens)
static struct token_t tokmap[TOKMAP_SIZE];

/* auto-generated with mph-1.2 */
/*
 * d=3
 * n=28
 * m=22
 * c=1.23
 * maxlen=7
 * minklen=2
 * maxklen=7
 * minchar=47
 * maxchar=89
 * loop=0
 * numiter=200
 * seed=
 */

static int g[] = {
          19,   12,   -1,   15,   17,    1,    4,    4,    0,   13,
          10,   19,   18,   -1,   14,   19,   19,   21,   19,    0,
           7,    0,   15,   18,    4,    0,    4,    3,
};

static int T0[] = {
        0x15, 0x0c, 0x1a, 0x17, 0x16, 0x02, 0x16, 0x07, 0x17, 0x09,
        0x0f, 0x0f, 0x0f, 0x17, 0x07, 0x0c, 0x1a, 0x08, 0x17, 0x04,
        0x14, 0x07, 0x06, 0x07, 0x10, 0x0e, 0x0e, 0x18, 0x01, 0x08,
        0x1b, 0x17, 0x14, 0x19, 0x12, 0x1a, 0x0b, 0x0c, 0x12, 0x07,
        0x05, 0x05, 0x06, 0x13, 0x06, 0x19, 0x07, 0x0d, 0x01, 0x0c,
        0x1a, 0x09, 0x15, 0x1b, 0x16, 0x03, 0x15, 0x19, 0x12, 0x07,
        0x1b, 0x05, 0x08, 0x0a, 0x15, 0x05, 0x16, 0x0b, 0x0a, 0x00,
        0x0e, 0x0d, 0x08, 0x0a, 0x04, 0x1b, 0x07, 0x17, 0x0c, 0x15,
        0x07, 0x0a, 0x02, 0x0c, 0x15, 0x08, 0x0d, 0x12, 0x10, 0x00,
        0x07, 0x13, 0x15, 0x10, 0x17, 0x0a, 0x0e, 0x01, 0x14, 0x14,
        0x1a, 0x06, 0x15, 0x05, 0x0a, 0x14, 0x03, 0x12, 0x04, 0x0c,
        0x11, 0x1b, 0x0e, 0x1b, 0x17, 0x03, 0x08, 0x09, 0x15, 0x08,
        0x15, 0x0c, 0x1b, 0x0e, 0x00, 0x16, 0x08, 0x1b, 0x08, 0x10,
        0x0a, 0x00, 0x09, 0x01, 0x00, 0x09, 0x13, 0x03, 0x19, 0x03,
        0x05, 0x15, 0x10, 0x06, 0x13, 0x02, 0x19, 0x15, 0x13, 0x12,
        0x18, 0x16, 0x02, 0x0b, 0x0a, 0x1b, 0x0a, 0x15, 0x08, 0x06,
        0x09, 0x12, 0x06, 0x12, 0x14, 0x12, 0x0c, 0x17, 0x15, 0x09,
        0x1a, 0x0b, 0x05, 0x00, 0x1b, 0x0c, 0x10, 0x09, 0x02, 0x17,
        0x11, 0x0a, 0x15, 0x11, 0x18, 0x12, 0x16, 0x00, 0x0e, 0x10,
        0x18, 0x11, 0x0b, 0x07, 0x0b, 0x1a, 0x1b, 0x07, 0x07, 0x15,
        0x0a, 0x16, 0x1a, 0x0f, 0x06, 0x19, 0x0c, 0x17, 0x06, 0x0e,
        0x12, 0x08, 0x18, 0x17, 0x09, 0x0c, 0x03, 0x1b, 0x15, 0x0e,
        0x06, 0x01, 0x11, 0x0e, 0x04, 0x08, 0x05, 0x1b, 0x05, 0x03,
        0x15, 0x04, 0x17, 0x10, 0x0e, 0x07, 0x0e, 0x14, 0x13, 0x17,
        0x14, 0x1b, 0x0a, 0x07, 0x18, 0x04, 0x03, 0x00, 0x10, 0x19,
        0x1a, 0x16, 0x0a, 0x0f, 0x14, 0x1a, 0x18, 0x19, 0x0c, 0x1b,
        0x06, 0x0a, 0x03, 0x0b, 0x0e, 0x1b, 0x1b, 0x0d, 0x03, 0x0d,
        0x19, 0x0a, 0x0f, 0x14, 0x14, 0x06, 0x00, 0x01, 0x10, 0x0d,
        0x19, 0x18, 0x09, 0x1b, 0x17, 0x16, 0x14, 0x15, 0x0b, 0x10,
        0x14, 0x02, 0x1b, 0x08, 0x0d, 0x0d, 0x13, 0x0c, 0x0a, 0x16,
        0x09,
};

static int T1[] = {
        0x13, 0x0e, 0x17, 0x12, 0x13, 0x12, 0x08, 0x19, 0x05, 0x0d,
        0x0b, 0x07, 0x06, 0x05, 0x0b, 0x0f, 0x06, 0x07, 0x12, 0x06,
        0x11, 0x0d, 0x08, 0x10, 0x18, 0x1b, 0x18, 0x12, 0x03, 0x00,
        0x00, 0x16, 0x1a, 0x07, 0x18, 0x11, 0x19, 0x10, 0x1a, 0x0e,
        0x0e, 0x16, 0x16, 0x02, 0x10, 0x0d, 0x11, 0x0a, 0x02, 0x14,
        0x00, 0x0c, 0x1a, 0x1a, 0x08, 0x02, 0x01, 0x06, 0x0b, 0x0b,
        0x06, 0x02, 0x13, 0x15, 0x0f, 0x11, 0x0d, 0x01, 0x04, 0x0c,
        0x0b, 0x13, 0x02, 0x11, 0x06, 0x12, 0x0e, 0x07, 0x01, 0x10,
        0x1b, 0x01, 0x01, 0x0a, 0x0b, 0x09, 0x0c, 0x13, 0x11, 0x0a,
        0x03, 0x11, 0x02, 0x13, 0x01, 0x08, 0x0c, 0x0a, 0x06, 0x0e,
        0x00, 0x13, 0x1b, 0x03, 0x12, 0x01, 0x01, 0x0e, 0x0d, 0x09,
        0x11, 0x1b, 0x08, 0x05, 0x0b, 0x0c, 0x02, 0x08, 0x10, 0x13,
        0x02, 0x13, 0x14, 0x04, 0x16, 0x15, 0x18, 0x06, 0x10, 0x13,
        0x1a, 0x01, 0x03, 0x16, 0x17, 0x12, 0x1b, 0x10, 0x0f, 0x0f,
        0x06, 0x0f, 0x0f, 0x1a, 0x10, 0x17, 0x18, 0x09, 0x06, 0x10,
        0x01, 0x03, 0x06, 0x0d, 0x03, 0x0d, 0x0d, 0x0f, 0x16, 0x09,
        0x13, 0x14, 0x0b, 0x16, 0x1a, 0x12, 0x0d, 0x1a, 0x06, 0x00,
        0x19, 0x18, 0x17, 0x18, 0x1b, 0x10, 0x0d, 0x14, 0x17, 0x16,
        0x0a, 0x04, 0x0e, 0x03, 0x10, 0x1a, 0x01, 0x10, 0x19, 0x04,
        0x09, 0x0f, 0x08, 0x0b, 0x1a, 0x0f, 0x0f, 0x09, 0x09, 0x1b,
        0x18, 0x08, 0x16, 0x03, 0x10, 0x05, 0x14, 0x02, 0x19, 0x0f,
        0x18, 0x13, 0x03, 0x16, 0x06, 0x1b, 0x01, 0x0f, 0x19, 0x0d,
        0x00, 0x0a, 0x11, 0x0f, 0x0d, 0x0e, 0x08, 0x10, 0x1b, 0x0c,
        0x1b, 0x19, 0x08, 0x17, 0x0c, 0x1b, 0x0a, 0x12, 0x0d, 0x0f,
        0x0a, 0x14, 0x04, 0x0f, 0x0b, 0x05, 0x0f, 0x18, 0x04, 0x18,
        0x09, 0x05, 0x06, 0x1b, 0x04, 0x13, 0x19, 0x0c, 0x1b, 0x0c,
        0x18, 0x19, 0x08, 0x0e, 0x11, 0x0b, 0x03, 0x16, 0x1b, 0x15,
        0x11, 0x14, 0x09, 0x09, 0x17, 0x0e, 0x12, 0x1a, 0x14, 0x12,
        0x19, 0x08, 0x16, 0x07, 0x12, 0x0a, 0x17, 0x14, 0x13, 0x06,
        0x10, 0x0f, 0x03, 0x18, 0x0d, 0x04, 0x13, 0x10, 0x1b, 0x03,
        0x09,
};

static int T2[] = {
        0x16, 0x04, 0x18, 0x10, 0x13, 0x0f, 0x08, 0x19, 0x19, 0x17,
        0x13, 0x0b, 0x0b, 0x08, 0x0a, 0x08, 0x01, 0x05, 0x15, 0x1a,
        0x11, 0x02, 0x16, 0x0f, 0x0d, 0x09, 0x16, 0x13, 0x17, 0x0d,
        0x05, 0x11, 0x11, 0x0d, 0x05, 0x14, 0x01, 0x19, 0x12, 0x0a,
        0x15, 0x15, 0x15, 0x17, 0x13, 0x15, 0x0e, 0x18, 0x11, 0x0f,
        0x03, 0x04, 0x03, 0x09, 0x1a, 0x13, 0x04, 0x08, 0x09, 0x07,
        0x15, 0x16, 0x05, 0x11, 0x02, 0x00, 0x15, 0x10, 0x1b, 0x0d,
        0x14, 0x18, 0x10, 0x0f, 0x13, 0x13, 0x14, 0x11, 0x10, 0x0a,
        0x05, 0x03, 0x1a, 0x14, 0x0d, 0x19, 0x05, 0x1a, 0x1a, 0x00,
        0x06, 0x0c, 0x07, 0x11, 0x01, 0x18, 0x12, 0x06, 0x02, 0x06,
        0x1a, 0x17, 0x03, 0x0d, 0x09, 0x02, 0x09, 0x19, 0x03, 0x01,
        0x16, 0x0d, 0x17, 0x10, 0x01, 0x06, 0x18, 0x06, 0x10, 0x16,
        0x07, 0x06, 0x12, 0x1a, 0x17, 0x04, 0x16, 0x19, 0x0a, 0x1b,
        0x15, 0x16, 0x13, 0x11, 0x09, 0x1b, 0x02, 0x05, 0x12, 0x0c,
        0x10, 0x11, 0x08, 0x0f, 0x0c, 0x05, 0x0d, 0x0f, 0x19, 0x01,
        0x1b, 0x17, 0x03, 0x08, 0x00, 0x1a, 0x0d, 0x0c, 0x07, 0x19,
        0x17, 0x01, 0x03, 0x0e, 0x02, 0x18, 0x19, 0x10, 0x02, 0x00,
        0x0c, 0x02, 0x0a, 0x0b, 0x1a, 0x11, 0x19, 0x01, 0x16, 0x02,
        0x09, 0x01, 0x10, 0x1a, 0x0c, 0x0c, 0x12, 0x10, 0x18, 0x1b,
        0x14, 0x17, 0x17, 0x12, 0x1b, 0x08, 0x0d, 0x0c, 0x07, 0x1a,
        0x04, 0x03, 0x0b, 0x0e, 0x1b, 0x16, 0x10, 0x08, 0x17, 0x16,
        0x16, 0x10, 0x17, 0x0a, 0x1a, 0x0c, 0x17, 0x0f, 0x0a, 0x19,
        0x00, 0x0c, 0x08, 0x11, 0x17, 0x09, 0x17, 0x0f, 0x00, 0x05,
        0x17, 0x05, 0x05, 0x17, 0x17, 0x15, 0x0f, 0x00, 0x00, 0x19,
        0x0c, 0x19, 0x0f, 0x11, 0x08, 0x18, 0x01, 0x10, 0x17, 0x17,
        0x0d, 0x18, 0x13, 0x15, 0x0d, 0x0e, 0x0e, 0x09, 0x05, 0x17,
        0x14, 0x17, 0x16, 0x16, 0x1b, 0x10, 0x10, 0x0c, 0x06, 0x02,
        0x17, 0x12, 0x05, 0x08, 0x12, 0x00, 0x0b, 0x17, 0x07, 0x12,
        0x08, 0x01, 0x18, 0x0a, 0x19, 0x16, 0x01, 0x0b, 0x12, 0x12,
        0x06, 0x0a, 0x0e, 0x0c, 0x10, 0x0d, 0x00, 0x04, 0x19, 0x12,
        0x06,
};

#define uchar unsigned char

static int
tok_hash(const uchar *key)
{
        int i;
        unsigned f0, f1, f2;
        const uchar *kp = key;

        for (i=-47, f0=f1=f2=0; *kp; ++kp) {
                if (*kp < 47 || *kp > 89)
                        return -1;
                if (kp-key > 6)
                        return -1;
                f0 += T0[i + *kp];
                f1 += T1[i + *kp];
                f2 += T2[i + *kp];
                i += 43;
        }

        if (kp-key < 2)
                return -1;

        f0 %= 28;
        f1 %= 28;
        f2 %= 28;

        return (g[f0] + g[f1] + g[f2]) % 22;
}

static void tokmap_insert(struct token_t *tok)
{
    uint32_t hash = tok_hash(tok->tok) % TOKMAP_SIZE;
    if(hash < 0 || tokmap[hash].tok)
        error("FIXME: hash map collision");
    memcpy(tokmap+hash, tok, sizeof(*tok));
}

static void init_tokmap(void)
{
    memset(tokmap, 0, sizeof(tokmap));
    for(unsigned int i = 0; i < ARRAYLEN(tokens); ++i)
    {
        tokmap_insert(tokens+i);
    }
}

static void init_globals(void)
{
    line_offset = NULL;
    file_des = -1;
    stack_frame = 0;
    current_line = 0;
    bytes_written = 0;
}

void ducky_compile(int fd, bool verbose, int out)
{
    init_globals();

    if(verbose)
    {
        vid_logf("COMPILER INIT");
    }

    file_des = fd;
    out_fd = out;

    atexit(exit_handler);

    if(file_des < 0)
        error("invalid file");

    init_optable();
    init_tokmap();

    line_offset = index_lines(file_des, &num_lines);
    write_imm(DUCKY_MAGIC);
    write_imm(num_lines);
    off_t linetab_off = bytes_written;
    for(unsigned i = 1; i <= num_lines; ++i)
    {
        write_imm(0);
    }
    if(verbose)
    {
        vid_logf("Indexing complete (%u lines).", num_lines);
        vid_logf("Compiling...");
    }

    /* initialize some other constants */

    setVariable(".", 0);
    setConst(".", true);

    setVariable("true", 1);
    setConst("true", true);

    setVariable("false", 0);
    setConst("false", true);

    /* initialize labels (using output from index_lines) */
    index_labels(file_des);

    int repeats_left = 0;
    off_t code_start = bytes_written;

    while(1)
    {
        char instr_buf[MAX_LINE_LEN];
        memset(instr_buf, 0, sizeof(instr_buf));
        if(read_line(file_des, instr_buf, sizeof(instr_buf)) <= 0)
        {
            if(verbose)
                vid_logf("end of file");
            goto done;
        }
        char *tok = NULL, *save = NULL;

        ++current_line;
        write_instr(LINEMARK);

        char *buf = instr_buf;

        line_offset[current_line] = bytes_written;

        /* compile all the commands on this line/instruction */
        do {
            tok = strtok_r(buf, " -\t", &save);
            buf = NULL;

            if(!tok)
                break;

            int hash = tok_hash(tok) % TOKMAP_SIZE;
            struct token_t *t = tokmap+hash;
            if(hash >= 0 && strcmp(t->tok, tok) == 0)
                switch(tokmap[hash].func(&save))
                {
                case OK:
                    break;
                case BREAK:
                    goto break_out;
                case DONE:
                    goto done;
                case NEXT:
                    goto next_line;
                default:
                    error("FIXME: invalid return value");
                }
            else if(tok[0] != '#')
            {
                error("unknown token `%s` on line %d %d", tok, current_line);
                goto done;
            }
        } while(tok);
    break_out:
        ;
    next_line:
        ;
    }

done:

    /* go back and fill in the offset table */
    lseek(out_fd, linetab_off, SEEK_SET);
    for(unsigned i = 1; i <= num_lines; ++i)
    {
        write_imm(line_offset[i]);
    }

    return;
}
