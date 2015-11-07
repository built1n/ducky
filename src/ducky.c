#include <bsd/string.h>

#include <fcntl.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/*******************************************************************************
 * The scripting language implemented here is an extension of DuckyScript.
 * DuckyScript as it is now is limited to simple tasks, as it lacks good flow
 * control or variable support.
 *
 * These following extensions to DuckyScript are (to be) implemented.
 *
 * Variables: variable names consist of all characters that are not operators
 *            or digits. The limit on variable names is 24 characters.
 *
 * NOTE: to have a command on a line following a command using an expression, a
 *       semicolon (;) is needed to separate them.
 *
 * "JUMP <EXPR>;" - jumps to line <EXPR> in the file
 *
 * "IF <CONDITION>;" - if <CONDITION> equals zero, skip the rest of the line
 *
 * "LET X=<EXPR>;" - loads the value of <EXPR> into variable X. Greedy.
 *
 * "LOG ..." - outputs any remaining text to the device's screen. Greedy.
 * "LOGVAR <EXPR>;" - outputs variable <EXPR> in decimal to the device's screen
 ******************************************************************************/

/*** Defines ***/

#define DEFAULT_DELAY 0
#define STRING_DELAY 0
#define TOKEN_IS(str) (strcmp(tok, str) == 0)
#define MAX_LINE_LEN 512

#define MAXOPSTACK 64
#define MAXNUMSTACK 64
#define CALL_STACK_SZ 64
#define VARMAP_SIZE 256

#define VARFORMAT "%d"
#define VARNAME_MAX 24

#define ARRAYLEN(x) (sizeof(x)/sizeof(x[0]))

#define MIN(x,y) ((x<y)?(x):(y))

typedef int vartype;

/*** Globals ***/

off_t *line_offset = NULL;
int default_delay = DEFAULT_DELAY, string_delay = STRING_DELAY;

unsigned lines_executed = 0, current_line = 0, num_lines;

unsigned call_stack[CALL_STACK_SZ];
unsigned stack_frame = 0;

int log_fd = -1, file_des = -1;

int keys_sent = 0;

struct varnode_t {
    char name[VARNAME_MAX + 1];
    vartype val;
    bool constant; /* used by setVariable */
    struct varnode_t *next;
};

void error(const char *fmt, ...) __attribute__((noreturn,format(print,1,2)));
void vid_write(const char *str);
void vid_writef(const char *fmt, ...) __attribute__((format(printf,1,2)));
void debug(const char *fmt, ...) __attribute__((format(printf,1,2)));
bool isValidVariable(const char *c);

/* variables are stored in a chained hash map */
/* collisions are manageable, but should be minimized */

struct varnode_t *var_map[VARMAP_SIZE] = { 0 };

/* simple DJB hash */
uint32_t var_hash(const char *str)
{
    uint32_t hash = 5381;
    char c;
    while((c = *str++))
    {
        hash = ((hash << 5) + hash) ^ c;
    }

    return hash;
}

struct varnode_t *lookup_var(const char *name)
{
    static bool firstrun = true;
    if(firstrun)
    {
        firstrun = false;
        memset(var_map, 0, sizeof(var_map));
    }
    vid_writef("lookup up variable %s", name);
    uint32_t hash = var_hash(name) % VARMAP_SIZE;

    struct varnode_t *iter = var_map[hash];
    struct varnode_t *last = NULL;

    while(iter)
    {
        if(strcmp(iter->name, name) == 0)
            return iter;
        last = iter;
        iter = iter->next;
    }

    /* not found in this bucket, so add it to the linked list */
    struct varnode_t *new = malloc(sizeof(struct varnode_t));

    memset(new, 0, sizeof(struct varnode_t));
    strlcpy(new->name, name, sizeof(new->name));
    new->val = 0;
    new->constant = false;
    new->next = NULL;

    if(!last)
        var_map[hash] = new;
    else
        last->next = new;

    return new;
}

vartype getVariable(const char *name)
{
    return lookup_var(name)->val;
}

void setVariable(const char *name, vartype val)
{
    struct varnode_t *node = lookup_var(name);
    if(!node->constant)
        node->val = val;
    else
        error("attempted to modify a constant variable");
}

void setConst(const char *name, bool c)
{
    lookup_var(name)->constant = c;
}

void incVar(const char *name)
{
    struct varnode_t *node = lookup_var(name);
    if(!node->constant)
        ++lookup_var(name)->val;
    else
        error("attempted to modify a constant variable");
}

void decVar(const char *name)
{
    struct varnode_t *node = lookup_var(name);
    if(!node->constant)
        --lookup_var(name)->val;
    else
        error("attempted to modify a constant variable");
}

/*** Utility functions ***/

void exit_handler(void)
{
    if(file_des >= 0)
        close(file_des);
    if(log_fd >= 0)
        close(log_fd);
}

void vid_write(const char *str)
{
    puts(str);
}

void __attribute__((format(printf,1,2))) vid_writef(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    vid_write(fmtbuf);
    va_end(ap);
}

void __attribute__((noreturn,format(printf,1,2))) error(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    if(current_line)
        vid_writef("Line %d: ", current_line);
    vid_writef("ERROR: %s",  fmtbuf);
    va_end(ap);

    exit(EXIT_FAILURE);
}

void __attribute__((format(printf,1,2))) warning(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    vid_writef("Line %d: WARNING: %s", current_line, fmtbuf);
    va_end(ap);
}

int read_line(int fd, char *buf, size_t sz)
{
    unsigned i = 0;
    int bytes_read = 0;
    while(i < sz)
    {
        char c;
        if(read(fd, &c, 1) != 1)
            break;

        ++bytes_read;

        if(c == '\r')
            continue;
        if(c == '\n')
            break;

        buf[i++] = c;
    }
    buf[MIN(i, sz - 1)] = '\0';

    return bytes_read;
}

/* index_lines() precalculates the offset of each line for faster jumping */
/* also it does a quick pass to index all the labels */

off_t *index_lines(int fd, unsigned *numlines)
{
    vid_write("Indexing file...");

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

        if(!read_line(fd, buf, sizeof(buf)))
            break;

        char *save = NULL;
        char *tok = strtok_r(buf, " \t", &save);
        if(strcmp(tok, "LABEL") == 0 || strcmp("LBL", tok) == 0)
        {
            tok = strtok_r(NULL, " \t", &save);
            if(tok && isValidVariable(tok))
            {
                setVariable(tok, idx);
                lookup_var(tok)->constant = true;
            }
        }

        ++idx;
    }

    lseek(fd, 0, SEEK_SET);

    *numlines = idx - 1;

    return data;
}

void jump_line(int fd, unsigned where)
{
    if(1 <= where && where <= num_lines)
    {
        lseek(fd, line_offset[where], SEEK_SET);
    }
    else
        error("JUMP target out of range (%u)", where);
    current_line = where - 1;
}

void sub_call(int fd, unsigned where)
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

void sub_return(int fd)
{
    if(stack_frame > 0)
    {
        --stack_frame;
        jump_line(fd, call_stack[stack_frame]);
    }
}

/** Expression Parsing **/

/* based on http://en.literateprograms.org/Shunting_yard_algorithm_%28C%29 */

vartype eval_uminus(vartype a1, vartype a2)
{
    (void) a2;
    return -a1;
}
vartype eval_exp(vartype a1, vartype a2)
{
    return a2<0 ? 0 : (a2==0?1:a1*eval_exp(a1, a2-1));
}
vartype eval_mul(vartype a1, vartype a2)
{
    return a1*a2;
}
vartype eval_div(vartype a1, vartype a2)
{
    if(!a2) {
        error("division by zero");
    }
    return a1/a2;
}
vartype eval_mod(vartype a1, vartype a2)
{
    if(!a2) {
        error("division by zero");
    }
    return a1%a2;
}
vartype eval_add(vartype a1, vartype a2)
{
    return a1+a2;
}
vartype eval_sub(vartype a1, vartype a2)
{
    return a1-a2;
}
vartype eval_eq(vartype a1, vartype a2)
{
    return a1 == a2;
}
vartype eval_neq(vartype a1, vartype a2)
{
    return a1 != a2;
}
vartype eval_leq(vartype a1, vartype a2)
{
    return a1 <= a2;
}
vartype eval_geq(vartype a1, vartype a2)
{
    return a1 >= a2;
}
vartype eval_lt(vartype a1, vartype a2)
{
    return a1 < a2;
}
vartype eval_gt(vartype a1, vartype a2)
{
    return a1 > a2;
}
vartype eval_log_neg(vartype a1, vartype a2)
{
    (void) a2;
    return !a1;
}
vartype eval_log_and(vartype a1, vartype a2)
{
    return a1 && a2;
}
vartype eval_log_or(vartype a1, vartype a2)
{
    return a1 || a2;
}
vartype eval_bit_and(vartype a1, vartype a2)
{
    return a1 & a2;
}
vartype eval_bit_xor(vartype a1, vartype a2)
{
    return a1 ^ a2;
}
vartype eval_bit_or(vartype a1, vartype a2)
{
    return a1 | a2;
}
vartype eval_bit_comp(vartype a1, vartype a2)
{
    (void) a2;
    return ~a1;
}
vartype eval_lsh(vartype a1, vartype a2)
{
    return a1 << a2;
}
vartype eval_rsh(vartype a1, vartype a2)
{
    return a1 >> a2;
}
vartype eval_sqrt(vartype a1, vartype a2)
{
    (void) a2;
    return sqrt(a1);
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

struct op_s {
    const char *op;
    int prec;
    int assoc;
    int unary;
    vartype (*eval)(vartype a1, vartype a2);
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

void op_hash_round(char c, uint32_t *hash)
{
    *hash *= 70;
    *hash ^= c;
}

uint32_t op_hash(const char *c)
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
struct op_s op_map[OPMAP_SIZE];
size_t longest_op = 0;

void opmap_insert(struct op_s *op)
{
    if(op->len > longest_op)
        longest_op = op->len;

    uint32_t hash = op_hash(op->op) % OPMAP_SIZE;

    if(op_map[hash].op)
        error("hash map collision %lu: %s VS %s", hash, op->op, op_map[hash].op);
    memcpy(op_map+hash, op, sizeof(*op));
}

void init_optable(void)
{
    memset(op_map, 0, sizeof(op_map));
    for(unsigned int i = 0; i < ARRAYLEN(ops); ++i)
    {
        ops[i].len = strlen(ops[i].op);
        opmap_insert(ops+i);
    }
}

const struct op_s *getop(const char *ch, int *len)
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

const struct op_s *opstack[MAXOPSTACK];
int nopstack;

vartype numstack[MAXNUMSTACK];
int nnumstack;

void push_opstack(const struct op_s *op)
{
    if(nopstack>MAXOPSTACK - 1) {
        error("operator stack overflow");
    }
    opstack[nopstack++] = op;
}

const struct op_s *pop_opstack(void)
{
    if(!nopstack) {
        error("operator stack empty");
    }
    return opstack[--nopstack];
}

void push_numstack(vartype num)
{
    if(nnumstack>MAXNUMSTACK - 1) {
        error("number stack overflow");
    }
    numstack[nnumstack++] = num;
}

vartype pop_numstack(void)
{
    if(!nnumstack) {
        error("number stack empty");
    }
    return numstack[--nnumstack];
}

bool isDigit(char c)
{
    return '0' <= c && c <= '9';
}

bool isValidNumber(char *str)
{
    //vid_writef("isValidNumber %s", str);
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

bool isSpace(char c)
{
    //vid_writef("isSpace '%c'", c);
    return (c == ' ') || (c == '\t');
}

bool isValidVariable(const char *c)
{
    //vid_writef("isValidVariable %s", c);
    if(!isDigit(*c) && !getop(c, NULL) && !isSpace(*c))
    {
        return true;
    }
    return false;
}

vartype getValue(char *str, char *cur)
{
    //vid_writef("getValue %s", str);
    if(str && isValidVariable(str))
    {
        /* isolate the variable name into a buffer */
        char varname[VARNAME_MAX + 1] = { 0 };
        memcpy(varname, str, cur - str);
        return getVariable(varname);
    }
    return strtol(str, NULL, 0);
}

bool isValidNumberOrVariable(const char *c)
{
    //vid_writef("isValidNumberOrVariable %s", c);
    if(isDigit(*c) || isValidVariable(c))
    {
        return true;
    }
    return false;
}

void shunt_op(const struct op_s *op)
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
            n1  = pop_numstack();

            if(pop->unary)
                push_numstack(pop->eval(n1, 0));
            else
            {
                n2 = pop_numstack();
                push_numstack(pop->eval(n2, n1));
            }
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
            n1  = pop_numstack();
            if(pop->unary)
                push_numstack(pop->eval(n1, 0));
            else
            {
                n2 = pop_numstack();
                push_numstack(pop->eval(n2, n1));
            }
        }
    }
    else
    {
        while(nopstack && op->prec<opstack[nopstack - 1]->prec)
        {
            pop = pop_opstack();
            n1  = pop_numstack();
            if(pop->unary)
                push_numstack(pop->eval(n1, 0));
            else
            {
                n2 = pop_numstack();
                push_numstack(pop->eval(n2, n1));
            }
        }
    }

    push_opstack(op);
}

vartype eval_expr(char *str)
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

    nopstack = 0;
    nnumstack = 0;

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
                push_numstack(getValue(tstart, expr));
                tstart = NULL;
                lastop = NULL;
            }
            else if((op = getop(expr, &len)))
            {
                push_numstack(getValue(tstart, expr));
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
        push_numstack(getValue(tstart, expr));

    while(nopstack) {
        op = pop_opstack();
        n1 = pop_numstack();
        if(!op->unary)
        {
            n2 = pop_numstack();
            push_numstack(op->eval(n2, n1));
        }
        else
            push_numstack(op->eval(n1, 0));
    }

    if(nnumstack != 1) {
        error("invalid expression");
    }

    vid_writef("eval_expr %s = %d", str, numstack[0]);
    return numstack[0];
}

#define OK 0
#define DONE 1
#define NEXT 2
#define BREAK 3

int let_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *varname = strtok_r(NULL, "= \t", save);

    if(varname && isValidVariable(varname))
    {
        struct varnode_t *node = lookup_var(varname);
        if(node->constant)
            error("attempted to modify a constant variable");

        char *tok = strtok_r(NULL, "=;", save);
        if(tok)
            node->val = eval_expr(tok);
        else
            error("exprected valid expression after LET");
    }
    else
    {
        error("invalid variable name for LET");
    }
    return OK;
}

int repeat_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        if(current_line == 1)
            error("REPEAT without a previous instruction");
        *repeats_left = eval_expr(tok) - 1;
        jump_line(file_des, current_line - 1);
        return NEXT;
    }
    else
        error("expected valid expression after REPEAT");
}

int goto_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        jump_line(file_des, eval_expr(tok));
        return NEXT;
    }
    else
        error("expected valid expression after GOTO");
}

int call_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, "", save);
    if(tok)
    {
        sub_call(file_des, eval_expr(tok));
        return NEXT;
    }
    else
        error("expected destination for CALL");
}

int ret_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    sub_return(file_des);
    return NEXT;
}

int inc_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, " \t", save);
    if(isValidVariable(tok))
        incVar(tok);
    return OK;
}

int dec_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, " \t", save);
    if(isValidVariable(tok))
        decVar(tok);
    return OK;
}

int if_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, ";", save);

    if(!tok)
        error("expected conditional after IF");

    /* break out of the do-while if the condition is false */
    if(!eval_expr(tok))
    {
        return BREAK;
    }
    return OK;
}

int delay_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    /* delay N 1000ths of a sec */
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        int ms = eval_expr(tok);
        struct timespec t;
        t.tv_sec = ms / 1000;
        t.tv_nsec = (ms % 1000) * 1000000;
        nanosleep(&t, NULL);
    }
    else
        error("expected valid expression after DELAY");

    return OK;
}

int log_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, "", save);

    vid_write(tok);
    return OK;
}

int logvar_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    char *tok = strtok_r(NULL, ";", save);
    if(tok)
    {
        vid_writef("%d", eval_expr(tok));
        return OK;
    }
    else
        error("expected expression after LOGVAR");

}

int rem_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    return BREAK;
}

int quit_handler(char **save, int *repeats_left)
{
    (void) save; (void) repeats_left;
    return DONE;
}

struct token_t {
    const char *tok;
    int (*func)(char **save, int *repeats_left);
} tokens[] = {
    { "LET",           let_handler     },
    { "REPEAT",        repeat_handler  },
    { "JUMP",          goto_handler    },
    { "GOTO",          goto_handler    },
    { "CALL",          call_handler    },
    { "GOSUB",         call_handler    },
    { "RET",           ret_handler     },
    { "RETURN",        ret_handler     },
    { "INC",           inc_handler     },
    { "DEC",           dec_handler     },
    { "IF",            if_handler      },
    { "DELAY",         delay_handler   },
    { "LOG",           log_handler     },
    { "LOGVAR",        logvar_handler  },
    { "REM",           rem_handler     },
    { "//",            rem_handler     },
    { "LABEL",         rem_handler     },
    { "LBL",           rem_handler     },
    { "QUIT",          quit_handler    },
    { "EXIT",          quit_handler    },
};

/* once again, this lookup table is implemented with a perfect hash map */

#define TOKMAP_SIZE 20
struct token_t tokmap[TOKMAP_SIZE];

/* auto-generated with mph-1.2 */
/*
 * d=3
 * n=25
 * m=20
 * c=1.23
 * maxlen=6
 * minklen=2
 * maxklen=6
 * minchar=47
 * maxchar=89
 * loop=0
 * numiter=20
 * seed=
 */

static int g[] = {
          12,   11,    0,   16,    0,   -1,    9,   17,   14,    2,
          19,    9,   11,   11,   11,   10,   18,    5,    1,    0,
          14,   18,    7,    0,   19,
};

static int T0[] = {
        0x14, 0x15, 0x0b, 0x0e, 0x11, 0x16, 0x18, 0x11, 0x04, 0x0f,
        0x10, 0x14, 0x0a, 0x01, 0x11, 0x05, 0x06, 0x04, 0x12, 0x17,
        0x14, 0x01, 0x11, 0x0a, 0x04, 0x10, 0x18, 0x15, 0x0c, 0x10,
        0x0a, 0x07, 0x0e, 0x17, 0x16, 0x06, 0x15, 0x15, 0x00, 0x02,
        0x0e, 0x11, 0x16, 0x10, 0x15, 0x08, 0x10, 0x08, 0x0b, 0x0b,
        0x14, 0x03, 0x0a, 0x04, 0x02, 0x0e, 0x17, 0x0b, 0x03, 0x0f,
        0x07, 0x15, 0x05, 0x16, 0x14, 0x0c, 0x00, 0x0f, 0x0a, 0x0e,
        0x09, 0x05, 0x12, 0x0b, 0x15, 0x11, 0x13, 0x0c, 0x02, 0x05,
        0x00, 0x18, 0x0a, 0x0a, 0x03, 0x0f, 0x09, 0x06, 0x09, 0x0e,
        0x16, 0x08, 0x06, 0x18, 0x11, 0x12, 0x03, 0x0f, 0x0a, 0x03,
        0x15, 0x01, 0x00, 0x0d, 0x0e, 0x12, 0x00, 0x03, 0x06, 0x18,
        0x13, 0x10, 0x14, 0x01, 0x01, 0x17, 0x08, 0x0a, 0x06, 0x12,
        0x01, 0x05, 0x03, 0x0a, 0x04, 0x16, 0x03, 0x08, 0x0d, 0x14,
        0x03, 0x0a, 0x07, 0x17, 0x0a, 0x13, 0x06, 0x09, 0x18, 0x0e,
        0x00, 0x14, 0x01, 0x0a, 0x07, 0x18, 0x0a, 0x10, 0x07, 0x18,
        0x0f, 0x11, 0x06, 0x0b, 0x14, 0x18, 0x12, 0x06, 0x06, 0x02,
        0x01, 0x0a, 0x0e, 0x0b, 0x0a, 0x18, 0x05, 0x10, 0x09, 0x06,
        0x05, 0x09, 0x14, 0x09, 0x08, 0x0e, 0x18, 0x18, 0x16, 0x0f,
        0x11, 0x0f, 0x18, 0x03, 0x12, 0x03, 0x06, 0x12, 0x07, 0x11,
        0x02, 0x12, 0x12, 0x0a, 0x0d, 0x18, 0x16, 0x16, 0x05, 0x15,
        0x08, 0x0d, 0x14, 0x05, 0x16, 0x03, 0x14, 0x15, 0x05, 0x13,
        0x0e, 0x16, 0x09, 0x0d, 0x02, 0x02, 0x0f, 0x00, 0x14, 0x09,
        0x13, 0x0f, 0x10, 0x02, 0x0e, 0x13, 0x00, 0x14, 0x0f, 0x0e,
        0x0b, 0x05, 0x0d, 0x0a, 0x11, 0x0b, 0x07, 0x08, 0x0d, 0x03,
        0x0e, 0x08, 0x01, 0x0e, 0x02, 0x04, 0x12, 0x13, 0x07, 0x0f,
        0x03, 0x01, 0x08, 0x16, 0x03, 0x18, 0x12, 0x05,
};

static int T1[] = {
        0x0a, 0x17, 0x12, 0x16, 0x09, 0x03, 0x16, 0x00, 0x08, 0x02,
        0x0e, 0x04, 0x07, 0x0c, 0x11, 0x11, 0x03, 0x04, 0x0f, 0x03,
        0x0d, 0x0a, 0x06, 0x16, 0x04, 0x08, 0x11, 0x0d, 0x0f, 0x05,
        0x0e, 0x00, 0x06, 0x09, 0x16, 0x0f, 0x0f, 0x15, 0x12, 0x17,
        0x00, 0x07, 0x04, 0x05, 0x00, 0x0f, 0x14, 0x05, 0x02, 0x05,
        0x04, 0x07, 0x03, 0x0b, 0x00, 0x05, 0x16, 0x0e, 0x00, 0x11,
        0x0c, 0x06, 0x0b, 0x14, 0x06, 0x17, 0x0a, 0x01, 0x0e, 0x18,
        0x0d, 0x16, 0x0e, 0x0f, 0x04, 0x10, 0x05, 0x00, 0x15, 0x08,
        0x05, 0x01, 0x11, 0x0a, 0x0c, 0x13, 0x0d, 0x06, 0x09, 0x09,
        0x03, 0x03, 0x12, 0x0c, 0x14, 0x17, 0x08, 0x04, 0x15, 0x05,
        0x03, 0x15, 0x17, 0x17, 0x0c, 0x0e, 0x0d, 0x03, 0x13, 0x16,
        0x08, 0x07, 0x01, 0x0c, 0x05, 0x07, 0x18, 0x14, 0x10, 0x0a,
        0x06, 0x13, 0x10, 0x18, 0x08, 0x0b, 0x17, 0x13, 0x10, 0x0e,
        0x14, 0x00, 0x05, 0x07, 0x18, 0x17, 0x0a, 0x14, 0x0e, 0x05,
        0x14, 0x09, 0x05, 0x0e, 0x0a, 0x03, 0x0e, 0x0a, 0x11, 0x04,
        0x16, 0x18, 0x02, 0x16, 0x0b, 0x01, 0x0b, 0x08, 0x17, 0x00,
        0x18, 0x12, 0x02, 0x07, 0x01, 0x03, 0x07, 0x0d, 0x18, 0x16,
        0x14, 0x15, 0x12, 0x11, 0x0e, 0x00, 0x01, 0x0f, 0x08, 0x00,
        0x0a, 0x03, 0x0c, 0x13, 0x15, 0x15, 0x0f, 0x03, 0x17, 0x0d,
        0x02, 0x0d, 0x0e, 0x0b, 0x03, 0x0c, 0x12, 0x14, 0x0f, 0x00,
        0x08, 0x11, 0x0e, 0x01, 0x09, 0x05, 0x02, 0x0a, 0x14, 0x0c,
        0x0d, 0x06, 0x0f, 0x00, 0x00, 0x15, 0x07, 0x14, 0x00, 0x07,
        0x12, 0x00, 0x09, 0x17, 0x15, 0x07, 0x0f, 0x11, 0x03, 0x0f,
        0x0e, 0x17, 0x18, 0x06, 0x06, 0x0a, 0x15, 0x11, 0x05, 0x08,
        0x17, 0x04, 0x16, 0x11, 0x15, 0x02, 0x0f, 0x03, 0x18, 0x0f,
        0x0c, 0x11, 0x11, 0x15, 0x11, 0x0f, 0x05, 0x07,
};

static int T2[] = {
        0x06, 0x02, 0x17, 0x0e, 0x15, 0x01, 0x0b, 0x05, 0x0d, 0x02,
        0x18, 0x0a, 0x03, 0x12, 0x00, 0x10, 0x17, 0x04, 0x18, 0x12,
        0x13, 0x01, 0x18, 0x06, 0x11, 0x17, 0x09, 0x01, 0x0e, 0x06,
        0x10, 0x15, 0x09, 0x10, 0x0c, 0x05, 0x13, 0x01, 0x0c, 0x09,
        0x05, 0x0e, 0x14, 0x16, 0x17, 0x03, 0x08, 0x18, 0x00, 0x15,
        0x0b, 0x14, 0x18, 0x08, 0x0e, 0x18, 0x02, 0x13, 0x07, 0x0f,
        0x0d, 0x15, 0x07, 0x13, 0x00, 0x05, 0x03, 0x12, 0x0c, 0x06,
        0x0a, 0x03, 0x0c, 0x05, 0x03, 0x0a, 0x08, 0x0d, 0x0b, 0x0a,
        0x0a, 0x00, 0x05, 0x09, 0x08, 0x15, 0x12, 0x06, 0x0e, 0x04,
        0x17, 0x0d, 0x0c, 0x02, 0x15, 0x0a, 0x12, 0x17, 0x11, 0x13,
        0x0e, 0x13, 0x09, 0x15, 0x14, 0x10, 0x0d, 0x16, 0x09, 0x13,
        0x09, 0x0b, 0x04, 0x18, 0x0b, 0x13, 0x0f, 0x07, 0x00, 0x06,
        0x0b, 0x00, 0x15, 0x00, 0x03, 0x12, 0x0a, 0x15, 0x10, 0x0a,
        0x00, 0x01, 0x0c, 0x06, 0x0c, 0x05, 0x14, 0x09, 0x08, 0x18,
        0x18, 0x15, 0x18, 0x15, 0x0b, 0x0a, 0x16, 0x04, 0x08, 0x12,
        0x0e, 0x04, 0x14, 0x11, 0x16, 0x0c, 0x16, 0x11, 0x05, 0x03,
        0x03, 0x06, 0x06, 0x0f, 0x0e, 0x12, 0x16, 0x09, 0x03, 0x07,
        0x08, 0x04, 0x05, 0x17, 0x03, 0x0a, 0x02, 0x0b, 0x02, 0x11,
        0x0a, 0x05, 0x01, 0x0b, 0x07, 0x09, 0x0c, 0x18, 0x00, 0x0f,
        0x0d, 0x16, 0x09, 0x06, 0x02, 0x02, 0x02, 0x06, 0x06, 0x16,
        0x11, 0x02, 0x03, 0x16, 0x02, 0x06, 0x0a, 0x04, 0x13, 0x0c,
        0x16, 0x06, 0x13, 0x00, 0x14, 0x00, 0x0a, 0x0f, 0x04, 0x12,
        0x03, 0x0f, 0x0b, 0x0f, 0x11, 0x15, 0x01, 0x0c, 0x13, 0x0e,
        0x17, 0x07, 0x0c, 0x10, 0x02, 0x00, 0x03, 0x0f, 0x06, 0x00,
        0x0a, 0x18, 0x01, 0x12, 0x14, 0x12, 0x14, 0x05, 0x0a, 0x02,
        0x18, 0x10, 0x11, 0x0c, 0x08, 0x09, 0x08, 0x09,
};

#define uchar unsigned char

int
tok_hash(const uchar *key)
{
        int i;
        unsigned f0, f1, f2;
        const uchar *kp = key;

        for (i=-47, f0=f1=f2=0; *kp; ++kp) {
                if (*kp < 47 || *kp > 89)
                        return -1;
                if (kp-key > 5)
                        return -1;
                f0 += T0[i + *kp];
                f1 += T1[i + *kp];
                f2 += T2[i + *kp];
                i += 43;
        }

        if (kp-key < 2)
                return -1;

        f0 %= 25;
        f1 %= 25;
        f2 %= 25;

        return (g[f0] + g[f1] + g[f2]) % 20;
}

void tokmap_insert(struct token_t *tok)
{
    uint32_t hash = tok_hash(tok->tok) % TOKMAP_SIZE;
    if(tokmap[hash].tok)
        error("hash map collision %lu: %s VS %s", hash, tok->tok, tokmap[hash].tok);
    memcpy(tokmap+hash, tok, sizeof(*tok));
}

void init_tokmap(void)
{
    memset(tokmap, 0, sizeof(tokmap));
    for(unsigned int i = 0; i < ARRAYLEN(tokens); ++i)
    {
        tokmap_insert(tokens+i);
    }
}

void ducky_main(int fd)
{
    vid_write("*** DS-2 INIT ***");
    vid_write("QUACK AT YOUR OWN RISK!");
    vid_write("The author assumes no liability for any damages caused by this program.");

    file_des = fd;

    atexit(exit_handler);

    if(file_des < 0)
        error("invalid file");

    init_optable();
    init_tokmap();

    /* initialize the "." variable, which is the line counter */
    setVariable(".", 0);
    setConst(".", true);

    struct varnode_t *dot_var = lookup_var(".");

    /* initialize some other constants */
    setVariable("true", 1);
    setConst("true", true);

    setVariable("false", 0);
    setConst("false", true);

    line_offset = index_lines(file_des, &num_lines);
    vid_writef("Indexing complete (%u lines).", num_lines);

    vid_write("Executing...");

    int repeats_left = 0;

    while(1)
    {
        char instr_buf[MAX_LINE_LEN];
        memset(instr_buf, 0, sizeof(instr_buf));
        if(read_line(file_des, instr_buf, sizeof(instr_buf)) == 0)
        {
            vid_writef("end of file");
            goto done;
        }
        char *tok = NULL, *save = NULL;

        ++current_line;
        dot_var->val = current_line;
        ++lines_executed;

        char *buf = instr_buf;

        /* execute all the commands on this line/instruction */
        do {
            tok = strtok_r(buf, " -\t", &save);
            buf = NULL;

            if(!tok)
                break;

            int hash = tok_hash(tok) % TOKMAP_SIZE;
            if(hash >= 0 && strcmp(tokmap[hash].tok, tok) == 0)
                switch(tokmap[hash].func(&save, &repeats_left))
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
                    error("unknown return");
                }
            else
            {
                error("unknown token `%s` on line %d %d, %d", tok, current_line, hash, tok_hash("CTRL"));
                goto done;
            }
        } while(tok);
    break_out:

        if(repeats_left > 0)
        {
            --repeats_left;
            if(repeats_left)
                jump_line(file_des, current_line);
            else
                jump_line(file_des, current_line + 2);
        }
    next_line:
        ;
    }

done:

    return;
}
