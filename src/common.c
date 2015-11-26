#include "ducky.h"

vartype spec_zero(void)
{
    return 0;
}

vartype spec_rand(void)
{
    return rand();
}

vartype spec_time(void)
{
    return time(NULL);
}

vartype (*special_funcs[])(void) = {
    spec_zero,
    spec_rand,
    spec_time,
};

void __attribute__((noreturn,format(printf,3,4))) error(jmp_buf exit_point, int current_line, const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    if(current_line)
        vid_logf("Line %d: ", current_line);
    vid_logf("ERROR: %s",  fmtbuf);
    va_end(ap);

    longjmp(exit_point, 1);
}

vartype get_special(enum special_id special, jmp_buf exit_point, int current_line)
{
    if(special < ARRAYLEN(special_funcs))
        return special_funcs[special]();
    else
        error(exit_point, current_line, "unknown special stream");
}

/* grabs a line from a file, -1 on error, returns # bytes read otherwise */
int read_line(int fd, char *buf, size_t sz)
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

/* index_lines() precalculates the offset of each line for faster jumping */

off_t *index_lines(int fd, unsigned *numlines, bool labels, bool (*isValidVariable)(const char*), void (*setConst)(const char*, bool), void (*setVariable)(const char*, vartype))
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

        if(labels)
        {
            char *save = NULL;
            char *tok = strtok_r(buf, " \t", &save);
            if(tok && (strcmp(tok, "LABEL") == 0 || strcmp("LBL", tok) == 0))
            {
                tok = strtok_r(NULL, " \t", &save);
                if(tok && isValidVariable(tok))
                {
                    setVariable(tok, idx);
                    setConst(tok, true);
                }
            }
        }

        ++idx;
    }

    lseek(fd, 0, SEEK_SET);

    *numlines = idx - 1;

    return data;
}

void __attribute__((format(printf,1,2))) vid_logf(const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    printf("%s\n", fmtbuf);
    va_end(ap);
}

void __attribute__((format(printf,2,3))) warning_real(int current_line, const char *fmt, ...)
{
    char fmtbuf[256];

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(fmtbuf, sizeof(fmtbuf), fmt, ap);
    vid_logf("Line %d: WARNING: %s\n", current_line, fmtbuf);
    va_end(ap);
}
