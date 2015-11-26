#include <ducky.h>
#include <platform.h>

#define COMPILE 1
#define INTERP 2
#define EXECUTE 3
#define TRANSCOMPILE 4
#define EVERYTHING 5

char *progname;

void arg_error(void)
{
    printf("Usage: %s [-aceit] FILE\n", progname);
    printf(" -a: compile to machine code\n");
    printf(" -c: compile to bytecode\n");
    printf(" -e: execute bytecode (not needed)\n");
    printf(" -i: interpret directly (default action)\n");
    printf(" -t: transcompile to C\n");
    printf("Default action is to interpret or execute bytecode.\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    int action = INTERP;
    progname = argv[0];
    char *file = NULL;
    if(argc >= 2)
    {
        for(int i = 1; i < argc; ++i)
        {
            if(!strcmp(argv[i], "-a"))
                action = EVERYTHING;
            else if(!strcmp(argv[i], "-c"))
                action = COMPILE;
            else if(!strcmp(argv[i], "-i"))
                action = INTERP;
            else if(!strcmp(argv[i], "-e"))
                action = EXECUTE;
            else if(!strcmp(argv[i], "-t"))
                action = TRANSCOMPILE;
            else
                file = argv[i];
        }
        if(file)
        {
            int fd = open(file, O_RDONLY), out_fd;
            unsigned char header[4];
            read(fd, &header, sizeof(header));
            if(*((uint32_t*)header) == DUCKY_MAGIC)
            {
                printf("Detected ducky bytecode signature.\n");
                if(action != TRANSCOMPILE)
                    action = EXECUTE;
            }
            lseek(fd, 0, SEEK_SET);
            switch(action)
            {
            case INTERP:
                if(ducky_interp(fd, false))
                {
                    printf("Interpreter error.\n");
                    return 1;
                }
                break;
            case COMPILE:
                out_fd = open("a.out", O_WRONLY | O_CREAT | O_TRUNC, 0644);
                if(ducky_compile(fd, false, out_fd))
                {
                    printf("Compiler error.\n");
                    return 1;
                }
                break;
            case EXECUTE:
                if(ducky_vm(fd))
                {
                    printf("Bytecode interpreter error.\n");
                    return 1;
                }
                break;
            case TRANSCOMPILE:
                out_fd = open("a.c", O_WRONLY | O_CREAT | O_TRUNC, 0644);
                if(ducky_to_c(fd, out_fd))
                {
                    printf("Transcompiler error.\n");
                    return 1;
                }
                break;
            case EVERYTHING:
            {
                char bytecode[L_tmpnam];
                tmpnam(bytecode);
                out_fd = open(bytecode, O_WRONLY | O_CREAT | O_TRUNC, 0644);
                if(ducky_compile(fd, false, out_fd))
                {
                    printf("Compiler error.\n");
                    return 1;
                }

                close(fd);
                close(out_fd);

                char c_code[L_tmpnam];
                tmpnam(c_code);
                fd = open(bytecode, O_RDONLY);
                out_fd = open(c_code, O_WRONLY | O_CREAT | O_TRUNC, 0644);
                if(ducky_to_c(fd, out_fd))
                {
                    printf("Transcompiler error.\n");
                    return 1;
                }
                close(fd);
                close(out_fd);
                char cmd[256];
                snprintf(cmd, sizeof(cmd), "cc -O3 -lm -x c %s", c_code);
                system(cmd);
            }
            default:
                break;
            }
        }
    }
    else
    {
        arg_error();
    }
    return 0;
}
