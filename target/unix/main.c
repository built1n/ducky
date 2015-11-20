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
            if(!strcmp(argv[i], "-c"))
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
                ducky_main(fd, false);
                break;
            case COMPILE:
                out_fd = open("a.out", O_WRONLY | O_CREAT | O_TRUNC, 0644);
                ducky_compile(fd, false, out_fd);
                break;
            case EXECUTE:
                ducky_vm(fd);
                break;
            case TRANSCOMPILE:
                out_fd = open("a.c", O_WRONLY | O_CREAT | O_TRUNC, 0644);
                ducky_to_c(fd, out_fd);
                break;
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
