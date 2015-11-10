#include <ducky.h>
#include <platform.h>

int main(int argc, char *argv[])
{
    if(argc >= 2)
    {
        if(strcmp(argv[1], "-c") == 0)
        {
            if(argc == 3)
            {
                int fd = open(argv[2], O_RDONLY);
                int out_fd = open("a.out", O_WRONLY | O_CREAT, 0644);
                ducky_compile(fd, true, out_fd);
                close(fd);
                close(out_fd);
            }
            else
                printf("expected filename\n");
        }
        else
        {
            int fd = open(argv[1], O_RDONLY);
            ducky_main(fd, false);
            close(fd);
        }
    }
    else
    {
        printf("Usage: %s [-c] FILE\n", argv[0]);
    }
    return 0;
}
