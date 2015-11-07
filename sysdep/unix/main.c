#include <ducky.h>
#include <platform.h>

int main(int argc, char *argv[])
{
    if(argc == 2)
    {
        int fd = open(argv[1], O_RDONLY);
        ducky_main(fd);
        close(fd);
    }
    else
    {
        printf("Usage: %s FILE\n", argv[0]);
    }
    return 0;
}
