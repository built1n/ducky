#include <ducky.h>

#include <fcntl.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    if(argc == 2)
    {
        int fd = open(argv[1], O_RDONLY);
        ducky_main(fd);
        close(fd);
    }

    return 0;
}
