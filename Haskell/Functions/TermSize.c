#include <sys/ioctl.h>
int ioctl_TIOCGWINSZ(int fd, struct winsize *wsz) {
    return ioctl(fd, TIOCGWINSZ, wsz);
}
