#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int 
main 
(int argc, char *argv[]){
  char *buf;
  int fd, size;
  
  fd = open(argv[1], O_RDONLY);
  
  size = atoi(getenv("MCAT_BUFSIZ"));
  buf = malloc(size);
  while ((size = read(fd,buf,size)) > 0) {
    write(STDOUT_FILENO, buf, size);
  }
  
  close(fd);
  free(buf);
  return 0;
}
