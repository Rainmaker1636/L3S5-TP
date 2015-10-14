#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int 
main 
(int argc, char *argv[]){
  int fd,size;
  char * buf;
  
  fd = open(argv[1], O_SYNC);
  size = 4096;
  buf = malloc(size);
  while ((size = read(fd,buf,size)) > 0) {
    write (STDOUT_FILENO,buf,size);
  }
  
  close(fd);
  free(buf);
  return 0;
}

