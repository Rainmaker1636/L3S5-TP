#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int 
main
(int argc, char *argv[]){
  int c;
  FILE *fp;
 
  fp = fopen(argv[1], "r");
  while ((c = fgetc(fp)) != EOF) {
    fputc(c, stdout);
  }
  
  fclose(fp);
  return 0;
}
