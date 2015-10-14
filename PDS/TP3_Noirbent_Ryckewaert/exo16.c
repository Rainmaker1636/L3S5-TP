#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>


#define BUFSIZE 1024

int nlines_buf(const char * buf, size_t size){
  int nlines = 0;
  int i;
  for (i=0; i<size; i++){
    if (buf[i] == '\n'){
      nlines++;
    }
  }
  return nlines;
}


int nlines_file (int fd){
  int nlines = 0;
  char buf[BUFSIZE];
  int bsize;
  while ((bsize = read(fd, &buf, BUFSIZE))){
    if (bsize == -1){
      exit(EXIT_FAILURE);
    }
    nlines += nlines_buf(buf, bsize);
  }
  return nlines;
}

int starting_line(int nb, int n){
  if (n>nb){
    return 0;
  }
  else{
    return nb - n;
  }
}

void print_ending_lines (FILE *file,int start){
  int nlines = 0;
  int i;
  char c;
  i=0;
  while (nlines < start){
    if (getc(file) == '\n'){
      nlines++;
    }
    i++;
  }
  while ((c= getc(file)) != EOF){
    printf("%c",c);
    i++;
  }
  return;
}

int main (int argc, char *argv[]){
  int fd ;
  int start;
  FILE *file;
  if (argc != 2){
    printf("Usage : %s <nombre de lignes voulues>", argv[0]);
    return 1;
  }
  fd = open("test.txt",R_OK);
  start=starting_line(nlines_file(fd), atoi(argv[1]));
  close(fd);
  file = fopen("test.txt","r");
  print_ending_lines(file,start);
  close(fd);
  return 0;
}
