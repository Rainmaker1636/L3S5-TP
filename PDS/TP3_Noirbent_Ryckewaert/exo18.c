#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>


#define BUFSIZE 1024

int 
index_tail_buffer
(const char *buf, int bufsize,int ntail,int *nlines){
  int i;
  int nblines;
  nblines=0;
  for (i=bufsize-1;i>=0;i--){
    if (buf[i] == '\n'){
      nblines++;
    }

    if (nblines-1 == ntail){
      *nlines=nblines;
      return i+1;
    }
    
  }

  *nlines=nblines;
  return -1;
}

void
tail_before_pos
(int fd,int pos,int ntail){
  char buf[BUFSIZE];
  int i;
  int nlines;
  int offset;
  int taille;
  int nrestant;
  nrestant=BUFSIZE;
  nlines=0;
  taille=lseek(fd,0,SEEK_END);
  if ((taille-pos)<BUFSIZE){
    nrestant=taille-pos;
  }
  offset=nrestant+pos;
  lseek(fd,-offset,SEEK_END);
  read(fd, &buf, nrestant);
  i=index_tail_buffer(buf,nrestant,ntail,&nlines);
 if (i<0 && nrestant>0){
    tail_before_pos(fd,offset,ntail-nlines);
  }
  while (i < nrestant){
    printf("%c",buf[i]);
    i++;
  }
  return;
}

int main (int argc, char *argv[]){
  int fd ;
  if (argc != 2){
    printf("Usage : %s <nombre de lignes voulues>", argv[0]);
    return 1;
  }
  fd = open("test.txt",R_OK);
  tail_before_pos(fd,0,atoi(argv[1]));
  close(fd);
  return 0;
}
