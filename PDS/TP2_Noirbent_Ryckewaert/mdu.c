#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
/*static int opt_follow_links=0;*/
static int opt_apparent_size=1;


int du_file(char *pathname);
int compute_dir( char *p);

void
build_path
(const char *a,const char *b, char *c){
  assert(strlen(a)+strlen(b)+1<PATH_MAX);
  strcpy(c,a);
  strcat(c,"/");
  strcat(c,b);
}

int
valid_name
(char *pathname){
  int i=strlen(pathname);
  while (pathname[i] != '/' && i>1){
    i--;
    if (strcmp(pathname+i+1,".")==0 || strcmp(pathname+i+1,"..")==0){
      return 0;
    }
    else{
      return 1;
    }
  }
  return 0;
}

int
compute_dir
(char *p){
  DIR *d;
  struct dirent *sd;
  struct stat st;
  int taille;
  lstat(p,&st);
  if (opt_apparent_size)
    taille=st.st_size;
  else
    taille=st.st_blocks;
  d=opendir(p);
  while ((sd=readdir(d)) != 0){
    build_path(p,sd->d_name,p);
    taille+=du_file(p);
  }
  return taille;
}

int
du_file
(char *pathname){
  struct stat st;
  lstat(pathname,&st);
  if (!S_ISDIR(st.st_mode)){
    if (opt_apparent_size) return st.st_size;
    else
      return st.st_blocks;
  }
  else{
    return compute_dir(pathname);
  }
  return 0;
}

int
main
(int argc,char *argv[]){
  if (argc < 3) {
    fprintf(stderr,"Usage : %s <filepath> <0/1> \n", argv[0]);
    exit (EXIT_FAILURE);
  }
  if (*argv[2] == '0')
    opt_apparent_size=0;
  printf("Taille : %i\n", du_file(argv[1]));
  return 0;
}


