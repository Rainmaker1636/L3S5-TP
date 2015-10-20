#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>

int
main
(int argc, char *argv[]){
  int ret,mode;
  int i, erreur=0;
  char *filename;
  if (argc < 3) {
    fprintf(stderr,"Usage : %s -[rwxv] <filepath> \n", argv[0]);
    exit (EXIT_FAILURE);
  }
  for (i=1;i<argc-1;i++){
    switch(argv[i][1]){
    case 'r' : mode |= R_OK;break;
    case 'w' : mode |= W_OK;break;
    case 'x' : mode |= X_OK;break;
    case 'v' : erreur=1;break;
    default :
      fprintf(stderr,"Bad option \n");
      exit (EXIT_FAILURE);
      break;
    }
  }
  filename=argv[argc-1];
  ret=access(filename,mode);
  if ((ret != 0) && erreur) {
    perror("Error :");
  }
  return ret;
}
