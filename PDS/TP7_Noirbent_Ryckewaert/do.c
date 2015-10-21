#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "makeargv.h"
#include <sys/types.h>
#include <sys/wait.h>

int main (int argc,char *argv[]){
  int fid,result,i,status;
  char **tab;

  if (!strcmp(argv[1],"--and")){
    result=0;
  }
  else{
    result=1;
  }
  
  if (argc<=3)
    fprintf(stderr,"%s [--and|--or]<cmd> ... <cmd>\n",argv[0]);

  for (i=2;i<argc;i++){
    switch(fid=fork()){
    case -1 : perror("Error in executing");
      exit(EXIT_FAILURE);
    case 0 : makeargv(argv[i]," ",&tab);
      execvp(*tab,tab+1);
      perror("Error in executing");
      exit(EXIT_FAILURE);
    default : {} /* Je dors */
    }
  }
  
  for (i=2;i<argc;i++){
    wait(&status);
    
    if (!strcmp(argv[1],"--and")){
      if (!WIFEXITED(status) || WEXITSTATUS(status) == EXIT_FAILURE){
	result++;
      }
    }
    
    if (!strcmp(argv[1],"--or")){
      if (WIFEXITED(status) && WEXITSTATUS(status) != EXIT_FAILURE){
	result*=0;
      }
    }
    
  }
  printf("%i\n",result);
  exit(result);
}
