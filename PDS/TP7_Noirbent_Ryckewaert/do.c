#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "makeargv.h"
#include <sys/types.h>
#include <sys/wait.h>

enum flag_e{
  NIHIL=0, AND = 1, OR = 2, KILL = 4, CC = 8
} ;

int getCtrlValue(int argc, char *argv[], char *optList, int *check){
  int opt ;
  while((opt = getopt(argc, argv, optList))!=-1){
    switch(opt){
    case -1 :
      perror("do : something wrong was happened. Case -1 uses normally not to be treated.\n") ;
      return EXIT_FAILURE ;
    case '?' :
      perror("do : You try to use a flag not recognized.\n") ;
      return EXIT_FAILURE ;
    case 'a' :
      *check |= AND ;
      break ;
    case 'o' :
      *check |= OR ;
      break ;
    case 'c' :
      *check |= CC ;
      break ;
    case 'k' :
      *check |= KILL ;
      break ;
    default :
      *check |= NIHIL ;
      break ;
    }
  }
  return EXIT_SUCCESS ;
}

void executeProgramme(int nbParam,int argc,char *argv[]){
  int i,fid;
  char **tab;
  for (i=nbParam;i<argc;i++){
    switch(fid=fork()){
    case -1 : 
      perror("Error in executing");
      exit(EXIT_FAILURE);
    case 0 : 
      makeargv(argv[i]," ",&tab);
      execvp(*tab,tab+1);
      perror("Error in executing");
      exit(EXIT_FAILURE);
    default : {} /* Je dors */
    }
  }
}

/* Programme permettant de faire le kill, nous n'avons pas réussi à le faire */
void executeProgrammeKill(int nbParam,int argc,char *argv[],pid_t *tabPid){
  int i;
  char **tab;
  for (i=nbParam;i<argc;i++){
    switch(tabPid[i-nbParam]=fork()){
    case -1 : 
      perror("Error in executing");
      exit(EXIT_FAILURE);
    case 0 : 
      makeargv(argv[i]," ",&tab);
      execvp(*tab,tab+1);
      perror("Error in executing");
      exit(EXIT_FAILURE);
    default : {} /* Je dors */
    }
  }
}

int main (int argc,char *argv[]){
  int result,i,status;
  pid_t *tabPid;
  char *optlist;
  int ctrl;
  int nbParam;
  optlist="aock";
  ctrl=0;
  if (argc<=2){
    fprintf(stderr,"%s [--and|--or]<cmd> ... <cmd>\n",argv[0]);
    exit(3);
  }
  getCtrlValue(argc,argv,optlist,&ctrl);
  
  switch (ctrl){
    /* Cas --and */
  case 1 :
    nbParam=2;
    executeProgramme(nbParam,argc,argv);
    result=0;
    for (i=nbParam;i<argc;i++){
      wait(&status);
      if (!WIFEXITED(status) || WEXITSTATUS(status) == EXIT_FAILURE){
	result++;
      }
    }
    if (result > 0){
      exit(EXIT_FAILURE);
    }
    exit(EXIT_SUCCESS);
    /* cas --or */
  case 2 : 
    nbParam=2;
    executeProgramme(nbParam,argc,argv);
    result=1;
    for (i=nbParam;i<argc;i++){
      wait(&status);
      if (WIFEXITED(status) && WEXITSTATUS(status) == EXIT_SUCCESS){
	result*=0;
      }
    }
    exit(result);
    /* Cas --and --cc */
  case 9  :
    nbParam=3;
    executeProgramme(nbParam,argc,argv);
    for (i=nbParam;i<argc;i++){
      wait(&status);
      if (!WIFEXITED(status) || WEXITSTATUS(status) == EXIT_FAILURE){
	printf("On s'arrete au parametre numéro %i\n",i);
	exit(EXIT_FAILURE);
      }
    }
    exit(EXIT_SUCCESS);
    /* Cas --or --cc */
  case 10 :
    nbParam=3;
    executeProgramme(nbParam,argc,argv);
    for (i=nbParam;i<argc;i++){
      wait(&status);
      if (WIFEXITED(status) && WEXITSTATUS(status) == EXIT_SUCCESS){
	printf("On s'arrete au parametre numéro %i\n",i);
	exit(EXIT_SUCCESS);
      }
    }
    exit(EXIT_FAILURE);
    /* Cas --and --kill -cc */
  case 13 : 
    nbParam=4;
    tabPid=malloc(sizeof(pid_t)*(argc-nbParam));
    executeProgrammeKill(nbParam,argc,argv,tabPid);
    for (i=nbParam;i<argc;i++){
      wait(&status);
      if (!WIFEXITED(status) || WEXITSTATUS(status) == EXIT_FAILURE){
	printf("On s'arrete au parametre numéro %i\n",i);
	exit(EXIT_FAILURE);
      }
    }
    exit(EXIT_SUCCESS);
    /* Cas --or -kill -cc */
  case 14 : 
    nbParam=4;
    tabPid=malloc(sizeof(pid_t)*(argc-nbParam));
    executeProgrammeKill(nbParam,argc,argv,tabPid);
    for (i=nbParam;i<argc;i++){
      wait(&status);
      if (WIFEXITED(status) && WEXITSTATUS(status) == EXIT_SUCCESS){
	printf("On s'arrete au parametre numéro %i\n",i);
	exit(EXIT_SUCCESS);
      }
    }
    exit(EXIT_FAILURE);

  default : {
    printf("Ne correspond à aucun cas\n");
    exit(EXIT_FAILURE);}
  }
}
