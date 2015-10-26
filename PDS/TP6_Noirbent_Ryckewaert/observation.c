
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int observation(int n){
	int i;
	int res_wait;
	for(i=1; i<=n; i++){
		switch(fork()){
			case -1:
				exit(0);
			case 0:
				while(1){
					printf("Fils %d : toujours vivant.\n", getpid());
					sleep(5);
				}
			default:
				{}
		}
	}
	system("ps -a");
	getchar();
	for(i=1; i<=n; i++){
		 printf("Mort du fils %d\n", wait(&res_wait));
		}
	exit(0);
}

int main (int argc, char *argv[]){
  if (argc != 2){
    printf("Usage : %s <nombre de fils voulus>", argv[0]);
    return 1;
  }
  observation(atoi(argv[1]));
  return 0;
}
