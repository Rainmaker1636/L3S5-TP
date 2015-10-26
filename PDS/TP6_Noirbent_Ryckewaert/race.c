
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int race(){
	int i;
	pid_t arrive[10];
	int res_wait;
	for(i=0; i<10; i++){
		switch(fork()){
			case -1:
				exit(0);
			case 0:
				for(i=0; i<100000000; i++){}
				printf("fils : %d\n", getpid());
				for(i=0; i<100000000; i++){}
				exit(0);
			default:
				{}
		}
	for(i=0; i<10; i++){
		arrive[i] = wait(&res_wait);
		}
	}
	for(i=0; i<10; i++){
		printf("Le %deme était le fils %d.\n", i, (int) arrive[i]);
	}
	exit(0);
}

int main (int argc, char *argv[]){
  race();
  return 0;
}
