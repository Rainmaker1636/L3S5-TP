
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

typedef int (*func_t) (int);

int multif(func_t f[], int args[], int n){
	int i;
	int bolean = 1;
	int result_wait;
	for(i=0; i<n; i++){
		switch(fork()){
			case -1:
				exit(0);
			case 0:
				exit(f[i](args[i]));
			default:
				wait(&result_wait);
				if(WIFEXITED(result_wait)){
					bolean *= WEXITSTATUS(result_wait);
				} else {
					bolean *= 0;
				}
		}
	}
	exit(bolean);
}
