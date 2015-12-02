/* mshell - a job manager */

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include "pipe.h"
#include <assert.h>

void do_pipe(char *cmds[MAXCMDS][MAXARGS], int nbcmd, int bg) {
	
 /*  	sigset_t mask; */
 /* 	int fds[MAXCMDS][2]; */
 /* 	int i, j, pid, nbargs; */
 /* 	char c = 'a'; */
	
 /* 	/\* Blocage des signaux pendant la création *\/ */
 /* 	if (sigemptyset(&mask) < 0) */
 /* 		unix_error("sigemptyset error"); */
 /* 	if (sigaddset(&mask, SIGCHLD)) */
 /* 		unix_error("sigaddset error"); */
 /* 	if (sigaddset(&mask, SIGINT)) */
 /* 		unix_error("sigaddset error"); */
 /* 	if (sigaddset(&mask, SIGTSTP)) */
 /* 		unix_error("sigaddset error"); */
 /* 	if (sigprocmask(SIG_BLOCK, &mask, NULL) < 0) */
 /* 		unix_error("sigprocmask error"); */
	
	
 /* 	/\* Création des tubes *\/ */
 /* 	for (i=0;i<(nbcmd-1);i++){ */
 /* 		assert(pipe(fds[i]) != -1); */
 /* 	} */
	
 /* 	/\* créatoin d'un fils *\/ */
 /* 	for (i=0;i<nbcmd;i++){ */
 /* 		if ((pid = fork()) < 0){ */
 /* 			unix_error("fork error"); */
 /* 		} */
	
 /* 	/\****************************************************************\/ */
 /* 	/\* Child  process *\/ */
 /* 		if (pid == 0) { */
 /* 			/\* Child unblocks signals *\/ */
 /* 			sigprocmask(SIG_UNBLOCK, &mask, NULL); */
 /* 			/\* Get a new process group ID *\/ */
 /* 			if (setpgid(0, 0) < 0) */
 /* 				unix_error("setpgid error"); */

 /* 			/\* Association aux tubes *\/ */
 /* 			if (i<(nbcmd-2)){ /\* n'est pas la dernière commande *\/ */
 /* 				if(write(fds[i+1][0], &c, 1) == -1) */
 /* 					assert(dup2(STDOUT_FILENO, fds[i+1][1]) != -1); */
 /* 				if(write(fds[i+1][1], &c, 1) == -1) */
 /* 					assert(dup2(STDOUT_FILENO,fds[i+1][0]) != -1); */
 /* 			} */
 /* 			if (i>0){ /\* n'est pas la première commande *\/ */
 /* 				if(read(fds[i][0], &c, 1) == -1) */
 /* 					assert(dup2(STDIN_FILENO, fds[i][1]) != -1); */
 /* 				if(read(fds[i][1], &c, 1) == -1) */
 /* 					assert(dup2(STDIN_FILENO,fds[i][0]) != -1); */
 /* 			} */
			
 /* 			/\* Ferme les tubes *\/ */
 /* 			for (j=0;j<(nbcmd-1);j++){ */
 /* 				close(fds[j][0]); */
 /* 				close(fds[j][1]); */
 /* 			} */
			
 /* 			/\* Count number of args for that job *\/ */
 /* 			nbargs=0; */
 /* 			while ((*cmds[MAXCMDS][nbargs] != NULL) && nbargs<MAXARGS){ */
 /* 				nbargs += 1; */
 /* 			} */
			
 /* 			/\* Load and run the program in the new job *\/ */
 /* 			if (execvp(*cmds[i][0], nbargs) < 0) { */
 /* 				printf("%s: Command not found\n", *cmds[i][0]); */
 /* 				exit(EXIT_FAILURE); */
 /* 			} */
 /* 		} */
 /* 	/\*****************************************************************\/ */
	
 /* /\* Parent process *\/ */
 /* 	 	if (i<(nbcmd-1)){ */
 /* 		 	jobs_addjob(pid, BG, *cmds[i]); */
 /* 	 	} else { */
 /* 			jobs_addjob(pid, (bg == 1 ? BG : FG), *cmds[i]); */
 /* 		} */
 /* 	} /\* fin du for de créattion *\/ */

 /* 		/\* Ferme les tubes *\/ */
 /* 	for (i=0;i<(nbcmd-1);i++){ */
 /* 		close(fds[i][0]); */
 /* 		close(fds[i][1]); */
 /* 	} */
	

 /* 	sigprocmask(SIG_UNBLOCK, &mask, NULL); */
 /* 	if (!bg){ */
 /* 		waitfg(pid); */
 /* 	} */
 /* 	return; */
}
	
