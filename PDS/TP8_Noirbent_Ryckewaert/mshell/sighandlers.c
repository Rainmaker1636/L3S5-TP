/* mshell - a job manager */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>
#include <errno.h>


#include "jobs.h"
#include "common.h"
#include "sighandlers.h"

/*
 * wrapper for the sigaction function
 */
int sigaction_wrapper(int signum, handler_t * handler) {
  /* Début implémentation */
  struct sigaction action;
  action.sa_handler=handler;
  action.sa_flags=SA_RESTART;
  sigemptyset(&action.sa_mask);
  sigaction(signum,&action,NULL);
  /* fin implémentation */
  return 1;
}

/*
 * sigchld_handler - The kernel sends a SIGCHLD to the shell whenever
 *     a child job terminates (becomes a zombie), or stops because it
 *     received a SIGSTOP or SIGTSTP signal. The handler reaps all
 *     available zombie children
 */
void sigchld_handler(int sig) {
  pid_t child_pid;
  int child_jid;
  int status;
  if (verbose)
    printf("sigchld_handler: entering\n");
  /* Début implémentation */

  if (verbose) printf("sigchild_handler entering");
  while ((child_pid = waitpid(-1,&status,WNOHANG||WUNTRACED)))>0)
  if (WIFSTOPPED(status)){
    struct job_t *j=jobs_getjobpid(child_pid);
    if (!j) {
      perror("sigchld_handler : le pid ne correspond à aucun job.");
      exit(EXIT_FAILURE);
    }
    j->state=ST;
  }
  else {
    if(WIFSIGNALED(status)){
      child_jid=jbs_pid2jd(child_pid);
      if (jobs_deletejob(child_jid)){
	if (verbose){
	  printf("sigchld_handler %d delete",child_jid);
	}
      }
      fprintf(stdout,"Job[%d] (%d) terminated by signal %d\n",child_jid,child_pid,WTERMSIG(status));
    }
    else {
      if (WIFEXITED(status)){
      child_jid=jbs_pid2jd(child_pid);
      if (jobs_deletejob(child_jid)){
	if (verbose){
	  printf("sigchld_handler %d delete",child_jid);
	}
      }
      fprintf(stdout,"Job[%d] (%d) terminated by exit %d\n",child_jid,child_pid,WEXITSTATUS(status));
      }
      if(!((child_pid==0)||(child_pid==-1 && errno=ECHILD))){
	perror("sigchld_handler : le pid ne correspond pas à un child.");
	exit(EXIT_FAILURE);
      }
    }
  }
/* fin implémentation */
if (verbose)
  printf("sigchld_handler: exiting\n");
return;
}


/*
 * sigint_handler - The kernel sends a SIGINT to the shell whenver the
 *    user types ctrl-c at the keyboard.  Catch it and send it along
 *    to the foreground job.
 */
void sigint_handler(int sig) {
  if (verbose)
    printf("sigint_handler: entering\n");
  /* Début implémentation */
  if ((pid=jobs_fgpid())>0){
    if (verbose){
      printf("Sending Kill %d to %d\n",sig,pid);
    }
    if (kill(pid,SIGINT)<0){
      perror("sigint_handler : l'envoi du signal a échoué.");
      exit(EXIT_FAILURE);
    }
  }
  /* fin implémentation */
  if (verbose)
    printf("sigint_handler: exiting\n");
  return;
}

/*
 * sigtstp_handler - The kernel sends a SIGTSTP to the shell whenever
 *     the user types ctrl-z at the keyboard. Catch it and suspend the
 *     foreground job by sending it a SIGTSTP.
 */
void sigtstp_handler(int sig) {
  if (verbose)
    printf("sigtstp_handler: entering\n");
  /* Début implémentation */
  if (verbose){
    printf("sigint_handler entering\n");
  }
  if ((pid=jobs_fgpid())>0){
    if (verbose){
      printf("Sending stop to %d\n",pid);
    }
    if (kill(pid,SIGTSTP)<0){
      perror("sigstp_handler : l'envoi du stop a échoué.");
      exit(EXIT_FAILURE);
    }
  }
  /* fin implémentation */
  if (verbose)
    printf("sigtstp_handler: exiting\n");

  return;
}
