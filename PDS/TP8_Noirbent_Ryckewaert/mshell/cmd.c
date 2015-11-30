/* mshell - a job manager */

#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include "jobs.h"
#include "sighandlers.h"

#define BOLD "\033[00;01m"
#define NORM "\033[00;00m"

void do_help() {
    printf("available commands are:\n");
    printf(" exit - cause the shell to exit\n");
    printf(BOLD "\t exit\n" NORM);
    printf(" jobs - display status of jobs in the current session\n");
    printf(BOLD "\t jobs\n" NORM);
    printf(" fg   - run a job identified by its pid or job id in the foreground\n");
    printf(BOLD "\t fg " NORM "pid" BOLD "|" NORM "jobid \n");
    printf(" bg   - run a job identified by its pid or job id in the background\n");
    printf(BOLD "\t bg " NORM "pid" BOLD "|" NORM "jobid \n");
    printf(" stop - stop a job identified by its pid or job id\n");
    printf(BOLD "\t stop " NORM "pid" BOLD "|" NORM "jobid \n");
    printf(" kill - kill a job identified by its pid or job id\n");
    printf(BOLD "\t kill " NORM "pid" BOLD "|" NORM "jobid \n");
    printf(" help - print this message\n");
    printf(BOLD "\t help\n" NORM);
    printf("\n");
    printf("ctrl-z and ctrl-c can be used to send a SIGTSTP and a SIGINT, respectively\n\n");
}

/* treat_argv - Determine pid or jobid and return the associated job structure */
struct job_t *treat_argv(char **argv) {
  struct job_t *jobp = NULL;

  /* Ignore command if no argument */
  if (argv[1] == NULL) {
    printf("%s command requires PID or %%jobid argument\n", argv[0]);
    return NULL;
  }

  /* Parse the required PID or %JID arg */
  if (isdigit((int) argv[1][0])) {
    pid_t pid = atoi(argv[1]);
    if (!(jobp = jobs_getjobpid(pid))) {
      printf("(%d): No such process\n", (int) pid);
      return NULL;
    }
  } else if (argv[1][0] == '%') {
    int jid = atoi(&argv[1][1]);
    if (!(jobp = jobs_getjobjid(jid))) {
      printf("%s: No such job\n", argv[1]);
      return NULL;
    }
  } else {
    printf("%s: argument must be a PID or %%jobid\n", argv[0]);
    return NULL;
  }

  return jobp;
}


/* do_bg - Execute the builtin bg command */
void do_bg(char **argv) {
  sigset_t mask;
  struct job_t *jobp = NULL;
  sigemptyset(&mask);
  sigaddset(&mask,SIGCHLD);
  sigaddset(&mask,SIGSTOP);
  sigaddset(&mask,SIGINT);
  sigprocmask(SIG_BLOCK,&mask,NULL);
  if ((jobp=treat_argv(argv)) != NULL){
    if (kill(-(jobp->jb_pid),SIGCONT)<0)
      unix_error("Kill error");
    jobp->jb_state=BG;
    sigprocmask(SIG_UNBLOCK,&mask,NULL);
  }
  else{
    perror("Aucun argument n'est donné");
    do_help();
  }
  return;
}

/* waitfg - Block until process pid is no longer the foreground process */
void waitfg(pid_t pid) {
  struct job_t *j =jobs_getjobpid(pid);
  if (!j){
    perror("waitfg : le pid ne correspond à aucun job.");
    exit(EXIT_FAILURE);
  }
  if (verbose){
    printf("waitfg : Blocked until process %d is no longer in foreground.\n",pid);
  }
  while (j->jb_state==FG){
    sleep(1);
  }
  return;
}

/* do_fg - Execute the builtin fg command */
void do_fg(char **argv) {
  sigset_t mask;
  struct job_t *jobp = NULL;
  sigemptyset(&mask);
  sigaddset(&mask,SIGCHLD);
  sigaddset(&mask,SIGSTOP);
  sigaddset(&mask,SIGINT);
  sigprocmask(SIG_BLOCK,&mask,NULL);
  if ((jobp=treat_argv(argv)) != NULL){
    if (kill(-(jobp->jb_pid),SIGCONT)<0)
      unix_error("Kill error");
    jobp->jb_state=FG;
    /*sigprocmask(SIG_UNBLOCK,&mask,NULL);*/
    waitfg(jobp->jb_pid);
  }
  else{
    perror("Aucun argument n'est donné");
    do_help();
  }
  return;
}

/* do_stop - Execute the builtin stop command */
void do_stop(char **argv) {
  sigset_t mask;
  struct job_t *jobp = NULL;
  sigemptyset(&mask);
  sigaddset(&mask,SIGCHLD);
  sigaddset(&mask,SIGSTOP);
  sigaddset(&mask,SIGINT);
  sigprocmask(SIG_BLOCK,&mask,NULL); 
  if ((jobp=treat_argv(argv)) != NULL){
    if (kill(-(jobp->jb_pid),SIGSTOP)<0)
      unix_error("Kill error");
    sigchld_handler(SIGSTOP); /* Appel au handler pour mettre à jour l'état du processus si il s'est arreté */
    sigprocmask(SIG_UNBLOCK,&mask,NULL);
  }
  else{
    perror("Aucun argument n'est donné");
    do_help();
  }
    return;
}

/* do_kill - Execute the builtin kill command */
void do_kill(char **argv) {
  sigset_t mask;
  struct job_t *jobp = NULL;
  sigemptyset(&mask);
  sigaddset(&mask,SIGCHLD);
  sigaddset(&mask,SIGSTOP);
  sigaddset(&mask,SIGINT);
  sigprocmask(SIG_BLOCK,&mask,NULL);
  if ((jobp=treat_argv(argv)) != NULL){
    if (kill(-(jobp->jb_pid),SIGINT)<0)
      unix_error("Kill error");
    sigchld_handler(SIGSTOP);
    sigprocmask(SIG_UNBLOCK,&mask,NULL);
  }
  else{
    perror("Aucun argument n'est donné");
    do_help();
  }
  return;
}

/* do_exit - Execute the builtin exit command */
void do_exit() {
  int i;
  for (i = 0; i < MAXJOBS; i++) {
    if ((kill(i,SIGKILL))==0){ /* si un processus a été tué */
    		jobs_deletejob(i);
    	}
    }
    exit(EXIT_SUCCESS); /* fin mini shell */
}

/* do_jobs - Execute the builtin fg command */
void do_jobs() {
  jobs_listjobs();
    return;
}
