enum jstate{UNDEF,BG,FG,ST};
struct job_t{
  pid_t jb_pid;
  int jb_jid;
  int jb_state;
  char jb_cmdline[MAXLINE];
};
#define MAXJOBS 16
static struct job_t[MAXJOBS];

void jobs_initjobs();
int void_addjob(pid_t pid,int state,char *cmdline);
pid_t jobs_fgpid();
int jobs_pidzjid();
void jobs_listjobs();

SIGINT -> Ctrl C
SIGSTP -> Ctrl Z
SIGCHLD

typedef void(*handler_t)(int);
int sigaction_wrapper(int signum,handler_t *handler){
  struct sigaction action;
  action.sa_handler=handler;
  sigemptyset(&action.sa_mask);
  sigaction(signum,&action,NULL);
}
void sigint_handler(int sig)
{
  if (verbose) printf("sigint_handler entering\n");
  if ((pid=jobs_fgpid())>0){
    if (verbose) printf("Sending Kill to %d\n",pid);
    if (kill(pid,SIGINT)<0){
      // Error
    }
  }
}

/* sigchld se déclenche quand un fils est suspendu ou interrompu */
void sigchld_handler(int sig)
{
  pid_t child_pid;
  int child_jid;
  int status;
  if (verbose) printf('sigchild_handler entering');
  while ((child_pid = waitpid(-1,&status,WNOHANG(WUNTRACED)))>0)
    if (WIFSTOPPED(status)){
      struct job_t *j=jobs_getjobpid(child_pid);
      if (!j) //Error
	j->state=ST;
    }
    else {
      if(WIFSIGNALED(status)){
	child_jid=jbs_pid2jd(child_pid);
	if (jobs_deletejob(child_jid))
	  if (verbose)
	    printf("sigchld_ahandler %d delete",child_jid);
	fprintf(stdout,"Job[%d] (%d) terminated by signal %d\n",child_jid,child_pid,WTERMSIG(status));
      }
      else {
	if (WIFEXITED(status)){
	  
	}
	if(!((child_pid==0)||(child_pid==-1 && errno=ECHILD))){ // ERROR 
	
	}
      }
    }
}

void waitfg(pid_t pid){
  struct job_t *j = jobs_getjobpid(pid);
  if (!j) return
while (j->jb_pid==pid && j->jb_state=FG)
	sleep(1);
}

/* ********************************************* */
// Exo 10

/*
creer tube
creer fils 1
	- ferme lecture tube
	- dup sortie standard par écriture tube
	- ferme écriture tube
	- exec
creer fils 2
	- ferme écriture tube
	- dup entrée standard par lecture tube
	- ferme lecture tube
	- exec
ferme lecture tube
ferme écriture tube
*/


