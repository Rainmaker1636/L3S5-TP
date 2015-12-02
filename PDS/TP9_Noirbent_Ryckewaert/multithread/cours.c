void * myThread(void *arg){
  int myarg=*((int) arg);
  int result;

  return &resultat;
}

struct param{
  int a;
  char string[20];
  int b;
  int res_x;
  int res_y;
}; 
void * mythread(void *arg){
  struct param *argp=(struct param*)arg;
  arg->a;
  return;
}

pthread_t tid;
int param=5;
int ret=pthread_create(&tid,NULL,myThread,&param);

int *adresse;
int status=pthread_join(tid,&adresse);
printf("Le résultat est %d\n",*adresse);
