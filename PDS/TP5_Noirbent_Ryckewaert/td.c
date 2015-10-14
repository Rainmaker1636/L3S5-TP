Appels système descripteurs de fichier avec tampon
open read write lseek

struct
pds_file_t{
	int pds_fd; // Descripteur de fichier
	unsigned char* pds_buf;
	long pds_pos;
	int pds_bpos;
	int pds_bcnt;
	int pds_dirty;
	int pds_error;
}

struct pds_file_t * pds_open(chr *fname, int op){
	struct pds_file_t *s=malloc(sizeof(struct pds_file_t));
	s->pds_fd=open(fname,op); // O_RDONLY O_WRONLY O_RDWR
	s->pds_buf)malloc(BUFSIZE);
	s->pds_pos=0;
	s->pds_bpos=0;
	s->pds_bcnt=0;
	s->pds_dirty=0;
	s->pds_error=0;
}

int pds_close(struct pds_file_t *s){
	if (s->pds_dirty){
		lseek(Déplace la position à la position qui correspond au début du fichier dans le buffer);
		write(s->pds_fd,s->pds_buf,s->pds_bcnt);
	}
	close (s->pds_fd);
	free(s->pds_buf);
	free(s;
}

int pds_getc(pds_file_t *s){
	if(s->bpos == s->bcnt){
		s->bcnt=read(s->pds_fd,s->pds_buf,BUFSIZE); // Il faut vérifier l'erreur (Fin de fichier)
		s->bpos=0;
	}
	return s->buf[s->bpos++];
}


