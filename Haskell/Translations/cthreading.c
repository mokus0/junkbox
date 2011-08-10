#include <pthread.h>
#include <stdio.h>

pthread_mutex_t lock1;

static void* worker_thread(void *arg){
  long long *n;
  pthread_mutex_lock( &lock1 );

  n = (long long *) arg;
  printf("Testing: %lld.\n", n);

  pthread_mutex_unlock( &lock1 );

  return NULL;
}

//nthreads is the total number of threads
#define nthreads 1000
pthread_t thread_id[nthreads] = {};

int main() {
    int i;
    
    pthread_mutex_init( &lock1, NULL );
    
    for(i=0; i < nthreads ; i++){
      pthread_create( &thread_id[i], NULL, worker_thread, &i); //Problem
    }


    for(i=0; i < nthreads ; i++){
      pthread_join( thread_id[i], NULL);
    }
}