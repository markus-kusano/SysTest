// Source: Azadeh Farzan, Zachary Kincaid, Andreas Podelski: "Inductive Data
// Flow Graphs", POPL 2013

#include <pthread.h>
#include <assert.h>

volatile int x;
#define N 2

void* thr1(void* arg) {
  assert(x < N);
  return NULL;
}

void* thr2(void* arg) {
  int t;
  t = x;
  x = t + 1;
  return NULL;
}

int main(int argc, char* argv[]) {
  pthread_t t1;
  pthread_t t2s[N];
  int i;
  x = 0;
  pthread_create(&t1, 0, thr1, 0);    
  for (i = 0; i < N; i++) {
    pthread_create(&(t2s[i]), 0, thr2, 0);
  }
  for (i = 0; i < N; i++) {
    pthread_join((t2s[i]), NULL);
  }
  return 0;
}
