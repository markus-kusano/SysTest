//extern void __VERIFIER_error() __attribute__ ((__noreturn__));

#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include <assert.h>

//void __VERIFIER_assert(int expression) { if (!expression) { ERROR: __VERIFIER_error();}; return; }


char *v;

void *thread1(void * arg)
{
  v = malloc(sizeof(char) * 8);
  return NULL;
}

void *thread2(void *arg)
{
  if (v) strcpy(v, "Bigshot");
  return NULL;
}


int main()
{
  pthread_t t1, t2;

  pthread_create(&t1, 0, thread1, 0);
  pthread_create(&t2, 0, thread2, 0);
  pthread_join(t1, 0);
  pthread_join(t2, 0);

  assert(!v || v[0] == 'B');

  return 0;
}

