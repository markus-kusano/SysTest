//extern void __VERIFIER_error() __attribute__ ((__noreturn__));

#include <pthread.h>
#include <assert.h>

//#define assume(e) __VERIFIER_assume(e)
//#define assert(e) { if(!(e)) { ERROR: __VERIFIER_error();(void)0; } }

/*
#define MONITOR_EQ(x,y) \
{ \
  while(1) \
  {\
    __VERIFIER_atomic_acquire();\
    assert(g0==g1);\
    __VERIFIER_atomic_release();\
  }\
}
*/
#define MONITOR_EQ(x,y) \
{ \
    __VERIFIER_atomic_acquire();\
    assert(g0==g1);\
    __VERIFIER_atomic_release();\
}

int g0 = 0,g1 = 0,x = 0;
_Bool lock = 0;
int mutex = 0;

int __VERIFIER_nondet_int() {
  return 7;
}

void __VERIFIER_atomic_acquire()
{
	//assume(mutex==0);
  if (mutex != 0) return;
	mutex = 1;
}

void __VERIFIER_atomic_release()
{
	//assume(mutex==1);
	if (mutex != 1) return;
	mutex = 0;
}

void* thr3(void* arg)
{

  __VERIFIER_atomic_acquire();
    g0=0;
    g1=0;
    assert(g0 == g1);
  __VERIFIER_atomic_release();
  
  return 0;
}

void* thr2(void* arg)
{
  MONITOR_EQ(g0,g1);
  return 0;
}

void* thr1(void* arg)
{
  __VERIFIER_atomic_acquire();
  g0=1,g1=1;
  __VERIFIER_atomic_release();

  return 0;
}

int main() {
  pthread_t t1,t2,t3;

  pthread_create(&t1, 0, thr1, 0);
  pthread_create(&t2, 0, thr2, 0);
  pthread_create(&t3, 0, thr3, 0);

  pthread_join(t1, 0);
  pthread_join(t2, 0);
  pthread_join(t3, 0);
}

