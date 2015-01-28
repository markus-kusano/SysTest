/*
 * Author: Markus Kusano
 *
 * DPOR Instrumentation functions.
 */
#include <pthread.h>
#include <iostream>
#include <cassert>
#include <map>
#include <cstdint>

#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "exit_tracker.hpp"
#include "trans_types.hpp"
#include "socket_msg.hpp"

//#define MK_DEBUG
#include "debug.h"
#define DEBUG_FUNC_ENTRY() do { DEBUG_MSG("[DEBUG] In " << __FUNCTION__ << "()\n"); } while (false)

// Helper functions: these prevent a data race after thread creation. A parent
// will acquire a mutex which the child will block on before the child is
// allowed to execute its first statement. This ensures that after a thread
// creation the parent always executes its next statement first followed by the
// child
static void parentWakeupChild();
static void childWaitForParent();

// Name of the UNIX socket used by the program under test and scheduler
static const char *SOCK_FILE_NAME = "/tmp/systest.sock";

// Value sent by scheduler on permit
static const char PERMIT_VAL = 9;

// If true, then instrumentation is skipped
static bool skip = false;

// This lock protects the global data structures
static pthread_mutex_t systestLock = PTHREAD_MUTEX_INITIALIZER;

// Map from pthread_t to a unique identifier for a thread. Threads are given
// IDs based on the order of their creation
static std::map<pthread_t, uint64_t> tidMap;

// Map from an address to a unique identifier. Addresses are uniquely
// identified based on the order they are accessed
static std::map<uint64_t, uint64_t> objMap;

// Map from an address of a mutex to a unique ID. Mutexes are identified based
// on their initialization order (i.e., when mutex_init() is called).
//
// As a result, static mutex initialization is not currently supported.
static std::map<void *, uint64_t> mutMap;

// Map from an address of a condition variable to a unique ID. Conds are
// identified based on their initialization order (i.e., when cond_init() is
// called).
//
// As a result, static cond initialization is not currently supported.
static std::map<void *, uint64_t> condMap;

// Map from a Thread ID (from tidMap) to a socket file descriptor
static std::map<uint64_t, int> sockMap;

// If a parent spawns a child, it must block the child from executing its frist
// transition. This prevents a datarace on the exeuction of the first statement
// of the child thread and the statement following the thread_create of the
// parent thread
static std::map<uint64_t, pthread_mutex_t *> parentChildLock;
// A child which was just created must block on the mutex in this map. It will
// be allowed to proceed when its parent completes its first execution.
static std::map<uint64_t, pthread_mutex_t *> childParentLock;

// Arguments passed to systest_thread_start
struct thread_start_args {
  // Function the thread should execut
  void *(*func)(void *);
  // Arguments to pass to the function
  void *arg;
  // Lock held by the parent thread. This will be released when the child
  // thread can proceed
  pthread_mutex_t *l;
};

// Data structure to keep track of which threads have exited. Allows for
// pthread_exit() to be used in all threads without the scheduler getting
// confused.
static ExitTracker exit_tracker;

// Called when thread ends. Requests permission from scheduler to proceed.
static void systest_thread_end(void);

// Returns true if the calling thread is the main thread
static bool is_main_thread(void);

// Wait for the permit value to be sent over the passed socket.
//
// The socket is assumed to be setup and ready for communication
static void waitForPermit(int fd);

// Called by main thread on exit
static void systest_main_atexit(void);

// Returns true if the thread was found and removed from tidMap. Returns false
// if it was not found, and nothing was modified.
//
// Acquires systestLock and accesses tidMap
static bool threadRemove(pthread_t tid);

// Removes the passed mutex from mutMap.
// Returns true if the mutex was found and removed. Returns false if it was not
// found and nothing was modified.
//
// Acquires systestLock and potentially modifies mutMap
static bool mutexRemove(void *mutex);

// Gets the ID for the passed mutex.
//
// If the ID is not found, the thread will crash with an assertion violation
//
// Acquires systestLock
static uint64_t getMut(void *mutex);

// Gets the ID for the passed cond.
//
// If the cond is not found, the thread will crash with an assertion violation
//
// Acquires systestLock
static uint64_t getCond(void *cond);

// Adds a socket to the sockMap based on the passed thread ID.
//
// Returns -1 on failure. On success, returns the connected socket file
// descriptor which is ready for send() and recv().
//
// Currently, when -1 is returned then the global var skip is set to true: all
// subsequent communication with the scheduler will be disabled.
//
// Acquires systestLock
static int addSocketFromTid(uint64_t tid);

// Returns the socket fd for the passed thread.
//
// This will crash if the socket has not yet been added to sockMap
//
// Acquires systestLock
static int getThdSocket(uint64_t tid);

// Returns a newly connected socket.
//
// If the socket fails to connect, returns -1 and sets global variable skip to
// true.
//
// If the socket fails in its creation (call to socket()) crashes the program.
static int getNewSocket();

// Gets and adds the thread ID (from tidMap) for the calling thread (i.e., the
// pthread_t from calling pthread_self())
//
// Acquires systestLock
static uint64_t getTIdAndAddSelf();

// Gets and adds the thread ID (from tidMap) of the passed thread.
//
// Acquires systestLock
static uint64_t getTIdAndAdd(pthread_t s);

// Add the passed mutex to the mutex map if it doesnt already exist.
//
// Acquires and releases systestLock. Accesses mutMap
static uint64_t getMutAndAdd(void *mutex) {
  uint64_t ret;
  pthread_mutex_lock(&systestLock);
  auto i = mutMap.find(mutex);
  if (i == mutMap.end()) {
    // Add the mutex to the next availible integer
    ret = mutMap.size();
    mutMap[mutex] = ret;
  }
  else {
    // mutex already found
    ret = i->second;
  }
  pthread_mutex_unlock(&systestLock);
  return ret;
}

// Add the passed condition variable to the mutex map if it doesnt already
// exist.
//
// Acquires and releases systestLock. Accesses condMap.
static uint64_t getCondAndAdd(void *cond) {
  uint64_t ret;
  pthread_mutex_lock(&systestLock);
  auto i = condMap.find(cond);
  if (i == condMap.end()) {
    // Add the cond to the next availible integer
    ret = condMap.size();
    condMap[cond] = ret;
  }
  else {
    // cond already present
    ret = i->second;
  }
  pthread_mutex_unlock(&systestLock);
  return ret;
}


static uint64_t getTIdAndAdd(pthread_t s) {
  pthread_mutex_lock(&systestLock);
  std::map<pthread_t, uint64_t>::iterator i = tidMap.find(s);
  uint64_t ret = 0;
  // If the thread was added, some additional setup tasks need to be performed 
  //bool found = false;
  if (i == tidMap.end()) {
    // Thread not found, add a new ID to the map
    //found = false;
    ret = tidMap.size();
    tidMap[s] = ret;

    // If the thread is the first thread to be added then it is the main
    // thread. We need to setup a cleanup handler for it
		if (tidMap.size() == 1) {
      // mark main thread as started
      exit_tracker.started((long) pthread_self());
      // Setup cleanup for main thread
      atexit(systest_main_atexit);
    }
  }
  else {
    //found = true;
    // Thread already found in map
    ret = i->second;
  }
  pthread_mutex_unlock(&systestLock);

/*
 * Sockets are only created on a per-message basis
  if (found == false) {
    // Setup a socket for the thread
    addSocketFromTid(ret);
  }
*/
  return ret;
}

// Returns the thread ID of the currently running thread (i.e., will return the
// thread ID matching the pthread_t returned by pthread_self.
//
// This method acquires and releases systestLock and accesses tidMap
static uint64_t getTIdAndAddSelf() {
  return getTIdAndAdd(pthread_self());
}

// Returns the unique identifier for the passed address. If it does not exist
// in objMap, then it is added
//
// This method acquires and releases systestLock and accesses objMap
extern "C" uint64_t getObjIdAndAdd(uint64_t addr) {
  pthread_mutex_lock(&systestLock);
  std::map<uint64_t, uint64_t>::iterator i = objMap.find(addr);
  uint64_t ret = 0;
  if (i == objMap.end()) {
    // Give the new object the next unused integer
    ret = objMap.size();
    objMap[addr] = ret;
  }
  else {
    // Object already found in map, so return the value
    ret = i->second;
  }
  pthread_mutex_unlock(&systestLock);
  return ret;
}

void childWaitForParent() {
  uint64_t tid = getTIdAndAddSelf();
  // check if we are a child thread and need to wait for our parent to proceed
  pthread_mutex_lock(&systestLock);
  auto f = childParentLock.find(tid);
  pthread_mutex_unlock(&systestLock);
  if (f != childParentLock.end()) {
    // Block until our parent lets us proceed
    pthread_mutex_lock(f->second);

    // No need to hold the lock to protect anything. This is just a signal
    pthread_mutex_unlock(f->second);

    // Destroy and free the mutex
    pthread_mutex_destroy(f->second);
    free(f->second);

    // Remove the item from the map so we do not block again
    pthread_mutex_lock(&systestLock);
    childParentLock.erase(f);
    pthread_mutex_unlock(&systestLock);
  }
}

void parentWakeupChild() {
  uint64_t tid = getTIdAndAddSelf();
  // check if we are a parent thread who needs to unlock a mutex blocking a
  // child
  pthread_mutex_lock(&systestLock);
  auto f = parentChildLock.find(tid);
  pthread_mutex_unlock(&systestLock);
  if (f != parentChildLock.end()) {
    // unlock the mutex and let the child proceed. The child will free the
    // resources once it wakes up
    pthread_mutex_unlock(f->second);

    // remove the item from the map so we don't come here again and unlock an
    // already unlocked mutex
    pthread_mutex_lock(&systestLock);
    parentChildLock.erase(f);
    pthread_mutex_unlock(&systestLock);
  }
}


// The address and instruction ID of each load is recorded. Inserted before
// each load statement
extern "C" void systest_load_pre(uint64_t addr, uint64_t instid) {
  DEBUG_FUNC_ENTRY();

  SocketMsg msg;
  // If this is the first transition by this  thread it needs to register
  // itself with the thread map.
  msg.tId = getTIdAndAddSelf();

  childWaitForParent();

  // Get a socket for this message
  int fd = getNewSocket();

  if (fd == -1) {
    return;
  }

  // If this is the first transition on this address it needs to be given an ID
  msg.objId = getObjIdAndAdd(addr);
  msg.type = loadPre;
  msg.instId = instid;

  msg.send(fd);

  // request permission to proceed
  waitForPermit(fd);

  parentWakeupChild();

  if (close(fd) == -1) {
    perror("closing socket");
  }
}


// For a store, save the address and instruction ID
extern "C" void systest_store_pre(uint64_t addr, uint64_t instid) {
  DEBUG_FUNC_ENTRY();

  // Send current state over socket
  // If this is the first transition by this  thread it needs to register
  // itself with the thread map.
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  childWaitForParent();

  // Get a socket for this message
  int fd = getNewSocket();

  if (fd == -1) {
    return;
  }

  // If this is the first transition on this address it needs to be given an ID
  msg.objId = getObjIdAndAdd(addr);
  msg.type = storePre;
  msg.instId = instid;

  msg.send(fd);

  // Wait for permission to proceed
  waitForPermit(fd);

  parentWakeupChild();

  if (close(fd) == -1) {
    perror("closing socket");
  }
}

void systest_thread_end_atexit(void *arg) {
	if (skip) {
		return;
  }

  // Mark the thread as exited
  assert(!exit_tracker.has_exited((long) pthread_self()) && "thread exited twice");
  exit_tracker.exited((long) pthread_self());
  // Request permission to exit from scheduler
	systest_thread_end();
}

// Wrapper function around any thread routine created using pthread_create. It
// simply registers a cleanup handler to call the thread_end instrumentation
// function
static void *systest_thread_start(void *arg) {
	getTIdAndAddSelf();

  thread_start_args *tsa = (thread_start_args *)(arg);

	//void **args = (void**) arg;
	//void *start_routine_arg = args[1];

  void *start_routine_arg = tsa->arg;
	void *(*start_routine)(void*) = tsa->func;
  pthread_mutex_t *l = tsa->l;

  if (skip) {
    return start_routine(start_routine_arg);
  }

  // wait until the parent thread allows us to continue
  pthread_mutex_lock(l);

  // destroy the mutex and free it
  pthread_mutex_destroy(l);
  free(l);

  // mark the thread as starting. The thread is marked as exited in the
  // cleanup handler (systest_thread_end_atexit)
  exit_tracker.started((long) pthread_self());

	void* res;
	{
		/// register cleanup routine (for normal or cancelled exit)
		pthread_cleanup_push(systest_thread_end_atexit, (void*)0);
			res = start_routine(start_routine_arg);
			/// 1 means leave the cleanup routine in the system
			pthread_cleanup_pop(1);
	}
	return res;
}

// Instrumentation function for pthread_create.
extern "C" int systest_pthread_create(pthread_t *thread, const pthread_attr_t *attr, 
    void *(*func)(void*), void *arg) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  // Send current state over socket
  // If this is the first transition by this  thread it needs to register
  // itself with the thread map.
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  // Get the socket for this message
  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_create(thread, attr, func, arg);
  }

  // Create the thread in a wrapper function wich will reigster a cleanup
  // handler on exit. This allows us to instrument thread end.
  //
  // Addiontally, pass a mutex to the thread which is locked by the parent
  // thread. This allows for the call to pthread_create to complete (before it
  // is scheduled by the scheduler) giving us the TID of the child.
  //
  // The child will block on the mutex call (which is unseen by the scheduler)
  // until we are given a permit from the scheduler.
  
  // setup child mutex
  pthread_mutex_t *child_mutex = (pthread_mutex_t *)malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(child_mutex, NULL);

  // lock the mutex so it will block the child thread
  pthread_mutex_lock(child_mutex);

  thread_start_args *as = (thread_start_args *)malloc(sizeof(thread_start_args));

  as->func = func;
  as->arg = arg;
  as->l = child_mutex;

	//void **args = (void**) malloc(sizeof(void*) * 4);
	//args[0] = (void*) func;
	//args[1] = arg;
	int res = pthread_create(thread, attr, systest_thread_start, (void*) as);

  // Add the child thread: this is racing with the child's own add of itself
  // but it is OK if the thread is added twice.
  uint64_t cid = getTIdAndAdd(*thread);
  uint64_t pid = getTIdAndAdd(pthread_self());

  // Setup another mutex for the child to block on before its first statement
  pthread_mutex_t *fst = (pthread_mutex_t *)malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(fst, NULL);

  pthread_mutex_lock(&systestLock);
  parentChildLock[pid] = fst;
  childParentLock[cid] = fst;
  pthread_mutex_unlock(&systestLock);

  // Acquire the lock so that the child cannot proceed: this will be released
  // in one of the instrumentation functions (the next one executed by the
  // parent)
  pthread_mutex_lock(fst);

  msg.type = threadCreate;
  msg.objId = cid;
  msg.send(fd);

  // Wait for permission to proceed
  waitForPermit(fd);

  // Unblock the child in the thread_start routine. The child is responsible
  // for destroying and freeing the mutex.
  pthread_mutex_unlock(child_mutex);

  parentWakeupChild();

  if (close(fd) == -1) {
    perror("closing socket");
  }

  return res;
}



// Instrumentation function for pthread_join
extern "C" int systest_pthread_join(pthread_t thread, void **value_ptr) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_join(thread, value_ptr);
  }

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();
  // The object id is the TId of the child
  msg.objId = getTIdAndAdd(thread);
  msg.type = threadJoin;
  msg.send(fd);

  // Wait for permission to proceed
  waitForPermit(fd);

  parentWakeupChild();

  if (close(fd) == -1) {
    perror("closing socket");
  }
  
  bool ret = pthread_join(thread, value_ptr);

  // remove the child from the thread index. The child thread must still be in
  // the thread_to_idx map
  bool childFound = threadRemove(thread);
  assert(childFound && "child thread not found in TID map");

  return ret;
}

// Instrumentation function for pthread_exit
extern "C" void systest_pthread_exit(void *ret) {
  DEBUG_FUNC_ENTRY();
  // the main thread does not have a pthread_cleanup handler so it will
  // not call clap_main_end_atexit when pthread_exit() is called. That
  // function is registered with atexit() which is not called when
  // pthread_exit is called
  if (is_main_thread()) {
    systest_main_atexit();
  }
  return pthread_exit(ret);
}


// pthread_mutex instrumentation functions
extern "C" int systest_pthread_mutex_init(pthread_mutex_t * mutex, 
    const pthread_mutexattr_t *attr) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  getTIdAndAddSelf();

  if (skip) {
    return pthread_mutex_init(mutex, attr);
  }

  getMutAndAdd(mutex);

  // The parent must make it to a scheduling point to wakeup the child
  //parentWakeupChild();

  // Mutex initialization is not controlled by the scheduler
  return pthread_mutex_init(mutex, attr);
}

extern "C" int systest_pthread_mutex_destroy(pthread_mutex_t *mutex) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  // This function can be called by a destructor routine which is not monitored
  // by our tool. Do not attempt to instrument if from our perspective the
  // thread has already exited
	if ((fd == -1) || exit_tracker.has_exited((long) pthread_self())) {
    return pthread_mutex_destroy(mutex);
  }

  msg.mutId = getMut(mutex);
  msg.type = mutDestroy;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);

  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  mutexRemove(mutex);
  return pthread_mutex_destroy(mutex);
}

extern "C" int systest_pthread_mutex_lock(pthread_mutex_t *mutex) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_mutex_lock(mutex);
  }

  msg.mutId = getMut(mutex);
  msg.type = mutLock;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);

  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  return pthread_mutex_lock(mutex);
}

extern "C" int systest_pthread_mutex_unlock(pthread_mutex_t *mutex) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_mutex_unlock(mutex);
  }

  msg.mutId = getMut(mutex);
  msg.type = mutUnlock;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);
  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  return pthread_mutex_unlock(mutex);
}


// pthread_cond instrumentation functions
extern "C" int systest_pthread_cond_init(pthread_cond_t *cond, 
    const pthread_condattr_t *attr) {
  DEBUG_FUNC_ENTRY();
  childWaitForParent();
  getTIdAndAddSelf();

  if (skip) {
    return pthread_cond_init(cond, attr);
  }

  // Generate a new ID for the condition variable
  getCondAndAdd(cond);

  // The parent must wait for a scheduling point to wakeup the child
  //parentWakeupChild();

  // cond init is not monitored by the scheduler
  return pthread_cond_init(cond, attr);
}

extern "C" int systest_pthread_cond_destroy(pthread_cond_t *cond) {
  DEBUG_FUNC_ENTRY();

  childWaitForParent();

  // Send state to scheduler

  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_cond_destroy(cond);
  }

  msg.condId = getCond(cond);
  msg.type = condDestroy;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);

  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  return pthread_cond_destroy(cond);
}

extern "C" int systest_pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex) {
  DEBUG_FUNC_ENTRY();
  childWaitForParent();

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_cond_wait(cond, mutex);
  }

  msg.condId = getCond(cond);
  msg.mutId= getMut(mutex);
  msg.type = condWait;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);
  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  return pthread_cond_wait(cond, mutex);
}

extern "C" int systest_pthread_cond_broadcast(pthread_cond_t *cond) {
  DEBUG_FUNC_ENTRY();
  childWaitForParent();

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_cond_broadcast(cond);
  }

  msg.condId = getCond(cond);
  msg.type = condBroadcast;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);
  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  return pthread_cond_broadcast(cond);
}

extern "C" int systest_pthread_cond_signal(pthread_cond_t *cond) {
  DEBUG_FUNC_ENTRY();
  childWaitForParent();

  // Send state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return pthread_cond_signal(cond);
  }

  msg.condId= getCond(cond);
  msg.type = condSignal;
  msg.send(fd);
  
  // request permission to proceed
  waitForPermit(fd);
  parentWakeupChild();

  // close the socket
  if (close(fd) == -1) {
    perror("closing socket");
  }

  return pthread_cond_signal(cond);
}

extern "C" int  systest_pthread_mutex_trylock(pthread_mutex_t *mutex) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_mutex_trylock(mutex);
  }
  // TODO: Implement this
  assert(0 && "trylock unimplemented");
}

extern "C" int  systest_pthread_mutex_timedlock(pthread_mutex_t *mutex,
    const struct timespec *abstime) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_mutex_timedlock(mutex, abstime);
  }
  // TODO: implement this
  assert(0 && "cond timed lock unimplemented");
}

extern "C" int systest_pthread_cond_timedwait(pthread_cond_t *cond, pthread_mutex_t *mutex, 
    const struct timespec *abstime) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_cond_timedwait(cond, mutex, abstime);
  }
  // TODO: Implement this
  assert(0 && "cond timed wait unimplemented");
}

// reader-writer lock instrumentation functions
extern "C" int systest_pthread_rwlock_init(pthread_rwlock_t *rwlock, 
    const pthread_rwlockattr_t *attr) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_init(rwlock, attr);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C" int systest_pthread_rwlock_destroy(pthread_rwlock_t *rwlock) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_destroy(rwlock);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C" int systest_pthread_rwlock_rdlock(pthread_rwlock_t *rwlock) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_rdlock(rwlock);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}
extern "C" int systest_pthread_rwlock_tryrdlock(pthread_rwlock_t *rwlock) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_tryrdlock(rwlock);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C" int systest_pthread_rwlock_timedrdlock(pthread_rwlock_t *rwlock, 
    const struct timespec *abstime) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_timedrdlock(rwlock, abstime);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}
extern "C" int systest_pthread_rwlock_wrlock(pthread_rwlock_t *rwlock) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_wrlock(rwlock);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C" int systest_pthread_rwlock_trywrlock(pthread_rwlock_t *rwlock) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_trywrlock(rwlock);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C" int systest_pthread_rwlock_timedwrlock(pthread_rwlock_t *rwlock, 
    const struct timespec *abstime) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_timedwrlock(rwlock, abstime);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C" int systest_pthread_rwlock_unlock(pthread_rwlock_t *rwlock) {
  DEBUG_FUNC_ENTRY();
  if (skip) {
    return pthread_rwlock_unlock(rwlock);
  }
  // TODO: Implement this
  assert(0 && "rwlocks are unimplemented");
}

extern "C"  void systest___assert_fail (const char *assert, const char *file,
    unsigned int line, const char *func) {
  DEBUG_FUNC_ENTRY();
  childWaitForParent();

  // Inform scheduler of state
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    __assert_fail(assert, file, line, func);
  }

  msg.type = assertFail;
  msg.send(fd);

  // request permission to proceed
  waitForPermit(fd);
  parentWakeupChild();

  if (close(fd) == -1) {
    perror("closing socket");
  }

  __assert_fail(assert, file, line, func);
}

static void systest_main_atexit(void) {
	if (skip) {
		return;
  }

  // it seems there is a chance that the main thread calls this function
  // twice (perhaps atexit() is firing)
  if (!exit_tracker.has_exited((long) pthread_self())) {
    exit_tracker.exited((long) pthread_self());
    systest_thread_end();
  }
}

static void systest_thread_end(void) {
  DEBUG_FUNC_ENTRY();
  childWaitForParent();
  // Send current state to scheduler
  SocketMsg msg;
  msg.tId = getTIdAndAddSelf();

  int fd = getNewSocket();

  if (fd == -1) {
    return;
  }

  msg.type = threadEnd;
  msg.send(fd);

  // request permission to proceed
  waitForPermit(fd);
  parentWakeupChild();

  if (close(fd) == -1) {
    perror("closing socket");
  }
}


// returns true if the calling thread is the main thread (thread_to_idx 0)
static bool is_main_thread() {
  pthread_t s = pthread_self();
  uint64_t tid;
  pthread_mutex_lock(&systestLock);
  auto i = tidMap.find(s);
  assert(i != tidMap.end() && "thread not found");
  tid = i->second;
  pthread_mutex_unlock(&systestLock);

  if (tid == 0) {
    return true;
  }
  return false;
}


// returns true if the thread was found and removed from tidMap. Returns false
// if it was not found, and nothing was modified.
//
// Acquires systestLock and accesses tidMap
static bool threadRemove(pthread_t tid) {
  bool found;
  pthread_mutex_lock(&systestLock);
  auto i = tidMap.find(tid);
  if (i == tidMap.end()) {
    found = false;
  }
  else {
    found = true;
    tidMap.erase(i);
    assert(tidMap.find(tid) == tidMap.end());
  }
  pthread_mutex_unlock(&systestLock);
  return found;
}

static bool mutexRemove(void *mutex) {
  bool found;
  pthread_mutex_lock(&systestLock);
  auto i = mutMap.find(mutex);
  if (i == mutMap.end()) {
    found = false;
  }
  else {
    found = true;
    mutMap.erase(i);
    assert(mutMap.find(mutex) == mutMap.end());
  }
  pthread_mutex_unlock(&systestLock);
  return found;
}

static uint64_t getMut(void *mutex) {
  uint64_t ret;
  pthread_mutex_lock(&systestLock);
  auto i = mutMap.find(mutex);
  if (i == mutMap.end()) {
    assert(0 && "Mutex not found in map. Used after deletion?");
    exit(EXIT_FAILURE);
  }
  else {
    ret = i->second;
  }
  pthread_mutex_unlock(&systestLock);
  return ret;
}

static uint64_t getCond(void *cond) {
  uint64_t ret;
  pthread_mutex_lock(&systestLock);
  auto i = condMap.find(cond);
  if (i == condMap.end()) {
    assert(0 && "Mutex not found in map. Used after deletion?");
    exit(EXIT_FAILURE);
  }
  else {
    ret = i->second;
  }
  pthread_mutex_unlock(&systestLock);
  return ret;
}

static int addSocketFromTid(uint64_t tid) {

  // Create a new socket fd
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1) {
    perror("socket");
    exit(EXIT_FAILURE);
  }

  // Connect the socket
  struct sockaddr_un address;
  int retval;
  address.sun_family = AF_UNIX;
  strcpy(address.sun_path, SOCK_FILE_NAME);  

  retval = ::connect(fd, (struct sockaddr *)&address, sizeof(address));
  if (retval == -1) {
    std::cerr << "Unable to connect to scheduler, starting free run\n";
    skip = true;
    return -1;
  }
  pthread_mutex_lock(&systestLock);
  assert(sockMap.find(tid) == sockMap.end() && "initializing thread socket twice");
  sockMap[tid] = fd;
  pthread_mutex_unlock(&systestLock);
  assert(fd != -1);
  return fd;
}

// Returns a newly connected(). Returns -1 and sets global variable skip to
// true if the socket could not be connected. This usually indicates that the
// scheduler is not on the other end
static int getNewSocket() {
  // Create a new socket fd
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1) {
    perror("socket");
    exit(EXIT_FAILURE);
  }

  // Connect the socket
  struct sockaddr_un address;
  int retval;
  address.sun_family = AF_UNIX;
  strcpy(address.sun_path, SOCK_FILE_NAME);  

  retval = ::connect(fd, (struct sockaddr *)&address, sizeof(address));
  if (retval == -1) {
    std::cerr << "Unable to connect to scheduler, starting free run\n";
    skip = true;
    return -1;
  }
  assert(fd != -1);
  return fd;
}

// Returns the socket fd for the passed thread.
//
// This will crash if the socket has not yet been added to sockMap
//
// Acquires systestLock
static int getThdSocket(uint64_t tid) {
  int fd = -1;
  pthread_mutex_lock(&systestLock);
  auto i = sockMap.find(tid);
  if (i != sockMap.end()) {
    fd = i->second;
  }
  pthread_mutex_unlock(&systestLock);
  if (fd == -1) {
    std::cerr << "[ERROR] Unable to find socket for thread " << tid << '\n';
    exit(EXIT_FAILURE);
  }

  return fd;

}

static void waitForPermit(int fd) {
  char buf[32];
  int n = ::recv(fd, buf, 100, 0);
  if (n <= 0) {
    perror("waitForPermit(), recv()");
    exit(EXIT_FAILURE);
  }
  assert(n == 1);
  assert(buf[0] == 9);
}
