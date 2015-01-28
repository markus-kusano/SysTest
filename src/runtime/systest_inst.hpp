/**
 * Abstract base class for runtime functions to instrument concurrency related
 * statements.
 *
 * For ease of sending data over the socket, all addresses are 64 bits.
 *
 * For pthread instrumentation calls, the systest_* functions will replace the
 * pthread calls (e.g., all pthread_join() calls will be replaced with
 * systest_pthread_join())
 */

class SystestInst {
  public:
    // The address and instruction ID of each load is recorded. Inserted before
    // each load statement
    virtual void systest_load_pre(uint64_t addr, uint64_t instid) = 0;

    // For a store, save the address and instruction ID
    virtual void systest_store_pre(uint64_t addr, uint64_t instid) = 0;

    // Instrumentation function for pthread_create.
    virtual int systest_pthread_create(pthread_t *thread, const pthread_attr_t *attr, 
        void *(*func)(void*), void *arg) = 0;

    // Instrumentation function for pthread_join
    virtual int systest_pthread_join(pthread_t thread, void **value_ptr) = 0;

    // Instrumentation function for pthread_exit
    virtual int systest_thread_exit(void *ret) = 0;

    // pthread_mutex instrumentation functions
    virtual int systest_pthread_mutex_init(pthread_mutex_t * mutex, 
        const pthread_mutexattr_t *attr) = 0;
    virtual int systest_pthread_mutex_destroy(pthread_mutex_t *mutex) = 0;
    virtual int systest_pthread_mutex_lock(pthread_mutex_t *mutex) = 0;
    virtual int systest_pthread_mutex_unlock(pthread_mutex_t *mutex) = 0;
    virtual int  systest_pthread_mutex_trylock(pthread_mutex_t *mutex) = 0
    virtual int  systest_pthread_mutex_timedlock(pthread_mutex_t *mutex, 
        const struct timespec *abstime) = 0;

    // pthread_cond instrumentation functions
    virtual int systest_pthread_cond_init(pthread_cond_t *cond, const pthread_condattr_t *attr) = 0;
    virtual int systest_pthread_cond_destroy(pthread_cond_t *cond) = 0;
    virtual int systest_pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex) = 0;
    virtual int systest_pthread_cond_broadcast(pthread_cond_t *cond) = 0;
    virtual int systest_pthread_cond_signal(pthread_cond_t *cond) = 0;
    virtual int systest_pthread_cond_timedwait(pthread_cond_t *cond, pthread_mutex_t *mutex, 
        const struct timespec *abstime) = 0;

    // reader-writer lock instrumentation functions
    virtual int systest_pthread_rwlock_init(pthread_rwlock_t *rwlock, 
        const pthread_rwlockattr_t *attr) = 0;
    virtual int systest_pthread_rwlock_destroy(pthread_rwlock_t *rwlock) = 0;
    virtual int systest_pthread_rwlock_rdlock(pthread_rwlock_t *rwlock) = 0;
    virtual int systest_pthread_rwlock_tryrdlock(pthread_rwlock_t *rwlock) = 0;
    virtual int systest_pthread_rwlock_timedrdlock(pthread_rwlock_t *rwlock, 
        const struct timespec *abstime) = 0;
    virtual int systest_pthread_rwlock_wrlock(pthread_rwlock_t *rwlock) = 0;
    virtual int systest_pthread_rwlock_trywrlock(pthread_rwlock_t *rwlock) = 0;
    virtual int systest_pthread_rwlock_timedwrlock(pthread_rwlock_t *rwlock, 
        const struct timespec *abstime) = 0;
    virtual int systest_pthread_rwlock_unlock(pthread_rwlock_t *rwlock) = 0;
}
