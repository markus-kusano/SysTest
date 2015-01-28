/**
 * Author: Markus Kusano
 *
 * Class to keep track if a thread has already exited. This is used in
 * inspect_clap.cc to stop code instrumentation when a thread has exited.
 *
 * This "solves" the problem of destructors being instrumented after a thread
 * exits. See README.md for more information
 *
 * The TIDs in this class are pthread_t's casted to longs (this is non
 * portable).
 *
 * All operations in this class should be thread safe
 */
#pragma once
#include <pthread.h>
#include <map>


class ExitTracker {
  public:
    ExitTracker();

    // Write operation to internal map: potentially will insert the tid to the
    // map if it is not there already
    void started(long tid);

    // Similar to started, but marks the passed tid as exited. If it does not
    // exist, it will be inserted
    void exited(long tid);

    // Read operation: the thread should already be in the map. That is, it is
    // required to have called thread_started() on the passed thread before
    // calling this.
    bool has_exited(long tid);

    typedef std::map<long, bool> map;

  private:
    // TODO: optimization: this could be a read-write lock
    pthread_mutex_t m_;

    // internal map: maps a tid to a bool that is true if the thread has exited
    // and false if it is still running
    map exit_map_;
};
