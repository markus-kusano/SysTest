#include "exit_tracker.hpp"
#include <cassert>


ExitTracker::ExitTracker() {
  pthread_mutex_init(&m_, NULL);
}

// Write operation to internal map: potentially will insert the tid to the
// map if it is not there already
void ExitTracker::started(long tid) {
  pthread_mutex_lock(&m_);
  exit_map_[tid] = false; // not exited
  pthread_mutex_unlock(&m_);
}

// Mark the passed thread has exited
void ExitTracker::exited(long tid) {
  pthread_mutex_lock(&m_);
  exit_map_[tid] = true; // exited
  pthread_mutex_unlock(&m_);
}

// Read operation: the thread should already be in the map. That is, it is
// required to have called thread_started() on the passed thread before
// calling this.
bool ExitTracker::has_exited(long tid) {
  bool ret;

  pthread_mutex_lock(&m_);

  ExitTracker::map::iterator it = exit_map_.find(tid);
  assert(it != exit_map_.end() && "thread not found");
  ret = it->second;

  pthread_mutex_unlock(&m_);

  return ret;
}
