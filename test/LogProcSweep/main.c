// Copyright 2011 The University of Michigan
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Authors - Jie Yu (jieyu@umich.edu)

/**
 * \file
 * \author Jie Yu (jieyu@umich.edu)
 * \author Markus Kusano (mukusano@vt.edu)
 *
 * Source: https://github.com/jieyu/maple
 *
 * Example program provided with Maple (see source) exemplifying an atomicity
 * violation.
 *
 * See README.md for more information.
 */
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>

/* Comment out to remove calls to printf() */
//#define DEBUG

/* Comment out to remove calls to sleep() that attempt to force the bug */
//#define FORCE

#ifdef DEBUG
#include <stdio.h>
#endif

#ifdef FORCE
#include <unistd.h>
#endif

/**
 * Typedef for the node struct of the linked list stored in log_type
 */
typedef struct list_node_st list_node;
struct list_node_st {
  const char *log_entry;    /**< Data to be stored in the node */
  list_node *next;  /**< Next pointer for the linked list */
};

/**
 * A linked list of list_nodes representing a log. Keeps track of the number of
 * entries in the list and has a lock to synchronize access
 */
typedef struct {
  list_node log_entry_list; /**< Head node of the list */
  int num_entries;  /**< Number of entries in the list */
  pthread_mutex_t lock;	/**< Lock to synchronize access to the list */
} log_type;

/**
 * Global instance of the log
 */
log_type *shared_log = NULL;

/**
  Lock to provide thread safe calls to malloc
 */
pthread_mutex_t mem_lock;

/**
 * Creates a new list_node. mem_lock is used to provide a threadsafe
 * implementation of malloc(). This might not be necessary on systems that
 * provide a thread safe malloc.
 *
 * \return Returns a newly allocated list_node
 */
list_node *new_log_entry() {
  pthread_mutex_lock(&mem_lock);
  list_node *node = malloc(sizeof(list_node));
  pthread_mutex_unlock(&mem_lock);
  return node;
}

/**
 * Adds a log entry to the log.
 * A new list node is created and filled with the passed entry.
 *
 * \param log log_type where the entry is stored
 * \param entry Entry to be stored in the log
 */
void add_log_entry(log_type *log, const char *entry) {
  assert(log);
  list_node *node = new_log_entry();
  node->log_entry = entry;

  pthread_mutex_lock(&log->lock);

  /* place the node in the head position */
  node->next = log->log_entry_list.next;    
  log->log_entry_list.next = node;

  /* increment num_entries to reflect the change */
  log->num_entries++;
  pthread_mutex_unlock(&log->lock);
}

/**
 * Initializes the log. 
 *
 * \param log log_type to be initialize
 */
void init_log(log_type *log) {
  log->log_entry_list.log_entry = "HEAD";
  log->log_entry_list.next = NULL;
  log->num_entries = 0;
  pthread_mutex_init(&log->lock, NULL);
}

/**
 * If the log is initialized, then add an entry to it. There is no lock
 * protecting the atomicity of checking if the log is allocated and adding the
 * entry.
 *
 * \param entry Value to be added to the log
 */
void logging(const char *entry) {
  if (shared_log) {

#ifdef FORCE
      sleep(1);
#endif

    add_log_entry(shared_log, entry);
  }
}

/**
 * Thread function. Logs the string "LOG" into shared_log
 *
 * \param args Unused
 * \return Returns NULL upon completion
 */
void *t1_main(void *args) {
#ifdef DEBUG
  printf("t1 is logging\n");
#endif

  logging("LOG");

#ifdef DEBUG
  printf("logging done\n");
#endif

  return NULL;
}

/**
 * Thread function. Resets the log (sets it to NULL) and then allocates memory
 * for the log.
 *
 * \param args Unused
 * \return Returns NULL upon completion
 */
void *t2_main(void *args) {
#ifdef DEBUG
  printf("t2 is resetting the log\n");
#endif

  shared_log = NULL;

#ifdef FORCE
  sleep(1);
#endif

  shared_log = malloc(sizeof(log_type));

#ifdef DEBUG
  printf("resetting done\n");
#endif

  return NULL;
}

/**
 * Initializes global mutexes and the log. Spawns one thread in t1_main() and
 * another in t2_main().
 *
 * \param argc Unused
 * \param argv Unused
 * \return Returns 0 upon completion
 */
int main(int argc, char *argv[]) {
  pthread_mutex_init(&mem_lock, NULL);
  shared_log = malloc(sizeof(log_type));
  init_log(shared_log);

  pthread_t *tids = malloc(sizeof(pthread_t) * 2);
  pthread_create(&tids[0], NULL, t1_main, NULL);
  pthread_create(&tids[1], NULL, t2_main, NULL);
  pthread_join(tids[0], NULL);
  pthread_join(tids[1], NULL);

  return 0;
}

