/*
 * Author: Markus Kusano
 *
 * Structure defining a message that can be sent over a socket to the
 * scheduler.
 *
 * The default constructed result is a message containing all invalid values.
 *
 * The intention is to create a default value of this structure and then fill
 * in the required fields for a given event type
 */
#pragma once

#include <limits>
#include "trans_types.hpp"
#include <cstdint>

struct SocketMsg {
  // The type of transition about to be executed by the thread
  transTypes type = invalid;
  // Thread ID
  uint64_t tId = std::numeric_limits<uint64_t>::max();
  // ID of the memory object
  uint64_t objId = std::numeric_limits<uint64_t>::max();
  // ID of the mutex
  uint64_t mutId = std::numeric_limits<uint64_t>::max();
  // ID of the condition variable
  uint64_t condId = std::numeric_limits<uint64_t>::max();
  // Instruction ID 
  uint64_t instId = std::numeric_limits<uint64_t>::max();

  // Send the underlying data over the passed socket.
  //
  // On failure, will crash the program
  void send(int fd);
};
