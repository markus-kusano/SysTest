#include "socket_msg.hpp"
#include "debug.h"
extern "C" {
#include <sys/socket.h>
}
#include <cstdio>
#include <cstdlib>

void SocketMsg::send(int fd) {
  // There are 6 uint64_t variables to send
  uint64_t data[6];

  data[0] = type;
  data[1] = objId;
  data[2] = mutId;
  data[3] = condId;
  data[4] = instId;
  data[5] = tId;

  DEBUG_MSG("[DEBUG] Sending socket data of size: " << sizeof(data) << '\n');

  int ret = ::send(fd, data, sizeof(data), 0);

  if (ret == -1) {
    perror("socket send");
    exit(EXIT_FAILURE);
  }
}
