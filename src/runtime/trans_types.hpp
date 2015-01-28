/*
 * Author: Markus Kusano
 *
 * Enum representing the different transition types that can be sent to the
 * scheduler
 */
#pragma once

enum transTypes {
  loadPre = 0,
  storePre = 1,

  threadEnd = 2,
  threadCreate = 3,
  threadJoin = 4,

  mutDestroy = 5,
  mutLock = 6,
  mutUnlock = 7,

  condDestroy = 8,
  condWait = 9,
  condBroadcast = 10,
  condSignal = 11,

  assertFail = 12,

  invalid = 999
};
