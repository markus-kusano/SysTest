/*
 * Debuging macros
 */
#pragma once


// Debug message macro
#ifdef MK_DEBUG
#include <iostream>
#define DEBUG_MSG(str)  do { std::cerr << str; } while (false)
#define DEBUG_PAUSE()   do { std::cerr << "[DEBUG] press enter...\n"; \
                          std::cin.ignore(); } while (false)
#else
#define DEBUG_MSG(str)  do { } while (false)
#define DEBUG_PAUSE()   do { } while (false)
#endif

