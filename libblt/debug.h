#pragma once

#include <iostream>

#ifdef DEBUG
#  define TRACE(x) do { std::cerr << x << std::endl; } while (0)
#  define TRACE2(x) do { std::cerr << #x << x <<< std::endl; } while (0)
#else
#  define TRACE(x)
#  define TRACE2(x)
#endif

#ifdef PROGRESS
#  define PROG(x) do { std::cout << x; std::cout.flush(); } while (0)
#else
#  define PROG(x)
#endif
