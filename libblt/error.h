#pragma once

/* ERROR is always enabled */
#define ERROR(x) do { std::cerr << "Error [" << __FILE__ << ":"; \
                      std::cerr << __LINE__  << "]: " << x << std::endl; } \
                 while (0)

/* WARN is enabled as long as NWARN is not set */
#ifndef NWARN
#  define WARN(x) do { std::cerr << "Warning: " << x << std::endl; } while (0)
#else
#  define WARN(x)
#endif
