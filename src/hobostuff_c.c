#include "hobostuff_c.h"
#include <inttypes.h>

void c_64_bitwise_not(char* p) {
  uint64_t* a = (uint64_t*) p;
  *a = ~(*a);
}
