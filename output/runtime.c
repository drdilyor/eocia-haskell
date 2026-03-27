#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

void print_int(int64_t x) {
  if (!printf("%" PRId64 "\n", x)) {
    fprintf(stderr, "print_int: error: failed to write output: %s\n", strerror(errno));
    exit(1);
  }
}
int input_int() {
  int64_t x;
  if (!scanf("%" SCNd64, &x)) {
    fprintf(stderr, "input_int: error: failed to read input: %s\n", strerror(errno));
    exit(1);
  }
  return x;
}
