/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 */

#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


int write_int64_in_reverse
(
  uint8_t* dst,
  int64_t val
)
{
  uint8_t* original_dst = dst;
  
  bool negate = false;

  if (val < 0) {
    val = -val;
    negate = true;
  }

  do
  {
    int64_t b4 = val;
    val /= 10;
    *dst-- = b4 - val * 10 + 48;
  }
  while(val);

  if (negate) {
    *dst-- = '-';
  }

  return original_dst - dst;
}
