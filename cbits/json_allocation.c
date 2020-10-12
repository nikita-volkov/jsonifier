/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 */


#include <string.h>
#include <stdint.h>
#include <stdio.h>


static const int allocation_by_septet[128] =
  {6,6,6,6,6,6,6,6,6,2,2,6,6,2,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

int count_string_allocation
(
  const uint16_t *src_ptr,
  size_t src_off,
  size_t src_len
)
{
  src_ptr += src_off;
  const uint16_t *end_ptr = src_ptr + src_len;

  size_t allocation = 0;

  while (src_ptr < end_ptr) {
    uint16_t w = *src_ptr++;

    if (w <= 0x7F) {
      allocation += allocation_by_septet[w];
    }
    else if (w <= 0x7FF) {
      allocation += 2;
    }
    else if (w < 0xD800 || w > 0xDBFF) {
      allocation += 3;
    } else {
      src_ptr++;
      allocation += 4;
    }
  }

  return allocation;
}
