/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 */


#include <string.h>
#include <stdint.h>
#include <stdio.h>


static const int allocation_by_septet[128] =
  {6,6,6,6,6,6,6,6,6,2,2,6,6,2,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

// UTF-16

int measure_utf16_text_
(
  const uint16_t *src_ptr,
  const uint16_t *end_ptr
)
{
  size_t allocation = 0;

  while (src_ptr < end_ptr) {
    uint16_t w = *src_ptr;

    if (w <= 0x7F) {
      allocation += allocation_by_septet[w];
      src_ptr += 1;
    }
    else if (w <= 0x7FF) {
      allocation += 2;
      src_ptr += 1;
    }
    else if (w < 0xD800 || w > 0xDBFF) {
      allocation += 3;
      src_ptr += 1;
    } else {
      allocation += 4;
      src_ptr += 2;
    }
  }

  return allocation;
}

int measure_utf16_text_off_len
(
  const uint16_t *src_ptr,
  size_t src_off,
  size_t src_len
)
{
  src_ptr += src_off;
  const uint16_t *end_ptr = src_ptr + src_len;

  return measure_utf16_text_(src_ptr, end_ptr);
}

// UTF-8

int measure_utf8_text_
(
  const uint8_t *src_ptr,
  const uint8_t *end_ptr
)
{
  size_t allocation = 0;

  while (src_ptr < end_ptr) {
    uint8_t x = *src_ptr;

    if (x < 0x80) {
      allocation += allocation_by_septet[x];
      src_ptr += 1;
    } else if (x < 0xE0) {
      allocation += 2;
      src_ptr += 2;
    } else if (x < 0xF0) {
      allocation += 3;
      src_ptr += 3;
    } else {
      allocation += 4;
      src_ptr += 4;
    }
  }

  return allocation;
}

int measure_utf8_text_off_len
(
  const uint8_t *src_ptr,
  size_t src_off,
  size_t src_len
)
{
  src_ptr += src_off;
  const uint8_t *end_ptr = src_ptr + src_len;

  return measure_utf8_text_(src_ptr, end_ptr);
}
