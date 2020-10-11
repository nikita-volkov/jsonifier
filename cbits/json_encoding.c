/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 *
 * Portions copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <string.h>
#include <stdint.h>
#include <stdio.h>


static const char* digits = "0123456789abcdef";

uint8_t* _hs_json_lego_encode_string
(
  uint8_t *dest,
  const uint16_t *src,
  size_t src_offset,
  size_t src_length
)
{
  const uint16_t *srcend;

  src += src_offset;
  srcend = src + src_length;

  while (src < srcend) {
    uint16_t x = *src++;

    if (x <= 0x7F) {
      switch (x) {
        case 92:
          *dest++ = 92;
          *dest++ = 92;
          continue;
        case 34:
          *dest++ = 92;
          *dest++ = 34;
          continue;
        case 10:
          *dest++ = 92;
          *dest++ = 110;
          continue;
        case 13:
          *dest++ = 92;
          *dest++ = 114;
          continue;
        case 9:
          *dest++ = 92;
          *dest++ = 116;
          continue;
        default:
          if (x < 32) {

            // \u
            *dest++ = 92;
            *dest++ = 117;

            // hex encoding of 4 nibbles
            *dest++ = digits[x >> 12 & 0xF];
            *dest++ = digits[x >> 8 & 0xF];
            *dest++ = digits[x >> 4 & 0xF];
            *dest++ = digits[x & 0xF];

          } else {
            *dest++ = x;
          }
      }
    }
    else if (x <= 0x7FF) {
      *dest++ = (x >> 6) | 0xC0;
      *dest++ = (x & 0x3f) | 0x80;
    }
    else if (x < 0xD800 || x > 0xDBFF) {
      *dest++ = (x >> 12) | 0xE0;
      *dest++ = ((x >> 6) & 0x3F) | 0x80;
      *dest++ = (x & 0x3F) | 0x80;
    } else {
      uint32_t c =
        ((((uint32_t) x) - 0xD800) << 10) + 
        (((uint32_t) *src++) - 0xDC00) + 0x10000;
      *dest++ = (c >> 18) | 0xF0;
      *dest++ = ((c >> 12) & 0x3F) | 0x80;
      *dest++ = ((c >> 6) & 0x3F) | 0x80;
      *dest++ = (c & 0x3F) | 0x80;
    }
  }

  return dest;
}
