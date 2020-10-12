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

static const uint16_t slash_slash_bytes = '\\' | '\\' << 8;
static const uint16_t slash_doublequote_bytes = '\\' | '"' << 8;
static const uint16_t slash_n_bytes = '\\' | 'n' << 8;
static const uint16_t slash_r_bytes = '\\' | 'r' << 8;
static const uint16_t slash_t_bytes = '\\' | 't' << 8;
static const uint16_t slash_u_bytes = '\\' | 'u' << 8;

static uint16_t two_byte_seq_array[128];

void init(void)
{
  two_byte_seq_array[92] = slash_slash_bytes;
  two_byte_seq_array[34] = slash_doublequote_bytes;
  two_byte_seq_array[10] = slash_n_bytes;
  two_byte_seq_array[13] = slash_r_bytes;
  two_byte_seq_array[9] = slash_t_bytes;
}

uint8_t* _hs_json_lego_encode_string
(
  uint8_t *dest,
  const uint16_t *src,
  size_t src_offset,
  size_t src_length
)
{
  init();

  src += src_offset;
  
  const uint16_t *src_end = src + src_length;

  *dest++ = 34;

  while (src < src_end) {
    uint16_t x = *src++;

    if (x <= 0x7F) {
      uint16_t two_byte_seq = two_byte_seq_array[x];
      if (two_byte_seq) {
        *((uint16_t*) dest) = two_byte_seq;
        dest += 2;
      } else if (x < 32) {
        // \u
        *((uint16_t*) dest) = slash_u_bytes;
        dest += 2;

        // hex encoding of 4 nibbles
        *dest++ = digits[x >> 12 & 0xF];
        *dest++ = digits[x >> 8 & 0xF];
        *dest++ = digits[x >> 4 & 0xF];
        *dest++ = digits[x & 0xF];
      } else {
        *dest++ = x;
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

  *dest++ = 34;

  return dest;
}
