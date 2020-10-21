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
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>


static const char* digits = "0123456789abcdef";

#define slash_slash_seq_def '\\' | '\\' << 8
#define slash_doublequote_seq_def '\\' | '"' << 8
#define slash_n_seq_def '\\' | 'n' << 8
#define slash_r_seq_def '\\' | 'r' << 8
#define slash_t_seq_def '\\' | 't' << 8
#define slash_u_seq_def '\\' | 'u' << 8

static const uint16_t slash_u_seq = slash_u_seq_def;

static const bool pass_through_by_septet[128] =
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

static const uint16_t two_byte_seq_by_septet[128] =
  {0,0,0,0,0,0,0,0,0,slash_t_seq_def,slash_n_seq_def,0,0,slash_r_seq_def,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,slash_doublequote_seq_def,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,slash_slash_seq_def};

uint8_t* encode_utf16_as_string
(
  uint8_t *dest,
  const uint16_t *src,
  size_t src_offset,
  size_t src_length
)
{

  src += src_offset;
  
  const uint16_t *src_end = src + src_length;

  *dest++ = 34;

  while (src < src_end) {
    uint16_t x = *src++;

    if (x <= 0x7F) {
      if (pass_through_by_septet[x]) {
        *dest++ = x;
      } else {
        uint16_t two_byte_seq = two_byte_seq_by_septet[x];
        if (two_byte_seq) {
          *((uint16_t*) dest) = two_byte_seq;
          dest += 2;
        } else {
          // \u
          *((uint16_t*) dest) = slash_u_seq;

          // hex encoding of 4 nibbles
          *(dest + 2) = digits[x >> 12 & 0xF];
          *(dest + 3) = digits[x >> 8 & 0xF];
          *(dest + 4) = digits[x >> 4 & 0xF];
          *(dest + 5) = digits[x & 0xF];
          dest += 6;
        }
      }
    }
    else if (x <= 0x7FF) {
      *((uint16_t*) dest) = (x >> 6 | x << 8) & 0x3f3f | 0x80C0;
      dest += 2;
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
