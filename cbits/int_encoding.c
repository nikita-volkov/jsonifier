/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 */

#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


uint8_t* poke_int64_in_reverse
(
  uint8_t* dst,
  int64_t val
)
{
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

  return dst;
}

int write_int64_in_reverse
(
  uint8_t* dst,
  int64_t val
)
{
  return dst - poke_int64_in_reverse(dst, val);
}

int dec_allocation_of_int64(int64_t x)
{
  return
    x < 0
      ? (
        x < -9999999999
          ? (
            x < -99999999999999
              ? (
                x < -9999999999999999
                  ? (
                    x < -99999999999999999
                      ? (
                        x < -999999999999999999
                          ? 20
                          : 19
                        )
                      : 18
                    )
                  : x < -999999999999999
                      ? 17
                      : 16
                )
              : x < -999999999999
                  ? (
                    x < -9999999999999
                      ? 15
                      : 14
                    )
                  : x < -99999999999
                      ? 13
                      : 12
            )
          : x < -99999
              ? (
                x < -9999999
                  ? (
                    x < -99999999
                      ? (
                        x < -999999999
                          ? 11
                          : 10
                        )
                      : 9
                    )
                  : x < -999999
                    ? 8
                    : 7
                )
              : x < -99
                  ? (
                    x < -999
                      ? (
                        x < -9999
                          ? 6
                          : 5
                        )
                      : 4
                    )
                  : x < -9
                      ? 3
                      : 2
        )
      : x > 9999999999
          ? (
            x > 99999999999999
              ? (
                x > 9999999999999999
                  ? (
                    x > 99999999999999999
                      ? (
                        x > 999999999999999999
                          ? 19
                          : 18
                        )
                      : 17
                    )
                  : x > 999999999999999
                      ? 16
                      : 15
                )
              : x > 999999999999
                  ? (
                    x > 9999999999999
                      ? 14
                      : 13
                    )
                  : x > 99999999999
                      ? 12
                      : 11
            )
          : x > 99999
              ? (
                x > 9999999
                  ? (
                    x > 99999999
                      ? (
                        x > 999999999
                          ? 10
                          : 9
                        )
                      : 8
                    )
                  : x > 999999
                    ? 7
                    : 6
                )
              : x > 99
                  ? (
                    x > 999
                      ? (
                        x > 9999
                          ? 5
                          : 4
                        )
                      : 3
                    )
                  : x > 9
                      ? 2
                      : 1;
}
