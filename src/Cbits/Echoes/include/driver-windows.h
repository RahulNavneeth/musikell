#ifndef DRIVER_WINDOWS_H
#define DRIVER_WINDOWS_H

#include "driver.h"
#include <stdio.h>

typedef struct {
  DriverIMPL base;
  char *driver_name;
} DriverWindowsIMPL;

DriverIMPL *init_driver_windows_impl(char *);

int write_sound_windows_impl(DriverIMPL *, sample);

#endif /* DRIVER_WINDOWS_H */
