#ifndef DRIVER_LINUX_H
#define DRIVER_LINUX_H

#include "driver.h"
#include <stdio.h>

typedef struct {
  DriverIMPL base;
  char *driver_name;
} DriverLinuxIMPL;

DriverIMPL *init_driver_linux_impl(char *);

int write_sound_linux_impl(DriverIMPL *, sample);

#endif /* DRIVER_WINDOWS_H */
