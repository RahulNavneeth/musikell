#include "driver-linux.h"
#include "driver.h"
#include <stdio.h>
#include <stdlib.h>

int write_sound_linux_impl(DriverIMPL *base, sample frequency_data) {
  printf("Sound Written - linux\n");
  /* implement sound card write function - linux */
  return 0;
}

DriverIMPL *init_driver_linux_impl(char *driver_name) {
  DriverLinuxIMPL *new_linux_driver =
      (DriverLinuxIMPL *)malloc(sizeof(DriverLinuxIMPL));
  if (new_linux_driver == NULL)
    return NULL;
  new_linux_driver->driver_name = driver_name;
  new_linux_driver->base.write = write_sound_linux_impl;
  return (DriverIMPL *)new_linux_driver;
}
