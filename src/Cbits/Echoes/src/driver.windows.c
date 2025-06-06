#include "driver.h"
#include "driver-windows.h"
#include <stdio.h>
#include <stdlib.h>

int write_sound_windows_impl(DriverIMPL *base, sample sample_data) {
  printf("Sound Written - Windows\n");
  /* implement sound card write function - Windows */
  return 0;
}

DriverIMPL *init_driver_windows_impl(char *driver_name) {
  DriverWindowsIMPL *new_windows_driver =
      (DriverWindowsIMPL *)malloc(sizeof(DriverWindowsIMPL));
  if (new_windows_driver == NULL)
    return NULL;
  new_windows_driver->driver_name = driver_name;
  new_windows_driver->base.write = write_sound_windows_impl;

  return (DriverIMPL *)new_windows_driver;
}
