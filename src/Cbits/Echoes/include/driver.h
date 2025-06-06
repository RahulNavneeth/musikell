#ifndef DRIVER_H
#define DRIVER_H

#include <stdbool.h>
#include <stdio.h>

/* TODO : get from arg */
#define SAMPLE_RATE 48000
#define GET_PARENT_IMPL(type, base_driver) ((type *)base_driver)

typedef struct {
  float *sample;
  size_t sample_count;
  size_t current_position;
  bool completed;
} sample;

typedef struct {
  sample *instances;
  int max_instances;
  int count;
} audio_mixer;

typedef struct DriverIMPL {
  int (*write)(struct DriverIMPL *, sample);
  char *(*get_driver_name)(struct DriverIMPL *);
  void (*sound_check)();
  void (*dispose)(struct DriverIMPL *);
  /* other functions  */
} DriverIMPL;

#endif /* DRIVER_H */
