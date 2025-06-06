#ifndef IO_H
#define IO_H

#include "driver.h"

int init_audio_driver(char *);
int write_audio(sample);
void cleanup_audio_driver(void);
DriverIMPL *get_driver_impl();

sample generate_test_tone(double frequency, double duration, float amplitude);

#endif /* IO_H */
