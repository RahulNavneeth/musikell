#ifndef DRIVER_MAC_H
#define DRIVER_MAC_H

#include "AudioToolBox/AudioToolbox.h"
#include "driver.h"
#include <stdio.h>

typedef struct {
  DriverIMPL base;
  AudioUnit audio_unit;
  OSStatus OS_status;
  char *driver_name;
  audio_mixer mixer;
  int audio_initialized;
} DriverMacIMPL;

DriverIMPL *init_driver_mac_impl(char *);
AudioComponentDescription
get_default_audio_component_description_mac_impl(); /* make this a macro if
                                                       possible */
AudioComponent get_default_audio_component_mac_impl(AudioComponentDescription);
AudioStreamBasicDescription
get_default_audio_format_output_stream_mac_impl(); /* make this a macro if
                                                      possible  */

int write_sound_mac_impl(DriverIMPL *, sample);
char *get_driver_name_mac_impl(DriverIMPL *);
void sound_check_mac_impl();
void dispose_mac_impl(DriverIMPL *);

#endif /* DRIVER_MAC_H */
