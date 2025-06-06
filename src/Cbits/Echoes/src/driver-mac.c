#include "driver-mac.h"
#include "AudioToolBox/AudioToolbox.h"
#include "driver.h"
#include <AudioToolbox/AudioServices.h>
#include <AudioToolbox/MusicPlayer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

OSStatus _render_tone(void *inRefCon, AudioUnitRenderActionFlags *ioActionFlags,
                      const AudioTimeStamp *inTimeStamp, UInt32 inBusNumber,
                      UInt32 inNumberFrames, AudioBufferList *ioData) {

  audio_mixer *mixer = (audio_mixer *)inRefCon;
  Float32 *buffer = (Float32 *)ioData->mBuffers[0].mData;

  if (!buffer)
    return noErr;

  memset(buffer, 0, inNumberFrames * sizeof(Float32));

  for (int i = 0; i < mixer->count; i++) {
    sample *instance = &mixer->instances[i];

    if (instance->completed)
      continue;

    for (UInt32 j = 0; j < inNumberFrames; j++) {
      if (instance->current_position < instance->sample_count) {
        buffer[j] += instance->sample[instance->current_position++] * 0.7f;
        if (buffer[j] > 1.0f)
          buffer[j] = 1.0f;
        if (buffer[j] < -1.0f)
          buffer[j] = -1.0f;
      } else {
        instance->completed = 1;
        break;
      }
    }
  }

  return noErr;
}

void _init_audio_mixer(DriverMacIMPL *driver, int max_instances) {
  driver->mixer.max_instances = max_instances;
  driver->mixer.count = 0;
  driver->mixer.instances = (sample *)calloc(max_instances, sizeof(sample));
}

int add_sound_to_mixer(audio_mixer *mixer, sample sample_data) {
  int slot = -1;

  for (int i = 0; i < mixer->count; i++) {
    if (mixer->instances[i].completed) {
      slot = i;
      break;
    }
  }

  if (slot == -1) {
    if (mixer->count < mixer->max_instances) {
      slot = mixer->count++;
    } else {
      slot = 0;
      printf("Warning: Maximum concurrent sounds reached, replacing oldest "
             "sound\n");
    }
  }

  sample *instance = &mixer->instances[slot];

  if (instance->sample) {
    free(instance->sample);
  }

  instance->sample =
      (Float32 *)malloc(sample_data.sample_count * sizeof(Float32));
  if (!instance->sample) {
    printf("Error: Failed to allocate memory for sample buffer\n");
    return -1;
  }

  memcpy(instance->sample, sample_data.sample,
         sample_data.sample_count * sizeof(Float32));
  instance->sample_count = sample_data.sample_count;
  instance->current_position = 0;
  instance->completed = 0;

  return slot;
}

int write_sound_mac_impl(DriverIMPL *base, sample sample_data) {
  DriverMacIMPL *parent_mac_impl = GET_PARENT_IMPL(DriverMacIMPL, base);

  if (!parent_mac_impl->audio_initialized) {
    parent_mac_impl->OS_status =
        AudioUnitInitialize(parent_mac_impl->audio_unit);
    if (parent_mac_impl->OS_status != noErr) {
      printf("Err: Failed to initialize audio unit: %d\n",
             (int)parent_mac_impl->OS_status);
      return 1;
    }

    _init_audio_mixer(parent_mac_impl, 16);

    AURenderCallbackStruct callback_struct = {
        .inputProc = _render_tone, .inputProcRefCon = &parent_mac_impl->mixer};

    parent_mac_impl->OS_status = AudioUnitSetProperty(
        parent_mac_impl->audio_unit, kAudioUnitProperty_SetRenderCallback,
        kAudioUnitScope_Global, 0, &callback_struct, sizeof(callback_struct));

    if (parent_mac_impl->OS_status != noErr) {
      printf("Err: Failed to set render callback: %d\n",
             (int)parent_mac_impl->OS_status);
      return 1;
    }

    parent_mac_impl->OS_status =
        AudioOutputUnitStart(parent_mac_impl->audio_unit);
    if (parent_mac_impl->OS_status != noErr) {
      printf("Err: Failed to start audio unit: %d\n",
             (int)parent_mac_impl->OS_status);
      return 1;
    }

    parent_mac_impl->audio_initialized = 1;
  }

  int sound_slot = add_sound_to_mixer(&parent_mac_impl->mixer, sample_data);
  if (sound_slot < 0) {
    return 1;
  }

  printf("Playing sound in slot %d\n", sound_slot);
  return 0;
}

void dispose_mac_impl(DriverIMPL *base) {
  DriverMacIMPL *parent_mac_impl = GET_PARENT_IMPL(DriverMacIMPL, base);

  AudioOutputUnitStop(parent_mac_impl->audio_unit);
  AudioUnitUninitialize(parent_mac_impl->audio_unit);
  AudioComponentInstanceDispose(parent_mac_impl->audio_unit);

  for (int i = 0; i < parent_mac_impl->mixer.count; i++) {
    if (parent_mac_impl->mixer.instances[i].sample) {
      free(parent_mac_impl->mixer.instances[i].sample);
    }
  }

  if (parent_mac_impl->mixer.instances) {
    free(parent_mac_impl->mixer.instances);
  }
}

char *get_driver_name_mac_impl(DriverIMPL *base) {
  return GET_PARENT_IMPL(DriverMacIMPL, base)->driver_name;
}

AudioComponentDescription _get_default_audio_component_description_mac_impl() {
  return (AudioComponentDescription){
      .componentType = kAudioUnitType_Output,
      .componentSubType = kAudioUnitSubType_DefaultOutput,
      .componentManufacturer = kAudioUnitManufacturer_Apple,
      .componentFlags = 0,
      .componentFlagsMask = 0};
}

AudioStreamBasicDescription _get_default_audio_format_output_stream_mac_impl() {
  AudioStreamBasicDescription audio_format_output_stream = {0};
  audio_format_output_stream.mSampleRate = SAMPLE_RATE;
  audio_format_output_stream.mFormatID = kAudioFormatLinearPCM;
  audio_format_output_stream.mFormatFlags = kAudioFormatFlagsNativeFloatPacked;
  audio_format_output_stream.mBitsPerChannel = 32;
  audio_format_output_stream.mChannelsPerFrame = 1;
  audio_format_output_stream.mFramesPerPacket = 1;
  audio_format_output_stream.mBytesPerFrame =
      sizeof(Float32) * audio_format_output_stream.mChannelsPerFrame;
  audio_format_output_stream.mBytesPerPacket =
      audio_format_output_stream.mBytesPerFrame *
      audio_format_output_stream.mFramesPerPacket;
  return audio_format_output_stream;
}

AudioComponent _get_default_audio_component_mac_impl(
    AudioComponentDescription audio_component_description) {

  AudioComponent component =
      AudioComponentFindNext(NULL, &audio_component_description);
  if (component == NULL) {
    printf("Err: Failed to find output component\n");
    return NULL;
  }

  return component;
}

void sound_check_mac_impl() {
  for (int id = 0; id < 100000; id++) {
    AudioServicesPlaySystemSoundWithCompletion(id, ^{
      AudioServicesDisposeSystemSoundID(id);
    });
  }
}

DriverIMPL *init_driver_mac_impl(char *driver_name) {
  DriverMacIMPL *new_mac_driver =
      (DriverMacIMPL *)malloc(sizeof(DriverMacIMPL));
  if (new_mac_driver == NULL) {
    printf("Err: Memory allocation failed\n");
    return NULL;
  }

  new_mac_driver->driver_name = driver_name;
  new_mac_driver->base.get_driver_name = get_driver_name_mac_impl;
  new_mac_driver->base.sound_check = sound_check_mac_impl;
  new_mac_driver->base.dispose = dispose_mac_impl;
  new_mac_driver->audio_initialized = 0;

  /* Init audio component */
  AudioComponent audio_component = _get_default_audio_component_mac_impl(
      _get_default_audio_component_description_mac_impl());

  if (!audio_component) {
    free(new_mac_driver);
    return NULL;
  }

  new_mac_driver->OS_status =
      AudioComponentInstanceNew(audio_component, &new_mac_driver->audio_unit);
  if (new_mac_driver->OS_status != noErr) {
    printf("Err: Failed to create audio unit\n");
    free(new_mac_driver);
    return NULL;
  }

  AudioStreamBasicDescription audio_format =
      _get_default_audio_format_output_stream_mac_impl();
  new_mac_driver->OS_status = AudioUnitSetProperty(
      new_mac_driver->audio_unit, kAudioUnitProperty_StreamFormat,
      kAudioUnitScope_Input, 0, &audio_format, sizeof(audio_format));

  if (new_mac_driver->OS_status != noErr) {
    printf("Err: Failed to set audio unit format: %d\n",
           (int)new_mac_driver->OS_status);
    AudioComponentInstanceDispose(new_mac_driver->audio_unit);
    free(new_mac_driver);
    return NULL;
  }

  new_mac_driver->base.write = write_sound_mac_impl;
  return (DriverIMPL *)new_mac_driver;
}
