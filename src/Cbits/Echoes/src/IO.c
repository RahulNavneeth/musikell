#include <IO.h>

#include <math.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __APPLE__
#include "driver-mac.h"
#elif defined(__unix__)
#include "driver-linux.h"
#elif defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
#include "driver-windows.h"
#endif

static DriverIMPL *driver_impl = NULL;

int testI() { return 420; }

int init_audio_driver(char *driver_name) {
  if (driver_impl != NULL) {
    return 0;
  }

#ifdef __APPLE__
  driver_impl = init_driver_mac_impl(driver_name);
#elif defined(__unix__)
  driver_impl = init_driver_linux_impl(driver_name);
#elif defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
  driver_impl = init_driver_window_impl(driver_name);
#endif

  if (driver_impl == NULL) {
    return 1;
  }
  return 0;
}

int write_audio(sample sample_data) {
  return driver_impl->write(driver_impl, sample_data);
}

sample generate_test_tone(double frequency, double duration, float amplitude) {
  sample tone_sample;
  tone_sample.sample_count = (size_t)(SAMPLE_RATE * duration);
  tone_sample.sample =
      (float *)malloc(tone_sample.sample_count * sizeof(float));
  tone_sample.current_position = 0;
  tone_sample.completed = 0;

  if (tone_sample.sample == NULL) {
    tone_sample.sample_count = 0;
    return tone_sample;
  }

  double theta = 2.0 * M_PI * frequency / SAMPLE_RATE;
  for (size_t i = 0; i < tone_sample.sample_count; i++) {
    tone_sample.sample[i] = amplitude * sin(theta * i);
  }

  return tone_sample;
}

void cleanup_audio_driver(void) {
  if (driver_impl != NULL) {
    driver_impl->dispose(driver_impl);
    free(driver_impl);
    driver_impl = NULL;
  }
}

DriverIMPL *get_driver_impl() { return driver_impl; }
