#include <stdio.h>
#include <stdlib.h>

#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_system.h"
#include "freertos/event_groups.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/unistd.h>
#include <esp_timer.h>


extern EventGroupHandle_t mirage_event_group;
void wifi_set_event_group(EventGroupHandle_t event_group, int offset);

CAMLprim value
ml_register_wifi_events(value unit) {
    CAMLparam0();

    wifi_set_event_group(mirage_event_group, 0);

    CAMLreturn(Val_unit);
}