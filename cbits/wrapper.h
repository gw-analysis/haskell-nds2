#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus

#include <nds_connection.hh>
extern "C" {
using connection = NDS::connection;

#else // __cplusplus

// Opaque NDSconnection struct (C++ class).
typedef struct _connection connection;

#endif // __cplusplus

typedef int32_t port_t;
typedef int64_t gps_time_t;

typedef enum {
      CHANNEL_TYPE_UNKNOWN = 0,	///< Unknown
      CHANNEL_TYPE_ONLINE = (1 << 0), ///< Online channel
      CHANNEL_TYPE_RAW = (1 << 1),		  ///< Raw channel
      CHANNEL_TYPE_RDS = (1 << 2),		  ///< Reduced data set
      CHANNEL_TYPE_STREND = (1 << 3), ///< Second trend
      CHANNEL_TYPE_MTREND = (1 << 4), ///< Minute trend
      CHANNEL_TYPE_TEST_POINT = (1 << 5), ///< Test point data
      CHANNEL_TYPE_STATIC = (1 << 6)  ///< Static data
} channel_type;

typedef enum {
      PROTOCOL_INVALID = -1,  ///< Unknown or invalid connection
      PROTOCOL_ONE = 1,       ///< Connect with NDS1 protocol
      PROTOCOL_TWO = 2,       ///< Connect with NDS2 protocol
      PROTOCOL_TRY = 3        ///< Autodetect server protocol
} protocol_type;

typedef enum {
      DATA_TYPE_UNKNOWN = 0, ///< Unkown
      DATA_TYPE_INT16 = (1 << 0),	///< 16 bit signed integer
      DATA_TYPE_INT32 = (1 << 1),	///< 32 bit signed integer
      DATA_TYPE_INT64 = (1 << 2),	///< 64 bit signed integer
      DATA_TYPE_FLOAT32 = (1 << 3), ///< 32 bit float value
      DATA_TYPE_FLOAT64 = (1 << 4), ///< 64 bit float value
      DATA_TYPE_COMPLEX32 = (1 << 5), ///< Complex value, two 32 bit floats
      DATA_TYPE_UINT32 = (1 << 6)	///< 32 bit unsigned integer value
} data_type;

typedef struct {
    char* name;
    channel_type type;
    data_type dataType;
    float sampleRate;
    float gain;
    float slope;
    float offset;
    char* units;
 } channel;

// Error message must be allocated to be >= 255 characters long.
#define ERRBUF_LENGTH 255

connection* hsnds2_connect(const char* hostname,
                           port_t port,
                           protocol_type protocol,
                           char* errbuf);

void hsnds2_disconnect(connection* conn);

void hsnds2_destroy(connection* conn);

// C allocates list of channels; caller responsible for fereing them, by calling freeChannels.
// Returns number of channels found.
int hsnds2_find_channels(connection* conn,
                         const char* channelGlob,
                         channel** channels,
                         char* errbuf);

void hsnds2_free_channels(channel* channels);

// Caller is responsible for allocating the pointer array for buffers, not buffers themselves
int hsnds2_fetch(connection* conn,
                 gps_time_t startgps_time,
                 gps_time_t endgps_time,
                 const char* channel_list[],
                 size_t num_channels,
                 double* buffers[],
                 size_t buffer_lengths[],
                 char* errbuf);

// Free a single buffer allocated by hsnds2_fetch().
void hsnds2_free_buffer(double* buffer);

// Returns a bool (0/1) indicating failure or success.
int hsnds2_set_parameter(connection* conn,
                         const char* param,
                         const char* value,
                         char* errbuf);

char* hsnds2_get_parameter(connection* conn,
                           const char* param,
                           char* errbuf);

#ifdef __cplusplus
}
#endif

#endif // _WRAPPER_H_
