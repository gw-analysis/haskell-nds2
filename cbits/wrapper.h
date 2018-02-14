#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus

#include <nds_connection.hh>
extern "C" {
using Connection = NDS::connection;

#else // __cplusplus

#include <stdbool.h>

// Opaque NDSConnection struct (C++ class).
typedef struct _Connection Connection;

#endif // __cplusplus

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
 } Channel;

typedef struct {
    char* channelGlob;
    channel_type channelTypeMask;
    data_type dataTypeMask;
    float minSampleRate;
    float maxSampleRate;
} ChannelFilter;

// Error message must be allocated to be >= 255 characters long.
#define ERRBUF_LENGTH 255

Connection* hsnds2_connect(const char* hostname, int port, protocol_type protocol, char* errbuf);

void hsnds2_disconnect(Connection* conn);

void hsnds2_destroy(Connection* conn);

// C allocates list of channels; caller responsible for fereing them, by calling freeChannels.
int hsnds2_find_channels(Connection* conn, const ChannelFilter* filter, Channel** channels, char* errbuf);

void hsnds2_free_channels(Channel* channels);

// Caller is responsible for allocating the pointer array for buffers, not buffers themselves
int hsnds2_fetch(Connection* conn, int64_t startGpsTime, int64_t endGpsTime, const char** channelList, size_t nChannels, double** buffers, char* errbuf);

bool hsnds2_set_parameter(Connection* conn, const char* param, const char* value);

char* hsnds2_get_parameter(Connection* conn, const char* param);

#ifdef __cplusplus
}
#endif

#endif // _WRAPPER_H_
