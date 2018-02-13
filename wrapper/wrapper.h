#ifndef _WRAPPER_H_
#define _WRAPPER_H_

#include <nds_connection.hh>

typedef struct {
    char* name;
    NDS::channel::channel_type type;
    NDS::channel::data_type dataType;
    float sampleRate;
    float gain;
    float slope;
    float offset;
    char* units;
} Channel;

typedef struct {
    char* channelGlob;
    NDS::channel::channel_type channelTypeMask;
    NDS::channel::data_type dataTypeMask;
    float minSampleRate;
    float maxSampleRate;
} ChannelFilter;

#ifdef __cplusplus
extern "C" {
#endif

// Error message must be allocated to be >= 255 characters long.
#define ERRBUF_LENGTH 255

NDS::connection* connect(const char* hostname, int port, int protocol, char* errbuf);

void disconnect(NDS::connection* conn);

void destroy(NDS::connection* conn);

// C allocates list of channels; caller responsible for fereing them, by calling freeChannels.
int findChannels(NDS::connection* conn, const ChannelFilter* filter, Channel** channels, char* errbuf);

void freeChannels(Channel* channels);

// Caller is responsible for allocating the pointer array for buffers, not buffers themselves
int fetch(NDS::connection* conn, int64_t startGpsTime, int64_t endGpsTime, const char** channelList, size_t nChannels, double** buffers, char* errbuf);

bool setParameter(NDS::connection* conn, const char* param, const char* value);

char* getParameter(NDS::connection* conn, const char* param);

#ifdef __cplusplus
}
#endif

#endif // _WRAPPER_H_
