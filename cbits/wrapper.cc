#include <nds_connection.hh>
#include "buffer_helper.h"
#include "wrapper.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
using namespace std;

static inline void copy_errmsg(char* errbuf, const char* err)
{
    strncpy(errbuf, err, ERRBUF_LENGTH);
    // Null-terminate the buffer in case of overflow.
    errbuf[ERRBUF_LENGTH] = '\0';
}

extern "C" {

connection* hsnds2_connect(const char* hostname, port_t port, protocol_type protocol, char* errbuf)
{
    assert(hostname != nullptr && errbuf != nullptr);

    try {
        return new NDS::connection(string(hostname), port,
                                   (NDS::connection::protocol_type)protocol);
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return nullptr;
    }
}

void hsnds2_disconnect(NDS::connection* conn)
{
    assert(conn != nullptr);
    conn->close();
}

void hsnds2_destroy(NDS::connection* conn)
{
    delete conn;
}

// C allocates list of channels; caller responsible for fereing them.
// channels is NULL-terminated.
int hsnds2_find_channels(NDS::connection* conn, const channel_filter* filter, channel* channels[], char* errbuf)
{
    assert(conn != nullptr && filter != nullptr && channels != nullptr && errbuf != nullptr);

    try {
        auto channels_vec = conn->find_channels(filter->channelGlob,
                                                (NDS::channel::channel_type)filter->channelTypeMask,
                                                (NDS::channel::data_type)filter->dataTypeMask,
                                                filter->minSampleRate,
                                                filter->maxSampleRate);
        *channels = (channel*) calloc(channels_vec.size() + 1, sizeof(channel));
        for (int i=0; i < channels_vec.size(); i++) {
            (*channels)[i].name = strdup(channels_vec[i]->Name().c_str());
            (*channels)[i].type = (channel_type) channels_vec[i]->Type();
            (*channels)[i].dataType = (data_type) channels_vec[i]->DataType();
            (*channels)[i].sampleRate = channels_vec[i]->SampleRate();
            (*channels)[i].gain = channels_vec[i]->Gain();
            (*channels)[i].slope = channels_vec[i]->Slope();
            (*channels)[i].offset = channels_vec[i]->Offset();
            (*channels)[i].units = strdup(channels_vec[i]->Units().c_str());
        }
        memset((*channels) + channels_vec.size(), 0, sizeof(channel));

        return channels_vec.size();
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return -1;
    }
}

void hsnds2_free_channels(channel* channels)
{
    assert(channels != nullptr);
    channel* cur_channel = channels;
    while (cur_channel->name != nullptr) {
        free(cur_channel->name);
        free(cur_channel->units);
    }
    free(channels);
}

int hsnds2_fetch(NDS::connection* conn, int64_t startGpsTime, int64_t endGpsTime, const char* channelList[], size_t num_channels, double* buffers[], size_t buffer_lengths[], char* errbuf)
{
    assert(conn != nullptr && buffers != nullptr);

    if (startGpsTime > endGpsTime) {
        strcpy(errbuf, "Invalid time interval");
        return -EINVAL;
    }

    memset(buffers, 0, sizeof(double*) * num_channels);
    memset(buffer_lengths, 0, sizeof(size_t) * num_channels);

    try {
        vector<string> channelListVec;
        for (size_t i=0; i<num_channels; i++)
            channelListVec.emplace_back(channelList[i]);
        auto bufs = conn->fetch(startGpsTime, endGpsTime, channelListVec);
        assert(bufs.size() == num_channels);
        for (size_t i = 0; i<bufs.size(); i++) {
            buffer_helper buf_data(*bufs[i]);
            buffers[i] = (double*) malloc(sizeof(double) * buf_data.size());
            for (size_t j=0; j < buf_data.size(); j++) {
                buffers[i][j] = buf_data[j];
            }
            buffer_lengths[i] = buf_data.size();
        }
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());

        // Free any allocated buffers.
        for (size_t i = 0; i < num_channels; i++)
            if (buffers[i]) {
                free(buffers[i]);
                buffers[i] = nullptr;
                buffer_lengths[i] = 0;
            }

        return -1;
    }
}

void hsnds2_free_buffer(double* buffer)
{
    if (buffer != nullptr)
        free(buffer);
}

int hsnds2_set_parameter(NDS::connection* conn, const char* param, const char* value, char* errbuf)
{
    assert(conn != nullptr && param != nullptr && value != nullptr && errbuf != nullptr);

    bool success = false;
    try {
        success = conn->set_parameter(param, value);
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return false;
    }
    return (int)success;
}

char* hsnds2_get_parameter(NDS::connection* conn, const char* param, char* errbuf)
{
    assert(conn != nullptr && param != nullptr && errbuf != nullptr);

    try {
        const string& val = conn->get_parameter(param);
        if (val.empty())
            return nullptr;
        else
            return strdup(val.c_str());
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return nullptr;
    }
}

} // extern "C"
