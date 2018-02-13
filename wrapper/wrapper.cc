#include <nds_connection.hh>
#include "buffer_helper.h"
#include "wrapper.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
using namespace std;

inline void copy_errmsg(char* errbuf, const char* err)
{
    strncpy(errbuf, err, ERRBUF_LENGTH);
    // Null-terminate the buffer in case of overflow.
    errbuf[ERRBUF_LENGTH] = '\0';
}

extern "C" {

NDS::connection* connect(const char* hostname, int port, int protocol, char* errbuf)
{
    assert(hostname != nullptr && errbuf != nullptr);
    try {
        return new NDS::connection(hostname, port,
                                   (NDS::connection::protocol_type)protocol);
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return nullptr;
    }
}

void disconnect(NDS::connection* conn)
{
    assert(conn != nullptr);
    conn->close();
}

void destroy(NDS::connection* conn)
{
    delete conn;
}

// C allocates list of channels; caller responsible for fereing them.
// channels is NULL-terminated.
int findChannels(NDS::connection* conn, const ChannelFilter* filter, Channel** channels, char* errbuf)
{
    assert(conn != nullptr && filter != nullptr && channels != nullptr && errbuf != nullptr);
    try {
        auto channels_vec = conn->find_channels(filter->channelGlob,
                                                filter->channelTypeMask,
                                                filter->dataTypeMask,
                                                filter->minSampleRate,
                                                filter->maxSampleRate);
        *channels = (Channel*) calloc(channels_vec.size() + 1, sizeof(Channel));
        for (int i=0; i < channels_vec.size(); i++) {
            (*channels)[i].name = strdup(channels_vec[i]->Name().c_str());
            (*channels)[i].type = channels_vec[i]->Type();
            (*channels)[i].dataType = channels_vec[i]->DataType();
            (*channels)[i].sampleRate = channels_vec[i]->SampleRate();
            (*channels)[i].gain = channels_vec[i]->Gain();
            (*channels)[i].slope = channels_vec[i]->Slope();
            (*channels)[i].offset = channels_vec[i]->Offset();
            (*channels)[i].units = strdup(channels_vec[i]->Units().c_str());
        }
        memset((*channels) + channels_vec.size(), 0, sizeof(Channel));

        return channels_vec.size();
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return -1;
    }
}

void freeChannels(Channel* channels)
{
    assert(channels != nullptr);
    Channel* cur_channel = channels;
    while (cur_channel->name != nullptr) {
        free(cur_channel->name);
        free(cur_channel->units);
    }
    free(channels);
}

int fetch(NDS::connection* conn, int64_t startGpsTime, int64_t endGpsTime, const char** channelList, size_t nChannels, double** buffers, char* errbuf)
{
    assert(conn != nullptr && buffers != nullptr);

    if (startGpsTime > endGpsTime) {
        strcpy(errbuf, "Invalid time interval");
        return -EINVAL;
    }

    memset(buffers, 0, sizeof(double*) * nChannels);

    try {
        vector<string> channelListVec;
        for (size_t i=0; i<nChannels; i++)
            channelListVec.emplace_back(channelList[i]);
        auto bufs = conn->fetch(startGpsTime, endGpsTime, channelListVec);
        assert(bufs.size() == nChannels);
        for (size_t i = 0; i<bufs.size(); i++) {
            buffer_helper buf_data(*bufs[i]);
            buffers[i] = (double*) malloc(sizeof(double) * buf_data.size());
            for (size_t j=0; j < buf_data.size(); j++) {
                buffers[i][j] = buf_data[j];
            }
        }
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());

        // Free any allocated buffers.
        for (size_t i = 0; i < nChannels; i++)
            if (buffers[i]) {
                free(buffers[i]);
                buffers[i] = nullptr;
            }

        return -1;
    }
}

bool setParameter(NDS::connection* conn, const char* param, const char* value)
{
    assert(conn != nullptr && param != nullptr && value != nullptr);
    bool success = false;
    try {
        success = conn->set_parameter(param, value);
    } catch (...) {
        // TODO error message
        return false;
    }
    return success;
}

char* getParameter(NDS::connection* conn, const char* param)
{
    try {
        return strdup(conn->get_parameter(param).c_str());
    } catch (...) {
        // TODO error message
        return nullptr;
    }
}

} // extern "C"
