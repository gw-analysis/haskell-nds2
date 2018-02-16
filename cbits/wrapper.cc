#include <nds_connection.hh>
#include "buffer_helper.h"
#include "wrapper.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
using namespace std;

// Copy an error message to caller's error buffer. Truncates at ERRBUF_LENGTH.
static inline void copy_errmsg(char* errbuf, const char* err)
{
    strncpy(errbuf, err, ERRBUF_LENGTH);
    // Null-terminate the buffer in case of overflow.
    errbuf[ERRBUF_LENGTH] = '\0';
}

// Construct a string vector out of char* list.
static vector<string> make_string_vec(const char* strs[], size_t num)
{
    vector<string> vec;
    for (size_t i=0; i<num; i++) {
        assert(strs[i] != nullptr);
        vec.emplace_back(strs[i]);
    }
    return vec;
}

// Duplicate a channel struct, using strdup on name and units.
static void dup_channel(channel_t& out, const NDS::channel& channel)
{
    out.name = strdup(channel.Name().c_str());
    out.type = (channel_type) channel.Type();
    out.dataType = (data_type) channel.DataType();
    out.sampleRate = channel.SampleRate();
    out.gain = channel.Gain();
    out.slope = channel.Slope();
    out.offset = channel.Offset();
    out.units = strdup(channel.Units().c_str());
}

// Duplicate a list of out buffers, mallocing channelInfo and timeseries array.
static void dup_buffers(out_buffer_t out[], const NDS::buffers_type& buffers)
{
    const auto bufsize = buffers.size();
    try {
        for (auto i=0; i<bufsize; i++) {
            // Copy channel information
            out[i].channelInfo = (channel_t*) malloc(sizeof(channel_t));
            dup_channel(*out[i].channelInfo, *buffers[i]);

            // Copy start/stop time
            out[i].startGpsTime = buffers[i]->Start();
            out[i].stopGpsTime  = buffers[i]->Stop();

            // Copy buffer data
            buffer_helper buf_data(*buffers[i]);
            out[i].timeseries = (double*) malloc(sizeof(double) * buf_data.size());
            for (auto j=0; j < buf_data.size(); j++) {
                out[i].timeseries[j] = buf_data[j];
            }

            out[i].timeseries_length = buf_data.size();
        }
    } catch (...) {
        // Free any allocated buffers.
        for (auto i=0; i < bufsize; i++) {
            hsnds2_free_channel(out[i].channelInfo);
            out[i].channelInfo = nullptr;
            out[i].timeseries_length = 0;
            free(out[i].timeseries);
            out[i].timeseries = nullptr;
        }

        // Rethrow the exception.
        throw;
    }
}

extern "C" {

connection_t* hsnds2_connect(const char* hostname, port_t port, protocol_type protocol, char* errbuf)
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

// C allocates list of channels; caller responsible for fereing them.
// channels is NULL-terminated.
int hsnds2_find_channels(NDS::connection* conn, const char* channelGlob, channel_t** channels, char* errbuf)
{
    assert(conn != nullptr && channelGlob != nullptr && channels != nullptr && errbuf != nullptr);

    try {
        auto channels_vec = conn->find_channels(channelGlob);
        *channels = (channel_t*) calloc(channels_vec.size() + 1, sizeof(channel_t));
        for (int i=0; i < channels_vec.size(); i++)
            dup_channel(*channels[i], *channels_vec[i]);

        return channels_vec.size();
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return -1;
    }
}

void hsnds2_free_channel(channel_t* channel)
{
    if (channel != nullptr) {
        free(channel->name);
        free(channel->units);
        free(channel);
    }
}

void hsnds2_free_channels(channel_t channels[])
{
    assert(channels != nullptr);
    channel_t* cur_channel = channels;
    for (;cur_channel->name != nullptr; ++cur_channel) {
        hsnds2_free_channel(cur_channel);
        cur_channel = nullptr;
    }
    free(channels);
}

int hsnds2_fetch(NDS::connection* conn, gps_second_t start_gps_time, gps_second_t stop_gps_time, const char* channel_names[], size_t num_channels, out_buffer_t out_buffers[], char* errbuf)
{
    assert(conn != nullptr && out_buffers != nullptr);

    if (start_gps_time > stop_gps_time) {
        strcpy(errbuf, "Invalid time interval");
        return -EINVAL;
    }

    memset(out_buffers, 0, sizeof(out_buffer_t) * num_channels);

    try {
        vector<string> channel_name_vec = make_string_vec(channel_names, num_channels);
        auto bufs = conn->fetch(start_gps_time, stop_gps_time, channel_name_vec);
        assert(bufs.size() == num_channels);
        dup_buffers(out_buffers, bufs);
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return -1;
    }

    return 0;
}

void hsnds2_free_timeseries(double* timeseries)
{
    if (timeseries != nullptr) {
        free(timeseries);
    }
}

int hsnds2_start_realtime(NDS::connection* conn,
                          const char* channel_names[],
                          size_t num_channels,
                          gps_second_t stride,
                          char* errbuf) {
    assert(conn != nullptr && channel_names != nullptr && errbuf != nullptr);

    try {
        vector<string> channel_name_vec =
            make_string_vec(channel_names, num_channels);

        conn->iterate(stride, channel_name_vec);
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return -1;
    }
    return 0;
}

int hsnds2_next(NDS::connection* conn, size_t num_channels, out_buffer_t out_buffers[], char* errbuf) {
    assert(conn != nullptr && out_buffers != nullptr && errbuf != nullptr);

    memset(out_buffers, 0, sizeof(double*) * num_channels);

    try {
        auto bufs = conn->next();
        assert(bufs.size() == num_channels);
        dup_buffers(out_buffers, bufs);
    } catch (const out_of_range&) {
        return -ERANGE;
    } catch (const exception& ex) {
        copy_errmsg(errbuf, ex.what());
        return -1;
    }

    return 0;
}

} // extern "C"
