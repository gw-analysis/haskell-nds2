// To build: g++ -lndsclient -lndsxxwrap test.cc -I /usr/local/include/nds2-client -L /usr/local/lib64/ -o test
#include <nds_connection.hh>
#include "buffer_helper.h"
#include <iostream>
using namespace std;



bool ends_with(const std::string& value, const std::string& ending)
{
    if (ending.size() > value.size())
        return false;
    return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
}

int main()
{
    NDS::connection conn("10.68.10.122", 8088, (NDS::connection::protocol_type)3);
    cout << "Conn ok" << endl;

    // Find if any channel has complex type (DataType() > 16384).
#if 0
    for (const auto& channel : conn.find_channels()) {
        if (channel->DataType() > 16) {
            cout << channel->Name() << " DataType = " << channel->DataType() << endl;
        }
    }
#endif
    // NOTE: It appears that DataType() > 16 (Complex) DNE.

    NDS::channels_type channels = conn.find_channels("*CRY-TEMPERATURE*");
    vector<string> chanList;
    for (const auto& channel : channels) {
        if (!ends_with(channel->Name(), "trend")) {
            cout << channel->Name() << ' ' << channel->SampleRate() << endl;
            chanList.emplace_back(channel->Name());
            if (chanList.size() >= 10) break;
        }
    }

    // TODO: test_nds_iterate_live_data.py
    // Seems to be able to stream.

    // Test live data
    // FIXME: there's a bug in NDS such that the following command returns only 1 block of data.
    // The only way to continue operations seem to be making a new connection.

    //conn.iterate(0, 6000, {"K1:PEM-TEMPERATURE_RACK_IMC", "K1:PEM-HUMIDITY_RACK_IMC"});
    conn.iterate({"K1:PEM-TEMPERATURE_RACK_IMC", "K1:PEM-HUMIDITY_RACK_IMC"});
    for (;;) {
        try {
            auto live_buf = conn.next(); // NOTE: throws std::out_of_range when done.
            for (const auto& buf : live_buf) {
                cout << endl;
                cout << "-------" << buf->Name() << endl;
                cout << "-------" << buf->DataType() << endl;
                buffer_helper data(*buf);
                for (int i=0; i<data.size(); i++) {
                    cout << data[i] << ' ';
                }
                cout << endl;
            }
        } catch (const std::out_of_range&) {
            break;
        }
    }
    auto buffer = conn.fetch(1202178040, 1202178140, chanList);
    for (const auto& buf : buffer) {
        cout << endl;
        cout << "-------" << buf->Name() << endl;
        cout << "-------" << buf->DataType() << endl;
        buffer_helper data(*buf);
        for (int i=0; i<data.size(); i++) {
            cout << data[i] << ' ';
        }
        cout << endl;
    }

    return 0;
}
