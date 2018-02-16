#include <nds2-client/nds_connection.hh>
#include "buffer_helper.h"
#include "wrapper.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
using namespace std;

int main()
{
    char errbuf[256];
    auto conn = hsnds2_connect("10.68.10.122", 8088, PROTOCOL_TRY, errbuf);
    if (*errbuf) {
        cerr << "cerr:" << errbuf << endl;
        return 0;
    }

    channel_t** channels_ptr = new channel_t*;
    hsnds2_find_channels(conn, "*CRY-TEMPERATURE*", channels_ptr, errbuf);
    hsnds2_free_channels(*channels_ptr);
    hsnds2_destroy(conn);

    delete channels_ptr;
    return 0;
}
