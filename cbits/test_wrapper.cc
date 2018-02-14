#include <nds_connection.hh>
#include "buffer_helper.h"
#include "wrapper.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
using namespace std;

int main()
{
    char errbuf[256];
    auto conn = hsnds2_connect("127.0.0.1", 8088, PROTOCOL_TRY, errbuf);
    if (*errbuf) {
        cerr << "cerr:" << errbuf << endl;
    } else {
        hsnds2_destroy(conn);
    }
    return 0;
}
