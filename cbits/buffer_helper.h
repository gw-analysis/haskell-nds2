#ifndef _BUFFER_HELPER_H_
#define _BUFFER_HELPER_H_

#include <nds2-client/nds_connection.hh>
#include <stdint.h>

// Helper class for accessing and interpreting data from NDS::buffer.
class buffer_helper
{

public:
    buffer_helper(const NDS::buffer& buffer) : buffer_(buffer) {}

    inline size_t size() const
    {
        return buffer_.Samples();
    }

    double operator[](size_t i) const
    {
        if (i >= size())
            throw std::out_of_range(
                "Out of range when accessing buffer_helper");

        size_t idx = buffer_.DataTypeSize() * i;

        // Copy the raw bytes of the element at i to val.
        uint8_t val[16];
        for (int j=0;j<buffer_.DataTypeSize(); j++) {
            val[j] = buffer_[j];
        }

        static_assert(sizeof(float) == 4, "require 32-bit floats");
        static_assert(sizeof(double) == 8, "require 64-bit doubles");

        // Interpret the raw bytes in val.
        switch (buffer_.DataType()) {
        case NDS::channel::DATA_TYPE_FLOAT32:
            return double(*reinterpret_cast<float*>(val));

        case NDS::channel::DATA_TYPE_FLOAT64:
            return *reinterpret_cast<double*>(val);

        case NDS::channel::DATA_TYPE_INT16:
            return double(*reinterpret_cast<int16_t*>(val));

        case NDS::channel::DATA_TYPE_INT32:
            return double(*reinterpret_cast<int32_t*>(val));

        case NDS::channel::DATA_TYPE_INT64:
            // NOTE: double may not have enough precision.
            return double(*reinterpret_cast<int64_t*>(val));

        case NDS::channel::DATA_TYPE_UINT32:
            return double(*reinterpret_cast<uint32_t*>(val));

        case NDS::channel::DATA_TYPE_COMPLEX32:
            throw std::logic_error("NDS Complex32 type not implemented");

        default:
            throw std::logic_error("Unknown NDS data type");
        }
    }

private:
    const NDS::buffer& buffer_;
};

#endif  // _BUFFER_HELPER_H_
