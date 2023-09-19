# protos is the proto*buf* s*erializer and deserializer*

The **protos** utility can convert binary data to
[.textproto](https://developers.google.com/protocol-buffers/docs/text-format-spec)
file and vice versa.
It can be used to test gRPC services.

```text
USAGE: protos [--help] [serialize <proto3File> <messageName>]
              [deserialize <proto3File> <messageName>] [--input <inputFile>]
              [--output <outputFile>]

OPTIONS:

    serialize, ser, s <proto3File> <messageName>
                          serialize message using .proto v3 file; input is
                          textproto, output is binary
    deserialize, de, d <proto3File> <messageName>
                          deserialize message using .proto v3 file; input is
                          binary, output is textproto
    --input, -i <inputFile>
                          input file; if missed, `stdin` is used
    --output, -o <outputFile>
                          output file; if missed, `stdout` is used
    --help                display this list of options.
```

## Example

Imagine we have a service that can calculate distance between two points on the Earth.

The schema of request and response data is described in the **example.proto**.
```text
syntax = "proto3";

option csharp_namespace = "Server";

package geo;

service Geo {
  rpc GetDistance (DistanceRequest) returns (DistanceReply);
}

// The request message containing the user's name.
message DistanceRequest {
  Point from = 1;
  Point to = 2;
  optional CalculationMethod method = 3;
}

message Point {
  double latitude = 1;
  double longitude = 2;
}

enum CalculationMethod {
    COSINE = 0;
    HAVERSINE = 1;
}

// The response message containing the greetings.
message DistanceReply {
  double result = 1;
}
```

Here the `DistanceRequest` is input message and the `DistanceReply` is output one.

To send gRPC request we need text data from **example.textproto**.

```text
from: {
  latitude: 55.75124 
  longitude: 37.61842
}
to: {
  latitude: 59.93863 
  longitude: 30.31413
}
```

Now we can serialize text data to binary format:
```shell
proto serialize example.proto DistanceRequest --input example.textproto --output example.bin
```

We'll get binary file **example.bin**. Here is its hexadecimal representation:

```text
0000000000: 0A 12 09 39 B9 DF A1 28 | E0 4B 40 11 9E 98 F5 62
0000000010: 28 CF 42 40 12 12 09 B2 | 85 20 07 25 F8 4D 40 11
0000000020: 46 B1 DC D2 6A 50 3E 40 |
```

We can send the file to the service.

```shell
curl --http0.9 -X POST -H "Content-Type: application/grpc+proto" -d @example.bin http://localhost:5000/Geo/GetDistance --output distance.bin
```

Here is **response.bin** in hexadecimal representation:

```text

```