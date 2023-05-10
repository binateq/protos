using Grpc.Core;
using Server;

namespace Server.Services;

public class GeoService : Geo.GeoBase
{
    private readonly ILogger<GeoService> _logger;

    public GeoService(ILogger<GeoService> logger)
    {
        _logger = logger;
    }

    public override Task<DistanceReply> GetDistance(DistanceRequest request, ServerCallContext context)
    {
        return Task.FromResult(new DistanceReply { Result = 0.0 });
    }
}