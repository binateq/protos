using Grpc.Core;

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
        var result = request.Method switch
        {
            CalculationMethod.Cosine => GeoDistance.GetByCosines(request.From.Latitude,
                request.From.Longitude, request.To.Latitude, request.To.Longitude),
            CalculationMethod.Haversine => GeoDistance.GetByHaversines(request.From.Latitude,
                request.From.Longitude, request.To.Latitude, request.To.Longitude),
            _ => GeoDistance.GetByCosines(request.From.Latitude,
                request.From.Longitude, request.To.Latitude, request.To.Longitude)
        };

        _logger.LogInformation("Method: {@Method}, From: {@From}, To: {@To}, Result: {@Result}",
            request.Method, request.From, request.To, result);
        
        return Task.FromResult(new DistanceReply
        {
            Result = result
        });
    }
}