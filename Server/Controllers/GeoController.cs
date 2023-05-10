using Microsoft.AspNetCore.Mvc;

namespace Server.Controllers;

[ApiController]
[Route("geo")]
public class GeoController
{
    [HttpPost("distance")]
    public Task<DistanceReply> GetDistance(DistanceRequest request)
    {
        var method = request.Method ?? CalculationMethod.Cosine;

        var reply = new DistanceReply
        {
            Result = GetByMethod(method, request.From.Latitude, request.From.Longitude,
                request.To.Latitude, request.To.Longitude)
        };
        
        return Task.FromResult(reply);
    }

    private static double GetByMethod(CalculationMethod method, double latitudeFrom, double longitudeFrom,
        double latitudeTo, double longitudeTo)
    {
        if (method == CalculationMethod.Cosine)
            return GeoDistance.GetByCosines(latitudeFrom, longitudeFrom, latitudeTo, longitudeTo);
        
        if (method == CalculationMethod.Haversine)
            return GeoDistance.GetByHaversines(latitudeFrom, longitudeFrom, latitudeTo, longitudeTo);

        throw new ArgumentException("Invalid method", nameof(method));
    }
}