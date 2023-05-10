using Microsoft.AspNetCore.Mvc;

namespace Server.Controllers;

[ApiController]
[Route("geo")]
public class GeoController
{
    [HttpPost("distance")]
    public Task<DistanceReply> GetDistance(DistanceRequest request)
    {
        return Task.FromResult(new DistanceReply { Result = 0.0 });
    }
}