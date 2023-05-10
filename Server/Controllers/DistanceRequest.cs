namespace Server.Controllers;

public class DistanceRequest
{
    public Point From { get; init; }
    
    public Point To { get; init; }
    
    public CalculationMethod? Method { get; set; }
}