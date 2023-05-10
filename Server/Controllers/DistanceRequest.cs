namespace Server.Controllers;

public class DistanceRequest
{
    public Point From { get; set; }
    
    public Point To { get; set; }
    
    public CalculationMethod? Method { get; set; }
}