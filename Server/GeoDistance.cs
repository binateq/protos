namespace Server;

public static class GeoDistance
{
    private const double Radius = 6371.0;

    private static double ToRadians(double degrees) => Math.PI * degrees / 180;
    private static double Square(double x) => x * x;

    public static double GetByCosines(double latitudeFrom, double longitudeFrom,
        double latitudeTo, double longitudeTo)
    {
        var phi1 = ToRadians(latitudeFrom);
        var phi2 = ToRadians(latitudeTo);
        var deltaLambda = ToRadians(latitudeFrom - latitudeTo);

        var cos = Math.Sin(phi1) * Math.Sin(phi2) +
                  Math.Cos(phi1) * Math.Cos(phi2) * Math.Cos(deltaLambda);
        
        return Radius * Math.Acos(cos);
    }

    public static double GetByHaversines(double latitudeFrom, double longitudeFrom,
        double latitudeTo, double longitudeTo)
    {
        var phi1 = ToRadians(latitudeFrom);
        var phi2 = ToRadians(latitudeTo);
        var deltaPhi = phi2 - phi1;
        var deltaLambda = ToRadians(latitudeFrom - latitudeTo);

        var tan = Square(Math.Sin(deltaPhi / 2)) +
                  Math.Cos(phi1) * Math.Cos(phi2) *
                  Square(Math.Sin(deltaLambda / 2));

        return Radius * Math.Atan2(Math.Sqrt(tan), Math.Sqrt(1 - tan));
    }
}