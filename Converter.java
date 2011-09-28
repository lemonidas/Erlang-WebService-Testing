package wtp;

public class Converter
{
  public float celsiusToFarenheit ( float celsius )
  {
    return (celsius * 9 / 5) + 32;
  }

  public float farenheitToCelsius ( float farenheit )
  {
    return (farenheit - 32) * 5 / 9;
  }
}
