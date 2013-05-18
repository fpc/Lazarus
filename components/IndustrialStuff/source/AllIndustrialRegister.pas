
{**********************************************************************
 Package industrial Lazarus
 
 This unit is part of Lazarus Project
***********************************************************************}

unit AllIndustrialRegister;

interface


 uses
  Classes, LResources, IndAdvLed, IndLed, LedNumber, Sensors, IndGnouMeter;

procedure Register;

implementation


//==========================================================
procedure Register;
begin
  RegisterComponents ('Industrial',[
    TindAdvLed, TIndLed, TLedNumber, TStopLightSensor, TAnalogSensor, TindGnouMeter]);

end;

initialization
  {$i industrial_icons.lrs}

end.
