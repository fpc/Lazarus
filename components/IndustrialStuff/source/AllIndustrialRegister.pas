
{**********************************************************************
 Package industrial Lazarus
 
 This unit is part of Lazarus Project
***********************************************************************}

unit AllIndustrialRegister;

interface


 uses
  Classes, LResources, AdvLed, IndLed, LedNumber, Sensors, IndGnouMeter;

procedure Register;

implementation


//==========================================================
procedure Register;
begin
  RegisterComponents ('Industrial',[
    TAdvLed, TIndLed, TLedNumber, TStopLightSensor, TAnalogSensor, TindGnouMeter]);

end;

initialization
  {$i industrial_icons.lrs}

end.
