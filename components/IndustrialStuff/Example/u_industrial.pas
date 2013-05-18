unit u_industrial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IndAdvLed, IndLed, Sensors, LedNumber,
  IndGnouMeter, Forms, Controls, Graphics, Dialogs, Arrow;

type

  { TForm1 }

  TForm1 = class(TForm)
    AnalogSensor1: TAnalogSensor;
    Arrow1: TArrow;
    indAdvLed1: TindAdvLed;
    indGnouMeter1: TindGnouMeter;
    indLed1: TindLed;
    LEDNumber1: TLEDNumber;
    StopLightSensor1: TStopLightSensor;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

end.

