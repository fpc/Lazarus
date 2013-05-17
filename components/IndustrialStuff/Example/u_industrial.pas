unit u_industrial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, indAdvLed, IndLed, grArrow, Sensors, LedNumber,
  indGnouMeter, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    AnalogSensor1: TAnalogSensor;
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

end.

