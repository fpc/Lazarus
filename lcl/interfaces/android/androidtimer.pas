{
  Android4Pascal - Android API Bindings for Pascal

  Author: Felipe Monteiro de Carvalho - 2011
  License: modified LGPL (the same as the Free Pascal RTL, Lazarus LCL)
}
unit androidtimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, javalang;

type

  { TAndroidTimer }

  TAndroidTimer = class(TJavaObject)
    OnTimer: TNotifyEvent;
    constructor Create;
    procedure postDelayed(ADelay: Integer);
    procedure removeCallbacks();
    procedure callOnTimerListener();
  end;

const
  amkTimer_New_Runnable = $0000;
  amkTimer_postDelayed = $0001;
  amkTimer_removeCallbacks = $0002;
  amkTimer_Callback_Finished = $0EEE0000;

implementation

uses androidpipescomm;

{ TAndroidTimer }

constructor TAndroidTimer.Create;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkTimer));
  vAndroidPipesComm.SendInt(amkTimer_New_Runnable);
  vAndroidPipesComm.SendInt(PtrInt(Self)); // Self, Pascal pointer
  Index := vAndroidPipesComm.WaitForIntReturn();
end;

procedure TAndroidTimer.postDelayed(ADelay: Integer);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkTimer));
  vAndroidPipesComm.SendInt(amkTimer_postDelayed);
  vAndroidPipesComm.SendInt(Index); // Self
  vAndroidPipesComm.SendInt(ADelay);
  vAndroidPipesComm.WaitForReturn();
end;

procedure TAndroidTimer.removeCallbacks();
begin
  vAndroidPipesComm.SendByte(ShortInt(amkTimer));
  vAndroidPipesComm.SendInt(amkTimer_removeCallbacks);
  vAndroidPipesComm.SendInt(Index); // Self
  vAndroidPipesComm.WaitForReturn();
end;

procedure TAndroidTimer.callOnTimerListener();
begin
  if Assigned(OnTimer) then OnTimer(Self);
end;

end.

