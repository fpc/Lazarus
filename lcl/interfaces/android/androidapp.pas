unit androidapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, android_all, javalang;

type

//  TEventBoolean_Menu = function (param1: TMenu): Boolean of object;
  TEventKeyUp = function (param1: Integer): Boolean of object;

  { TDisplay }

  TDisplay = class(TJavaObject)
  public
    procedure getMetrics(var outMetrics: TDisplayMetrics);
  end;

  { TWindowManager }

  TWindowManager = class(TJavaObject)
  public
    function getDefaultDisplay(): TDisplay;
  end;

  { TActivity }

  TActivity = class
  public
    onStart: TNotifyEvent;
    onRestart: TNotifyEvent;
    onResume: TNotifyEvent;
    onPause: TNotifyEvent;
    onStop: TNotifyEvent;
    onDestroy: TNotifyEvent;
//    onCreateOptionsMenu: TEventBoolean_Menu;
    onKeyUp: TEventKeyUp;
    procedure HandleonStart(); virtual;
    procedure HandleonRestart(); virtual;
    procedure HandleonResume(); virtual;
    procedure HandleonPause(); virtual;
    procedure HandleonStop(); virtual;
    procedure HandleonDestroy(); virtual;
//    function HandleonCreateOptionsMenu(menu: TMenu): Boolean; virtual;
    function HandleonKeyUp(keyCode: Integer): Boolean; virtual;
    procedure setContentView(view: TView);
    function getWindowManager: TWindowManager;
  end;

var
  Activity: TActivity;

implementation

uses androidpipescomm;

const
  amkUI_Activity_setContentView = $0000;
  amkUI_Activity_getWindowManager = $0080;

  amkUI_WindowManager_getDefaultDisplay = $00A0;

  amkUI_Display_getMetrics = $00D0;

{ TActivity }

procedure TActivity.HandleonStart();
begin
  if Assigned(onStart) then onStart(Self);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onStartFinished);
end;

procedure TActivity.HandleonRestart();
begin
  if Assigned(onRestart) then onRestart(Self);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onRestartFinished);
end;

procedure TActivity.HandleonResume();
begin
  if Assigned(onResume) then onResume(Self);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onResumeFinished);
end;

procedure TActivity.HandleonPause();
begin
  if Assigned(onPause) then onPause(Self);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onPauseFinished);
end;

procedure TActivity.HandleonStop();
begin
  if Assigned(onStop) then onStop(Self);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onStopFinished);
end;

procedure TActivity.HandleonDestroy();
begin
  if Assigned(onDestroy) then onDestroy(Self);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onDestroyFinished);
end;

{function TActivity.HandleonCreateOptionsMenu(menu: TMenu): Boolean;
begin
  Result := False;
  if Assigned(onCreateOptionsMenu) then Result := onCreateOptionsMenu(menu);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onCreateOptionsMenuFinished);
  vAndroidPipesComm.SendInt(Integer(Result));
end;                  }

function TActivity.HandleonKeyUp(keyCode: Integer): Boolean;
begin
  Result := False;
  if Assigned(onKeyUp) then Result := onKeyUp(keyCode);
  vAndroidPipesComm.SendMessage(amkActivityCallback, ams_ActivityCallback_onKeyUpFinished);
  vAndroidPipesComm.SendInt(Integer(Result));
end;

// android.app.Activity
// public void setContentView (View view)
procedure TActivity.setContentView(view: TView);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_Activity_setContentView);
  // Without Self because this is a global object
  vAndroidPipesComm.SendInt(view.Index);
  vAndroidPipesComm.WaitForReturn();
end;

function TActivity.getWindowManager: TWindowManager;
var
  lHandle: Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_Activity_getWindowManager);
  // Without Self because this is a global object
  lHandle := vAndroidPipesComm.WaitForIntReturn();
  Result := TWindowManager.Create(lHandle);
end;

{ TDisplay }

procedure TDisplay.getMetrics(var outMetrics: TDisplayMetrics);
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_Display_getMetrics);
  vAndroidPipesComm.SendInt(Index); // Self
  vAndroidPipesComm.SendInt(outMetrics.Index); // Param
  vAndroidPipesComm.WaitForReturn();
end;

{ TWindowManager }

function TWindowManager.getDefaultDisplay(): TDisplay;
var
  lHandle: Integer;
begin
  vAndroidPipesComm.SendByte(ShortInt(amkUICommand));
  vAndroidPipesComm.SendInt(amkUI_WindowManager_getDefaultDisplay);
  vAndroidPipesComm.SendInt(Index); // Self
  lHandle := vAndroidPipesComm.WaitForIntReturn();
  Result := TDisplay.Create(lHandle);
end;

initialization
  Activity := TActivity.Create;
finalization
  Activity.Free;
end.

