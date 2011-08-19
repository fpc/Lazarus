{
  Android4Pascal - Android API Bindings for Pascal

  Author: Felipe Monteiro de Carvalho - 2011
  License: modified LGPL (the same as the Free Pascal RTL, Lazarus LCL)
}
unit androidpipescomm;

{$mode objfpc}{$H+}

// ANDROID_NO_COMM is utilized to test the code flow in the desktop
{$ifndef ARM}{$define ANDROID_NO_COMM}{$endif}

interface

uses
  Classes, SysUtils; 

const
  // Android Message Kind
  amkFloatResult = 103;
  amkIntResult = 102;
  amkResult = 101;
  amkActivityCallback = 0;
  amkLog = 1;
  amkUICommand = 2;
  amkJavaLangCall = 3;
  amkTimer = 4;

  // Android Message subtype
  ams_ActivityCallback_onCreateStarted = $0001;
  ams_ActivityCallback_onStartStarted = $0002;
  ams_ActivityCallback_onRestartStarted = $0003;
  ams_ActivityCallback_onResumeStarted = $0004;
  ams_ActivityCallback_onPauseStarted = $0005;
  ams_ActivityCallback_onStopStarted = $0006;
  ams_ActivityCallback_onDestroyStarted = $0007;
  ams_ActivityCallback_onCreateOptionsMenuStarted = $0008;
  ams_ActivityCallback_onKeyUpStarted = $0010;

  ams_ActivityCallback_onCreateFinished = $1001;
  ams_ActivityCallback_onStartFinished = $1002;
  ams_ActivityCallback_onRestartFinished = $1003;
  ams_ActivityCallback_onResumeFinished = $1004;
  ams_ActivityCallback_onPauseFinished = $1005;
  ams_ActivityCallback_onStopFinished = $1006;
  ams_ActivityCallback_onDestroyFinished = $1007;
  ams_ActivityCallback_onCreateOptionsMenuFinished = $1008;
  ams_ActivityCallback_onKeyUpFinished = $1010;

type
  { TAndroidPipesComm }

  TAndroidPipesComm = class
  private
    dataByte1: Byte;
    dataString1: string;
  public
    constructor Create;
    procedure onCreateFinished();
    procedure MessageLoop();
    function ReadByte: ShortInt;
    function ReadInt: Integer;
    function ReadFloat: Single;
    procedure SendMessage(AKind: ShortInt; ASubtype: DWord);
    procedure SendByte(AData: ShortInt);
    procedure SendInt(AData: Integer);
    procedure SendFloat(AData: Single);
    procedure SendString(AStr: string);
    procedure WaitForReturn();
    function WaitForIntReturn(): Integer;
    function WaitForFloatReturn(): Single;
    procedure CommError(AStr: string);
    procedure Log(AStr: string);
  end;

var
  vAndroidPipesComm: TAndroidPipesComm;

implementation

uses android_all, androidapp, androidtimer, javalang;

var
  Str: string;
  lByte: Byte = 1;
  lInt: Integer = 1;
  lPascalPointer: PtrInt = -1;
  InputStream, OutputStream: THandleStream;

{ TAndroidPipesComm }

constructor TAndroidPipesComm.Create;
begin
  inherited Create;

  InputStream := THandleStream.Create(StdInputHandle);
  OutputStream := THandleStream.Create(StdOutputHandle);
end;

procedure TAndroidPipesComm.onCreateFinished();
begin
  SendMessage(amkActivityCallback, ams_ActivityCallback_onCreateFinished);
end;

procedure TAndroidPipesComm.MessageLoop();
{var
  lMenuItem: TMenuItem;}
begin
  // Starts the message loop
  while (true) do
  begin
    // Waits for a Java message
    lByte := InputStream.ReadByte();
    // Respond to it
    case lByte of
      amkActivityCallback:
      begin
        lInt := ReadInt(); // Android Message subtype
        case lInt of
          ams_ActivityCallback_onStartStarted: Activity.HandleonStart();
          ams_ActivityCallback_onRestartStarted: Activity.HandleonRestart();
          ams_ActivityCallback_onResumeStarted: Activity.HandleonResume();
          ams_ActivityCallback_onPauseStarted: Activity.HandleonPause();
          ams_ActivityCallback_onStopStarted: Activity.HandleonStop();
          ams_ActivityCallback_onDestroyStarted: Activity.HandleonDestroy();
          ams_ActivityCallback_onCreateOptionsMenuStarted:
          begin
            lInt := ReadInt(); // param
//            Activity.HandleonCreateOptionsMenu(TMenu.Create(lInt));
            // The method sends the result
          end;
          ams_ActivityCallback_onKeyUpStarted:
          begin
            lInt := ReadInt(); // param
            Activity.HandleonKeyUp(lInt);
            // The method sends the result
          end;
        end;
      end;
      amkUICommand:
      begin
        lInt := ReadInt(); // Android Message subtype
        android_all.HandleMessage(lInt);
{        case lInt of
          amkUI_MenuItem_setOnMenuItemClickListener_Start:
          begin
            lInt := ReadInt();
            lMenuItem := TMenuItem(FindItemIdInList(MenuItems, lInt));
            if lMenuItem <> nil then
              lInt := lMenuItem.callOnMenuItemClickListener();
            vAndroidPipesComm.SendMessage(amkUICommand, amkUI_MenuItem_setOnMenuItemClickListener_Finished);
            vAndroidPipesComm.SendInt(lInt);
          end;
        end;}
      end;
      amkTimer: // A Timer Callback
      begin
        lPascalPointer := ReadInt();
        TAndroidTimer(lPascalPointer).callOnTimerListener();
        vAndroidPipesComm.SendMessage(amkTimer, amkTimer_Callback_Finished);
      end;
    end;
//    dataString1 := 'Pascal log: ' + IntToStr(lByte) + LineEnding;
//    SendMessage(amkLog, 0);
  end;
end;

function TAndroidPipesComm.ReadByte: ShortInt;
begin
  {$ifdef ANDROID_NO_COMM}Exit(0);{$ENDIF}
  Result := InputStream.ReadByte();
end;

function TAndroidPipesComm.ReadInt: Integer;
begin
  {$ifdef ANDROID_NO_COMM}Exit(0);{$ENDIF}
  Result := BEToN(InputStream.ReadDWord());
end;

function TAndroidPipesComm.ReadFloat: Single;
var
  lNum: DWord;
begin
  {$ifdef ANDROID_NO_COMM}Exit(0.0);{$ENDIF}
  lNum := BEToN(InputStream.ReadDWord());
  Move(lNum, Result, 4);
end;

procedure TAndroidPipesComm.SendMessage(AKind: ShortInt; ASubtype: DWord);
begin
  OutputStream.WriteByte(AKind);
  case AKind of
    amkActivityCallback:
    begin
//      OutputStream.WriteDWord(NToBe(ASubType));
    end;
    amkLog:
    begin
      SendString(dataString1);
    end;
  else
    OutputStream.WriteDWord(NToBe(ASubType));
  end;
end;

procedure TAndroidPipesComm.SendByte(AData: ShortInt);
begin
  OutputStream.WriteByte(Byte(AData));
end;

// Java uses Big-Endian when reading pipes,
// so we need to convert
procedure TAndroidPipesComm.SendInt(AData: Integer);
begin
  OutputStream.WriteDWord(NToBe(DWord(AData)));
end;

procedure TAndroidPipesComm.SendFloat(AData: Single);
begin
  OutputStream.WriteDWord(NToBe(DWord(AData)));
end;

procedure TAndroidPipesComm.SendString(AStr: string);
begin
  OutputStream.WriteDWord(NToBE(Length(AStr)));
  OutputStream.Write(AStr[1], Length(AStr));
end;

procedure TAndroidPipesComm.WaitForReturn();
var
  lByte: ShortInt;
begin
  {$ifdef ANDROID_NO_COMM}Exit;{$ENDIF}
  lByte := ReadByte();
  if lByte <> amkResult then CommError('[TAndroidPipesComm.WaitForIntReturn] expected amkResult but got: ' + IntToStr(lByte));
end;

function TAndroidPipesComm.WaitForIntReturn(): Integer;
var
  lByte: ShortInt;
begin
  {$ifdef ANDROID_NO_COMM}Exit(0);{$ENDIF}
  lByte := ReadByte();
  if lByte <> amkIntResult then CommError('[TAndroidPipesComm.WaitForIntReturn] expected amkIntResult but got: ' + IntToStr(lByte));
  Result := ReadInt();
end;

function TAndroidPipesComm.WaitForFloatReturn(): Single;
var
  lByte: ShortInt;
begin
  {$ifdef ANDROID_NO_COMM}Exit(0.0);{$ENDIF}
  lByte := ReadByte();
  if lByte <> amkFloatResult then CommError('[TAndroidPipesComm.WaitForFloatReturn] expected amkFloatResult but got: ' + IntToStr(lByte));
  Result := ReadFloat();
end;

procedure TAndroidPipesComm.CommError(AStr: string);
begin
  dataString1 := AStr;
  SendMessage(amkLog, 0);
end;

procedure TAndroidPipesComm.Log(AStr: string);
begin
  dataString1 := AStr;
  SendMessage(amkLog, 0);
end;

initialization
  vAndroidPipesComm := TAndroidPipesComm.Create;
finalization
  vAndroidPipesComm.Free;
end.

