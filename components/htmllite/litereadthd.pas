{*********************************************************}
{*                   LITEREADTHD.PAS                     *}
{*                 Copyright (c) 2002 by                 *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i LiteCons.inc}

unit LiteReadThd;

interface

uses
  Classes, LitePars;

type
  TFakeThread = class
  private
    FHandle: THandle;
    FThreadID: THandle;
    FTerminated: Boolean;
    FSuspended: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FReturnValue: Integer;
    FOnTerminate: TNotifyEvent;
    FPriority: TThreadPriority;
    FMethod: TThreadMethod;
    FSynchronizeException: TObject;
    procedure CallOnTerminate;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    procedure SetSuspended(Value: Boolean);
    procedure DoExecute;
  protected
    procedure DoTerminate; virtual;
    procedure Execute; virtual; abstract;
    procedure Synchronize(Method: TThreadMethod);
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor: Integer;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Handle: THandle read FHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Suspended: Boolean read FSuspended write SetSuspended;
    property ThreadID: THandle read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

  TParseThread = class({$IFDEF NoThreads}TFakeThread{$ELSE}TThread{$ENDIF})
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    Parser: ThlParser;
    St: string;
    ASectionList: TList;
    AIncludeEvent: TIncludeType;
    ASoundEvent: TSoundType;
    AMetaEvent: TMetaType;
    ANameList: TStringList;
    Buffer, BuffEnd: PChar;
    Text: boolean;
    Done: boolean;

    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod);
    procedure AddString({$IFDEF HL_LAZARUS}const {$ENDIF}S: string);
  end;


implementation


constructor TParseThread.Create(CreateSuspended: Boolean);
begin
inherited;
FreeOnTerminate := False;
St := '';
Buffer := PChar(St);
BuffEnd := Buffer;
end;

procedure TParseThread.AddString({$IFDEF HL_LAZARUS}const {$ENDIF}S: string);
{Call only when thread is suspended}
var
  Space: integer;
begin
Space := Buffer - PChar(St);
St := St + S;
Buffer := PChar(St) + Space;
BuffEnd := PChar(St) + Length(St);
end;

procedure TParseThread.Execute;
begin
if Text then
  Parser.HTMLParseTextString(ASectionList, ANameList)
else
  Parser.HTMLParseString(ASectionList, ANameList, AIncludeEvent, ASoundEvent, AMetaEvent);
ReturnValue := 0;
Done := True;
end;

procedure TParseThread.Synchronize(Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;

destructor TParseThread.Destroy;
begin
  inherited;
end;

{ TFakeThread }

procedure TFakeThread.CallOnTerminate;
begin
  if Assigned(OnTerminate) then OnTerminate(Self);
end;

function TFakeThread.GetPriority: TThreadPriority;
begin
  Result:=FPriority;
end;

procedure TFakeThread.SetPriority(Value: TThreadPriority);
begin
  FPriority:=Value;
end;

procedure TFakeThread.SetSuspended(Value: Boolean);
begin
  FSuspended:=Value;
end;

procedure TFakeThread.DoExecute;
begin
  FSuspended:=false;
  Execute;
  FSuspended:=true;
  DoTerminate;
end;

procedure TFakeThread.DoTerminate;
begin
writeln('TFakeThread.DoTerminate ',HexStr(Cardinal(Self),8));
  CallOnTerminate;
  FFinished:=true;
  if FFreeOnTerminate then Free;
  FTerminated:=true;
end;

procedure TFakeThread.Synchronize(Method: TThreadMethod);
begin
writeln('TFakeThread.Synchronize ',HexStr(Cardinal(Self),8));
  Method();
end;

constructor TFakeThread.Create(CreateSuspended: Boolean);
begin
writeln('TFakeThread.Create CreateSuspended=',CreateSuspended,' ',HexStr(Cardinal(Self),8));
  inherited Create;
  if CreateSuspended then Suspend else DoExecute;
end;

destructor TFakeThread.Destroy;
begin
writeln('TFakeThread.Destroy ',HexStr(Cardinal(Self),8));
  if not FFinished and not Suspended then
   begin
     Terminate;
     WaitFor;
   end;
  inherited Destroy;
end;

procedure TFakeThread.Resume;
begin
writeln('TFakeThread.Resume Self=',HexStr(Cardinal(Self),8));
  DoExecute;
end;

procedure TFakeThread.Suspend;
begin
writeln('TFakeThread.Suspend Self=',HexStr(Cardinal(Self),8));
  FSuspended:=true;
end;

procedure TFakeThread.Terminate;
begin
writeln('TFakeThread.Terminate Self=',HexStr(Cardinal(Self),8));
  DoTerminate;
end;

function TFakeThread.WaitFor: Integer;
begin
writeln('TFakeThread.WaitFor Self=',HexStr(Cardinal(Self),8));
  Result:=0;
end;

end.

