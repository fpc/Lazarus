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
  TParseThread = class(TThread)
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
    procedure AddString(S: string);
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

procedure TParseThread.AddString(S: string);
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

end.

