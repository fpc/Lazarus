program TestNewXMLCfg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, FileProcs, Laz_XMLCfg, Laz2_XMLCfg;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure Test1;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  Test1;

  // stop program loop
  Terminate;
end;

procedure TMyApplication.Test1;

  procedure Test(Filename: string; UseOld: boolean; WriteTo: boolean);
  var
    x1: Laz_XMLCfg.TXMLConfig;
    x2: Laz2_XMLCfg.TXMLConfig;
    i: Integer;

    procedure CheckValue(Path, Value: string);
    var
      NewValue: String;
    begin
      if WriteTo then
        if UseOld then
          x1.SetValue(Path,Value)
        else
          x2.SetValue(Path,Value);
      if UseOld then
        NewValue:=x1.GetValue(Path,'')
      else
        NewValue:=x2.GetValue(Path,'');
      if Value<>NewValue then begin
        debugln(['TMyApplication.Test1 failed UseOld=',UseOld,' WriteTo=',WriteTo]);
        debugln(['OldValue="',dbgstr(Value),'"']);
        debugln(['NewValue="',dbgstr(NewValue),'"']);
      end;
    end;

  begin
    if WriteTo then
      DeleteFileUTF8(Filename);
    if UseOld then
      x1:=Laz_XMLCfg.TXMLConfig.Create(Filename)
    else
      x2:=Laz2_XMLCfg.TXMLConfig.Create(Filename);
    for i:=1 to 130 do begin
      CheckValue('Item'+IntToStr(i)+'/Value',chr(i));
    end;
    CheckValue('AUmlaut/Value','Ä');
    CheckValue('LineBreak/Value','First#10Second#13#10Third');
    if WriteTo then
      if UseOld then
        x1.Flush
      else
        x2.Flush;
    if UseOld then
      x1.Free
    else
      x2.Free;
  end;

var
  Filename: String;
begin

  // write with old
  Filename:='test1.xml';
  Test(Filename,true,true);
  Test(Filename,true,false); // read old with old
  Test(Filename,false,false); // read old with new

  // write with new
  Filename:='test2.xml';
  Test(Filename,false,true);
  Test(Filename,false,false); // read new with new
  Test(Filename,true,false); // read new with old
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

