program restserver;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}  cwstring, {$endif}
  sysutils, fphttpapp, dmRestBridge, custApp, fpwebfile;

Type

  { TRestHTTPApplication }

  TRestHTTPApplication = class(THTTPApplication)
  Public
    Procedure Usage(Msg : String);
    Procedure DoRun; override;
  end;

{ TRestHTTPApplication }

procedure TRestHTTPApplication.Usage(Msg: String);
begin
  if (Msg<>'') then
    Writeln('Error : ',msg);
  Writeln('Usage : ',ChangeFileExt(ExtractFileName(ParamStr(0)),''), '[Options]');
  Writeln('Where options is one or more of');
  Writeln('-h --help             this message');
  Writeln('-c --connection=file  File with connection definitions');
  Writeln('-f --files[=Dir]      Serve files from directory');
  Writeln('-i --ini=file         File with dispatched settings');
  Writeln('-p --port=Num         Listen on port');
  Writeln('-q --quiet            do not log anything (overrides .ini settings)');
  {$IFNDEF VER3_0}
    Writeln('-s --ssl              Use SSL protocol');
  {$Endif}
  Writeln('-v --verbose          Log more (includes SQL logging)');
  ExitCode:=Ord(Msg<>'');
end;

procedure TRestHTTPApplication.DoRun;

Var
  S,aDir,Header : String;

begin
  Application.Title:='restserver';
  S:=CheckOptions('hb:c:i:p:vqf::',['help','base:','connection:','ini:','quiet','verbose','port:','file::']);
  if (S<>'') or HasOption('h','help') then
    begin
    Usage(S);
    Terminate;
    Exit;
    end;
  Port:=StrToIntDef(GetOptionvalue('p','port'),8080);
  Header:='Started REST server. listening on port: '+intToStr(Port);
  {$IFNDEF VER3_0}
  UseSSL:=Hasoption('s','ssl');
  if UseSSL then
    Header:=Header+'; Using SSL';
  {$Endif}
  if HasOption('f','file') then
     begin
     aDir:=GetOptionValue('f','file');
     if aDir='' then
       aDir:=ExtractFilePath(ParamStr(0))
     else
       ADir:=IncludeTrailingPathDelimiter(ADir);
     {$IFNDEF VER3_0}
     TSimpleFileModule.BaseDir:=aDir;
     TSimpleFileModule.RegisterDefaultRoute;
     {$else}
     RegisterFileLocation('files',aDir);
     {$Endif}
     Header:=Header+'; Serving files from: '+aDir;
     end;
  If IsConsole then
    Writeln(Header)
  else
    Log(etInfo,Header);
  inherited DoRun;
end;

begin
  FreeAndNil(Application);
  Application:=TRestHTTPApplication.Create(Nil);
  CustomApplication:=Application;
  Application.CreateForm(TRestDataModule,RestDataModule);
  Application.Initialize;
  Application.Run;
end.

