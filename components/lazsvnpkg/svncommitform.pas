unit SVNCommitForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Process, LCLProc;

type

  { TSVNCommitFrm }

  TSVNCommitFrm = class(TForm)
    ButtonPanel: TButtonPanel;
    SVNCommitMemo: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FSVNCommandLine: string;
  public
    { public declarations }
    property SVNCommandLine: string read FSVNCommandLine write FSVNCommandLine;
  end; 

procedure ShowSVNCommitFrm(ACmdLine: string);

implementation

uses
  SVNClasses;

procedure ShowSVNCommitFrm(ACmdLine: string);
var
  SVNCommitFrm: TSVNCommitFrm;
begin
  SVNCommitFrm := TSVNCommitFrm.Create(nil);

  SVNCommitFrm.SVNCommandLine:=ACmdLine;
  SVNCommitFrm.ShowModal;

  SVNCommitFrm.Free;
end;

{ TSVNCommitFrm }

procedure TSVNCommitFrm.FormShow(Sender: TObject);
var
  AProcess: TProcess;
  M: TMemoryStream;
  BytesRead: LongInt;
  n: LongInt;
begin
  Caption := rsLazarusSVNCommit;

  //commit the checked files
  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := SVNCommandLine;
  debugln('TSVNCommitFrm.FormShow CommandLine ' + AProcess.CommandLine);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.Execute;

  M := TMemoryStream.Create;
  BytesRead := 0;

  while AProcess.Running do
  begin
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n)
    else
      // no data, wait 100 ms
      Sleep(100);
  end;
  // read last part
  repeat
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);
    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n);
  until n <= 0;
  M.SetSize(BytesRead);

  SVNCommitMemo.Lines.LoadFromStream(M);
  M.Free;
end;

initialization
  {$I svncommitform.lrs}

end.

