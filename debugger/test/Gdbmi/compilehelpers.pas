unit CompileHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, UTF8Process, LCLProc;

type

  { TCompileHelper }

  TCompileHelper = class
  private
    FCommandLine: string;
    FLastError: String;
  public
    function TestCompile(const PrgName, FpcOpts, ExeName, FpcExe: string): String;
    function TestCompileUnits(const FpcExe, FpcOpts, SrcDirName, OutLibName: string): Boolean;
    property LastError: String read FLastError;
    property CommandLine: string read FCommandLine;
  end;

var CompileHelper: TCompileHelper;



implementation

function ReadOutput(AProcess:TProcess): TStringList;
const
  TIME_OUT = 300;
  READ_BYTES = 1024;
var
  BytesRead: Integer;
  n: Integer;
  EndTime: TDateTime;
  OutputStream: TMemoryStream;
begin
  OutputStream := TMemoryStream.Create;
  BytesRead := 0;
  EndTime := Now + TIME_OUT / (24 * 60 * 60);
  while AProcess.Running and (Now<EndTime) do
  begin
    // make sure we have room
    OutputStream.SetSize(BytesRead + READ_BYTES);

    // try reading it
    if AProcess.Output.NumBytesAvailable>0 then begin
      n := AProcess.Output.Read((OutputStream.Memory + BytesRead)^, READ_BYTES);
      Inc(BytesRead, n)
    end
    else
      // no data, wait 100 ms
      Sleep(100);
  end;
  // read last part
  repeat
    // make sure we have room
    OutputStream.SetSize(BytesRead + READ_BYTES);
    // try reading it
    if AProcess.Output.NumBytesAvailable>0 then begin
      n := AProcess.Output.Read((OutputStream.Memory + BytesRead)^, READ_BYTES);
      Inc(BytesRead, n);
    end
    else
      n := 0;
  until n <= 0;
  OutputStream.SetSize(BytesRead);
  OutputStream.Position:=0;
  Result := TStringList.Create;
  Result.LoadFromStream(OutputStream);
  OutputStream.Free;
end;


{ TCompileHelper }

function TCompileHelper.TestCompile(const PrgName, FpcOpts, ExeName, FpcExe: string): String;
var
  FpcBuild: TProcessUTF8;
  OutputLines: TStrings;
  CmdLine: string;
begin
  Result := 'Error';
  FpcBuild := TProcessUTF8.Create(nil);
  OutputLines := nil;
  try
    {$IFDEF windows}
    FpcBuild.Options := [poNewConsole, poUsePipes];
    {$ELSE}
    FpcBuild.Options := [poNoConsole, poUsePipes];
    {$ENDIF}
    FpcBuild.ShowWindow := swoHIDE;

    CmdLine := FpcExe + ' -MObjFPC  -FUlib -o'+ ExeName + ' ' + FpcOpts + ' ' + PrgName;
    debugln(['**** running compiler: ', CmdLine]);
    FpcBuild.CommandLine := CmdLine;
    FCommandLine := CmdLine;

    FpcBuild.CurrentDirectory := ExtractFileDir(PrgName);

    FpcBuild.Execute;
    OutputLines := ReadOutput(FpcBuild);
    if FpcBuild.Running then begin
      FpcBuild.Terminate(99);
    end;
    if FpcBuild.ExitStatus = 0 then
      Result := ''
    else
      Result := Result + LineEnding + OutputLines.Text;
  finally
    FpcBuild.Free;
    OutputLines.Free;
  end;
end;

function TCompileHelper.TestCompileUnits(const FpcExe, FpcOpts, SrcDirName,
  OutLibName: string): Boolean;
var
  FpcBuild: TProcessUTF8;
  OutputLines: TStrings;
  CmdLine: string;
begin
  Result := False;
  FpcBuild := TProcessUTF8.Create(nil);
  OutputLines := nil;
  try
    {$IFDEF windows}
    FpcBuild.Options := [poNewConsole, poUsePipes];
    {$ELSE}
    FpcBuild.Options := [poNoConsole, poUsePipes];
    {$ENDIF}
    FpcBuild.ShowWindow := swoHIDE;

    CmdLine := FpcExe + ' -MObjFPC  -FU' + OutLibName + ' ' + FpcOpts + ' ' + SrcDirName;
    debugln(['**** running compiler: ', CmdLine]);
    FpcBuild.CommandLine := CmdLine;
    FCommandLine := CmdLine;

    FpcBuild.CurrentDirectory := ExtractFileDir(SrcDirName);

    FpcBuild.Execute;
    OutputLines := ReadOutput(FpcBuild);
    if FpcBuild.Running then begin
      FpcBuild.Terminate(99);
    end;
    FLastError := OutputLines.Text;
    if FpcBuild.ExitStatus = 0
    then Result := True
    else Result := False;
  finally
    FpcBuild.Free;
    OutputLines.Free;
  end;
end;

initialization
  CompileHelper:= TCompileHelper.Create;

finalization
  FreeAndNil(CompileHelper);

end.

