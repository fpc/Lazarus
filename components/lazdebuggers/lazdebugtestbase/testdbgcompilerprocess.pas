unit TestDbgCompilerProcess;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, UTF8Process, LazLoggerBase, FileUtil, LazFileUtils,
  TestOutputLogger, process, fpcunit;

type

  { TCompilerProcess }

  TCompilerProcess = object
  private
    FCommandLine: string;
    FCompilerOutput: string;
    FDidRunCompiler: Boolean;
    FLastError: Integer;
    function CheckOutputFileOnDisk(AnExeName, ACommandLine: String): boolean;
    function CheckOutputDirOnDisk(AnDirName, ACommandLine: String): boolean;
    procedure MaybeDeleteFileOnDisk(AnExeName: String);
    procedure MaybeDeleteDirOnDisk(AnDirName: String);
    function ReadOutput(AProcess:TProcessUTF8): TStringList;
    function CallCompiler(const ACommand, ACurDir: String): Boolean;
  public
    function TestCompile(const FpcExe, FpcOpts, PascalPrgFile, ExeName: string; ForceReCompile: Boolean = False): boolean;
    function TestCompileUnits(const FpcExe, FpcOpts, SrcDirName, OutLibName: string; ForceReCompile: Boolean = False): Boolean;

    property DidRunCompiler: Boolean read FDidRunCompiler;
    property LastError: Integer read FLastError;
    property CommandLine: string read FCommandLine;
    property CompilerOutput: string read FCompilerOutput;
  end;

  procedure ClearFilesAndDirs;

implementation

type

  { TCreatedExecutable }

  TCreatedExecutable = record
    FExeName: string;
    FComandline: string;
    class operator = (a, b: TCreatedExecutable): Boolean;
  end;

  { TCreatedExecutableList }

  TCreatedExecutableList = class(specialize TFPGList<TCreatedExecutable>)
  private
    function IndexOfExe(AnExeName: String): Integer;
  protected
    procedure DeleteFiles;
    procedure DeleteDirectories;
  public
    procedure AddExe(AnExeName, ACommandLine: String);
    procedure RemoveExe(AnExeName: String);
    function HasExe(AnExeName: String): String;
  end;

var
  CreatedExecutableList: TCreatedExecutableList;
  CreatedLibDirList: TCreatedExecutableList;

class operator TCreatedExecutable. = (a, b: TCreatedExecutable): Boolean;
begin
  raise Exception.Create('unreachable'); // should never enter here
  Result := False;
end;

{ TCreatedExecutableList }

function TCreatedExecutableList.IndexOfExe(AnExeName: String): Integer;
begin
  Result := Count - 1;
  while Result >= 0 do begin
    if Items[Result].FExeName = AnExeName then
      break;
    Dec(Result);
  end;
end;

procedure TCreatedExecutableList.DeleteFiles;
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    if not DeleteFile(Items[i].FExeName) then
      TestLogger.DebugLn(['Failed to delete file ', Items[i].FExeName, ' err: ', GetLastOSError]);
    Dec(i);
  end;
  Clear;
end;

procedure TCreatedExecutableList.DeleteDirectories;
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    DeleteDirectory(Items[i].FExeName, False);
    Dec(i);
  end;
  Clear;
end;

procedure TCreatedExecutableList.AddExe(AnExeName, ACommandLine: String);
var
  i: Integer;
begin
  i := IndexOfExe(AnExeName);
  if i < 0 then begin
    i := Count;
    Count := i + 1;
  end;
  List^[i].FExeName := AnExeName;
  List^[i].FComandline := ACommandLine;
end;

procedure TCreatedExecutableList.RemoveExe(AnExeName: String);
var
  i: Integer;
begin
  i := IndexOfExe(AnExeName);
  if i >= 0 then
    Delete(i);
end;

function TCreatedExecutableList.HasExe(AnExeName: String): String;
var
  i: Integer;
begin
  Result := '';
  i := IndexOfExe(AnExeName);
  if i >= 0 then
    Result := Items[i].FExeName;
end;

{ TCompilerProcess }

function TCompilerProcess.CheckOutputFileOnDisk(AnExeName, ACommandLine: String
  ): boolean;
var
  LastCmdLine: String;
begin
  Result := False;
  LastCmdLine := CreatedExecutableList.HasExe(AnExeName);
  if LastCmdLine = '' then begin
    // should not yet exist
    if FileExists(AnExeName) then
      raise EAssertionFailedError.Create('Unexpected pre-existing compiled exe ' + AnExeName);
  end
  else if LastCmdLine = ACommandLine then  begin
    // existing file can be used
    if not FileExists(AnExeName) then begin
      DebugLn('Expected file from last build, but was missing: ' + AnExeName);
      CreatedExecutableList.RemoveExe(AnExeName);
      exit;
    end;
    Result := True;
  end
  else begin
    // need rebuild
    DebugLn(['Rebuilding: ', AnExeName]);
    DeleteFile(AnExeName);
    CreatedExecutableList.RemoveExe(AnExeName);
    if FileExists(AnExeName) then
      raise EAssertionFailedError.Create('Unable to delete old file ' + AnExeName);
  end;
end;

procedure TCompilerProcess.MaybeDeleteFileOnDisk(AnExeName: String);
begin
  if CreatedExecutableList.IndexOfExe(AnExeName) >= 0 then begin
    if not FileExists(AnExeName) then
      DebugLn('Expected file from last build, but was missing: ' + AnExeName)
    else
      DeleteFile(AnExeName);
    CreatedExecutableList.RemoveExe(AnExeName);
  end;

  if FileExists(AnExeName) then
    raise EAssertionFailedError.Create('Unexpected pre-existing compiled exe ' + AnExeName);
end;

procedure TCompilerProcess.MaybeDeleteDirOnDisk(AnDirName: String);
begin
  if CreatedLibDirList.IndexOfExe(AnDirName) >= 0 then begin
    if not DirectoryExists(AnDirName) then
      DebugLn('Expected dir from last build, but was missing: ' + AnDirName)
    else
      DeleteDirectory(AnDirName, False);
    CreatedLibDirList.RemoveExe(AnDirName);
  end;

  if DirectoryExists(AnDirName) then
    raise EAssertionFailedError.Create('Unexpected pre-existing compiled exe ' + AnDirName);
end;

function TCompilerProcess.CheckOutputDirOnDisk(AnDirName, ACommandLine: String
  ): boolean;
var
  LastCmdLine: String;
begin
  Result := False;
  LastCmdLine := CreatedLibDirList.HasExe(AnDirName);
  if LastCmdLine = '' then begin
    // should not yet exist
    if DirectoryExists(AnDirName) then
      raise EAssertionFailedError.Create('Unexpected pre-existing compiled dir ' + AnDirName);
  end
  else if LastCmdLine = ACommandLine then  begin
    // existing dir can be used
    if not DirectoryExists(AnDirName) then begin
      DebugLn('Expected dir from last build, but was missing: ' + AnDirName);
      CreatedLibDirList.RemoveExe(AnDirName);
      exit;
    end;
    Result := True;
  end
  else begin
    // need rebuild
    DebugLn(['Rebuilding(dir): ', AnDirName]);
    DeleteDirectory(AnDirName, False);
    CreatedLibDirList.RemoveExe(AnDirName);
    if DirectoryExists(AnDirName) then
      raise EAssertionFailedError.Create('Unable to delete old dir ' + AnDirName);
  end;
end;

function TCompilerProcess.ReadOutput(AProcess: TProcessUTF8): TStringList;
const
  TIME_OUT = 300;
  READ_BYTES = 8192;
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

function TCompilerProcess.CallCompiler(const ACommand, ACurDir: String
  ): Boolean;
var
  FpcBuild: TProcessUTF8;
  OutputLines: TStrings;
begin
  FpcBuild := TProcessUTF8.Create(nil);
  OutputLines := nil;
  FLastError := -1;
  try
    {$IFDEF windows}
    FpcBuild.Options := [poNewConsole, poUsePipes];
    {$ELSE}
    FpcBuild.Options := [poNoConsole, poUsePipes];
    {$ENDIF}
    FpcBuild.ShowWindow := swoHIDE;

    DebugLn(['**** running compiler: ', ACommand]);
    FCommandLine := ACommand;

    FpcBuild.CommandLine := ACommand;
    FpcBuild.CurrentDirectory := ACurDir;
    FpcBuild.PipeBufferSize:=64*1024;
    FpcBuild.Execute;

    OutputLines := ReadOutput(FpcBuild);
    if FpcBuild.Running then begin
      FpcBuild.Terminate(99);
    end;

    FLastError := FpcBuild.ExitStatus;
    FCompilerOutput := OutputLines.Text;
  finally
    FpcBuild.Free;
    OutputLines.Free;
  end;
  Result := FLastError = 0;
  if not Result then begin
    DebugLn(['**** compile error: ', FCompilerOutput, '  FLastError: ', FLastError, ' OS: ',GetLastOSError, ' ACurDir:', ACurDir]);
    TestLogger.DebugLn(['**** compile error: ', FCompilerOutput, '  FLastError: ', FLastError, ' OS: ',GetLastOSError, ' ACurDir:', ACurDir]);
  end;
end;

function TCompilerProcess.TestCompile(const FpcExe, FpcOpts, PascalPrgFile,
  ExeName: string; ForceReCompile: Boolean): boolean;
var
  CmdLine: string;
begin
  CmdLine := FpcExe + ' -B -MObjFPC  -FUlib -o'+ ExeName + ' ' + FpcOpts + ' ' + PascalPrgFile;

  if ForceReCompile then begin
    Result := False;
    MaybeDeleteFileOnDisk(ExeName);
  end
  else
    Result := CheckOutputFileOnDisk(ExeName, CmdLine);

  FDidRunCompiler := not Result;
  if Result then
    exit;

  Result := CallCompiler(CmdLine, ExtractFileDir(PascalPrgFile));

  if FileExists(ExeName) then
    CreatedExecutableList.AddExe(ExeName, CmdLine)
  else
    Result := False;
end;

function TCompilerProcess.TestCompileUnits(const FpcExe, FpcOpts, SrcDirName,
  OutLibName: string; ForceReCompile: Boolean): Boolean;
var
  CmdLine: string;
begin
  CmdLine := FpcExe + ' -MObjFPC  -FU' + OutLibName + ' ' + FpcOpts + ' ' + SrcDirName;

  if ForceReCompile then begin
    Result := False;
    MaybeDeleteDirOnDisk(OutLibName);
  end
  else
    Result := CheckOutputDirOnDisk(OutLibName, CmdLine);

  FDidRunCompiler := not Result;
  if Result then
    exit;

  CreateDirUTF8(OutLibName);
  Result := CallCompiler(CmdLine, ExtractFileDir(SrcDirName));

  if DirectoryExists(AppendPathDelim(OutLibName)) then
    CreatedLibDirList.AddExe(OutLibName, CmdLine)
  else
    Result := False;
end;


procedure ClearFilesAndDirs;
begin
  CreatedExecutableList.DeleteFiles;
  CreatedLibDirList.DeleteDirectories;
end;

initialization
  CreatedExecutableList := TCreatedExecutableList.Create;
  CreatedLibDirList := TCreatedExecutableList.Create;

finalization
  ClearFilesAndDirs;
  CreatedExecutableList.Free;
  CreatedLibDirList.Free;

end.

