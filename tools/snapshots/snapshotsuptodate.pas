unit SnapshotsUptodate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpcunit, testregistry, ftplister;

type

  { TFtpMonitorTestcase }

  TFtpMonitorTestcase= class(TTestCase)
  private
    FFileMask: string;
    FName: string;
    FMatchingFiles: TStrings;
    function GetFiles: TStrings;
    function GetMatchingFiles: TStrings;
    property Files : TStrings read GetFiles;
    property MatchingFiles: TStrings read GetMatchingFiles;
  public
    class function FileMaskSuite(const AName, AFileMask: string): TTestSuite;
    constructor Create(const AName, AFileMask, ATestName: string); reintroduce;
    destructor Destroy; override;
  published
    procedure TestHookUp; 
    procedure NumberOfFiles;
    procedure IsUptodateTest;
  end;

implementation

uses
  monitorcfg, masks;

var
  FileList: TStrings;
  
function TFtpMonitorTestcase.GetFiles: TStrings;
var
  FtpLister : TFTPLister;
begin
  if FileList=nil then begin
    FTPLister := TFTPLister.Create;
    FileList := FTPLister.GetList('ftp.hu.freepascal.org', '/pub/lazarus');
    FTPLister.Free;
  end;
  Result := FileList;
end;

function TFtpMonitorTestcase.GetMatchingFiles: TStrings;
var
  i: integer;
  FileMask: TMask;
  FtpFile: TFtpFile;
begin
  if not assigned(FMatchingFiles) then begin
    FileMask := TMask.Create(FFileMask);
    FMatchingFiles := TStringList.Create;
    for i := 0 to Files.Count-1 do
    begin
      FtpFile := TFtpFile.Create(Files[i]);
      if FileMask.Matches(FtpFile.FileName) then
        FMatchingFiles.AddObject(FtpFile.FileName, FtpFile);
    end;
    FileMask.Free;
  end;
  Result := FMatchingFiles;
end;

class function TFtpMonitorTestcase.FileMaskSuite(const AName, AFileMask: string
  ): TTestSuite;
begin
  Result := TTestSuite.Create(AName);
  Result.AddTest(Create(AName, AFileMask, 'NumberOfFiles'));
  Result.AddTest(Create(AName, AFileMask, 'IsUptodateTest'));
end;

constructor TFtpMonitorTestcase.Create(const AName, AFileMask, ATestName: string);
begin
  inherited CreateWithName(ATestName);
  FName := AName;
  FFileMask := AFileMask;
end;

destructor TFtpMonitorTestcase.Destroy;
var
  i: integer;
begin
  if assigned(FMatchingFiles) then begin
    for i := 0 to FMatchingFiles.Count-1 do
      FMatchingFiles.Objects[i].Free;
    FMatchingFiles.Free;
  end;
  inherited Destroy;
end;

procedure TFtpMonitorTestcase.TestHookUp;
begin
  AssertTrue('No files retrieved', Files.Count>0);
end;

procedure TFtpMonitorTestcase.NumberOfFiles;
begin
  AssertEquals('Wrong number of file matches for '+ FName, 1, MatchingFiles.Count);
end;

procedure TFtpMonitorTestcase.IsUptodateTest;
var
  FtpFile: TFtpFile;
begin
  AssertTrue(MatchingFiles.Count>0);
  FtpFile := TFtpFile(MatchingFiles.Objects[0]);
  AssertTrue(
    FtpFile.FileName +' is too old: ' + DateTimeToStr(FtpFile.FileDate),
    (Now - FtpFile.FileDate) < (26/24));
end;

procedure InitFromXml;
var
  MonitorConfig: TMonitorConfig;
  ServerIdx: integer;
  
  function CreateFileTestSuite(AFile: TFile) : TTestSuite;
  begin
    with AFile do
      Result := TFtpMonitorTestcase.FileMaskSuite(Description, Mask);
  end;

  function CreateFtpServerTestSuite(Server: TServer): TTestSuite;
  var
    FileIdx: integer;
  begin
    Result := TTestSuite.Create(Server.Description);
    for FileIdx := 0 to Server.FileCount-1 do
      Result.AddTest(CreateFileTestSuite(Server.Files[FileIdx]));
  end;
  
begin
  MonitorConfig := TMonitorConfig.Create;
  try
    MonitorConfig.Load(ExtractFilePath(ParamStrUTF8(0)) + 'monitorconfig.xml');
    for ServerIdx := 0 to MonitorConfig.ServerCount-1 do
      GetTestRegistry.AddTest(
        CreateFtpServerTestSuite(MonitorConfig.Servers[ServerIdx]));
  finally
    MonitorConfig.Free;
  end;
end;

initialization
  FileList := nil;
  InitFromXml;
finalization
  FreeAndNil(FileList);
end.

