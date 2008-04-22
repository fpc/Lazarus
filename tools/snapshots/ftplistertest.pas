unit ftplistertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ftplister,
  fpcunit, testregistry;

  { TFtpListerTestcase }
type
  TFtpListerTestcase= class(TTestCase)
  private
    function GetFiles: TStrings;
    property Files : TStrings read GetFiles;
  published
    procedure TestHookUp;
    procedure TestFtpFile;
    procedure TestOldFtpFile;
  end;

implementation

var
  FileList: TStrings;

function TFtpListerTestcase.GetFiles: TStrings;
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

procedure TFtpListerTestcase.TestHookUp;
begin
  AssertTrue('No files retrieved', Files.Count>0);
end;

procedure TFtpListerTestcase.TestFtpFile;
var
  FtpFile : TFtpFile;
const
  FtpLine = '-rw-r--r--    1 1003     100      43560307 Oct 22 14:34 Lazarus-0.9.19-fpc-2.0.4-20061022-win32.exe';
begin
  FtpFile := TFtpFile.Create(FtpLine);
  try
    AssertEquals('Wrong file name:', 'Lazarus-0.9.19-fpc-2.0.4-20061022-win32.exe', FtpFile.FileName);
    AssertEquals('Wrong file date', '22-10-2008 14:34:00', DateTimeToStr(FtpFile.FileDate));
  finally
    FtpFile.Free;
  end;
end;

procedure TFtpListerTestcase.TestOldFtpFile;
var
  FtpFile : TFtpFile;
const
  FtpLine = '-rw-r--r--    1 1003     100      52533164 Jul 07  2006 lazarus-arm-wince-20060707.7z';
begin
  FtpFile := TFtpFile.Create(FtpLine);
  try
    AssertEquals('Wrong file date', '07-07-2006', DateTimeToStr(FtpFile.FileDate));
  finally
    FtpFile.Free;
  end;
end;

initialization
  RegisterTest(TFtpListerTestcase);
  
finalization
  FileList.Free;
  
end.

