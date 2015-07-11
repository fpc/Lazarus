unit TestSvnCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  SvnCommand, svnclasses;

type

  { TTestSvnCommand }

  TTestSvnCommand= class(TTestCase)
  published
    procedure TestHookUp; 
    procedure TestGetInfo;
  end; 

implementation

procedure TTestSvnCommand.TestHookUp; 
var
  SvnExitCode : integer;
  XmlOutput: TMemoryStream;
begin
  try
    XmlOutput:= TMemoryStream.Create;
    SvnExitCode := ExecuteSvnCommand('log --xml -rHEAD', XmlOutput);
    AssertEquals('Unexpected exit code', 0, SvnExitCode);
    AssertTrue('No XmlOutput', XmlOutput.Size>0)
  finally
    XmlOutput.Free;
  end;
end;

procedure TTestSvnCommand.TestGetInfo;
var
  SvnExitCode : integer;
  XmlOutput: TMemoryStream;
  SvnInfo: TSvnInfo;
begin
  try
    XmlOutput:= TMemoryStream.Create;
    SvnExitCode := ExecuteSvnCommand('info --xml .', XmlOutput);
    AssertEquals('Unexpected exit code', 0, SvnExitCode);
    AssertTrue('No XmlOuput', XmlOutput.Size>0);
    SvnInfo := TSvnInfo.Create;
    try
      XmlOutput.Position := 0;
      SvnInfo.LoadFromStream(XmlOutput);
      AssertEquals('Wrong repository UUID',
        '8e941d3f-bd1b-0410-a28a-d453659cc2b4',
        SvnInfo.Entry.Repository.UUID);
    finally
      SvnInfo.Free;
    end;
  finally
    XmlOutput.Free;
  end;
end;

initialization

  RegisterTest(TTestSvnCommand); 
end.

