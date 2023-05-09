unit LazDelphiReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEExternToolIntf, RegExpr;

const
  SubToolDelphi = 'Delphi';
  SubToolDelphiPriority = SubToolFPCPriority-10;

type

  { TDelphiCompilerParser }

  TDelphiCompilerParser = class(TExtToolParser)
  private
  protected
    FRegExprFilenameLineIDMsg: TRegExpr;
    FRegExprFilenameLineUrgencyIDMsg: TRegExpr;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadLine(Line: string; OutputIndex: integer; IsStdErr: boolean;
      var Handled: boolean); override; // (worker thread)
    class function DefaultSubTool: string; override;
    class function GetParserName: string; override;
    class function GetLocalizedParserName: string; override;
    class function Priority: integer; override;
  end;
  TDelphiCompilerParserClass = class of TDelphiCompilerParser;

var
  IDEDelphiCompilerParserClass: TDelphiCompilerParserClass = nil;

procedure Register;

implementation

procedure Register;
begin
  ExternalToolList.RegisterParser(TDelphiCompilerParser);
end;

{ TDelphiCompilerParser }

constructor TDelphiCompilerParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // filename(linenumber): E2003 Undeclared identifier: 'foo'
  FRegExprFilenameLineIDMsg:=TRegExpr.Create;
  FRegExprFilenameLineIDMsg.ModifierStr:='I';
  FRegExprFilenameLineIDMsg.Expression:='^(.*)\(([0-9]+)\): ([HNWEF])([0-9]+) (.*)$';

  // filename(linenumber): Fatal: F2613 Unit 'Unit3' not found.
  FRegExprFilenameLineUrgencyIDMsg:=TRegExpr.Create;
  FRegExprFilenameLineUrgencyIDMsg.ModifierStr:='I';
  FRegExprFilenameLineUrgencyIDMsg.Expression:='^(.*)\(([0-9]+)\) ([a-zA-Z]+): ([HNWEF])([0-9]+) (.*)$';
end;

destructor TDelphiCompilerParser.Destroy;
begin
  FreeAndNil(FRegExprFilenameLineIDMsg);
  FreeAndNil(FRegExprFilenameLineUrgencyIDMsg);
  inherited Destroy;
end;

procedure TDelphiCompilerParser.ReadLine(Line: string; OutputIndex: integer;
  IsStdErr: boolean; var Handled: boolean);

  procedure Add(const aFilename, LineNoStr, UrgencyLetter, IDStr, MsgStr: String);
  var
    MsgLine: TMessageLine;
  begin
    MsgLine:=CreateMsgLine(OutputIndex);
    case UrgencyLetter of
    'H': MsgLine.Urgency:=mluHint;
    'N': MsgLine.Urgency:=mluNote;
    'W': MsgLine.Urgency:=mluWarning;
    'E': MsgLine.Urgency:=mluError;
    'F': MsgLine.Urgency:=mluFatal;
    else MsgLine.Urgency:=mluImportant;
    end;
    MsgLine.Filename:=aFilename;
    MsgLine.Line:=StrToIntDef(LineNoStr,0);
    MsgLine.MsgID:=StrToIntDef(IDStr,0);
    MsgLine.Msg:=MsgStr;
    if IsStdErr then
      MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
    AddMsgLine(MsgLine);
  end;

  procedure AddFilenameLineIDMsg;
  var
    RE: TRegExpr;
    aFilename, LineNoStr, UrgencyLetter, IDStr, MsgStr: String;
  begin
    RE:=FRegExprFilenameLineIDMsg;
    aFilename:=RE.Match[1];
    LineNoStr:=RE.Match[2];
    UrgencyLetter:=RE.Match[3];
    IDStr:=RE.Match[4];
    MsgStr:=RE.Match[5];
    Add(aFilename,LineNoStr,UrgencyLetter,IDStr,MsgStr);
  end;

  procedure AddFilenameLineUrgencyIDMsg;
  var
    RE: TRegExpr;
    aFilename, LineNoStr, UrgencyLetter, IDStr, MsgStr: String;
  begin
    RE:=FRegExprFilenameLineUrgencyIDMsg;
    aFilename:=RE.Match[1];
    LineNoStr:=RE.Match[2];
    //UrgencyStr:=RE.Match[3];
    UrgencyLetter:=RE.Match[4];
    IDStr:=RE.Match[5];
    MsgStr:=RE.Match[6];
    Add(aFilename,LineNoStr,UrgencyLetter,IDStr,MsgStr);
  end;

  procedure AddOtherLine;
  var
    MsgLine: TMessageLine;
  begin
    MsgLine:=CreateMsgLine(OutputIndex);
    MsgLine.MsgID:=0;
    MsgLine.SubTool:=SubToolDelphi;
    if MsgLine.Msg<>'' then
      MsgLine.Urgency:=mluImportant
    else
      MsgLine.Urgency:=mluVerbose2;
    if IsStdErr then
      MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
    AddMsgLine(MsgLine);
  end;

begin
  FRegExprFilenameLineIDMsg.InputString:=Line;
  if FRegExprFilenameLineIDMsg.ExecPos(1) then
  begin
    AddFilenameLineIDMsg;
    exit;
  end;

  FRegExprFilenameLineUrgencyIDMsg.InputString:=Line;
  if FRegExprFilenameLineUrgencyIDMsg.ExecPos(1) then
  begin
    AddFilenameLineUrgencyIDMsg;
    exit;
  end;

  AddOtherLine;

  Handled:=true;
end;

class function TDelphiCompilerParser.DefaultSubTool: string;
begin
  Result:='DCC';
end;

class function TDelphiCompilerParser.GetParserName: string;
begin
  Result:='Delphi Compiler';
end;

class function TDelphiCompilerParser.GetLocalizedParserName: string;
begin
  Result:='Delphi Compiler';
end;

class function TDelphiCompilerParser.Priority: integer;
begin
  Result:=SubToolDelphiPriority;
end;

end.

