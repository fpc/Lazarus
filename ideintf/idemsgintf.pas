{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the IDE messages (below the source editor).
}
unit IDEMsgIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LCLProc,
  TextTools, IDECommands, IDEExternToolIntf;
  
type

  { TIDEMessageLine

    The IDE (TOutputFilter) parses each message line.
    If it sees FPC output, it fills the Parts property and set various
    Name=Value pairs.

      Name    | Value
      --------|-----------------------------------------------------------------
      Stage    Indicates what part of the build process the message
               belongs to. Common values are 'FPC', 'Linker' or 'make'
      Type     For FPC: 'Hint', 'Note', 'Warning', 'Error', 'Fatal', 'Panic',
               'Compiling', 'Assembling'
               For make: 'entering directory', 'leaving directory'
               For Linker:
      Line     An integer for the linenumber as given by FPC in brackets.
      Column   An integer for the column as given by FPC in brackets.
      Message  The message text without other parsed items.


    Example:
      Message written by FPC:
        unit1.pas(21,3) Warning: unit buttons not used

      Creates the following lines in Parts:
        Stage=FPC
        Type=Warning
        Filename=/path/unit1.pas
        Line=21
        Column=3
        Message=unit buttons not used
        
    You can access them via:
      if ALine.Parts.Values['Stage']='FPC' then ...
  }

  TIDEMessageLine = class
  private
    FDirectory: string;
    FMsg: string;
    FOriginalIndex: integer;
    FParts: TStrings;
    FPosition: integer;
    FVisible: boolean;
    FVisiblePosition: integer;
    procedure SetDirectory(const AValue: string);
    procedure SetMsg(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetSourcePosition(out Filename: string;
                                out LineNumber, Column: integer);
    procedure SetSourcePosition(const Filename: string;
                                const LineNumber, Column: integer);
    property Msg: string read FMsg write SetMsg;
    property Directory: string read FDirectory write SetDirectory;
    property Position: integer read FPosition write FPosition; // position in all available messages
    property VisiblePosition: integer read FVisiblePosition write FVisiblePosition;// filtered position
    property OriginalIndex: integer read FOriginalIndex write FOriginalIndex;// unsorted, unfiltered position
    property Parts: TStrings read FParts write FParts;
    property Visible: boolean read FVisible write FVisible;
  end;

  TOnFilterLine = procedure(MsgLine: TIDEMessageLine; var Show: boolean) of object;
  
  { TIDEMessageLineList }

  TIDEMessageLineList = class
  private
    FItems: TFPList;
    function GetCount: integer;
    function GetItems(Index: integer): TIDEMessageLine;
    function GetParts(Index: integer): TStrings;
    procedure SetParts(Index: integer; const AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Msg: string): integer;
    function Add(Item: TIDEMessageLine): integer;
    procedure Delete(Index: Integer);
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TIDEMessageLine read GetItems; default;
    property Parts[Index: integer]: TStrings read GetParts write SetParts;
  end;

  TIDEMsgScannerType = class;

  { TIDEMsgScanner }

  TIDEMsgScanner = class(TPersistent)
  private
    FLines: TIDEMessageLineList;
    FScannerType: TIDEMsgScannerType;
    FWorkingDirectory: string;
  protected
    procedure SetWorkingDirectory(const AValue: string);
  public
    constructor Create(TheScannerType: TIDEMsgScannerType;
                       TheLines: TIDEMessageLineList);
    function ParseLine(MsgLine: TIDEMessageLine; var Show: boolean): boolean; virtual; abstract;// true if line was handled
    procedure EndScan; virtual;
    property Lines: TIDEMessageLineList read FLines;
    property ScannerType: TIDEMsgScannerType read FScannerType;
    property WorkingDirectory: string read FWorkingDirectory write SetWorkingDirectory;
  end;

  { TIDEMsgScannerType }

  TIDEMsgScannerType = class(TComponent)
  public
    function ShortDescription: string; virtual; abstract;
    function Description: string; virtual; abstract;
    function StartScan(Lines: TIDEMessageLineList): TIDEMsgScanner; virtual; abstract;
  end;

  { TIDEMsgScanners }

  TIDEMsgScanners = class(TPersistent)
  private
    fScanners: TFPList;// current instances
    fTypes: TFPList;// registered scanner types
    function GetCount: integer;
    function GetItems(Index: integer): TIDEMsgScannerType;
    function GetScannerCount: integer;
    function GetScanners(Index: integer): TIDEMsgScanner;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterType(aType: TIDEMsgScannerType);
    procedure UnregisterType(aType: TIDEMsgScannerType);
    function CreateUniqueName(const aName: string): string;
    function IndexOfName(const aTypeName: string): integer;
    function TypeOfName(const aTypeName: string): TIDEMsgScannerType;
    function IndexOfShortDesc(const ShortDescription: string): integer;
    function TypeOfShortDesc(const ShortDescription: string): TIDEMsgScannerType;
    property Count: integer read GetCount;
    property Items[Index: integer]: TIDEMsgScannerType read GetItems; default;
    property ScannerCount: integer read GetScannerCount;
    property Scanners[Index: integer]: TIDEMsgScanner read GetScanners;
  end;

  { TIDEMsgQuickFixItem }
  
  TIMQuickFixStep = (
    imqfoMenuItem,      // Popup menu opens. Add now the menu item.
    imqfoImproveMessage,// Message can now be rewritten/beautified.
    imqfoJump           // Override what happens, when user clicks on message.
    );
  TIMQuickFixSteps = set of TIMQuickFixStep;

  TIMQFExecuteMethod = procedure(Sender: TObject; Step: TIMQuickFixStep;
                                 Msg: TIDEMessageLine) of object;
  TIMQFExecuteProc = procedure(Sender: TObject; Step: TIMQuickFixStep;
                               Msg: TIDEMessageLine);

  TIDEMsgQuickFixItem = class(TPersistent)
  private
    FCaption: string;
    FName: string;
    FOnExecuteMethod: TIMQFExecuteMethod;
    FOnExecuteProc: TIMQFExecuteProc;
    FRegExpression: string;
    FRegExprModifiers: string;
    FSteps: TIMQuickFixSteps;
    function GetCaption: string;
    procedure SetCaption(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetRegExpression(const AValue: string);
    procedure SetRegExprModifiers(const AValue: string);
  public
    constructor Create;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); virtual;
    function IsApplicable(Line: TIDEMessageLine): boolean; virtual;
  public
    property Name: string read FName write SetName;
    property Caption: string read GetCaption write SetCaption;
    property RegExpression: string read FRegExpression write SetRegExpression;
    property RegExprModifiers: string read FRegExprModifiers write SetRegExprModifiers;
    property OnExecuteMethod: TIMQFExecuteMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TIMQFExecuteProc read FOnExecuteProc write FOnExecuteProc;
    property Steps: TIMQuickFixSteps read FSteps write FSteps;
  end;
  
  { TIDEMsgQuickFixItems }

  TIDEMsgQuickFixItems = class(TPersistent)
  private
    FItems: TFPList;
    function GetCount: integer;
    function GetItems(Index: integer): TIDEMsgQuickFixItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Item: TIDEMsgQuickFixItem): integer;
    procedure Remove(Item: TIDEMsgQuickFixItem);
    function IndexOfName(const Name: string): integer;
    function FindByName(const Name: string): TIDEMsgQuickFixItem;
    function NewName(const StartValue: string): string;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TIDEMsgQuickFixItem read GetItems; default;
  end;
  
  { TIDEMessagesWindowInterface }

  TIDEMessagesWindowInterface = class(TForm)
  protected
    function GetLines(Index: integer): TIDEMessageLine; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    procedure AddMsg(const Msg, CurDir: string; OriginalIndex: integer; Parts: TStrings = nil); virtual; abstract;
    property Lines[Index: integer]: TIDEMessageLine read GetLines; default;
    function LinesCount: integer; virtual; abstract;
    procedure BeginBlock(ClearOldBlocks: Boolean = true); virtual; abstract;
    procedure EndBlock; virtual; abstract;
  end;
  
var
  IDEMsgQuickFixes: TIDEMsgQuickFixItems = nil; // initialized by the IDE
  IDEMessagesWindow: TIDEMessagesWindowInterface = nil;// initialized by the IDE
  IDEMsgScanners: TIDEMsgScanners = nil;// initialized by the IDE
  
procedure RegisterIDEMsgQuickFix(Item: TIDEMsgQuickFixItem);
function RegisterIDEMsgQuickFix(const Name, Caption, RegExpr: string;
  Steps: TIMQuickFixSteps;
  const ExecuteMethod: TIMQFExecuteMethod;
  const ExecuteProc: TIMQFExecuteProc = nil): TIDEMsgQuickFixItem; overload;
  
procedure RegisterIDEMsgScanner(Item: TIDEMsgScannerType);

type
  TFPCErrorType = (etNone, etHint, etNote, etWarning, etError, etFatal, etPanic);

const
  FPCErrorTypeNames : array[TFPCErrorType] of string = (
      'None','Hint','Note','Warning','Error','Fatal','Panic'
    );

function FPCErrorTypeNameToType(const Name:string): TFPCErrorType;
function ParseFPCMessage(const Line: string; out Filename:string;
      out CaretXY: TPoint; out MsgType: TFPCErrorType): boolean;

implementation

procedure RegisterIDEMsgQuickFix(Item: TIDEMsgQuickFixItem);
begin
  IDEMsgQuickFixes.Add(Item);
end;

function RegisterIDEMsgQuickFix(const Name, Caption, RegExpr: string;
  Steps: TIMQuickFixSteps;
  const ExecuteMethod: TIMQFExecuteMethod; const ExecuteProc: TIMQFExecuteProc
  ): TIDEMsgQuickFixItem;
begin
  Result:=TIDEMsgQuickFixItem.Create;
  Result.Name:=Name;
  Result.Caption:=Caption;
  Result.RegExpression:=RegExpr;
  Result.Steps:=Steps;
  Result.OnExecuteMethod:=ExecuteMethod;
  Result.OnExecuteProc:=ExecuteProc;
  IDEMsgQuickFixes.Add(Result);
end;

procedure RegisterIDEMsgScanner(Item: TIDEMsgScannerType);
begin
  IDEMsgScanners.RegisterType(Item);
end;

function FPCErrorTypeNameToType(const Name: string): TFPCErrorType;
begin
  for Result:=Succ(etNone) to High(TFPCErrorType) do
    if CompareText(FPCErrorTypeNames[Result],Name)=0 then exit;
  Result:=etNone;
end;

function ParseFPCMessage(const Line: string; out Filename: string; out
  CaretXY: TPoint; out MsgType: TFPCErrorType): boolean;
{ This assumes the line has one of the following formats
<filename>(123,45) <ErrorType>: <some text>
<filename>(123) <ErrorType>: <some text>
<filename>(456) <ErrorType>: <some text> in line (123)
Fatal: <some text>
}
var
  StartPos, EndPos: integer;
begin
  Result:=false;
  StartPos:=1;

  // skip time [0.000]
  if (Line<>'') and (Line[StartPos]='[') then begin
    inc(StartPos);
    while (StartPos<=length(Line)) and (Line[StartPos] in ['0'..'9','.']) do
      inc(StartPos);
    if (StartPos<=length(Line)) and (Line[StartPos]=']') then
      inc(StartPos);
    while (StartPos<=length(Line)) and (Line[StartPos] in [' ']) do
      inc(StartPos);
  end;

  if copy(Line,StartPos,7)='Fatal: ' then begin
    Result:=true;
    Filename:='';
    MsgType:=etFatal;
    exit;
  end;
  if copy(Line,StartPos,7)='Panic: ' then begin
    Result:=true;
    Filename:='';
    MsgType:=etPanic;
    exit;
  end;

  // find filename
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos]<>'(') do inc(EndPos);
  if EndPos>length(Line) then exit;
  FileName:=copy(Line,StartPos,EndPos-StartPos);
  // read linenumber
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
  if EndPos>length(Line) then exit;
  CaretXY.X:=1;
  CaretXY.Y:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
  if Line[EndPos]=',' then begin
    // format: <filename>(123,45) <ErrorType>: <some text>
    // read column
    StartPos:=EndPos+1;
    EndPos:=StartPos;
    while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
    if EndPos>length(Line) then exit;
    CaretXY.X:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
    // read error type
    StartPos:=EndPos+2;
    while (EndPos<=length(Line)) and (Line[EndPos]<>':') do inc(EndPos);
    if EndPos>length(Line) then exit;
    MsgType:=FPCErrorTypeNameToType(copy(Line,StartPos,EndPos-StartPos));
    Result:=true;
  end else if Line[EndPos]=')' then begin
    // <filename>(123) <ErrorType>: <some text>
    // <filename>(456) <ErrorType>: <some text> in line (123)
    // read error type
    StartPos:=EndPos+2;
    while (EndPos<=length(Line)) and (Line[EndPos]<>':') do inc(EndPos);
    if EndPos>length(Line) then exit;
    MsgType:=FPCErrorTypeNameToType(copy(Line,StartPos,EndPos-StartPos));
    // read second linenumber (more useful)
    while (EndPos<=length(Line)) and (Line[EndPos]<>'(') do inc(EndPos);
    if EndPos>length(Line) then begin
      // format: <filename>(123) <ErrorType>: <some text>
      Result:=true;
      exit;
    end;
    StartPos:=EndPos+1;
    EndPos:=StartPos;
    while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
    if EndPos>length(Line) then exit;
    CaretXY.Y:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
    Result:=true;
  end;
end;

{ TIDEMsgQuickFixItem }

function TIDEMsgQuickFixItem.GetCaption: string;
begin
  if FCaption<>'' then
    Result:=FCaption
  else
    Result:=FName;
end;

procedure TIDEMsgQuickFixItem.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TIDEMsgQuickFixItem.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

procedure TIDEMsgQuickFixItem.SetRegExpression(const AValue: string);
begin
  if FRegExpression=AValue then exit;
  FRegExpression:=AValue;
end;

procedure TIDEMsgQuickFixItem.SetRegExprModifiers(const AValue: string);
begin
  if FRegExprModifiers=AValue then exit;
  FRegExprModifiers:=AValue;
end;

constructor TIDEMsgQuickFixItem.Create;
begin
  FRegExprModifiers:='I';
  FSteps:=[imqfoMenuItem];
end;

procedure TIDEMsgQuickFixItem.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
begin
  if Assigned(OnExecuteMethod) then
    OnExecuteMethod(Self,Step,Msg);
  if Assigned(OnExecuteProc) then
    OnExecuteProc(Self,Step,Msg);
end;

function TIDEMsgQuickFixItem.IsApplicable(Line: TIDEMessageLine): boolean;
begin
  Result:=false;
  if RegExpression='' then exit;
  //DebugLn('TIDEMsgQuickFixItem.IsApplicable Line.Msg="',Line.Msg,'" RegExpression="',RegExpression,'"');
  Result:=REMatches(Line.Msg,RegExpression,RegExprModifiers);
end;

{ TIDEMsgQuickFixItems }

function TIDEMsgQuickFixItems.GetItems(Index: integer): TIDEMsgQuickFixItem;
begin
  Result:=TIDEMsgQuickFixItem(FItems[Index]);
end;

function TIDEMsgQuickFixItems.GetCount: integer;
begin
  Result:=FItems.Count;
end;

constructor TIDEMsgQuickFixItems.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TIDEMsgQuickFixItems.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TIDEMsgQuickFixItems.Clear;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do Items[i].Free;
end;

function TIDEMsgQuickFixItems.Add(Item: TIDEMsgQuickFixItem): integer;
begin
  Item.Name:=NewName(Item.Name);
  Result:=FItems.Add(Item);
end;

procedure TIDEMsgQuickFixItems.Remove(Item: TIDEMsgQuickFixItem);
begin
  FItems.Remove(Item);
end;

function TIDEMsgQuickFixItems.IndexOfName(const Name: string): integer;
begin
  for Result:=0 to Count-1 do
    if CompareText(Items[Result].Name,Name)=0 then exit;
  Result:=-1;
end;

function TIDEMsgQuickFixItems.FindByName(const Name: string
  ): TIDEMsgQuickFixItem;
var
  i: LongInt;
begin
  i:=IndexOfName(Name);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEMsgQuickFixItems.NewName(const StartValue: string): string;
begin
  Result:=StartValue;
  if IndexOfName(Result)<0 then exit;
  Result:=CreateFirstIdentifier(StartValue);
  //DebugLn('TIDEMsgQuickFixItems.NewName Result="',Result,'" StartValue="',StartValue,'"');
  while IndexOfName(Result)>=0 do
    Result:=CreateNextIdentifier(Result);
end;

{ TIDEMessageLine }

procedure TIDEMessageLine.SetDirectory(const AValue: string);
begin
  if FDirectory = AValue then
    exit;
  FDirectory := AValue;
end;

procedure TIDEMessageLine.SetMsg(const AValue: string);
begin
  if FMsg = AValue then
    exit;
  FMsg := AValue;
end;

constructor TIDEMessageLine.Create;
begin
  FPosition := -1;
  FVisiblePosition := -1;
end;

destructor TIDEMessageLine.Destroy;
begin
  FParts.Free;
  inherited Destroy;
end;

procedure TIDEMessageLine.GetSourcePosition(out Filename: string; out
  LineNumber, Column: integer);
begin
  if Parts<>nil then begin
    Filename:=Parts.Values['Filename'];
    LineNumber:=StrToIntDef(Parts.Values['Line'],0);
    Column:=StrToIntDef(Parts.Values['Column'],0);
  end else begin
    Filename:='';
    LineNumber:=0;
    Column:=0;
  end;
end;

procedure TIDEMessageLine.SetSourcePosition(const Filename: string;
  const LineNumber, Column: integer);
var
  BracketClosePos: LongInt;
  s: String;
  CommaPos: LongInt;
  BracketOpenPos: LongInt;
begin
  if Parts<>nil then begin
    if Filename<>'' then
      Parts.Values['Filename']:=Filename;
    if LineNumber>0 then
      Parts.Values['Line']:=IntToStr(LineNumber);
    if Column>0 then
      Parts.Values['Column']:=IntToStr(Column);
  end;
  BracketOpenPos:=System.Pos('(',Msg);
  if (BracketOpenPos>0) then begin
    if (Filename<>'') then begin
      Msg:=Filename+copy(Msg,BracketOpenPos,length(Msg));
      BracketOpenPos:=length(Filename)+1;
    end;
    CommaPos:=System.Pos(',',Msg);
    if LineNumber>0 then begin
      if CommaPos>0 then begin
        s:=IntToStr(LineNumber);
        Msg:=copy(Msg,1,BracketOpenPos)+s
             +copy(Msg,CommaPos,length(Msg));
      end else begin
        s:=IntToStr(LineNumber);
        Msg:=copy(Msg,1,BracketOpenPos)+s+',1'
            +copy(Msg,BracketOpenPos+1,length(Msg));
      end;
      CommaPos:=BracketOpenPos+length(s)+1;
    end;
    if CommaPos>0 then begin
      BracketClosePos:=System.Pos(')',Msg);
      if BracketClosePos>0 then begin
        if Column>0 then begin
          s:=IntToStr(Column);
          Msg:=copy(Msg,1,CommaPos)+s+copy(Msg,BracketClosePos,length(Msg));
        end;
      end;
    end;
  end else begin
    Msg:=Filename+'('+IntToStr(LineNumber)+','+IntToStr(Column)+')'+Msg;
  end;
end;

{ TIDEMessageLineList }

function TIDEMessageLineList.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TIDEMessageLineList.GetItems(Index: integer): TIDEMessageLine;
begin
  Result:=TIDEMessageLine(FItems[Index]);
end;

function TIDEMessageLineList.GetParts(Index: integer): TStrings;
begin
  Result:=Items[Index].Parts;
end;

procedure TIDEMessageLineList.SetParts(Index: integer; const AValue: TStrings);
begin
  Items[Index].Parts:=AValue;
end;

constructor TIDEMessageLineList.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TIDEMessageLineList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TIDEMessageLineList.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TIDEMessageLineList.Add(const Msg: string): integer;
var
  Item: TIDEMessageLine;
begin
  Item:=TIDEMessageLine.Create;
  Item.Msg:=Msg;
  Result:=Add(Item);
end;

function TIDEMessageLineList.Add(Item: TIDEMessageLine): integer;
begin
  Result:=FItems.Add(Item);
  Item.OriginalIndex:=Result;
end;

procedure TIDEMessageLineList.Delete(Index: Integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

{ TIDEMsgScanner }

procedure TIDEMsgScanner.SetWorkingDirectory(const AValue: string);
begin
  if FWorkingDirectory=AValue then exit;
  FWorkingDirectory:=AValue;
end;

constructor TIDEMsgScanner.Create(TheScannerType: TIDEMsgScannerType;
  TheLines: TIDEMessageLineList);
begin
  FScannerType:=TheScannerType;
  FLines:=TheLines;
end;

procedure TIDEMsgScanner.EndScan;
begin

end;

{ TIDEMsgScanners }

function TIDEMsgScanners.GetCount: integer;
begin
  Result:=fTypes.Count;
end;

function TIDEMsgScanners.GetItems(Index: integer): TIDEMsgScannerType;
begin
  Result:=TIDEMsgScannerType(fTypes[Index]);
end;

function TIDEMsgScanners.GetScannerCount: integer;
begin
  Result:=fScanners.Count;
end;

function TIDEMsgScanners.GetScanners(Index: integer): TIDEMsgScanner;
begin
  Result:=TIDEMsgScanner(fScanners[Index]);
end;

constructor TIDEMsgScanners.Create;
begin
  fTypes:=TFPList.Create;
  fScanners:=TFPList.Create;
end;

destructor TIDEMsgScanners.Destroy;
var
  i: Integer;
begin
  for i:=0 to fTypes.Count-1 do TObject(fTypes[i]).Free;
  FreeAndNil(fTypes);
  for i:=0 to fScanners.Count-1 do TObject(fScanners[i]).Free;
  FreeAndNil(fScanners);
  inherited Destroy;
end;

procedure TIDEMsgScanners.RegisterType(aType: TIDEMsgScannerType);
begin
  if (aType.Name='') or (not IsValidIdent(aType.Name)) then
    raise Exception.Create('TIDEMsgScanners.RegisterType invalid name "'+dbgstr(aType.Name)+'"');
  aType.Name:=CreateUniqueName(aType.Name);
  fTypes.Add(aType);
end;

procedure TIDEMsgScanners.UnregisterType(aType: TIDEMsgScannerType);
begin
  fTypes.Remove(aType);
end;

function TIDEMsgScanners.CreateUniqueName(const aName: string): string;
var
  i: Integer;
begin
  Result:=aName;
  if IndexOfName(Result)<0 then exit;
  i:=1;
  repeat
    Result:=aName+'_'+IntToStr(i);
  until IndexOfName(Result)<0;
end;

function TIDEMsgScanners.IndexOfName(const aTypeName: string): integer;
begin
  Result:=fTypes.Count-1;
  while (Result>=0)
  and (SysUtils.CompareText(Items[Result].Name,aTypeName)<>0) do
    dec(Result);
end;

function TIDEMsgScanners.TypeOfName(const aTypeName: string
  ): TIDEMsgScannerType;
var
  i: LongInt;
begin
  i:=IndexOfName(aTypeName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEMsgScanners.IndexOfShortDesc(const ShortDescription: string
  ): integer;
begin
  Result:=fTypes.Count-1;
  while (Result>=0)
  and (SysUtils.CompareText(Items[Result].ShortDescription,ShortDescription)<>0) do
    dec(Result);
end;

function TIDEMsgScanners.TypeOfShortDesc(const ShortDescription: string
  ): TIDEMsgScannerType;
var
  i: LongInt;
begin
  i:=IndexOfShortDesc(ShortDescription);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

end.

