{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, SysUtils, Forms, LCLProc, TextTools, IDECommands, IDEExternToolIntf;
  
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

  { TIDEMsgQuickFixItem }
  
  TIMQuickFixStep = (
    imqfoMenuItem,       // add menu item in popup menu for this item
    imqfoImproveMessage, // rewrites message
    imqfoJump            // user clicks on message
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
    procedure AddMsg(const Msg, CurDir: string; OriginalIndex: integer); virtual; abstract;
    property Lines[Index: integer]: TIDEMessageLine read GetLines; default;
    function LinesCount: integer; virtual; abstract;
    procedure BeginBlock; virtual; abstract;
    procedure EndBlock; virtual; abstract;
  end;

var
  IDEMsgQuickFixes: TIDEMsgQuickFixItems = nil; // initialized by the IDE
  IDEMessagesWindow: TIDEMessagesWindowInterface = nil;// initialized by the IDE
  
procedure RegisterIDEMsgQuickFix(Item: TIDEMsgQuickFixItem);
function RegisterIDEMsgQuickFix(const Name, Caption, RegExpr: string;
  Steps: TIMQuickFixSteps;
  const ExecuteMethod: TIMQFExecuteMethod;
  const ExecuteProc: TIMQFExecuteProc = nil): TIDEMsgQuickFixItem; overload;

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

end.

