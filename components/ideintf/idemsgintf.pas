{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the Messages window (below the source editor).
}
unit IDEMsgIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  // LCL
  Forms, Menus,
  // LazUtils
  IDEExternToolIntf, LazFileUtils, LazLoggerBase,
  // IdeIntf
  MenuIntf;

type
  TMsgQuickFixes = class;

  { TMsgQuickFix }

  TMsgQuickFix = class
  public
    procedure CreateMenuItems(Fixes: TMsgQuickFixes); virtual;
    procedure JumpTo({%H-}Msg: TMessageLine; var {%H-}Handled: boolean); virtual; // called when user (double) clicks on message
    procedure QuickFix(Fixes: TMsgQuickFixes; Msg: TMessageLine); virtual; // Msg=nil means fix all Fixes.Lines
  end;
  TMsgQuickFixClass = class of TMsgQuickFix;

  { TMsgQuickFixes }

  TMsgQuickFixes = class(TComponent)
  private
    function GetLines(Index: integer): TMessageLine; inline;
    function GetQuickFixes(Index: integer): TMsgQuickFix; inline;
  protected
    fMsg: TFPList; // list of TMessageLine
    fItems: TObjectList; // list of TMsgQuickFix
    FCurrentSender: TObject;
    FCurrentCommand: TIDEMenuCommand;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterQuickFix(Fix: TMsgQuickFix);
    procedure UnregisterQuickFix(Fix: TMsgQuickFix);
    function Count: integer; inline;
    property Items[Index: integer]: TMsgQuickFix read GetQuickFixes; default;
    function LineCount: integer; inline;
    property Lines[Index: integer]: TMessageLine read GetLines;
    function AddMenuItem(Fix: TMsgQuickFix; Msg: TMessageLine; aCaption: string;
      aTag: PtrInt = 0): TIDEMenuCommand; virtual; abstract;
    property CurrentSender: TObject read FCurrentSender;
    property CurrentCommand: TIDEMenuCommand read FCurrentCommand;
  end;

var
  MsgQuickFixes: TMsgQuickFixes = nil; // set by IDE

procedure RegisterIDEMsgQuickFix(Fix: TMsgQuickFix);

type

  { TIDEMessagesWindowInterface }

  TIDEMessagesWindowInterface = class(TForm)
  protected
    function GetViews(Index: integer): TExtToolView; virtual; abstract;
  public
    procedure Clear; virtual; abstract; // clears all finished views

    function ViewCount: integer; virtual; abstract;
    property Views[Index: integer]: TExtToolView read GetViews;
    function GetView(aCaption: string; CreateIfNotExist: boolean): TExtToolView; virtual; abstract;
    function CreateView(aCaptionPrefix: string): TExtToolView; virtual; abstract;
    function FindUnfinishedView: TExtToolView; virtual; abstract;
    procedure DeleteView(View: TExtToolView); virtual; abstract; // free view
    function IndexOfView(View: TExtToolView): integer; virtual; abstract;

    procedure SelectMsgLine(Msg: TMessageLine); virtual; abstract;
    function SelectFirstUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos: boolean): boolean; virtual; abstract;
    function SelectNextUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos, Downwards: boolean): boolean; virtual; abstract;

    function AddCustomMessage(TheUrgency: TMessageLineUrgency; Msg: string;
      aSrcFilename: string = ''; LineNumber: integer = 0; Column: integer = 0;
      const ViewCaption: string = ''): TMessageLine; virtual; abstract;
    function GetSelectedLine: TMessageLine; virtual; abstract;

    procedure BeginBlock(ClearOldBlocks: Boolean = true); deprecated; // not needed anymore
    procedure AddMsg(const Msg, {%H-}CurDir: string; {%H-}OriginalIndex: integer;
      Parts: TStrings = nil); deprecated; // use AddCustomMessages instead or create a new view via GetView or CreateView
    procedure EndBlock; deprecated; // not needed anymore
  end;

var
  IDEMessagesWindow: TIDEMessagesWindowInterface = nil;// initialized by the IDE

function AddIDEMessage(TheUrgency: TMessageLineUrgency; Msg: string;
  aSrcFilename: string = ''; LineNumber: integer = 0; Column: integer = 0;
  const ViewCaption: string = ''): TMessageLine;

implementation

procedure RegisterIDEMsgQuickFix(Fix: TMsgQuickFix);
begin
  MsgQuickFixes.RegisterQuickFix(Fix);
end;

function AddIDEMessage(TheUrgency: TMessageLineUrgency; Msg: string;
  aSrcFilename: string; LineNumber: integer; Column: integer;
  const ViewCaption: string): TMessageLine;
var
  s: String;
begin
  s:=aSrcFilename;
  if LineNumber>0 then
    s+='('+IntToStr(LineNumber)+','+IntToStr(Column)+')';
  s+=' '+MessageLineUrgencyNames[TheUrgency]+': ';
  if ViewCaption<>'' then
    s+='('+ViewCaption+') ';
  s+=Msg;
  DebugLn(s);
  if IDEMessagesWindow<>nil then
    Result:=IDEMessagesWindow.AddCustomMessage(TheUrgency,Msg,aSrcFilename,LineNumber,Column,ViewCaption)
  else
    Result:=nil;
end;

{ TIDEMessagesWindowInterface }

procedure TIDEMessagesWindowInterface.BeginBlock(ClearOldBlocks: Boolean);
begin
  if ClearOldBlocks then
    Clear;
end;

procedure TIDEMessagesWindowInterface.AddMsg(const Msg, CurDir: string;
  OriginalIndex: integer; Parts: TStrings);

  function StrToUrgency(s: string; Def: TMessageLineUrgency): TMessageLineUrgency;
  begin
    if CompareText(s,'Error')=0 then
      Result:=mluError
    else if CompareText(s,'Warning')=0 then
      Result:=mluWarning
    else if CompareText(s,'Note')=0 then
      Result:=mluNote
    else if CompareText(s,'Hint')=0 then
      Result:=mluHint
    else
      Result:=Def;
  end;

var
  s: String;
  Urgency: TMessageLineUrgency;
  Line: Integer;
  Column: Integer;
  p: SizeInt;
  ColonPos: SizeInt;
  Filename: String;
  Message: String;
begin
  Urgency:=mluImportant;
  Line:=0;
  Column:=0;
  Filename:='';
  Message:=Msg;
  ColonPos:=Pos(':',Message);
  if ColonPos>0 then begin
    // check for
    //  urgency: Msg
    //  filename(line) urgency: Msg
    //  filename(line,col) urgency: Msg
    s:=LeftStr(Message,ColonPos-1);
    p:=Pos('(',s);
    if p>0 then begin
      // has filename(...:
      Filename:=TrimFilename(LeftStr(s,p-1));
      Delete(s,1,p);
      // get line number
      p:=1;
      while (p<=length(s)) and (s[p] in ['0'..'9']) do inc(p);
      Line:=StrToIntDef(LeftStr(s,p-1),0);
      Delete(s,1,p-1);
      if (p<=length(s)) and (s[p]=',') then begin
        // get column
        Delete(s,1,1);
        while (p<=length(s)) and (s[p] in ['0'..'9']) do inc(p);
        Column:=StrToIntDef(LeftStr(s,p-1),0);
        Delete(s,1,p-1);
      end;
      if (p<=length(s)) and (s[p]=')') then begin
        inc(p);
        while (p<=length(s)) and (s[p]=' ') do inc(p);
        Delete(s,1,p-1);
      end;
    end;
    // check for urgency (a single word)
    p:=1;
    while (p<=length(s)) and (s[p] in ['a'..'z','A'..'Z',#128..#255]) do inc(p);
    if (p>1) and (p<length(s)) then begin
      Urgency:=StrToUrgency(s,Urgency);
      Delete(Message,1,ColonPos);
      Message:=Trim(Message);
    end;
  end;
  if Parts<>nil then begin
    Urgency:=StrToUrgency(Parts.Values['Type'],Urgency);
    Line:=StrToIntDef(Parts.Values['Line'],Line);
    Column:=StrToIntDef(Parts.Values['Column'],Column);
    if Parts.Values['Filename']<>'' then
      Filename:=Parts.Values['Filename'];
    if Parts.Values['Message']<>'' then
      Message:=Parts.Values['Message'];
  end;
  AddCustomMessage(Urgency,Message,Filename,Line,Column);
end;

procedure TIDEMessagesWindowInterface.EndBlock;
begin

end;

{ TMsgQuickFix }

procedure TMsgQuickFix.QuickFix(Fixes: TMsgQuickFixes; Msg: TMessageLine);
var
  i: Integer;
begin
  // this is purely an example

  if Msg<>nil then begin
    if Msg.MsgID=-11111 then begin
      // fix the cause for the message
      // ...
      // mark message as handled
      Msg.MarkFixed;
    end;
  end else begin
    // example for fixing multiple messages at once
    for i:=0 to Fixes.LineCount-1 do begin
      Msg:=Fixes.Lines[i];
      if Msg.MsgID=-11111 then begin
        // fix the cause for the message
        // ...
        // mark message as handled
        Msg.MarkFixed;
      end;
    end;
  end;
end;

procedure TMsgQuickFix.CreateMenuItems(Fixes: TMsgQuickFixes);
var
  i: Integer;
  Msg: TMessageLine;
begin
  // this is an example how to check the selected messages
  for i:=0 to Fixes.LineCount-1 do begin
    Msg:=Fixes.Lines[i];
    // here are some examples how to test if a message fits
    if (Msg.Urgency<mluWarning)
    and (Msg.MsgID=-11111)
    and (Msg.Line>0)
    and (Msg.Column>0)
    and (Msg.SubTool=SubToolFPC)
    and (Msg.GetFullFilename<>'')
    and (Pos('LazarusExample',Msg.Msg)>0)
    then
      // this message can be quick fixed => add a menu item
      Fixes.AddMenuItem(Self,Msg,'Change this or that to fix this item');
  end;
end;

procedure TMsgQuickFix.JumpTo(Msg: TMessageLine; var Handled: boolean);
begin

end;

{ TMsgQuickFixes }

// inline
function TMsgQuickFixes.GetLines(Index: integer): TMessageLine;
begin
  Result:=TMessageLine(fMsg[index]);
end;

// inline
function TMsgQuickFixes.GetQuickFixes(Index: integer): TMsgQuickFix;
begin
  Result:=TMsgQuickFix(fItems[Index]);
end;

// inline
function TMsgQuickFixes.Count: integer;
begin
  Result:=fItems.Count;
end;

// inline
function TMsgQuickFixes.LineCount: integer;
begin
  Result:=fMsg.Count;
end;

constructor TMsgQuickFixes.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fItems:=TObjectList.create(true);
  fMsg:=TFPList.Create;
end;

destructor TMsgQuickFixes.Destroy;
begin
  FreeAndNil(fMsg);
  FreeAndNil(fItems);
  inherited Destroy;
  if MsgQuickFixes=Self then
    MsgQuickFixes:=nil;
end;

procedure TMsgQuickFixes.RegisterQuickFix(Fix: TMsgQuickFix);
begin
  if fItems.IndexOf(Fix)>=0 then
    raise Exception.Create('quick fix already registered');
  fItems.Add(Fix);
end;

procedure TMsgQuickFixes.UnregisterQuickFix(Fix: TMsgQuickFix);
begin
  fItems.Remove(Fix);
end;

end.

