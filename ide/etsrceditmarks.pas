{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Source Editor marks for (compiler) messages.
}
unit etSrcEditMarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, SynGutterLineOverview, SynEditMarkupGutterMark,
  SynEditMarks, SynEditMiscClasses, SynEditTypes, SynEdit, LazSynEditText,
  LazLogger, LazFileUtils, AvgLvlTree, Graphics, Controls, Forms,
  IDEExternToolIntf;

type

  { TETMarkStyle }

  TETMarkStyle = class
  private
    FColor: TColor;
    FImageIndex: integer;
    FSourceMarkup: TSynSelectedColor;
    FUrgency: TMessageLineUrgency;
    procedure SetColor(AValue: TColor);
  public
    constructor Create(TheUrgency: TMessageLineUrgency; TheColor: TColor);
    destructor Destroy; override;
    property Urgency: TMessageLineUrgency read FUrgency;
    property Color: TColor read FColor write SetColor;
    property ImageIndex: integer read FImageIndex write FImageIndex;
    property SourceMarkup: TSynSelectedColor read FSourceMarkup;
  end;

  TETMarks = class;

  { TETMark }

  TETMark = class(TSynEditMarkupMark)
  private
    FMsgLine: TMessageLine;
    FSourceMarks: TETMarks;
    FUrgency: TMessageLineUrgency;
  public
    destructor Destroy; override;
    property Urgency: TMessageLineUrgency read FUrgency write FUrgency;
    property MsgLine: TMessageLine read FMsgLine write FMsgLine;
    property SourceMarks: TETMarks read FSourceMarks write FSourceMarks;
  end;

  { TLMsgViewLine }

  TLMsgViewLine = class(TMessageLine)
  public
    Mark: TETMark;
    destructor Destroy; override;
  end;

  { TETMarks }

  TOnGetSynEditOfFile = procedure(Sender: TObject; aFilename: string;
    var aSynEdit: TSynEdit) of object;

  TETMarks = class(TComponent)
  private
    FImageList: TImageList;
    fMarkStyles: array[TMessageLineUrgency] of TETMarkStyle;
    FOnGetSynEditOfFile: TOnGetSynEditOfFile;
    FPriority: integer;
    function GetMarkStyles(Urgency: TMessageLineUrgency): TETMarkStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateMark(MsgLine: TMessageLine; aSynEdit: TSynEdit = nil): TETMark;
    procedure RemoveMarks(aSynEdit: TSynEdit);
    property ImageList: TImageList read FImageList write FImageList; // must have same Width/Height as the TSynEdits bookmarkimages
    property OnGetSynEditOfFile: TOnGetSynEditOfFile read FOnGetSynEditOfFile write FOnGetSynEditOfFile;
    property MarkStyles[Urgency: TMessageLineUrgency]: TETMarkStyle read GetMarkStyles;
    property Priority: integer read FPriority write FPriority;
  end;

  { TExtToolSynGutterMarkProvider }

  TExtToolSynGutterMarkProvider = class(TSynGutterLOvProviderBookmarks)
  protected
    procedure AdjustColorForMark(AMark: TSynEditMark; var AColor: TColor;
      var APriority: Integer); override;
  end;

  TETSrcChangeAction = (
    etscaInsert,
    etscaDelete
    );

  { TETSrcChange }

  TETSrcChange = class
  public
    Action: TETSrcChangeAction;
    FromPos: TPoint;
    ToPos: TPoint;
    Prev, Next: TETSrcChange;
    constructor Create(AnAction: TETSrcChangeAction; const aFromPos, aToPos: TPoint);
    constructor Create(AnAction: TETSrcChangeAction; FromPosY, FromPosX, ToPosY, ToPosX: integer);
    function AsString: string;
  end;

  { TETSrcChanges }

  TETSrcChanges = class
  private
    FFilename: string;
    FFirst: TETSrcChange;
    FLast: TETSrcChange;
    procedure Append(Change: TETSrcChange);
    procedure Remove(Change: TETSrcChange);
    procedure SetFilename(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property First: TETSrcChange read FFirst;
    property Last: TETSrcChange read FLast;
    property Filename: string read FFilename write SetFilename;
    procedure GetRange(out MinY, MaxY, LineDiffBehindMaxY: integer);
    function Add(Action: TETSrcChangeAction; const FromPos, ToPos: TPoint): TETSrcChange; inline;
    function Add(Action: TETSrcChangeAction; FromPosY, FromPosX, ToPosY, ToPosX: integer): TETSrcChange;
    function AdaptCaret(var Line,Col: integer;
      LeftBound: boolean // true = position is bound to character on the left
                 ): boolean; // true if changed
    procedure ConsistencyCheck;
    procedure WriteDebugReport(Title: string);
  end;

  { TETMultiSrcChanges }

  TETMultiSrcChanges = class
  private
    fAllChanges: TAvgLvlTree; // tree of TETSrcChanges sorted for Filename
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer; inline;
    procedure Clear;
    function GetChanges(const aFilename: string; CreateIfNotExists: boolean): TETSrcChanges;
    function AdaptCaret(const aFilename: string; var Line,Col: integer;
      LeftBound: boolean // true = position is bound to character on the left
                 ): boolean;
    property AllChanges: TAvgLvlTree read fAllChanges; // tree of TETSrcChanges sorted for Filename
  end;

  { TETSynPlugin - create one per file, not one per synedit }

  TETSynPlugin = class(TLazSynEditPlugin)
  private
    FChanges: TETSrcChanges;
    FOnChanged: TNotifyEvent;
    FSyncQueued: boolean;
    procedure SetSyncQueued(AValue: boolean);
  protected
    procedure DoSync({%H-}Data: PtrInt); // called by Application.QueueAsyncCall
    procedure OnLineEdit(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
      aLineBrkCnt: Integer; {%H-}aText: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Changes: TETSrcChanges read FChanges;
    property SyncQueued: boolean read FSyncQueued write SetSyncQueued;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged; // called by Application.QueueAsyncCall
  end;

function IsCaretInFront(Line1, Col1, Line2, Col2: integer): boolean; inline; overload;
function IsCaretInFront(const P1: TPoint; Line2, Col2: integer): boolean; inline; overload;
function IsCaretInFront(Line1, Col1: integer; const P2: TPoint): boolean; inline; overload;
function IsCaretInFront(const P1,P2: TPoint): boolean; inline; overload;
function IsCaretInFrontOrSame(Line1, Col1, Line2, Col2: integer): boolean; inline; overload;
function IsCaretInFrontOrSame(const P1: TPoint; Line2, Col2: integer): boolean; inline; overload;
function IsCaretInFrontOrSame(Line1, Col1: integer; const P2: TPoint): boolean; inline; overload;
function IsCaretInFrontOrSame(const P1,P2: TPoint): boolean; inline; overload;
procedure AdaptCaret(var Line,Col: integer;
  LeftBound: boolean; // true = position is bound to character on the left
  Action: TETSrcChangeAction;
  FromPosY, FromPosX, ToPosY, ToPosX: integer);

function CompareETSrcChangesFilenames(Changes1, Changes2: Pointer): integer;
function CompareFilenameAndETSrcChanges(aFilenameStr, Changes: Pointer): integer;

function dbgs(Action: TETSrcChangeAction): string; overload;
procedure Test_AdaptCaret;
procedure Test_MergeTETSrcChanges;

implementation

function IsCaretInFront(Line1, Col1, Line2, Col2: integer): boolean;
begin
  Result:=(Line1<Line2) or ((Line1=Line2) and (Col1<Col2));
end;

function IsCaretInFront(const P1: TPoint; Line2, Col2: integer): boolean;
begin
  Result:=IsCaretInFront(P1.Y,P1.X,Line2,Col2);
end;

function IsCaretInFront(Line1, Col1: integer; const P2: TPoint): boolean;
begin
  Result:=IsCaretInFront(Line1,Col1,P2.Y,P2.X);
end;

function IsCaretInFront(const P1, P2: TPoint): boolean;
begin
  Result:=IsCaretInFront(P1.Y,P1.X,P2.Y,P2.X);
end;

function IsCaretInFrontOrSame(Line1, Col1, Line2, Col2: integer): boolean;
begin
  Result:=(Line1<Line2) or ((Line1=Line2) and (Col1<=Col2));
end;

function IsCaretInFrontOrSame(const P1: TPoint; Line2, Col2: integer): boolean;
begin
  Result:=IsCaretInFrontOrSame(P1.Y,P1.X,Line2,Col2);
end;

function IsCaretInFrontOrSame(Line1, Col1: integer; const P2: TPoint): boolean;
begin
  Result:=IsCaretInFrontOrSame(Line1,Col1,P2.Y,P2.X);
end;

function IsCaretInFrontOrSame(const P1, P2: TPoint): boolean;
begin
  Result:=IsCaretInFrontOrSame(P1.Y,P1.X,P2.Y,P2.X);
end;

procedure AdaptCaret(var Line, Col: integer; LeftBound: boolean;
  Action: TETSrcChangeAction; FromPosY, FromPosX, ToPosY, ToPosX: integer);
begin
  //debugln(['AdaptCaret Line=',Line,' Col=',Col,' LeftBound=',LeftBound,' Action=',dbgs(Action),' FromPos=',FromPosY,',',FromPosX,' ToPos=',ToPosY,',',ToPosX]);
  if Line<FromPosY then exit;
  if Action=etscaInsert then begin
    // insert
    if Line>FromPosY then begin
      // insert in lines in front => move vertically
      inc(Line,ToPosY-FromPosY);
    end else begin
      // insert in same line
      if LeftBound then begin
        if Col<=FromPosX then exit;
      end else begin
        if Col<FromPosX then exit;
      end;
      if FromPosY<ToPosY then begin
        // multi line insert
        inc(Line,ToPosY-FromPosY);
        Col:=ToPosX+Col-FromPosX;
      end else begin
        // inserting some characters
        inc(Col,ToPosX-FromPosX);
      end;
    end;
  end else begin
    // delete
    if Line>ToPosY then begin
      // delete some lines in front => move vertically
      dec(Line,ToPosY-FromPosY);
    end else if Line<ToPosY then begin
      if Line>FromPosY then begin
        // whole line of position was deleted => move to start of deletion
        Line:=FromPosY;
        Col:=FromPosX;
      end else begin
        // Line=FromPosY, Line<ToPosY
        if Col<=FromPosX then begin
          // delete is behind position => ignore
        end else begin
          // position was deleted => move to start of deletion
          Line:=FromPosY;
          Col:=FromPosX;
        end;
      end;
    end else begin
      // Line=ToPosY
      if Line>FromPosY then begin
        // multi line delete
        if Col<=ToPosX then begin
          // position was deleted => move to start of deletion
          Line:=FromPosY;
          Col:=FromPosX;
        end else begin
          // some characters at the start of the line were deleted
          Line:=FromPosY;
          dec(Col,ToPosX-1);
        end;
      end else begin
        // Line=FromPosY=ToPosY
        if Col<=FromPosX then begin
          // delete is behind position => ignore
        end else if Col<=ToPosX then begin
          // position was deleted => move to start of deletion
          Col:=FromPosX;
        end else begin
          // some characters in front were deleted
          dec(Col,ToPosX-FromPosX);
        end;
      end;
    end;
  end;
  //debugln(['AdaptCaret ',Line,',',Col]);
end;

function CompareETSrcChangesFilenames(Changes1, Changes2: Pointer): integer;
var
  SrcChanges1: TETSrcChanges absolute Changes1;
  SrcChanges2: TETSrcChanges absolute Changes2;
begin
  Result:=CompareFilenames(SrcChanges1.Filename,SrcChanges2.Filename);
end;

function CompareFilenameAndETSrcChanges(aFilenameStr, Changes: Pointer
  ): integer;
var
  SrcChanges: TETSrcChanges absolute Changes;
begin
  Result:=CompareFilenames(AnsiString(aFilenameStr),SrcChanges.Filename);
end;

function dbgs(Action: TETSrcChangeAction): string;
begin
  Result:='';
  WriteStr(Result,Action);
end;

procedure Test_AdaptCaret;

  procedure T(Title: string; Line,Col: integer;
    LeftBound: boolean; // true = position is bound to character on the left
    Action: TETSrcChangeAction;
    FromPosY, FromPosX, ToPosY, ToPosX: integer;
    ExpectedLine, ExpectedCol: integer);
  var
    Y: Integer;
    X: Integer;
    s: String;
  begin
    Y:=Line;
    X:=Col;
    AdaptCaret(Y,X,LeftBound,Action,FromPosY,FromPosX,ToPosY,ToPosX);
    if (Y=ExpectedLine) and (X=ExpectedCol) then exit;
    s:='Test_AdaptCaret: Caret='+dbgs(Line)+','+dbgs(Col)
     +' LeftBound='+dbgs(LeftBound)
     +' Action='+dbgs(Action)
     +' FromPos='+dbgs(FromPosY)+','+dbgs(FromPosX)
     +' ToPos='+dbgs(ToPosY)+','+dbgs(ToPosX)
     +' Expected='+dbgs(ExpectedLine)+','+dbgs(ExpectedCol)
     +' Actual='+dbgs(Y)+','+dbgs(X);
    raise Exception.Create(Title+': '+s);
  end;

begin
  T('Insert chars in front',10,10,true,etscaInsert,1,1, 1,2,  10,10);
  T('Insert lines in front',10,10,true,etscaInsert,1,1, 2,2,  11,10);
  T('Insert chars behind',10,10,true,etscaInsert,12,1, 12,2,  10,10);
  T('Insert chars in front, same line',10,10,true,etscaInsert,10,1, 10,2,  10,11);
  T('Insert chars in front, same line',10,40,true,etscaInsert,10,28, 10,29,  10,41);
  T('Insert chars behind, same line',10,10,true,etscaInsert,10,11, 10,12,  10,10);
  T('Insert chars behind, same line, leftbound',10,10,true,etscaInsert,10,10, 10,12,  10,10);
  T('Insert chars behind, same line, rightbound',10,10,false,etscaInsert,10,10, 10,12,  10,12);
  T('Insert chars and line breaks in front, same line',10,10,true,etscaInsert,10,1, 11,1,  11,10);
  T('Insert chars and line breaks in front, same line',10,10,true,etscaInsert,10,1, 11,2,  11,11);
  T('Insert chars and line breaks in front, same line',10,10,true,etscaInsert,10,2, 11,2,  11,10);
  T('Insert chars and line breaks in front, same line',10,10,true,etscaInsert,10,2, 11,5,  11,13);
  T('Insert chars and line breaks in front, same line',10,10,true,etscaInsert,10,2, 13,5,  13,13);

  T('Delete chars in front',10,10,true,etscaDelete, 1,1, 1,2, 10,10);
  T('Delete lines in front',10,10,true,etscaDelete, 1,1, 2,2, 9,10);
  T('Delete chars in front, same line',10,10,true,etscaDelete, 10,1, 10,2, 10,9);
  T('Delete lines behind',10,10,true,etscaDelete, 11,1, 12,2, 10,10);
  T('Delete chars behind, same line',10,10,true,etscaDelete, 10,11, 10,12, 10,10);
  T('Delete chars behind, same line',10,10,true,etscaDelete, 10,10, 10,12, 10,10);
  T('Delete lines in front, same line',10,10,true,etscaDelete, 9,1, 10,1, 9,10);
  T('Delete lines in front, same line',10,10,true,etscaDelete, 9,1, 10,3, 9,8);
  T('Delete position',10,10,true,etscaDelete, 9,1, 11,1, 9,1);
  T('Delete position',10,10,true,etscaDelete, 10,1, 11,1, 10,1);
  T('Delete position',10,10,true,etscaDelete, 10,5, 10,11, 10,5);
end;

procedure Test_MergeTETSrcChanges;
var
  Changes: TETSrcChanges;

  procedure Check(Title: string; aChanges: array of TETSrcChange);

    procedure E(Msg: string);
    var
      s: String;
    begin
      s:=Title+', '+Msg;
      Changes.WriteDebugReport(s);
      raise Exception.Create(s);
    end;

  var
    i: Integer;
    ActualChange: TETSrcChange;
    ExpectedChange: TETSrcChange;
  begin
    ActualChange:=Changes.First;
    try
      for i:=Low(aChanges) to High(aChanges) do begin
        ExpectedChange:=aChanges[i];
        if ExpectedChange=nil then begin
          if ActualChange<>nil then
            E('too many changes');
          exit;
        end;
        if ActualChange=nil then
          E('not enough changes (missing: '+ActualChange.AsString+')');
        if ExpectedChange.AsString<>ActualChange.AsString then
          E('diff: Expected=('+ExpectedChange.AsString+'), Actual=('+ActualChange.AsString+')');
        ActualChange:=ActualChange.Next;
      end;
    finally
      for i:=Low(aChanges) to High(aChanges) do
        aChanges[i].Free;
    end;
  end;

begin
  Changes:=TETSrcChanges.Create;
  try
    Changes.ConsistencyCheck;

    // test empty clear
    Changes.Clear;
    Changes.ConsistencyCheck;

    // test merge insert
    Changes.Add(etscaInsert,1,1,1,46);
    Changes.ConsistencyCheck;
    Changes.Add(etscaInsert,1,46,2,1);
    Changes.ConsistencyCheck;
    Check('Merge insert',[TETSrcChange.Create(etscaInsert,1,1,2,1)]);
    Changes.Clear;

    // insert characters into a previous multi line insert
    Changes.Add(etscaInsert,10,1,12,1);
    Changes.ConsistencyCheck;
    Changes.Add(etscaInsert,10,1,10,2);
    Changes.ConsistencyCheck;
    Check('Ignore small insert',[TETSrcChange.Create(etscaInsert,10,1,12,1)]);
    Changes.Clear;

    // delete behind previous delete
    Changes.Add(etscaDelete,1,2,1,4);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,2,1,5);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,2,1,7)]);
    Changes.Clear;

    // delete encloses a previous delete
    Changes.Add(etscaDelete,1,2,1,4);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,1,5);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,1,7)]);
    Changes.Clear;

    // delete in front of a previous delete
    Changes.Add(etscaDelete,2,2,2,4);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,2,2);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,2,4)]);
    Changes.Clear;

    // delete encloses a previous delete of characters
    Changes.Add(etscaDelete,2,2,2,4);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,3,1);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,3,1)]);
    Changes.Clear;

    // delete encloses a previous delete of a line
    Changes.Add(etscaDelete,2,2,3,4);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,4,1);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,5,1)]);
    Changes.Clear;

    // delete encloses a previous delete at end
    Changes.Add(etscaDelete,2,2,3,1);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,2,3);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,3,2)]);
    Changes.Clear;

    // delete encloses a previous delete at end
    Changes.Add(etscaDelete,2,2,3,2);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,2,3);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,3,3)]);
    Changes.Clear;

    // delete encloses a previous delete at start
    Changes.Add(etscaDelete,1,2,3,2);
    Changes.ConsistencyCheck;
    Changes.Add(etscaDelete,1,1,1,2);
    Changes.ConsistencyCheck;
    Check('combine deleting characters',[TETSrcChange.Create(etscaDelete,1,1,3,2)]);
    Changes.Clear;
  finally
    Changes.Free;
  end;
end;

{ TETMultiSrcChanges }

constructor TETMultiSrcChanges.Create;
begin
  fAllChanges:=TAvgLvlTree.Create(@CompareETSrcChangesFilenames);
end;

destructor TETMultiSrcChanges.Destroy;
begin
  Clear;
  FreeAndNil(fAllChanges);
  inherited Destroy;
end;

// inline
function TETMultiSrcChanges.Count: integer;
begin
  Result:=fAllChanges.Count;
end;

procedure TETMultiSrcChanges.Clear;
begin
  fAllChanges.FreeAndClear;
end;

function TETMultiSrcChanges.GetChanges(const aFilename: string;
  CreateIfNotExists: boolean): TETSrcChanges;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=fAllChanges.FindKey(Pointer(aFilename),@CompareFilenameAndETSrcChanges);
  if Node<>nil then
    Result:=TETSrcChanges(Node.Data)
  else if CreateIfNotExists then begin
    Result:=TETSrcChanges.Create;
    Result.Filename:=aFilename;
    fAllChanges.Add(Result);
  end else
    Result:=nil;
end;

function TETMultiSrcChanges.AdaptCaret(const aFilename: string; var Line,
  Col: integer; LeftBound: boolean): boolean;
var
  Changes: TETSrcChanges;
begin
  Changes:=GetChanges(aFilename,false);
  if Changes=nil then
    Result:=false
  else
    Result:=Changes.AdaptCaret(Line,Col,LeftBound);
end;

{ TETSrcChange }

constructor TETSrcChange.Create(AnAction: TETSrcChangeAction; const aFromPos,
  aToPos: TPoint);
begin
  Action:=AnAction;
  FromPos:=aFromPos;
  ToPos:=aToPos;
end;

constructor TETSrcChange.Create(AnAction: TETSrcChangeAction; FromPosY,
  FromPosX, ToPosY, ToPosX: integer);
begin
  Action:=AnAction;
  FromPos.Y:=FromPosY;
  FromPos.X:=FromPosX;
  ToPos.Y:=ToPosY;
  ToPos.X:=ToPosX;
end;

function TETSrcChange.AsString: string;
begin
  if Action=etscaInsert then
    Result:='Insert'
  else
    Result:='Delete';
  Result+='-From='+IntToStr(FromPos.Y)+','+IntToStr(FromPos.X);
  Result+='-To='+IntToStr(ToPos.Y)+','+IntToStr(ToPos.X);
end;

{ TETSrcChanges }

procedure TETSrcChanges.Append(Change: TETSrcChange);
begin
  if First=nil then begin
    FFirst:=Change;
  end else begin
    FLast.Next:=Change;
    Change.Prev:=Last;
  end;
  fLast:=Change;
end;

procedure TETSrcChanges.Remove(Change: TETSrcChange);
begin
  if First=Change then
    FFirst:=Change.Next;
  if Last=Change then
    fLast:=Change.Prev;
  if Change.Prev<>nil then
    Change.Prev.Next:=Change.Next;
  if Change.Next<>nil then
    Change.Next.Prev:=Change.Prev;
  Change.Prev:=nil;
  Change.Next:=nil;
end;

procedure TETSrcChanges.SetFilename(AValue: string);
var
  HasChanged: Boolean;
begin
  if FFilename=AValue then Exit;
  HasChanged:=CompareFilenames(FFilename,AValue)<>0;
  FFilename:=AValue;
  if HasChanged then
    Clear;
end;

constructor TETSrcChanges.Create;
begin
end;

destructor TETSrcChanges.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TETSrcChanges.Clear;
var
  Item: TETSrcChange;
  CurItem: TETSrcChange;
begin
  Item:=First;
  while Item<>nil do begin
    CurItem:=Item;
    Item:=Item.Next;
    CurItem.Free;
  end;
  fFirst:=nil;
  FLast:=nil;
end;

procedure TETSrcChanges.GetRange(out MinY, MaxY, LineDiffBehindMaxY: integer);
// true if there are changes
// All changes were done between lines MinY and MaxY (inclusive).
// Lines behind MaxY are moved by LineDiffBehindMaxY.
// In other words:
//   if MinY<=Line<=MaxY then AdaptCaret(Line,Col,...)
//   else if Line>MaxY then inc(Line,LineDiffBehindMaxY);
var
  Change: TETSrcChange;
  y: Integer;
  x: Integer;
begin
  MinY:=High(Integer);
  MaxY:=0;
  LineDiffBehindMaxY:=0;
  Change:=First;
  if Change=nil then exit;
  while Change<>nil do begin
    MinY:=Min(MinY,Change.FromPos.Y);
    if Change.Action=etscaInsert then
      MaxY:=Max(MaxY,Change.FromPos.Y)
    else
      MaxY:=Max(MaxY,Change.ToPos.Y);
    Change:=Change.Next;
  end;
  y:=MaxY+1;
  x:=1;
  AdaptCaret(y,x,true);
  LineDiffBehindMaxY:=y-(MaxY+1);
end;

// inline
function TETSrcChanges.Add(Action: TETSrcChangeAction; const FromPos,
  ToPos: TPoint): TETSrcChange;
begin
  Result:=Add(Action,FromPos.Y,FromPos.X,ToPos.Y,ToPos.X);
end;

function TETSrcChanges.Add(Action: TETSrcChangeAction; FromPosY, FromPosX,
  ToPosY, ToPosX: integer): TETSrcChange;

  procedure RaiseFromBehindFrom;
  begin
    raise Exception.CreateFmt('TETSrcChanges.Add FromPos=%s,%s behind ToPos=%s,%s',[FromPosY,FromPosX,ToPosY,ToPosX]);
  end;

  function Merge(Prev, Cur: TETSrcChange): boolean;
  begin
    if (Prev=nil) or (Prev.Action<>Action) then
      exit(false);
    // check if addition can be merged
    if Action=etscaInsert then begin
      if (Prev.ToPos.Y=Cur.FromPos.Y) and (Prev.ToPos.X=Cur.FromPos.X) then begin
        // Cur is an insert exactly behind Prev insert -> append insert
        Prev.ToPos.Y:=Cur.ToPos.Y;
        Prev.ToPos.X:=Cur.ToPos.X;
        {$IFDEF VerboseETSrcChange}
        debugln(['TETSrcChanges.Add appending insert: ',Prev.AsString]);
        {$ENDIF}
        exit(true);
      end;
      if (Cur.FromPos.Y=Cur.ToPos.Y)
      and (Prev.FromPos.Y<=Cur.FromPos.Y) and (Prev.ToPos.Y>Cur.FromPos.Y) then begin
        // Cur inserts characters into a Prev multi line insert -> ignore
        {$IFDEF VerboseETSrcChange}
        debugln(['TETSrcChanges.Add inserting characters into a multi line insert -> ignore']);
        {$ENDIF}
        exit(true);
      end;
      // ToDo: insert exactly in front
    end else begin
      if IsCaretInFrontOrSame(Cur.FromPos,Prev.FromPos)
      and IsCaretInFrontOrSame(Prev.FromPos,Cur.ToPos) then begin
        // Cur delete extends Prev delete => combine delete
        etSrcEditMarks.AdaptCaret(Cur.ToPos.Y,Cur.ToPos.X,false,etscaInsert,
          Prev.FromPos.Y,Prev.FromPos.X,Prev.ToPos.Y,Prev.ToPos.X);
        Prev.ToPos:=Cur.ToPos;
        Prev.FromPos:=Cur.FromPos;
        {$IFDEF VerboseETSrcChange}
        debugln(['TETSrcChanges.Add delete encloses previous delete: ',Prev.AsString]);
        {$ENDIF}
        exit(true);
      end;
    end;
    Result:=false;
  end;

begin
  {$IFDEF VerboseETSrcChange}
  debugln(['TETSrcChanges.Add Action=',dbgs(Action),' From=',FromPosY,',',FromPosX,' To=',ToPosY,',',ToPosX]);
  {$ENDIF}

  if (FromPosY=ToPosY) and (FromPosX=ToPosX) then
    exit; // no change => ignore

  // consistency check
  if IsCaretInFront(ToPosY,ToPosX,FromPosY,FromPosX) then
    RaiseFromBehindFrom;

  Result:=TETSrcChange.Create(Action, FromPosY, FromPosX, ToPosY, ToPosX);

  if Merge(Last,Result) then begin
    repeat
      Result.Free;
      Result:=Last;
      if not Merge(Result.Prev,Result) then exit;
      Remove(Last);
    until false;
  end else begin
    Append(Result);
  end;
end;

function TETSrcChanges.AdaptCaret(var Line, Col: integer; LeftBound: boolean
  ): boolean;
var
  Change: TETSrcChange;
  OldCol: Integer;
  OldLine: Integer;
begin
  OldCol:=Col;
  OldLine:=Line;
  Change:=First;
  while Change<>nil do begin
    etSrcEditMarks.AdaptCaret(Line,Col,LeftBound,Change.Action,
      Change.FromPos.Y,Change.FromPos.X,Change.ToPos.Y,Change.ToPos.X);
    Change:=Change.Next;
  end;
  Result:=(Line<>OldLine) or (Col<>OldCol);
end;

procedure TETSrcChanges.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TETSrcChanges ConsistencyError: '+Msg);
  end;

var
  Change: TETSrcChange;
  List: TFPList;
begin
  if (First=nil)<>(Last=nil) then
    E('(First=nil)<>(Last=nil)');
  List:=TFPList.Create;
  try
    Change:=First;
    while Change<>nil do begin
      if IsCaretInFront(Change.ToPos,Change.FromPos) then
        E('FromPos>ToPos: '+Change.AsString);
      if Change.Prev<>nil then begin
        if Change.Prev.Next<>Change then
          E('Change.Prev.Next<>Change '+Change.AsString);
      end else begin
        if Change<>First then
          E('Change.Prev=nil');
      end;
      if (Change.Next=nil) and (Change<>Last) then
        E('Change.Next=nil');
      if List.IndexOf(Change)>=0 then
        E('Cycle '+Change.AsString);
      List.Add(Change);
      Change:=Change.Next;
    end;
  finally
    List.Free;
  end;
end;

procedure TETSrcChanges.WriteDebugReport(Title: string);
var
  Change: TETSrcChange;
begin
  debugln('TETSrcChanges.WriteDebugReport ',Title);
  Change:=First;
  while Change<>nil do begin
    debugln('  ',Change.AsString);
    Change:=Change.Next;
  end;
end;

{ TETSynPlugin }

procedure TETSynPlugin.DoSync(Data: PtrInt);
begin
  FSyncQueued:=false;
  if FChanges.First=nil then exit;
  if Assigned(OnChanged) then
    OnChanged(Self);
  FChanges.Clear;
end;

procedure TETSynPlugin.SetSyncQueued(AValue: boolean);
begin
  if FSyncQueued=AValue then Exit;
  FSyncQueued:=AValue;
  if SyncQueued then
    Application.QueueAsyncCall(@DoSync,0)
  else
    Application.RemoveAsyncCalls(Self);
end;

procedure TETSynPlugin.OnLineEdit(Sender: TSynEditStrings; aLinePos, aBytePos,
  aCount, aLineBrkCnt: Integer; aText: String);
{
  aLinePos is 1-based
  aBytePos is 1-based column in line

  Insert:
    aCount > 0
  Delete:
    aCount < 0
    Example deleting line 290..292:
      LinePos=291 BytePos=1 Count=-45 LineBrkCnt=0 Text=""
      LinePos=292 BytePos=1 Count=-33 LineBrkCnt=0 Text=""
      LinePos=291 BytePos=1 Count=0 LineBrkCnt=-2 Text=""
      LinePos=290 BytePos=70 Count=0 LineBrkCnt=-1 Text=""
      LinePos=290 BytePos=1 Count=-69 LineBrkCnt=0 Text=""
}
begin
  {$IFDEF VerboseETSrcChange}
  debugln(['TETSynPlugin.OnLineEdit LinePos=',aLinePos,' BytePos=',aBytePos,' Count=',aCount,' LineBrkCnt=',aLineBrkCnt,' Text="',dbgstr(aText),'"']);
  {$ENDIF}
  if aCount>0 then begin
    // insert characters
    FChanges.Add(etscaInsert,aLinePos,aBytePos,aLinePos,aBytePos+aCount);
  end else if aCount<0 then begin
    // delete characters
    FChanges.Add(etscaDelete,aLinePos,aBytePos,aLinePos,aBytePos-aCount);
  end else if aLineBrkCnt>0 then begin
    // insert line breaks
    // Note: always at end of line, because Count=0
    FChanges.Add(etscaInsert,aLinePos,aBytePos,aLinePos+aLineBrkCnt,1);
  end else if aLineBrkCnt<0 then begin
    // delete line breaks / empty lines
    FChanges.Add(etscaDelete,aLinePos,aBytePos,aLinePos-aLineBrkCnt,1);
  end;
  SyncQueued:=true;
end;

constructor TETSynPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChanges:=TETSrcChanges.Create;
  ViewedTextBuffer.AddEditHandler(@OnLineEdit);
end;

destructor TETSynPlugin.Destroy;
begin
  SyncQueued:=false;
  ViewedTextBuffer.RemoveEditHandler(@OnLineEdit);
  inherited Destroy;
  FreeAndNil(FChanges);
end;

{ TETMark }

destructor TETMark.Destroy;
begin
  if MsgLine is TLMsgViewLine then
    TLMsgViewLine(MsgLine).Mark:=nil;
  MsgLine:=nil;
  inherited Destroy;
end;

{ TETMarks }

function TETMarks.GetMarkStyles(Urgency: TMessageLineUrgency): TETMarkStyle;
begin
  Result:=fMarkStyles[Urgency];
end;

constructor TETMarks.Create(AOwner: TComponent);
var
  u: TMessageLineUrgency;
begin
  inherited Create(AOwner);
  for u:=low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    fMarkStyles[u]:=TETMarkStyle.Create(u,clNone);
  fMarkStyles[mluHint].Color:=clGreen;
  fMarkStyles[mluNote].Color:=clGreen;
  fMarkStyles[mluWarn].Color:=clYellow;
  fMarkStyles[mluError].Color:=clRed;
  fMarkStyles[mluFatal].Color:=clRed;
  fMarkStyles[mluPanic].Color:=clRed;
end;

destructor TETMarks.Destroy;
var
  u: TMessageLineUrgency;
begin
  for u:=low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    FreeAndNil(fMarkStyles[u]);
  inherited Destroy;
end;

function TETMarks.CreateMark(MsgLine: TMessageLine; aSynEdit: TSynEdit
  ): TETMark;
begin
  Result:=nil;
  if (MsgLine.Line<1) or (MsgLine.Column<1) or (MsgLine.Filename='') then exit;
  if aSynEdit=nil then begin
    if OnGetSynEditOfFile=nil then exit;
    OnGetSynEditOfFile(Self,MsgLine.Filename,aSynEdit);
    if (aSynEdit=nil) then exit;
  end;
  Result:=TETMark.Create(aSynEdit);
  Result.SourceMarks:=Self;
  Result.MsgLine:=MsgLine;
  Result.Line:=MsgLine.Line;
  Result.Column:=MsgLine.Column;
  Result.Visible:=true;
  Result.Priority:=Priority;
  Result.Urgency:=MsgLine.Urgency;
  Result.ImageList:=ImageList;
  Result.ImageIndex:=MarkStyles[Result.Urgency].ImageIndex;
  Result.SourceMarkup:=MarkStyles[Result.Urgency].SourceMarkup;
  aSynEdit.Marks.Add(Result);
end;

procedure TETMarks.RemoveMarks(aSynEdit: TSynEdit);
var
  i: Integer;
  Mark: TSynEditMark;
begin
  for i:=aSynEdit.Marks.Count-1 downto 0 do begin
    Mark:=aSynEdit.Marks[i];
    if Mark is TETMark then
      Mark.Free;
  end;
end;

{ TETMarkStyle }

procedure TETMarkStyle.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  SourceMarkup.FrameColor:=Color;
end;

constructor TETMarkStyle.Create(TheUrgency: TMessageLineUrgency;
  TheColor: TColor);
begin
  FUrgency:=TheUrgency;
  FColor:=TheColor;
  FSourceMarkup:=TSynSelectedColor.Create;
  SourceMarkup.Foreground:=clNone;
  SourceMarkup.Background:=clNone;
  SourceMarkup.FrameStyle:=slsWaved;
  SourceMarkup.FrameEdges:=sfeBottom;
  SourceMarkup.FrameColor:=Color;
end;

destructor TETMarkStyle.Destroy;
begin
  FreeAndNil(FSourceMarkup);
  inherited Destroy;
end;

{ TExtToolSynGutterMarkProvider }

procedure TExtToolSynGutterMarkProvider.AdjustColorForMark(AMark: TSynEditMark;
  var AColor: TColor; var APriority: Integer);
var
  ETMark: TETMark;
begin
  //DebugLn(['TExtToolSynGutterMarkProvider.AdjustColorForMark Line=',AMark.Line,' Color=',AMark.Column]);
  if (AMark is TETMark) then begin
    ETMark:=TETMark(AMark);
    AColor:=ETMark.SourceMarks.MarkStyles[ETMark.Urgency].Color;
  end else
    inherited AdjustColorForMark(AMark, AColor, APriority);
end;

{ TLMsgViewLine }

destructor TLMsgViewLine.Destroy;
begin
  FreeAndNil(Mark);
  inherited Destroy;
end;

end.

