{
/***************************************************************************
                             SourceMarks.pas
                             ---------------

 ***************************************************************************/

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
    All source editor marks, except the bookmarks, are managed by the
    SourceEditorMarks. It extends the TSynEditMark and combines all marks of
    all editors.
}
unit SourceMarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Graphics, GraphType, Controls, Menus,
  AVL_Tree, FileProcs, SynEdit, SynEditMarks,
  MenuIntf, SrcEditorIntf,
  IDEProcs, EditorOptions;
  
type
  TSourceMarks = class;
  TSourceMark = class;

  { TSourceSynMark }

  TSourceSynMark = class(TSynEditMark)
  private
    FSourceEditor: TSourceEditorInterface;
    FSourceMark: TSourceMark;
    FSynEdit: TSynEdit;
    FOnChange: TNotifyEvent;
    FChangeLock2: Integer;
  protected
    procedure DoChange(AChanges: TSynEditMarkChangeReasons); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Assign(Src: TSourceSynMark);
  public
    constructor Create(AOwner: TSourceMark; AEditor: TSourceEditorInterface);
    destructor Destroy; override;
    function GetEdit: TSynEdit;
    property SourceMark: TSourceMark read FSourceMark write FSourceMark;
  end;

  { TSourceSynMarkList }

  TSourceSynMarkList = class(TFPList)
  private
    FOnChange: TNotifyEvent;
    function GetColumn: integer;
    function GetImageIndex: integer;
    function GetLine: integer;
    function GetPriority: integer;
    function GetSM(Index: Integer): TSourceSynMark;
    function GetVisible: boolean;
    procedure PutSM(Index: Integer; const AValue: TSourceSynMark);
    procedure SetColumn(const AValue: integer);
    procedure SetImageIndex(const AValue: integer);
    procedure SetLine(const AValue: integer);
    procedure SetPriority(const AValue: integer);
    procedure SetVisible(const AValue: boolean);
  public
    function Add(Item: TSourceSynMark): Integer;
    property Items[Index: Integer]: TSourceSynMark read GetSM write PutSM; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure DoChange(AChanges: TSynEditMarkChangeReasons);
    procedure DeleteWithSourceEditor(ASrcEditor: TSourceEditorInterface);
    function IndexOfSourceEditor(AEditor: TSourceEditorInterface): Integer;
    procedure IncChangeLock;
    procedure DecChangeLock;
  public
    property Line: integer read GetLine write SetLine;
    property Column: integer read GetColumn write SetColumn;
    property Priority: integer read GetPriority write SetPriority;
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
    property Visible: boolean read GetVisible write SetVisible;
    //property BookmarkNumber: integer read FBookmarkNum write fBookmarkNum;
    //property InternalImage: boolean read FInternalImage write SetInternalImage;
    //property IsBookmark: boolean read GetIsBookmark;
  end;

  { TSourceMark }
  
  TGetSourceMarkHintEvent =
    procedure(SenderMark: TSourceMark; var Hint: string) of object;
  TCreateSourceMarkPopupMenuEvent =
    procedure(SenderMark: TSourceMark;
              const AddMenuItem: TAddMenuItemProc) of object;

  TSourceMarkHandler = (
    smhPositionChanged,
    smhBeforeFree,
    smhGetHint,
    smhCreatePopupMenu
    );
    
  TSourceMark = class
  private
    FSourceMarks: TSourceMarks;
    FSourceEditorID: TObject;
    FSynMarks: TSourceSynMarkList;
    FSynMarkLock: Integer;
    FData: TObject;
    FHandlers: array[TSourceMarkHandler] of TMethodList;
    FLine: integer;
    FColumn: integer;
    FImage: integer;
    FVisible: boolean;
    FPriority: integer;
    FIsBreakPoint: boolean;
    FLineColorAttrib: TAdditionalHilightAttribute;
    FLineColorBackGround: TColor;
    FLineColorForeGround: TColor;
    function GetSourceEditor: TSourceEditorInterface;
    function GetSourceEditorID: TObject;
    procedure SetPriority(const AValue: integer);
    procedure SetSourceMarks(const AValue: TSourceMarks);
    procedure Changed;
    procedure SynMarkChanged(Sender: TObject);
  protected
    procedure AddHandler(HandlerType: TSourceMarkHandler;
                         const Handler: TMethod);
    procedure DoPositionChanged; virtual;
    procedure DoLineUpdate(Force: Boolean = False); virtual;
    function  EditorUpdateRequired: Boolean; virtual; // called to check if we need to update the editor if a property is changed
    procedure SetColumn(const Value: Integer); //override;
    procedure SetData(const AValue: TObject); virtual;
    procedure SetImage(const Value: Integer); //override;
    procedure SetIsBreakPoint(const AValue: boolean); virtual;
    procedure SetLine(const Value: Integer); //override;
    procedure SetLineColorAttrib(const AValue: TAdditionalHilightAttribute); virtual;
    procedure SetLineColorBackGround(const AValue: TColor); virtual;
    procedure SetLineColorForeGround(const AValue: TColor); virtual;
    procedure SetVisible(const AValue: boolean); //override;
  public
    constructor Create(TheOwner: TSourceEditorInterface; TheData: TObject);
    destructor Destroy; override;
    function Compare(OtherMark: TSourceMark): integer;
    function CompareEditorAndLine(ASrcEditID: TObject;
                                  ALine: integer): integer;
    function GetFilename: string;
    function GetHint: string; virtual;
    procedure CreatePopupMenuItems(const AddMenuItemProc: TAddMenuItemProc);
    procedure IncChangeLock;
    procedure DecChangeLock;
  public    // handlers
    procedure RemoveAllHandlersForObject(HandlerObject: TObject);
    procedure AddPositionChangedHandler(OnPositionChanged: TNotifyEvent);
    procedure RemovePositionChangedHandler(OnPositionChanged: TNotifyEvent);
    procedure AddBeforeFreeHandler(OnBeforeFree: TNotifyEvent);
    procedure RemoveBeforeFreeHandler(OnBeforeFree: TNotifyEvent);
    procedure AddGetHintHandler(OnGetHint: TGetSourceMarkHintEvent);
    procedure RemoveGetHintHandler(OnGetHint: TGetSourceMarkHintEvent);
    procedure AddCreatePopupMenuHandler(
                            OnCreatePopupMenu: TCreateSourceMarkPopupMenuEvent);
    procedure RemoveCreatePopupMenuHandler(
                            OnCreatePopupMenu: TCreateSourceMarkPopupMenuEvent);
  public
    // properties
    property Data: TObject read FData write SetData;
    property SourceMarks: TSourceMarks read FSourceMarks write SetSourceMarks;
    property SourceEditor: TSourceEditorInterface read GetSourceEditor;
    property SourceEditorID: TObject read GetSourceEditorID;
    function HasSourceEditor(AEditor: TSourceEditorInterface): Boolean;
    procedure AddSourceEditor(AEditor: TSourceEditorInterface);
  public
    property LineColorAttrib: TAdditionalHilightAttribute read FLineColorAttrib
                                                       write SetLineColorAttrib;
    property LineColorForeGround: TColor read FLineColorForeGround
                                         write SetLineColorForeGround;
    property LineColorBackGround: TColor read FLineColorBackGround
                                         write SetLineColorBackGround;
  public
    property Line: integer read FLine write SetLine;
    property Column: integer read FColumn write SetColumn;
    property Priority: integer read FPriority write SetPriority;
    property ImageIndex: integer read FImage write SetImage;
    property Visible: boolean read FVisible write SetVisible;
    property IsBreakPoint: boolean read FIsBreakPoint write SetIsBreakPoint;
    //property InternalImage: boolean read FInternalImage write SetInternalImage;
    //property BookmarkNumber: integer read FBookmarkNum write fBookmarkNum;
    //property IsBookmark: boolean read GetIsBookmark;
  end;
  
  TSourceMarkClass = class of TSourceMark;
  PSourceMark = ^TSourceMark;
  
  
  { TSourceMarks }
  
  //TGetSourceEditorEvent = function(ASynEdit: TCustomSynEdit): TSourceEditorInterface of object;
  TGetSourceEditorIDEvent = function(ASrcEdit: TSourceEditorInterface): TObject of object;
  TGetFilenameEvent = function(ASourceEditor: TObject): string of object;
  TMarksAction = (maAdded, maRemoved, maChanged);
  TMarksActionEvent = procedure(AMark: TSourceMark; Action: TMarksAction) of object;
  
  TSourceMarks = class(TComponent)
  private
    fActiveBreakPointImg: Integer;
    FCurrentLineBreakPointImg: Integer;
    FCurrentLineImg: Integer;
    FCurrentLineDisabledBreakPointImg: Integer;
    FOnGetSourceEditorID: TGetSourceEditorIDEvent;
    FSourceLineImg: Integer;
    FImgList: TImageList;
    fInactiveBreakPointImg: Integer;
    fInvalidBreakPointImg: Integer;
    fInvalidDisabledBreakPointImg: Integer;
    fItems: TList;// list of TSourceMark
    fMultiBreakPointImg: Integer;
    FOnGetFilename: TGetFilenameEvent;
    FOnAction: TMarksActionEvent;
    fSortedItems: TAVLTree;// tree of TSourceMark
    fUnknownBreakPointImg: Integer;
    fUnknownDisabledBreakPointImg: Integer;
    function GetItems(Index: integer): TSourceMark;
    procedure CreateImageList;
  protected
    function FindFirstMarkNode(ASrcEditID: TObject; ALine: integer): TAVLTreeNode;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Count: integer;
    function Add(AMark: TSourceMark): integer;
    function Add(ASrcEdit: TSourceEditorInterface; ALine: integer): TSourceMark;
    function AddCustomMark(TheOwner: TSourceEditorInterface; Data: TObject;
                           MarkClass: TSourceMarkClass): TSourceMark;
    function AddImage(const ResName: string): integer;
    function GetFilename(AMark: TSourceMark): string;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Remove(AMark: TSourceMark);
    procedure AddSourceEditor(ANewEditor, AExistingEditor: TSourceEditorInterface);
    procedure DeleteAllForEditor(ASrcEdit: TSourceEditorInterface);
    function FindFirstMark(ASrcEdit: TSourceEditorInterface;
                           ALine: integer): TSourceMark;
    function FindBreakPointMark(ASrcEdit: TSourceEditorInterface;
                                ALine: integer): TSourceMark;
    procedure GetMarksForLine(ASrcEdit: TSourceEditorInterface; ALine: integer;
                              var Marks: PSourceMark; var MarkCount: integer);
  public
    property ImgList: TImageList read FImgList write FImgList;
    property Items[Index: integer]: TSourceMark read GetItems; default;
    property OnGetFilename: TGetFilenameEvent read FOnGetFilename
                                              write FOnGetFilename;
    property OnAction: TMarksActionEvent read FOnAction write FOnAction;
    property OnGetSourceEditorID: TGetSourceEditorIDEvent
             read FOnGetSourceEditorID write FOnGetSourceEditorID;
  public
    // icon index
    property ActiveBreakPointImg: Integer read fActiveBreakPointImg;
    property InactiveBreakPointImg: Integer read fInactiveBreakPointImg;
    property InvalidBreakPointImg: Integer read fInvalidBreakPointImg;
    property InvalidDisabledBreakPointImg: Integer read fInvalidDisabledBreakPointImg;
    property MultiBreakPointImg: Integer read fMultiBreakPointImg;
    property UnknownBreakPointImg: Integer read fUnknownBreakPointImg;
    property UnknownDisabledBreakPointImg: Integer read fUnknownDisabledBreakPointImg;
    property CurrentLineImg: Integer read FCurrentLineImg;
    property CurrentLineBreakPointImg: Integer read FCurrentLineBreakPointImg;
    property CurrentLineDisabledBreakPointImg: Integer read FCurrentLineDisabledBreakPointImg;
    property SourceLineImg: Integer read FSourceLineImg;
  end;
  
var
  SourceEditorMarks: TSourceMarks;
  
function CompareSourceMarks(Data1, Data2: Pointer): integer;

implementation

type
  TEditorIDAndLine = record
    EditorID: TObject;
    Line: integer;
  end;
  PEditorAndLine = ^TEditorIDAndLine;

function CompareSourceMarks(Data1, Data2: Pointer): integer;
var
  Mark1: TSourceMark absolute Data1;
  Mark2: TSourceMark absolute Data2;
begin
  Result := Mark1.Compare(Mark2);
end;

function CompareEditorIDAndLineWithMark(Key, Data: Pointer): integer;
var
  EditorAndLine: PEditorAndLine absolute Key;
  AMark: TSourceMark absolute Data;
begin
  Result := -AMark.CompareEditorAndLine(EditorAndLine^.EditorID, EditorAndLine^.Line);
end;

procedure TSourceSynMark.DoChange(AChanges: TSynEditMarkChangeReasons);
begin
  inherited DoChange(AChanges);
  if FChangeLock2 > 0 then exit;
  if assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSourceSynMark.Assign(Src: TSourceSynMark);
begin
  inc(FChangeLock2);
  IncChangeLock;
  try
    Line := Src.Line;
    Column := Src.Column;
    ImageIndex := Src.ImageIndex;
    Priority := Src.Priority;
    Visible := Src.Visible;
    InternalImage := Src.InternalImage;
    BookmarkNumber := Src.BookmarkNumber;
  finally
    DecChangeLock;
    dec(FChangeLock2);
  end;
end;

constructor TSourceSynMark.Create(AOwner: TSourceMark; AEditor: TSourceEditorInterface);
begin
  FSourceMark := AOwner;
  FSourceEditor := AEditor;
  FSynEdit := TSynEdit(FSourceEditor.EditorControl);
  Inherited Create(FSynEdit);
  FChangeLock2 := 0;
  if FSynEdit <> nil then
    FSynEdit.Marks.Add(Self);
end;

destructor TSourceSynMark.Destroy;
begin
  if FSynEdit<>nil then
    FSynEdit.Marks.Remove(Self);
  inherited Destroy;
end;

{ TSourceSynMarkList }

function TSourceSynMarkList.GetSM(Index: Integer): TSourceSynMark;
begin
  Result := TSourceSynMark(Get(Index));
end;

function TSourceSynMarkList.GetColumn: integer;
begin
  if Count = 0 then
    Result := -1
  else
    Result := Items[0].Column;
end;

function TSourceSynMarkList.GetImageIndex: integer;
begin
  if Count = 0 then
    Result := -1
  else
    Result := Items[0].ImageIndex;
end;

function TSourceSynMarkList.GetLine: integer;
begin
  if Count = 0 then
    Result := -1
  else
    Result := Items[0].Line;
end;

function TSourceSynMarkList.GetPriority: integer;
begin
  if Count = 0 then
    Result := -1
  else
    Result := Items[0].Priority;
end;

function TSourceSynMarkList.GetVisible: boolean;
begin
  if Count = 0 then
    Result := False
  else
    Result := Items[0].Visible;
end;

procedure TSourceSynMarkList.PutSM(Index: Integer; const AValue: TSourceSynMark
  );
begin
  AValue.OnChange := FOnChange;
  Put(Index, AValue);
end;

procedure TSourceSynMarkList.SetColumn(const AValue: integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Column := AValue;
end;

procedure TSourceSynMarkList.SetImageIndex(const AValue: integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].ImageIndex := AValue;
end;

procedure TSourceSynMarkList.SetLine(const AValue: integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Line := AValue;
end;

procedure TSourceSynMarkList.SetPriority(const AValue: integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Priority := AValue;
end;

procedure TSourceSynMarkList.SetVisible(const AValue: boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Visible := AValue;
end;

function TSourceSynMarkList.Add(Item: TSourceSynMark): Integer;
begin
  Item.OnChange := FOnChange;
  Result := inherited Add(Item);
end;

procedure TSourceSynMarkList.DoChange(AChanges: TSynEditMarkChangeReasons);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DoChange(AChanges);
end;

procedure TSourceSynMarkList.DeleteWithSourceEditor(
  ASrcEditor: TSourceEditorInterface);
var
  i: Integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    if Items[i].FSourceEditor = ASrcEditor then begin
      Items[i].Free;
      Delete(i);
    end;
    dec(i);
  end;
end;

function TSourceSynMarkList.IndexOfSourceEditor(AEditor: TSourceEditorInterface
  ): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].FSourceEditor <> AEditor) do
    dec(Result);
end;

procedure TSourceSynMarkList.IncChangeLock;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].IncChangeLock;
end;

procedure TSourceSynMarkList.DecChangeLock;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DecChangeLock;
end;

{ TSourceSynMark }

function TSourceSynMark.GetEdit: TSynEdit;
begin
  Result := FSynEdit;
end;

{ TSourceMark }

procedure TSourceMark.SetSourceMarks(const AValue: TSourceMarks);
begin
  if FSourceMarks=AValue then exit;
  if FSourceMarks<>nil then
    FSourceMarks.Remove(Self);
  FSourceEditorID := nil;
  FSourceMarks := AValue;
  if AValue<>nil then
    AValue.Add(Self);
end;

procedure TSourceMark.SetPriority(const AValue: integer);
begin
  if FPriority = AValue then exit;
  FPriority := AValue;
  if FSynMarkLock = 0 then
    FSynMarks.Priority := AValue;
end;

function TSourceMark.GetSourceEditorID: TObject;
begin
  if (FSourceEditorID = nil ) and (FSourceMarks <> nil) and
     (SourceEditor <> nil) and Assigned(FSourceMarks.OnGetSourceEditorID)
  then
    FSourceEditorID := FSourceMarks.OnGetSourceEditorID(SourceEditor);
  Result := FSourceEditorID;
end;

function TSourceMark.GetSourceEditor: TSourceEditorInterface;
begin
  if FSynMarks.Count = 0 then
    Result := nil
  else
    Result := FSynMarks[0].FSourceEditor;
end;

procedure TSourceMark.Changed;
begin
  if Assigned(FSourceMarks) and Assigned(FSourceMarks.OnAction) then
    FSourceMarks.OnAction(Self, maChanged);
end;

procedure TSourceMark.SynMarkChanged(Sender: TObject);
begin
  // Only read Value from Mark => Do not write back to Mark(s)
  inc(FSynMarkLock);
  Line := TSourceSynMark(Sender).Line;
  Column := TSourceSynMark(Sender).Column;
  Priority := TSourceSynMark(Sender).Priority;
  ImageIndex := TSourceSynMark(Sender).ImageIndex;
  Visible := TSourceSynMark(Sender).Visible;
  dec(FSynMarkLock);
end;

procedure TSourceMark.SetLineColorBackGround(const AValue: TColor);
begin
  if FLineColorBackGround=AValue then exit;
  FLineColorBackGround:=AValue;
  DoLineUpdate;
  Changed;
end;

procedure TSourceMark.SetLineColorForeGround(const AValue: TColor);
begin
  if FLineColorForeGround=AValue then exit;
  FLineColorForeGround:=AValue;
  DoLineUpdate;
  Changed;
end;

procedure TSourceMark.SetLineColorAttrib(
  const AValue: TAdditionalHilightAttribute);
begin
  if FLineColorAttrib=AValue then exit;
  FLineColorAttrib:=AValue;
  DoLineUpdate;
  Changed;
end;

procedure TSourceMark.SetIsBreakPoint(const AValue: boolean);
begin
  if FIsBreakPoint=AValue then exit;
  FIsBreakPoint:=AValue;
  DoLineUpdate;
  Changed;
end;

procedure TSourceMark.SetVisible(const AValue: boolean);
begin
  if Visible = AValue then Exit;
  FVisible := AValue;
  if FSynMarkLock = 0 then
    FSynMarks.Visible := AValue;
  Changed;
end;

procedure TSourceMark.DoPositionChanged;
var
  i: Integer;
begin
  i:=FHandlers[smhPositionChanged].Count;
  while FHandlers[smhPositionChanged].NextDownIndex(i) do
    TNotifyEvent(FHandlers[smhPositionChanged][i])(Self);
end;

procedure TSourceMark.DoLineUpdate(Force: Boolean = False);
begin
  if Line <= 0 then Exit;
  if Visible or Force then
    FSynMarks.DoChange([smcrChanged]);
end;

procedure TSourceMark.SetData(const AValue: TObject);
begin
  if FData=AValue then exit;
  FData:=AValue;
end;

function TSourceMark.EditorUpdateRequired: Boolean;
begin
  Result := (FLineColorAttrib <> ahaNone)
         or (FLineColorBackGround <> clNone)
         or (FLineColorForeGround <> clNone);
end;

procedure TSourceMark.AddHandler(HandlerType: TSourceMarkHandler;
  const Handler: TMethod);
begin
  if Handler.Code=nil then RaiseGDBException('TSourceMark.AddHandler');
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(Handler);
end;

procedure TSourceMark.SetColumn(const Value: Integer);
begin
  if Column=Value then exit;
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Remove(Self);
  FColumn := Value;
  if FSynMarkLock = 0 then
    FSynMarks.Column := Value;
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Add(Self);
  DoPositionChanged;
end;

procedure TSourceMark.SetImage(const Value: Integer);
begin
  if ImageIndex=Value then exit;
  FImage := Value;
  if FSynMarkLock = 0 then
    FSynMarks.ImageIndex := Value;
  Changed;
end;

procedure TSourceMark.SetLine(const Value: Integer);
begin
  if Line=Value then exit;
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Remove(Self);
  FLine := Value;
  if FSynMarkLock = 0 then
    FSynMarks.Line := Value;
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Add(Self);
  DoPositionChanged;
  Changed;
end;

constructor TSourceMark.Create(TheOwner: TSourceEditorInterface; TheData: TObject);
begin
  FSynMarkLock := 0;
  FSynMarks := TSourceSynMarkList.Create;
  FSynMarks.OnChange := @SynMarkChanged;
  FSynMarks.Add(TSourceSynMark.Create(Self, TheOwner));
  FData:=TheData;
  FLineColorAttrib:=ahaNone;
  FLineColorBackGround:=clNone;
  FLineColorForeGround:=clNone;
end;

destructor TSourceMark.Destroy;
var
  HandlerType: TSourceMarkHandler;
  i: Integer;
begin
  // notify all who wants to know
  i:=FHandlers[smhBeforeFree].Count;
  while FHandlers[smhBeforeFree].NextDownIndex(i) do
    TNotifyEvent(FHandlers[smhBeforeFree][i])(Self);
  // remove from source marks
  SourceMarks:=nil;
  // remove from editor component
  for i := 0 to FSynMarks.Count - 1 do
    FSynMarks[i].Free;
  FreeAndNil(FSynMarks);

  // free handler lists
  for HandlerType:=Low(TSourceMarkHandler) to high(TSourceMarkHandler) do
    FreeThenNil(FHandlers[HandlerType]);
    
  inherited Destroy;
end;

function TSourceMark.Compare(OtherMark: TSourceMark): integer;
begin
  Result:=PtrInt(SourceEditorID)-PtrInt(OtherMark.SourceEditorID);
  if Result<>0 then exit;
  Result:=Line-OtherMark.Line;
  if Result<>0 then exit;
  Result:=Column-OtherMark.Column;
  if Result <> 0 then exit;
  Result:=Priority-OtherMark.Priority;
  if Result <> 0 then exit;
  Result := PtrInt(Self) - PtrInt(OtherMark);
end;

function TSourceMark.CompareEditorAndLine(ASrcEditID: TObject;
  ALine: integer): integer;
begin
  Result := PtrInt(SourceEditorID) - PtrInt(ASrcEditID);
  if Result <> 0 then Exit;
  Result := Line - ALine;
end;

function TSourceMark.GetFilename: string;
begin
  Result:='';
  if FSourceMarks=nil then exit;
  Result:=FSourceMarks.GetFilename(Self);
end;

function TSourceMark.GetHint: string;
var
  i: Integer;
begin
  Result:='';
  i:=FHandlers[smhGetHint].Count;
  while FHandlers[smhGetHint].NextDownIndex(i) do
    TGetSourceMarkHintEvent(FHandlers[smhGetHint][i])(Self,Result);
end;

procedure TSourceMark.CreatePopupMenuItems(
  const AddMenuItemProc: TAddMenuItemProc);
var
  i: Integer;
begin
  i:=FHandlers[smhCreatePopupMenu].Count;
  while FHandlers[smhCreatePopupMenu].NextDownIndex(i) do
    TCreateSourceMarkPopupMenuEvent(FHandlers[smhCreatePopupMenu][i])
      (Self,AddMenuItemProc);
end;

procedure TSourceMark.IncChangeLock;
begin
  FSynMarks.IncChangeLock;
end;

procedure TSourceMark.DecChangeLock;
begin
  FSynMarks.DecChangeLock;
end;

procedure TSourceMark.RemoveAllHandlersForObject(HandlerObject: TObject);
var
  HandlerType: TSourceMarkHandler;
begin
  for HandlerType:=Low(TSourceMarkHandler) to High(TSourceMarkHandler) do
    if FHandlers[HandlerType]<>nil then
      FHandlers[HandlerType].RemoveAllMethodsOfObject(HandlerObject);
end;

procedure TSourceMark.AddPositionChangedHandler(OnPositionChanged: TNotifyEvent
  );
begin
  AddHandler(smhPositionChanged,TMethod(OnPositionChanged));
end;

procedure TSourceMark.RemovePositionChangedHandler(
  OnPositionChanged: TNotifyEvent);
begin
  FHandlers[smhPositionChanged].Remove(TMethod(OnPositionChanged));
end;

procedure TSourceMark.AddBeforeFreeHandler(OnBeforeFree: TNotifyEvent);
begin
  AddHandler(smhBeforeFree,TMethod(OnBeforeFree));
end;

procedure TSourceMark.RemoveBeforeFreeHandler(OnBeforeFree: TNotifyEvent);
begin
  FHandlers[smhBeforeFree].Remove(TMethod(OnBeforeFree));
end;

procedure TSourceMark.AddGetHintHandler(OnGetHint: TGetSourceMarkHintEvent);
begin
  AddHandler(smhGetHint,TMethod(OnGetHint));
end;

procedure TSourceMark.RemoveGetHintHandler(OnGetHint: TGetSourceMarkHintEvent);
begin
  FHandlers[smhGetHint].Remove(TMethod(OnGetHint));
end;

procedure TSourceMark.AddCreatePopupMenuHandler(
  OnCreatePopupMenu: TCreateSourceMarkPopupMenuEvent);
begin
  AddHandler(smhCreatePopupMenu,TMethod(OnCreatePopupMenu));
end;

procedure TSourceMark.RemoveCreatePopupMenuHandler(
  OnCreatePopupMenu: TCreateSourceMarkPopupMenuEvent);
begin
  FHandlers[smhCreatePopupMenu].Remove(TMethod(OnCreatePopupMenu));
end;

function TSourceMark.HasSourceEditor(AEditor: TSourceEditorInterface): Boolean;
begin
  Result := FSynMarks.IndexOfSourceEditor(AEditor) >= 0;
end;

procedure TSourceMark.AddSourceEditor(AEditor: TSourceEditorInterface);
var
  NewSynMark: TSourceSynMark;
begin
  NewSynMark := TSourceSynMark.Create(Self, AEditor);
  if FSynMarks.Count > 0 then
    NewSynMark.Assign(FSynMarks[0]);
  FSynMarks.Add(NewSynMark);
end;

{ TSourceMarks }

function TSourceMarks.GetItems(Index: integer): TSourceMark;
begin
  Result:=TSourceMark(FItems[Index]);
end;

procedure TSourceMarks.CreateImageList;
var
  i: Integer;
begin
  // create default mark icons
  ImgList:=TImageList.Create(Self);
  ImgList.Width:=11;
  ImgList.Height:=11;

  // synedit expects the first 10 icons for the bookmarks
  for i := 0 to 9 do
    AddImage('bookmark'+IntToStr(i));

  // load active breakpoint image
  fActiveBreakPointImg:=AddImage('ActiveBreakPoint');
  // load disabled breakpoint image
  fInactiveBreakPointImg:=AddImage('InactiveBreakPoint');
  // load invalid breakpoint image
  fInvalidBreakPointImg:=AddImage('InvalidBreakPoint');
  // load invalid disabled breakpoint image
  fInvalidDisabledBreakPointImg := AddImage('InvalidDisabledBreakPoint');
  // load unknown breakpoint image
  fUnknownBreakPointImg:=AddImage('UnknownBreakPoint');
  // load unknown disabled breakpoint image
  fUnknownDisabledBreakPointImg := AddImage('UnknownDisabledBreakPoint');
  // load multi mixed breakpoint image
  fMultiBreakPointImg:=AddImage('MultiBreakPoint');
  // load current line image
  FCurrentLineImg:=AddImage('debugger_current_line');
  // load current line + breakpoint image
  FCurrentLineBreakPointImg:=AddImage('debugger_current_line_breakpoint');
  // load current line + disabled breakpoint image
  FCurrentLineDisabledBreakPointImg := AddImage('debugger_current_line_disabled_breakpoint');
  // load source line
  FSourceLineImg:=AddImage('debugger_source_line');
end;

function TSourceMarks.FindFirstMarkNode(ASrcEditID: TObject; ALine: integer
  ): TAVLTreeNode;
var
  LeftNode: TAVLTreeNode;
  EditorIDAndLine: TEditorIDAndLine;
begin
  EditorIDAndLine.EditorID := ASrcEditID;
  EditorIDAndLine.Line := ALine;
  Result := fSortedItems.FindKey(@EditorIDAndLine, @CompareEditorIDAndLineWithMark);
  while Result <> nil do
  begin
    LeftNode := fSortedItems.FindPrecessor(Result);
    if (LeftNode = nil) or
       (CompareEditorIDAndLineWithMark(@EditorIDAndLine, LeftNode.Data) <> 0) then break;
    Result := LeftNode;
  end;
end;

constructor TSourceMarks.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fItems:=TList.Create;
  fSortedItems:=TAVLTree.Create(@CompareSourceMarks);
  CreateImageList;
end;

destructor TSourceMarks.Destroy;
begin
  Clear;
  FreeThenNil(FItems);
  FreeThenNil(fSortedItems);
  inherited Destroy;
end;

function TSourceMarks.Count: integer;
begin
  Result:=fItems.Count;
end;

procedure TSourceMarks.Clear;
begin
  while fItems.Count>0 do Delete(fItems.Count-1);
end;

function TSourceMarks.Add(AMark: TSourceMark): integer;
begin
  if AMark=nil then exit(-1);
  AMark.FSourceMarks:=Self;
  Result:=fItems.Add(AMark);
  fSortedItems.Add(AMark);
  if Assigned(FOnAction) then
    FOnAction(AMark, maAdded);
end;

function TSourceMarks.Add(ASrcEdit: TSourceEditorInterface; ALine: integer): TSourceMark;
begin
  Result:=TSourceMark.Create(ASrcEdit, nil);
  Result.Line := ALine;
  Add(Result);
end;

function TSourceMarks.AddCustomMark(TheOwner: TSourceEditorInterface; Data: TObject;
  MarkClass: TSourceMarkClass): TSourceMark;
begin
  if MarkClass=nil then MarkClass:=TSourceMark;
  Result:=MarkClass.Create(TheOwner,Data);
  Add(Result);
end;

procedure TSourceMarks.Delete(Index: integer);
var
  AMark: TSourceMark;
begin
  AMark:=Items[Index];
  AMark.fSourceMarks:=nil;
  fItems.Delete(Index);
  fSortedItems.Remove(AMark);
  if Assigned(FOnAction) then
    FOnAction(AMark, maRemoved);
  AMark.Free;
end;

procedure TSourceMarks.Remove(AMark: TSourceMark);
var
  i: Integer;
begin
  if (AMark=nil) or (AMark.SourceMarks<>Self) then exit;
  i:=fItems.IndexOf(AMark);
  if i<0 then exit;
  fItems.Delete(i);
  fSortedItems.Remove(AMark);
  AMark.fSourceMarks:=nil;
  if Assigned(FOnAction) then
    FOnAction(AMark, maRemoved);
end;

procedure TSourceMarks.AddSourceEditor(ANewEditor,
  AExistingEditor: TSourceEditorInterface);
var
  i: Integer;
  CurMark: TSourceMark;
  SrcEditorID: TObject;
begin
  if not Assigned(OnGetSourceEditorID) then exit;
  SrcEditorID := OnGetSourceEditorID(ANewEditor);
  i:=fItems.Count-1;
  while i>=0 do begin
    CurMark:=Items[i];
    if (CurMark.SourceEditorID = SrcEditorID) and
       (not CurMark.HasSourceEditor(ANewEditor))
    then
      CurMark.AddSourceEditor(ANewEditor);
    dec(i);
  end;
end;

procedure TSourceMarks.DeleteAllForEditor(ASrcEdit: TSourceEditorInterface);
var
  i: Integer;
  CurMark: TSourceMark;
  SrcEditorID: TObject;
begin
  if not Assigned(OnGetSourceEditorID) then exit;
  SrcEditorID := OnGetSourceEditorID(ASrcEdit);
  i:=fItems.Count-1;
  while i>=0 do begin
    CurMark:=Items[i];
    if CurMark.SourceEditorID = SrcEditorID then begin
      CurMark.FSynMarks.DeleteWithSourceEditor(ASrcEdit);
      if CurMark.FSynMarks.Count = 0 then
        Delete(i);
    end;
    dec(i);
  end;
end;

function TSourceMarks.FindFirstMark(ASrcEdit: TSourceEditorInterface; ALine: integer
  ): TSourceMark;
var
  AVLNode: TAVLTreeNode;
begin
  if not Assigned(OnGetSourceEditorID) then exit(nil);
  AVLNode:=FindFirstMarkNode(OnGetSourceEditorID(ASrcEdit), ALine);
  if AVLNode<>nil then
    Result:=TSourceMark(AVLNode.Data)
  else
    Result:=nil;
end;

function TSourceMarks.FindBreakPointMark(ASrcEdit: TSourceEditorInterface;
  ALine: integer): TSourceMark;
var
  AVLNode: TAVLTreeNode;
  EditorIDAndLine: TEditorIDAndLine;
  CurMark: TSourceMark;
begin
  Result := nil;
  if not Assigned(OnGetSourceEditorID) then exit;
  EditorIDAndLine.EditorID := OnGetSourceEditorID(ASrcEdit);
  EditorIDAndLine.Line := ALine;
  AVLNode := FindFirstMarkNode(EditorIDAndLine.EditorID, ALine);
  while (AVLNode <> nil) do
  begin
    CurMark := TSourceMark(AVLNode.Data);
    if CompareEditorIDAndLineWithMark(@EditorIDAndLine, CurMark) <> 0 then break;
    if CurMark.IsBreakPoint then
    begin
      Result := CurMark;
      Exit;
    end;
    AVLNode := fSortedItems.FindSuccessor(AVLNode);
  end;
end;

procedure TSourceMarks.GetMarksForLine(ASrcEdit: TSourceEditorInterface;
  ALine: integer; var Marks: PSourceMark; var MarkCount: integer);
var
  i, Capacity: integer;
  AVLNode: TAVLTreeNode;
  EditorIDAndLine: TEditorIDAndLine;
  CurMark: TSourceMark;
  HasChange: Boolean;
begin
  Capacity := 0;
  MarkCount := 0;
  Marks := nil;
  if not Assigned(OnGetSourceEditorID) then exit;
  EditorIDAndLine.EditorID := OnGetSourceEditorID(ASrcEdit);
  EditorIDAndLine.Line := ALine;
  AVLNode := FindFirstMarkNode(EditorIDAndLine.EditorID, ALine);
  while (AVLNode <> nil) do
  begin
    CurMark := TSourceMark(AVLNode.Data);
    if CompareEditorIDAndLineWithMark(@EditorIDAndLine, CurMark) <> 0 then break;
    if Capacity <= MarkCount then
    begin
      inc(Capacity, Capacity + 4);
      ReAllocMem(Marks, Capacity * SizeOf(Pointer));
    end;
    Marks[MarkCount] := CurMark;
    inc(MarkCount);
    AVLNode := fSortedItems.FindSuccessor(AVLNode);
  end;
  HasChange := MarkCount > 1;
  // easy popup sort by priority
  while HasChange do
  begin
    HasChange := False;
    for i := 0 to MarkCount - 2 do
      if Marks[i].Priority < Marks[i+1].Priority then
      begin
        CurMark := Marks[i];
        Marks[i] := Marks[i+1];
        Marks[i+1] := CurMark;
        HasChange := True;
      end;
  end;
end;

function TSourceMarks.AddImage(const ResName: string): integer;
begin
  Result := ImgList.AddLazarusResource(Resname);
end;

function TSourceMarks.GetFilename(AMark: TSourceMark): string;
begin
  Result:='';
  if (AMark=nil) or (not Assigned(OnGetFilename)) then exit;
  if AMark.SourceEditor=nil then exit;
  Result:=OnGetFilename(AMark.SourceEditor);
end;

initialization
  SourceEditorMarks:=nil;

end.

