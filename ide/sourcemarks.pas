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
  AVL_Tree, SynEdit, IDEProcs, MenuIntf, EditorOptions;
  
type
  TSourceMarks = class;
  TSourceMark = class;

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
    
  TSourceMark = class(TSynEditMark)
  private
    FData: TObject;
    FHandlers: array[TSourceMarkHandler] of TMethodList;
    FIsBreakPoint: boolean;
    FLineColorAttrib: TAdditionalHilightAttribute;
    FLineColorBackGround: TColor;
    FLineColorForeGround: TColor;
    FSourceEditor: TObject;
    FSourceMarks: TSourceMarks;
    FSynEdit: TCustomSynEdit;
    procedure SetSourceMarks(const AValue: TSourceMarks);
  protected
    function GetEdit: TCustomSynEdit; override;
    procedure AddHandler(HandlerType: TSourceMarkHandler;
                         const Handler: TMethod);
    procedure DoPositionChanged; virtual;
    procedure DoLineUpdate; virtual;
    procedure GetSourceEditor; virtual;
    function  EditorUpdateRequired: Boolean; virtual; // called to check if we need to update the editor if a property is changed
    procedure SetColumn(const Value: Integer); override;
    procedure SetData(const AValue: TObject); virtual;
    procedure SetImage(const Value: Integer); override;
    procedure SetIsBreakPoint(const AValue: boolean); virtual;
    procedure SetLine(const Value: Integer); override;
    procedure SetLineColorAttrib(const AValue: TAdditionalHilightAttribute); virtual;
    procedure SetLineColorBackGround(const AValue: TColor); virtual;
    procedure SetLineColorForeGround(const AValue: TColor); virtual;
    procedure SetSourceEditor(const AValue: TObject); virtual;
    procedure SetVisible(const AValue: boolean); override;
  public
    constructor Create(TheOwner: TCustomSynEdit; TheData: TObject);
    destructor Destroy; override;
    function Compare(OtherMark: TSourceMark): integer;
    function CompareEditorAndLine(ASynEdit: TCustomSynEdit;
                                  ALine: integer): integer;
    function GetFilename: string;
    function GetHint: string; virtual;
    procedure CreatePopupMenuItems(const AddMenuItemProc: TAddMenuItemProc);
  public
    // handlers
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
    property SourceEditor: TObject read FSourceEditor write SetSourceEditor;
    property SourceMarks: TSourceMarks read FSourceMarks write SetSourceMarks;
    property SynEdit: TCustomSynEdit read FSynEdit;
    property IsBreakPoint: boolean read FIsBreakPoint write SetIsBreakPoint;
    property LineColorAttrib: TAdditionalHilightAttribute read FLineColorAttrib
                                                       write SetLineColorAttrib;
    property LineColorForeGround: TColor read FLineColorForeGround
                                         write SetLineColorForeGround;
    property LineColorBackGround: TColor read FLineColorBackGround
                                         write SetLineColorBackGround;
  end;
  
  TSourceMarkClass = class of TSourceMark;
  PSourceMark = ^TSourceMark;
  
  
  { TSourceMarks }
  
  TGetSourceEditorEvent = function(ASynEdit: TCustomSynEdit): TObject of object;
  TGetFilenameEvent = function(ASourceEditor: TObject): string of object;
  
  TSourceMarks = class(TComponent)
  private
    fActiveBreakPointImg: Integer;
    FImgList: TImageList;
    fInactiveBreakPointImg: Integer;
    fInvalidBreakPointImg: Integer;
    fItems: TList;// list of TSourceMark
    fMultiBreakPointImg: Integer;
    FOnGetFilename: TGetFilenameEvent;
    FOnGetSourceEditor: TGetSourceEditorEvent;
    fSortedItems: TAVLTree;// tree of TSourceMark
    fUnknownBreakPointImg: Integer;
    function GetItems(Index: integer): TSourceMark;
    procedure CreateImageList;
  protected
    function FindFirstMarkNode(ASynEdit: TCustomSynEdit;
                               ALine: integer): TAVLTreeNode;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Count: integer;
    function Add(AMark: TSourceMark): integer;
    function Add(ASynEdit: TCustomSynEdit; ALine: integer): TSourceMark;
    function AddCustomMark(TheOwner: TCustomSynEdit; Data: TObject;
                           MarkClass: TSourceMarkClass): TSourceMark;
    function AddImage(const ResName: string): integer;
    function GetFilename(AMark: TSourceMark): string;
    function GetSourceEditor(AMark: TSourceMark): TObject;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Remove(AMark: TSourceMark);
    procedure DeleteAllForEditor(ASynEdit: TCustomSynEdit);
    function FindFirstMark(ASynEdit: TCustomSynEdit;
                           ALine: integer): TSourceMark;
    function FindBreakPointMark(ASynEdit: TCustomSynEdit;
                                ALine: integer): TSourceMark;
    procedure GetMarksForLine(ASynEdit: TCustomSynEdit; ALine: integer;
                              var Marks: PSourceMark; var MarkCount: integer);
  public
    property ImgList: TImageList read FImgList write FImgList;
    property Items[Index: integer]: TSourceMark read GetItems; default;
    property OnGetFilename: TGetFilenameEvent read FOnGetFilename
                                              write FOnGetFilename;
    property OnGetSourceEditor: TGetSourceEditorEvent read FOnGetSourceEditor
                                                      write FOnGetSourceEditor;
  public
    // icon index
    property ActiveBreakPointImg: Integer read fActiveBreakPointImg;
    property InactiveBreakPointImg: Integer read fInactiveBreakPointImg;
    property InvalidBreakPointImg: Integer read fInvalidBreakPointImg;
    property MultiBreakPointImg: Integer read fMultiBreakPointImg;
    property UnknownBreakPointImg: Integer read fUnknownBreakPointImg;
  end;
  
var
  SourceEditorMarks: TSourceMarks;
  
function CompareSourceMarks(Data1, Data2: Pointer): integer;

implementation

type
  TEditorAndLine = record
    Editor: TCustomSynEdit;
    Line: integer;
  end;
  PEditorAndLine = ^TEditorAndLine;

function CompareSourceMarks(Data1, Data2: Pointer): integer;
var
  Mark1: TSourceMark;
  Mark2: TSourceMark;
begin
  Mark1:=TSourceMark(Data1);
  Mark2:=TSourceMark(Data2);
  Result:=Mark1.Compare(Mark2);
end;

function CompareEditorAndLineWithMark(Key, Data: Pointer): integer;
var
  EditorAndLine: PEditorAndLine;
  AMark: TSourceMark;
begin
  EditorAndLine:=PEditorAndLine(Key);
  AMark:=TSourceMark(Data);
  Result:=AMark.CompareEditorAndLine(EditorAndLine^.Editor,EditorAndLine^.Line);
  Result:=-Result;
end;

{ TSourceMark }

procedure TSourceMark.SetSourceMarks(const AValue: TSourceMarks);
begin
  if FSourceMarks=AValue then exit;
  if FSourceMarks<>nil then
    FSourceMarks.Remove(Self);
  if AValue<>nil then
    AValue.Add(Self);
end;

procedure TSourceMark.SetLineColorBackGround(const AValue: TColor);
begin
  if FLineColorBackGround=AValue then exit;
  FLineColorBackGround:=AValue;
  DoLineUpdate;
end;

procedure TSourceMark.SetLineColorForeGround(const AValue: TColor);
begin
  if FLineColorForeGround=AValue then exit;
  FLineColorForeGround:=AValue;
  DoLineUpdate;
end;

procedure TSourceMark.SetLineColorAttrib(
  const AValue: TAdditionalHilightAttribute);
begin
  if FLineColorAttrib=AValue then exit;
  FLineColorAttrib:=AValue;
  DoLineUpdate;
end;

procedure TSourceMark.SetIsBreakPoint(const AValue: boolean);
begin
  if FIsBreakPoint=AValue then exit;
  FIsBreakPoint:=AValue;
  DoLineUpdate;
end;

procedure TSourceMark.SetSourceEditor(const AValue: TObject);
begin
  if FSourceEditor=AValue then exit;
  FSourceEditor:=AValue;
  GetFilename;
end;

procedure TSourceMark.SetVisible(const AValue: boolean);
begin
  if Visible = AValue then Exit;
  inherited SetVisible(AValue);
  if EditorUpdateRequired then DoLineUpdate;
end;

procedure TSourceMark.DoPositionChanged;
var
  i: Integer;
begin
  i:=FHandlers[smhPositionChanged].Count;
  while FHandlers[smhPositionChanged].NextDownIndex(i) do
    TNotifyEvent(FHandlers[smhPositionChanged][i])(Self);
end;

procedure TSourceMark.DoLineUpdate;
begin
  if not Visible then Exit;
  if SynEdit = nil then Exit;
  if Line <= 0 then Exit;
  
  SynEdit.InvalidateLine(Line)
end;

procedure TSourceMark.SetData(const AValue: TObject);
begin
  if FData=AValue then exit;
  FData:=AValue;
end;

procedure TSourceMark.GetSourceEditor;
begin
  if FSourceMarks<>nil then
    FSourceEditor:=FSourceMarks.GetSourceEditor(Self);
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

function TSourceMark.GetEdit: TCustomSynEdit;
begin
  Result:=FSynEdit;
end;

procedure TSourceMark.SetColumn(const Value: Integer);
begin
  if Column=Value then exit;
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Remove(Self);
  inherited SetColumn(Value);
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Add(Self);
  DoPositionChanged;
end;

procedure TSourceMark.SetImage(const Value: Integer);
begin
  if ImageIndex=Value then exit;
  inherited SetImage(Value);
end;

procedure TSourceMark.SetLine(const Value: Integer);
begin
  if Line=Value then exit;
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Remove(Self);
  inherited SetLine(Value);
  if FSourceMarks<>nil then FSourceMarks.fSortedItems.Add(Self);
  DoPositionChanged;
  if EditorUpdateRequired then DoLineUpdate;
end;

constructor TSourceMark.Create(TheOwner: TCustomSynEdit; TheData: TObject);
begin
  inherited Create(TheOwner);
  fSynEdit:=TheOwner;
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
  // remove from editor component
  if fSynEdit<>nil then fSynEdit.Marks.Remove(Self);
  // remove from source marks
  SourceMarks:=nil;
    
  // free handler lists
  for HandlerType:=Low(TSourceMarkHandler) to high(TSourceMarkHandler) do
    FreeThenNil(FHandlers[HandlerType]);
    
  inherited Destroy;
end;

function TSourceMark.Compare(OtherMark: TSourceMark): integer;
begin
  Result:=PtrInt(fSynEdit)-PtrInt(OtherMark.fSynEdit);
  if Result<>0 then exit;
  Result:=Line-OtherMark.Line;
  if Result<>0 then exit;
  Result:=Column-OtherMark.Column;
end;

function TSourceMark.CompareEditorAndLine(ASynEdit: TCustomSynEdit;
  ALine: integer): integer;
begin
  Result:=PtrInt(fSynEdit)-PtrInt(ASynEdit);
  if Result<>0 then exit;
  Result:=Line-ALine;
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
  // load inactive breakpoint image
  fInactiveBreakPointImg:=AddImage('InactiveBreakPoint');
  // load invalid breakpoint image
  fInvalidBreakPointImg:=AddImage('InvalidBreakPoint');
  // load unknown breakpoint image
  fUnknownBreakPointImg:=AddImage('UnknownBreakPoint');
  // load multi mixed breakpoint image
  fMultiBreakPointImg:=AddImage('MultiBreakPoint');
end;

function TSourceMarks.FindFirstMarkNode(ASynEdit: TCustomSynEdit; ALine: integer
  ): TAVLTreeNode;
var
  EditorAndLine: TEditorAndLine;
begin
  EditorAndLine.Editor:=ASynEdit;
  EditorAndLine.Line:=ALine;
  Result:=fSortedItems.FindLeftMostKey(@EditorAndLine,
                                       @CompareEditorAndLineWithMark);
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
  if AMark=nil then exit;
  Result:=fItems.Add(AMark);
  fSortedItems.Add(AMark);
  AMark.FSourceMarks:=Self;
end;

function TSourceMarks.Add(ASynEdit: TCustomSynEdit; ALine: integer
  ): TSourceMark;
begin
  Result:=TSourceMark.Create(ASynEdit,nil);
  Result.Line := ALine;
  Add(Result);
end;

function TSourceMarks.AddCustomMark(TheOwner: TCustomSynEdit; Data: TObject;
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
  AMark.Free;
end;

procedure TSourceMarks.Remove(AMark: TSourceMark);
var
  i: Integer;
begin
  if (AMark=nil) or (AMark.SourceMarks<>Self) then exit;
  i:=fItems.IndexOf(AMark);
  if i<0 then exit;
  AMark.fSourceMarks:=nil;
  fItems.Delete(i);
  fSortedItems.Remove(AMark);
end;

procedure TSourceMarks.DeleteAllForEditor(ASynEdit: TCustomSynEdit);
var
  i: Integer;
  CurMark: TSourceMark;
begin
  i:=fItems.Count-1;
  while i>=0 do begin
    CurMark:=Items[i];
    if CurMark.SynEdit=ASynEdit then
      Delete(i);
    dec(i);
  end;
end;

function TSourceMarks.FindFirstMark(ASynEdit: TCustomSynEdit; ALine: integer
  ): TSourceMark;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindFirstMarkNode(ASynEdit,ALine);
  if AVLNode<>nil then
    Result:=TSourceMark(AVLNode.Data)
  else
    Result:=nil;
end;

function TSourceMarks.FindBreakPointMark(ASynEdit: TCustomSynEdit;
  ALine: integer): TSourceMark;
var
  AVLNode: TAVLTreeNode;
  EditorAndLine: TEditorAndLine;
  CurMark: TSourceMark;
begin
  Result:=nil;
  EditorAndLine.Editor:=ASynEdit;
  EditorAndLine.Line:=ALine;
  AVLNode:=fSortedItems.FindLeftMostKey(@EditorAndLine,
                                        @CompareEditorAndLineWithMark);
  while (AVLNode<>nil) do begin
    CurMark:=TSourceMark(AVLNode.Data);
    if CompareEditorAndLineWithMark(@EditorAndLine,CurMark)<>0 then break;
    if CurMark.IsBreakPoint then begin
      Result:=CurMark;
      exit;
    end;
    AVLNode:=fSortedItems.FindSuccessor(AVLNode);
  end;
end;

procedure TSourceMarks.GetMarksForLine(ASynEdit: TCustomSynEdit;
  ALine: integer; var Marks: PSourceMark; var MarkCount: integer);
var
  Capacity: integer;
  AVLNode: TAVLTreeNode;
  EditorAndLine: TEditorAndLine;
  CurMark: TSourceMark;
begin
  Capacity:=0;
  MarkCount:=0;
  Marks:=nil;
  EditorAndLine.Editor:=ASynEdit;
  EditorAndLine.Line:=ALine;
  AVLNode:=fSortedItems.FindLeftMostKey(@EditorAndLine,
                                        @CompareEditorAndLineWithMark);
  while (AVLNode<>nil) do begin
    CurMark:=TSourceMark(AVLNode.Data);
    if CompareEditorAndLineWithMark(@EditorAndLine,CurMark)<>0 then break;
    if Capacity<=MarkCount then begin
      inc(Capacity,Capacity+4);
      ReAllocMem(Marks,Capacity*SizeOf(Pointer));
    end;
    Marks[MarkCount]:=CurMark;
    inc(MarkCount);
    AVLNode:=fSortedItems.FindSuccessor(AVLNode);
  end;
end;

function TSourceMarks.AddImage(const ResName: string): integer;
var
  APixmap: TPixmap;
begin
  APixmap:=TPixMap.Create;
  APixmap.TransparentColor:=clBtnFace;
  APixmap.LoadFromLazarusResource(ResName);
  Result:=ImgList.Count;
  ImgList.AddDirect(APixmap,nil);
end;

function TSourceMarks.GetSourceEditor(AMark: TSourceMark): TObject;
begin
  Result:=nil;
  if (AMark=nil) or (AMark.fSynEdit=nil) then exit;
  if not Assigned(OnGetSourceEditor) then exit;
  Result:=OnGetSourceEditor(AMark.fSynEdit);
end;

function TSourceMarks.GetFilename(AMark: TSourceMark): string;
begin
  Result:='';
  if (AMark=nil) or (not Assigned(OnGetFilename)) then exit;
  if AMark.FSourceEditor=nil then begin
    AMark.GetSourceEditor;
    if AMark.FSourceEditor=nil then exit;
  end;
  Result:=OnGetFilename(AMark.FSourceEditor);
end;

initialization
  SourceEditorMarks:=nil;

end.

