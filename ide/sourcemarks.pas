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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  Graphics, Controls, LCLProc,
  // LazUtils
  LazMethodList,
  // SynEdit
  SynEdit, SynEditMarks, SynEditMarkupGutterMark,
  // IdeIntf
  MenuIntf, SrcEditorIntf, IDEExternToolIntf, IDEImagesIntf,
  // IDE
  etSrcEditMarks, ImgList;
  
type
  TAdditionalHilightAttribute =
    (ahaNone,              ahaTextBlock,          ahaExecutionPoint,
     ahaEnabledBreakpoint, ahaDisabledBreakpoint, ahaInvalidBreakpoint,
     ahaUnknownBreakpoint, ahaErrorLine,          ahaIncrementalSearch,
     ahaHighlightAll,      ahaBracketMatch,       ahaMouseLink,
     ahaLineNumber,        ahaLineHighlight,      ahaModifiedLine,
     ahaCodeFoldingTree,   ahaHighlightWord,      ahaFoldedCode,
     ahaFoldedCodeLine,    ahaHiddenCodeLine,
     ahaWordGroup,         ahaTemplateEditCur,    ahaTemplateEditSync,
     ahaTemplateEditOther, ahaSyncroEditCur,      ahaSyncroEditSync,
     ahaSyncroEditOther,   ahaSyncroEditArea,     ahaGutterSeparator,
     ahaGutter,            ahaRightMargin,        ahaSpecialVisibleChars,
     ahaTopInfoHint,
     ahaIfDefBlockInactive, ahaIfDefBlockActive, ahaIfDefBlockTmpActive,
     ahaIfDefNodeInactive, ahaIfDefNodeActive, ahaIfDefNodeTmpActive,
     ahaIdentComplWindow, ahaIdentComplWindowBorder, ahaIdentComplWindowSelection, ahaIdentComplWindowHighlight,
     ahaOutlineLevel1Color, ahaOutlineLevel2Color, ahaOutlineLevel3Color, ahaOutlineLevel4Color, ahaOutlineLevel5Color, ahaOutlineLevel6Color, ahaOutlineLevel7Color, ahaOutlineLevel8Color, ahaOutlineLevel9Color, ahaOutlineLevel10Color
     );

  TAhaGroupName = (
    agnDefault, agnLanguage, agnText, agnLine, agnGutter, agnTemplateMode, agnSyncronMode,
    agnIfDef, agnIdentComplWindow, agnOutlineColors
  );

  TSourceEditorBase = class;

  { TSourceEditorSharedValuesBase }

  TSourceEditorSharedValuesBase = class
  protected
    function GetSharedEditorsBase(Index: Integer): TSourceEditorBase; virtual abstract;
    function SharedEditorCount: Integer; virtual; abstract;
    function Filename: string; virtual; abstract;
  end;

  { TSourceEditorBase }

  TSourceEditorBase = class(TSourceEditorInterface)
  protected
    function GetSharedValues: TSourceEditorSharedValuesBase; virtual; abstract;
  end;

  { *** }

  TSourceMarks = class;
  TSourceMark = class;

  TGetSourceMarkHintEvent =
    procedure(SenderMark: TSourceMark; var Hint: string) of object;
  TCreateSourceMarkPopupMenuEvent =
    procedure(SenderMark: TSourceMark;
              const AddMenuItem: TAddMenuItemProc) of object;
  TGetFilenameEvent = function(ASourceEditor: TObject): string of object;

  TSourceMarkHandler = (
    smhPositionChanged,
    smhBeforeFree,
    smhGetHint,
    smhCreatePopupMenu
    );

  TMarksAction = (maAdded, maRemoved, maChanged);
  TMarksActionEvent = procedure(AMark: TSourceMark; Action: TMarksAction) of object;


  { TSourceMark }
  
  TSourceMark = class(TSynEditMarkupMark)
  private
    FData: TObject;
    FSourceMarks: TSourceMarks;
    FSourceEditorID: TSourceEditorSharedValuesBase;
    FHandlers: array[TSourceMarkHandler] of TMethodList;
    function  GetSourceEditor: TSourceEditorBase;
    procedure SetSourceMarks(const AValue: TSourceMarks);
    procedure Changed;
  private
    FIsBreakPoint: boolean;
    FLineColorAttrib: TAdditionalHilightAttribute;
    FLineColorBackGround: TColor;
    FLineColorForeGround: TColor;
  protected
    procedure DoChange(AChanges: TSynEditMarkChangeReasons); override;
    procedure AddHandler(HandlerType: TSourceMarkHandler;
                         const Handler: TMethod);
    procedure DoPositionChanged; virtual;
    procedure DoLineUpdate; virtual;

    procedure SetData(const AValue: TObject); virtual;
    procedure SetIsBreakPoint(const AValue: boolean); virtual;
    procedure SetLineColorAttrib(const AValue: TAdditionalHilightAttribute); virtual;
    procedure SetLineColorBackGround(const AValue: TColor); virtual;
    procedure SetLineColorForeGround(const AValue: TColor); virtual;

    procedure SetColumn(const Value: Integer); override;
    procedure SetLine(const Value: Integer); override;
  public
    constructor Create(TheOwner: TSourceEditorBase; TheData: TObject);
    destructor Destroy; override;
    function Compare(OtherMark: TSourceMark): integer;
    function CompareEditorAndLine(ASrcEditID: TObject;
                                  ALine: integer): integer;
    function GetFilename: string;
    function GetHint: string; virtual;
    procedure CreatePopupMenuItems(const AddMenuItemProc: TAddMenuItemProc);
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
    property SourceEditor: TSourceEditorBase read GetSourceEditor;
    property SourceEditorID: TSourceEditorSharedValuesBase read FSourceEditorID;
  public
    property LineColorAttrib: TAdditionalHilightAttribute read FLineColorAttrib
                                                       write SetLineColorAttrib;
    property LineColorForeGround: TColor read FLineColorForeGround
                                         write SetLineColorForeGround;
    property LineColorBackGround: TColor read FLineColorBackGround
                                         write SetLineColorBackGround;
  public
    property IsBreakPoint: boolean read FIsBreakPoint write SetIsBreakPoint;
  end;
  
  TSourceMarkClass = class of TSourceMark;
  PSourceMark = ^TSourceMark;
  
  
  { TSourceMarks }
  
  TSourceMarks = class(TComponent)
  private
    fActiveBreakPointImg: Integer;
    FCurrentLineBreakPointImg: Integer;
    FCurrentLineImg: Integer;
    FCurrentLineDisabledBreakPointImg: Integer;
    FExtToolsMarks: TETMarks;
    fPendingBreakPointImg: Integer;
    FSourceLineImg: Integer;
    FImgList: TLCLGlyphs;
    fInactiveBreakPointImg: Integer;
    fInvalidBreakPointImg: Integer;
    fInvalidDisabledBreakPointImg: Integer;
    fItems: TList;// list of TSourceMark
    fMultiBreakPointImg: Integer;
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
    function Add(ASrcEdit: TSourceEditorBase; ALine: integer): TSourceMark;
    function AddCustomMark(TheOwner: TSourceEditorBase; Data: TObject;
                           MarkClass: TSourceMarkClass): TSourceMark;
    function AddImage(const ResName: string): integer;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Remove(AMark: TSourceMark);
    procedure DeleteAllForEditor(ASrcEdit: TSourceEditorBase);
    procedure DeleteAllForEditorID(ASrcEditID: TSourceEditorSharedValuesBase);
    function FindFirstMark(ASrcEdit: TSourceEditorBase;
                           ALine: integer): TSourceMark;
    function FindBreakPointMark(ASrcEdit: TSourceEditorBase;
                                ALine: integer): TSourceMark;
    procedure GetMarksForLine(ASrcEdit: TSourceEditorBase; ALine: integer;
                              out Marks: PSourceMark; out MarkCount: integer);
  public
    property ImgList: TLCLGlyphs read FImgList;
    property Items[Index: integer]: TSourceMark read GetItems; default;
    property OnAction: TMarksActionEvent read FOnAction write FOnAction;
    property ExtToolsMarks: TETMarks read FExtToolsMarks;
  public
    // icon index
    property ActiveBreakPointImg: Integer read fActiveBreakPointImg;
    property InactiveBreakPointImg: Integer read fInactiveBreakPointImg;
    property InvalidBreakPointImg: Integer read fInvalidBreakPointImg;
    property InvalidDisabledBreakPointImg: Integer read fInvalidDisabledBreakPointImg;
    property PendingBreakPointImg: Integer read fPendingBreakPointImg;
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

{ TSourceMark }

procedure TSourceMark.SetSourceMarks(const AValue: TSourceMarks);
begin
  if FSourceMarks=AValue then exit;
  if FSourceMarks<>nil then
    FSourceMarks.Remove(Self);
  FSourceMarks := AValue;
  if AValue<>nil then
    AValue.Add(Self);
end;

function TSourceMark.GetSourceEditor: TSourceEditorBase;
begin
  if (FSourceEditorID <> nil) and (FSourceEditorID.SharedEditorCount > 0) then
    Result := FSourceEditorID.GetSharedEditorsBase(0)
  else
    Result := nil;
end;

procedure TSourceMark.Changed;
begin
  if Assigned(FSourceMarks) and Assigned(FSourceMarks.OnAction) then
    FSourceMarks.OnAction(Self, maChanged);
end;

procedure TSourceMark.DoChange(AChanges: TSynEditMarkChangeReasons);
begin
  inherited DoChange(AChanges);
  if AChanges * [smcrLine, smcrColumn] <> [] then
    DoPositionChanged;
  Changed;
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
  if (Line <= 0) or (not Visible) then Exit;
  DoChange([smcrChanged]);
end;

procedure TSourceMark.SetData(const AValue: TObject);
begin
  if FData=AValue then exit;
  FData:=AValue;
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
  IncChangeLock;
  if FSourceMarks<>nil then
    FSourceMarks.fSortedItems.Remove(Self);
  inherited;
  if FSourceMarks<>nil then
    FSourceMarks.fSortedItems.Add(Self);
  DecChangeLock;
end;

procedure TSourceMark.SetLine(const Value: Integer);
begin
  if Line=Value then exit;
  IncChangeLock;
  if FSourceMarks<>nil then
    FSourceMarks.fSortedItems.Remove(Self);
  inherited;
  if FSourceMarks<>nil then
    FSourceMarks.fSortedItems.Add(Self);
  DecChangeLock;
end;

constructor TSourceMark.Create(TheOwner: TSourceEditorBase; TheData: TObject);
begin
  FSourceEditorID := TheOwner.GetSharedValues;
  inherited Create(TSynEdit(TheOwner.EditorControl));
  FData:=TheData;
  FLineColorAttrib:=ahaNone;
  FLineColorBackGround:=clNone;
  FLineColorForeGround:=clNone;
  TSynEdit(TheOwner.EditorControl).Marks.Add(Self);
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
  SourceMarks := nil;
  FSourceEditorID := nil;

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
  Result := ComparePointers(Self,OtherMark);
end;

function TSourceMark.CompareEditorAndLine(ASrcEditID: TObject;
  ALine: integer): integer;
begin
  Result := ComparePointers(SourceEditorID,ASrcEditID);
  if Result <> 0 then Exit;
  Result := Line - ALine;
end;

function TSourceMark.GetFilename: string;
begin
  Result:=FSourceEditorID.Filename;
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

procedure TSourceMark.AddPositionChangedHandler(OnPositionChanged: TNotifyEvent);
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
  ImgIDFatal: Integer;
  ImgIDError: Integer;
  ImgIDWarning: Integer;
  ImgIDNote: Integer;
  ImgIDHint: Integer;
begin
  // create default mark icons
  FImgList:=TLCLGlyphs.Create(Self);
  FImgList.Width := 11;
  FImgList.Height := 11;
  FImgList.RegisterResolutions([11, 16, 22], [100, 150, 200]);

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
  // load pending active breakpoint image
  fPendingBreakPointImg := AddImage('PendingBreakPoint');
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

  ExtToolsMarks.ImageList:=ImgList;
  ImgIDFatal:=AddImage('state11x11_fatal');
  ImgIDError:=AddImage('state11x11_error');
  ImgIDWarning:=AddImage('state11x11_warning');
  ImgIDNote:=AddImage('state11x11_note');
  ImgIDHint:=AddImage('state11x11_hint');
  ExtToolsMarks.MarkStyles[mluNone].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluProgress].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluDebug].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluVerbose3].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluVerbose2].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluVerbose].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluHint].ImageIndex:=ImgIDHint;
  ExtToolsMarks.MarkStyles[mluNote].ImageIndex:=ImgIDNote;
  ExtToolsMarks.MarkStyles[mluWarning].ImageIndex:=ImgIDWarning;
  ExtToolsMarks.MarkStyles[mluImportant].ImageIndex:=-1;
  ExtToolsMarks.MarkStyles[mluError].ImageIndex:=ImgIDError;
  ExtToolsMarks.MarkStyles[mluFatal].ImageIndex:=ImgIDFatal;
  ExtToolsMarks.MarkStyles[mluPanic].ImageIndex:=ImgIDFatal;
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
  FExtToolsMarks:=TETMarks.Create(nil);
  CreateImageList;
end;

destructor TSourceMarks.Destroy;
begin
  Clear;
  FreeAndNil(FExtToolsMarks);
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

function TSourceMarks.Add(ASrcEdit: TSourceEditorBase; ALine: integer): TSourceMark;
begin
  Result:=TSourceMark.Create(ASrcEdit, nil);
  Result.Line := ALine;
  Add(Result);
end;

function TSourceMarks.AddCustomMark(TheOwner: TSourceEditorBase; Data: TObject;
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

procedure TSourceMarks.DeleteAllForEditor(ASrcEdit: TSourceEditorBase);
begin
  DeleteAllForEditorID(ASrcEdit.GetSharedValues);
end;

procedure TSourceMarks.DeleteAllForEditorID(ASrcEditID: TSourceEditorSharedValuesBase);
var
  i: Integer;
begin
  if ASrcEditID = nil then
    exit;
  i:=fItems.Count-1;
  while i>=0 do begin
    if Items[i].SourceEditorID = ASrcEditID then
      Delete(i);
    dec(i);
  end;
end;

function TSourceMarks.FindFirstMark(ASrcEdit: TSourceEditorBase; ALine: integer): TSourceMark;
var
  AVLNode: TAVLTreeNode;
  SrcEditorID: TSourceEditorSharedValuesBase;
begin
  Result := nil;
  SrcEditorID := ASrcEdit.GetSharedValues;
  if SrcEditorID = nil then
    exit;
  AVLNode:=FindFirstMarkNode(SrcEditorID, ALine);
  if AVLNode<>nil then
    Result:=TSourceMark(AVLNode.Data);
end;

function TSourceMarks.FindBreakPointMark(ASrcEdit: TSourceEditorBase;
  ALine: integer): TSourceMark;
var
  AVLNode: TAVLTreeNode;
  EditorIDAndLine: TEditorIDAndLine;
  CurMark: TSourceMark;
  SrcEditorID: TSourceEditorSharedValuesBase;
begin
  Result := nil;
  SrcEditorID := ASrcEdit.GetSharedValues;
  if SrcEditorID = nil then
    exit;

  EditorIDAndLine.EditorID := SrcEditorID;
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

procedure TSourceMarks.GetMarksForLine(ASrcEdit: TSourceEditorBase;
  ALine: integer; out Marks: PSourceMark; out MarkCount: integer);
var
  i, Capacity: integer;
  AVLNode: TAVLTreeNode;
  EditorIDAndLine: TEditorIDAndLine;
  CurMark: TSourceMark;
  HasChange: Boolean;
  SrcEditorID: TSourceEditorSharedValuesBase;
begin
  MarkCount := 0;
  Marks := nil;
  SrcEditorID := ASrcEdit.GetSharedValues;
  if SrcEditorID = nil then
    exit;

  Capacity := 0;
  EditorIDAndLine.EditorID := SrcEditorID;
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
  Result := FImgList.GetImageIndex(Resname);
end;

initialization
  SourceEditorMarks:=nil;

end.

