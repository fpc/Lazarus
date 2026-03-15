{ $Id: carbonwsstdctrls.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                              CocoaWSStdCtrls.pp                           *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSStdCtrls;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  // Libs
  MacOSAll, CocoaAll, Classes, sysutils,
  // LCL
  Controls, StdCtrls, ComboEx, Graphics, LCLType, LMessages, LCLProc, LCLMessageGlue,
  Forms, ComCtrls,
  // LazUtils
  LazUTF8, TextStrings,
  // Widgetset
  WSStdCtrls, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate, CocoaCallback, CocoaListControl, CocoaTables, CocoaGroupBox,
  CocoaConst, CocoaConfig, CocoaWSCommon, CocoaWSButtons, CocoaUtils,
  CocoaGDIObjects, CocoaTextEdits, CocoaComboBox, CocoaWSTextEdits,
  CocoaCustomControl, CocoaScrollers, CocoaWSScrollers, Cocoa_Extra;

type

  { TCocoaWSScrollBar }

  TCocoaWSScrollBar = class(TWSScrollBar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TCocoaWSCustomGroupBox }

  TCocoaWSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  end;

  { TComboBoxAsyncHelper }

  TComboBoxAsyncHelper = class
  private
    _cocoaCmb: NSObject;
    _newText: String;
  private
    procedure AsyncResetText(Data:PtrInt);
    procedure AsyncSetLastIndex(Data:PtrInt);
  public
    constructor Create(cocoaCmb:NSObject);
  public
    class procedure ResetTextIfNecessary(cocoaCmb:NSObject; ANewText:String);
    class procedure SetLastIndex(cocoaCmb:NSObject);
  end;

  { TLCLComboboxCallback }

  TLCLComboboxCallback = class(TLCLCommonCallback, IComboBoxCallback)
  public
    isShowPopup: Boolean;
    procedure ComboBoxWillPopUp;
    procedure ComboBoxWillDismiss;
    procedure ComboBoxSelectionDidChange;
    procedure ComboBoxSelectionIsChanging;

    procedure GetRowHeight(rowidx: integer; var h: Integer);
    procedure ComboBoxDrawItem(itemIndex: Integer; ctx: TCocoaContext;
      const r: TRect; isSelected: Boolean; backgroundPainted: Boolean);
  end;

  { TCocoaWSCustomComboBox }

  TCocoaWSCustomComboBox = class(TWSCustomComboBox)
  public
    class function getNSText(const ACustomComboBox: TCustomComboBox): NSText;
    class function GetObjectItemIndex(const AObject: TObject): integer;
    class function GetObjectAutoComplete(const AObject: TObject): boolean;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;

    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    {class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;}

    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    {class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;}
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;
    class procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure FreeItems(var AItems: TStrings); override;
    {class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}

    class function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; override;
    class procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); override;
    class procedure GetPreferredSize(
       const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
       WithThemeSpace: Boolean); override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetTextHint(const ACustomComboBox: TCustomComboBox; const ATextHint: string); override;
  end;

  { TCocoaWSToggleBox }

  TCocoaWSToggleBox = class(TWSToggleBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
  end;

  { TCocoaWSCustomStaticText }

  TCocoaWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

implementation

function ComboBoxStyleIsReadOnly(AStyle: TComboBoxStyle): Boolean;
begin
  Result := not AStyle.HasEditBox;
end;

function ComboBoxIsReadOnly(cmb: TCustomComboBox): Boolean;
begin
  Result := Assigned(cmb) and (ComboBoxStyleIsReadOnly(cmb.Style));
end;

function ComboBoxIsOwnerDrawn(AStyle: TComboBoxStyle): Boolean;
begin
  Result := AStyle.IsOwnerDrawn;
end;

function ComboBoxIsVariable(AStyle: TComboBoxStyle): Boolean;
begin
  Result := AStyle.IsVariable;
end;

procedure ComboBoxSetBorderStyle(box: NSComboBox; astyle: TBorderStyle);
begin
  {$IFDEF BOOLFIX}
  box.setBezeled_(Ord(astyle <> bsNone));
  {$else}
  box.setBezeled(astyle <> bsNone);
  {$endif}
end;

{ TComboBoxAsyncHelper }

constructor TComboBoxAsyncHelper.Create(cocoaCmb:NSObject);
begin
  _cocoaCmb:= cocoaCmb;
  _cocoaCmb.retain;
end;

procedure TComboBoxAsyncHelper.AsyncResetText(Data:PtrInt);
var
  cmb: TCustomComboBox;
begin
  try
    cmb:= TCustomComboBox(_cocoaCmb.lclGetTarget);
    if not Assigned(cmb) then
      exit;
    TCocoaWSCustomComboBox.SetText(cmb, _newText);
    cmb.SelStart:= UTF8Length(_newText);
    cmb.SelLength:= 0;
  finally
    _cocoaCmb.release;
    Free;
  end;
end;

procedure TComboBoxAsyncHelper.AsyncSetLastIndex(Data:PtrInt);
var
  cmb: TCustomComboBox;
begin
  try
    cmb:= TCustomComboBox(_cocoaCmb.lclGetTarget);
    if not Assigned(cmb) then
      exit;
    TCocoaWSCustomComboBox.SetItemIndex(cmb, TCocoaReadOnlyComboBox(_cocoaCmb).lastSelectedItemIndex);
  finally
    _cocoaCmb.release;
    Free;
  end;
end;

class procedure TComboBoxAsyncHelper.ResetTextIfNecessary(cocoaCmb:NSObject; ANewText:String);
var
  helper: TComboBoxAsyncHelper;
  cmb: TCustomComboBox;
begin
  cmb:= TCustomComboBox(cocoaCmb.lclGetTarget);
  if not Assigned(cmb) then
    exit;
  if not (cbactRetainPrefixCase in cmb.AutoCompleteText) then
    exit;
  helper:= TComboBoxAsyncHelper.Create(cocoaCmb);
  helper._newText:= ANewText;
  Application.QueueAsyncCall(@helper.AsyncResetText, 0);
end;

class procedure TComboBoxAsyncHelper.SetLastIndex(cocoaCmb:NSObject);
var
  helper: TComboBoxAsyncHelper;
begin
  helper:= TComboBoxAsyncHelper.Create(cocoaCmb);
  Application.QueueAsyncCall(@helper.AsyncSetLastIndex, 0);
end;

{ TLCLComboboxCallback }

type
  TCustomComboBoxAccess = class(TCustomComboBox);

procedure TLCLComboboxCallback.ComboBoxWillPopUp;
begin
  isShowPopup := true;
  LCLSendDropDownMsg(Target);
end;

procedure TLCLComboboxCallback.ComboBoxWillDismiss;
begin
  LCLSendCloseUpMsg(Target);
  isShowPopup := false;
end;

procedure TLCLComboboxCallback.ComboBoxSelectionDidChange;
begin
  SendSimpleMessage(Target, LM_SELCHANGE);
end;

procedure TLCLComboboxCallback.ComboBoxSelectionIsChanging;
begin

end;

procedure TLCLComboboxCallback.GetRowHeight(rowidx: integer; var h: Integer);
begin
  TCustomComboBoxAccess(Target).MeasureItem(rowidx, h);
end;

procedure TLCLComboboxCallback.ComboBoxDrawItem(itemIndex: Integer;
  ctx: TCocoaContext; const r: TRect; isSelected: Boolean; backgroundPainted: Boolean);
var
  itemstruct: TDrawListItemStruct;
begin
  itemstruct.ItemID := UINT(itemIndex);
  itemstruct.Area := r;
  itemstruct.DC := HDC(ctx);
  itemstruct.ItemState := [];
  if isSelected then Include(itemstruct.ItemState, odSelected);
  // we don't distingiush at the moment
  if isSelected then Include(itemstruct.ItemState, odFocused);
  if backgroundPainted then Include(itemstruct.ItemState, odBackgroundPainted);

  LCLSendDrawListItemMsg(Target, @itemstruct);
end;

{ TCocoaWSCustomStaticText }

class function TCocoaWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lclStaticText: TCustomStaticText absolute AWinControl;
  field: NSTextField;
begin
  field := NSTextField(AllocTextField(AWinControl, AParams));
  {$ifdef BOOLFIX}
  field.setBezeled_(Ord(False));
  field.setDrawsBackground_(Ord(False));
  field.setEditable_(Ord(False));
  field.setSelectable_(Ord(False));
  {$else}
  field.setBezeled(False);
  field.setDrawsBackground(False);
  field.setEditable(False);
  field.setSelectable(False);
  {$endif}
  TCocoaTextControlUtil.setAllignment(field, lclStaticText.Alignment);
  Result:=TLCLHandle(field);
end;

class procedure TCocoaWSCustomStaticText.SetAlignment(
  const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  if not Assigned(ACustomStaticText) or (not ACustomStaticText.HandleAllocated) or (ACustomStaticText.Handle=0) then
    exit;
  TCocoaTextControlUtil.setAllignment(NSTextField(ACustomStaticText.Handle), NewAlignment);
end;

{ TCocoaWSCustomComboBox }

class function TCocoaWSCustomComboBox.getNSText(const ACustomComboBox: TCustomComboBox): NSText;
var
  control: NSControl;
begin
  Result:= nil;
  if not Assigned(ACustomComboBox) or (not ACustomComboBox.HandleAllocated) or (ACustomComboBox.Handle=0) then
    exit;
  control:= NSControl( ACustomComboBox.Handle );
  Result:= control.currentEditor;
end;

class function TCocoaWSCustomComboBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLHandle;
var
  cmb: TCocoaComboBox;
  rocmb: TCocoaReadOnlyComboBox;
begin
  Result:=0;
  if ComboBoxIsReadOnly(TCustomComboBox(AWinControl)) then
  begin
    rocmb := NSView(TCocoaReadOnlyComboBox.alloc).lclInitWithCreateParams(AParams);
    if not Assigned(rocmb) then Exit;
    rocmb.isComboBoxEx:= AWinControl is TCustomComboBoxEx;
    rocmb.list:=TCocoaReadOnlyComboBoxList.Create(rocmb);
    rocmb.lastSelectedItemIndex:= -1;
    TComboBoxAsyncHelper.SetLastIndex(rocmb);
    rocmb.callback:=TLCLComboboxCallback.Create(rocmb, AWinControl);
    Result:=TLCLHandle(rocmb);
    rocmb.lclSetDefaultItemHeight( TCustomComboBoxAccess(AWinControl).ItemHeight );
    rocmb.isOwnerDrawn := ComboBoxIsOwnerDrawn(TCustomComboBox(AWinControl).Style);
    rocmb.isOwnerMeasure := ComboBoxIsVariable(TCustomComboBox(AWinControl).Style);
  end
  else
  begin
    cmb := NSView(TCocoaComboBox.alloc).lclInitWithCreateParams(AParams);
    if not Assigned(cmb) then Exit;
    //cmb.setCell(TCocoaComboBoxCell.alloc.initTextCell(NSString.string_));
    cmb.list:=TCocoaEditComboBoxList.Create(cmb);
    cmb.setUsesDataSource(true);
    cmb.setDataSource(cmb);
    cmb.setDelegate(cmb);
    cmb.setStringValue(StrToNSString(AParams.Caption));
    cmb.callback:=TLCLComboboxCallback.Create(cmb, AWinControl);
    if (cmb.respondsToSelector(ObjCSelector('cell'))) and Assigned(cmb.cell) then
      NSTextFieldCell(cmb.cell).setUsesSingleLineMode(true);
    // default BorderStyle for TComboBox is bsNone! and it looks ugly!
    // also, Win32 doesn't suppot borderstyle for TComboBox at all.
    // to be tested and considered
    //ComboBoxSetBorderStyle(cmb, TCustomComboBoxAccess(AWinControl).BorderStyle);
    Result:=TLCLHandle(cmb);
    UpdateControlFocusRing(cmb, AWinControl);
  end;
  //todo: 26 pixels is the height of 'normal' combobox. The value is taken from the Interface Builder!
  //      use the correct way to set the size constraints
  AWinControl.Constraints.SetInterfaceConstraints(
    0,
    CocoaConfigComboBox.minHeight,
    0,
    CocoaConfigComboBox.maxHeight );
end;

class procedure TCocoaWSCustomComboBox.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  ACustomComboBox : TCustomComboBox;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;
  ACustomComboBox:= TCustomComboBox(AWinControl);

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then
    //Result := TCocoaReadOnlyComboBox(ACustomComboBox.Handle).indexOfSelectedItem
  else
  begin
    //todo: consider the use of border style
    //ComboBoxSetBorderStyle(TCocoaComboBox(ACustomComboBox.Handle), ABorderStyle);
  end;

end;

class function TCocoaWSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
var
  comboBox: TCocoaComboBox;
begin
  Result:=false;
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;
  comboBox:= TCocoaComboBox(ACustomComboBox.Handle);
  Result:= comboBox.isDown;
end;

class procedure TCocoaWSCustomComboBox.SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
var
  comboBox: TCocoaComboBox;
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;
  if not ADroppedDown then exit;
  comboBox:= TCocoaComboBox(ACustomComboBox.Handle);
  if comboBox.cell.respondsToSelector(ObjCSelector('moveDown:')) then
    comboBox.cell.performSelector_withObject_afterDelay( ObjCSelector('moveDown:'), nil, 0 );
end;


class function TCocoaWSCustomComboBox.GetSelStart(
  const ACustomComboBox: TCustomComboBox): integer;
var
  txt: NSText;
begin
  Result:= 0;
  txt:= getNSText( ACustomComboBox );
  if Assigned(txt) then
    Result:= txt.selectedRange.location;
end;

class function TCocoaWSCustomComboBox.GetSelLength(
  const ACustomComboBox: TCustomComboBox): integer;
var
  txt: NSText;
begin
  Result:= 0;
  txt:= getNSText( ACustomComboBox );
  if Assigned(txt) then
    Result:= txt.selectedRange.length;
end;

class function TCocoaWSCustomComboBox.GetObjectItemIndex(const AObject: TObject): integer;
begin
  if AObject is TCustomComboBox then
    Result:= GetItemIndex( TCustomComboBox(AObject) )
  else
    Result:= -1;
end;

class function TCocoaWSCustomComboBox.GetObjectAutoComplete(const AObject: TObject): boolean;
begin
  if AObject is TCustomComboBox then
    Result:= TCustomComboBox(AObject).AutoComplete
  else
    Result:= false;
end;

class procedure TCocoaWSCustomComboBox.SetSelStart(
  const ACustomComboBox: TCustomComboBox; NewStart: integer);
var
  txt: NSText;
  range: NSRange;
begin
  txt:= getNSText( ACustomComboBox );
  if not Assigned(txt) then
    exit;
  range:= txt.selectedRange;
  range.location:= NewStart;
  txt.setSelectedRange( range );
end;

class procedure TCocoaWSCustomComboBox.SetSelLength(
 const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  txt: NSText;
  range: NSRange;
begin
  txt:= getNSText( ACustomComboBox );
  if not Assigned(txt) then
    exit;
  range:= txt.selectedRange;
  range.length:= NewLength;
  txt.setSelectedRange( range );
end;

class function TCocoaWSCustomComboBox.GetItemIndex(const ACustomComboBox:
  TCustomComboBox):integer;
var
  idx : NSInteger;
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
  begin
    Result:=-1;
    Exit;
  end;

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then
    idx := TCocoaReadOnlyComboBox(ACustomComboBox.Handle).indexOfSelectedItem
  else
    idx := ACustomComboBox.MatchListItem(ACustomComboBox.Text);
  if idx = NSNotFound then
    Result := -1
  else
    Result := Integer(idx);
end;

class procedure TCocoaWSCustomComboBox.SetItemIndex(const ACustomComboBox:
  TCustomComboBox;NewIndex:integer);
var
  rocmb: TCocoaReadOnlyComboBox;
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then
  begin
    rocmb := TCocoaReadOnlyComboBox(ACustomComboBox.Handle);
    rocmb.lastSelectedItemIndex := NewIndex;
    rocmb.selectItemAtIndex(NewIndex);
  end
  else
    TCocoaComboBox(ACustomComboBox.Handle).selectItemAtIndex(NewIndex);
end;

class procedure TCocoaWSCustomComboBox.SetStyle(
  const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;
  RecreateWnd(ACustomComboBox);
end;

class procedure TCocoaWSCustomComboBox.SetReadOnly(
  const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean);
var
  box : NSComboBox;
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;
  if not (NSObject(ACustomComboBox.Handle).isKindOfClass(NSComboBox)) then Exit;
  box := NSComboBox(ACustomComboBox.Handle);
  box.setEditable(not NewReadOnly);
  {$ifdef BOOLFIX}
  box.setSelectable_(1);
  {$ELSE}
  box.setSelectable(true);
  {$endif}
end;

class procedure TCocoaWSCustomComboBox.SetDropDownCount(const ACustomComboBox:
  TCustomComboBox;NewCount:Integer);
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then Exit;
  TCocoaComboBox(ACustomComboBox.Handle).setNumberOfVisibleItems(NewCount);
end;

class function TCocoaWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
  begin
    Result:=nil;
    Exit;
  end;

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then
    Result:=TCocoaReadOnlyComboBox(ACustomComboBox.Handle).list
  else
    Result:=TCocoaComboBox(ACustomComboBox.Handle).list;
end;

class procedure TCocoaWSCustomComboBox.FreeItems(var AItems: TStrings);
begin
  // in Cocoa, TCocoaComboBox.list should be released in TCocoaComboBox.dealloc(),
  // to avoid invalid TCocoaComboBox.list in TCocoaComboBox.comboBox_indexOfItemWithStringValue().
  // which will be called even in TCocoaWSWinControl.DestoryWnd(),
  // when TCocoaComboBox hasMarkedText (in IME state).
  // after all, it is more appropriate to release with other resources in dealloc().
end;

class function TCocoaWSCustomComboBox.GetItemHeight(const ACustomComboBox:
  TCustomComboBox):Integer;
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
  begin
    Result:=0;
    Exit;
  end;

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then
    Result:=TCocoaReadOnlyComboBox(ACustomComboBox.Handle).lclGetDefaultItemHeight
  else
    Result:=Round(TCocoaComboBox(ACustomComboBox.Handle).itemHeight);
end;

class procedure TCocoaWSCustomComboBox.SetItemHeight(const ACustomComboBox:
  TCustomComboBox;const AItemHeight:Integer);
begin
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;

  if ComboBoxStyleIsReadOnly(ACustomComboBox.Style) then
    TCocoaReadOnlyComboBox(ACustomComboBox.Handle).lclSetDefaultItemHeight(AItemHeight)
  else
    TCocoaComboBox(ACustomComboBox.Handle).setItemHeight(AItemHeight);
end;

class procedure TCocoaWSCustomComboBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if NOT ComboBoxStyleIsReadOnly(TCustomComboBox(AWinControl).Style) then
    Exit;

  if TCocoaReadOnlyComboBox(AWinControl.Handle).isComboBoxEx then
    Exit;

  TCocoaWSWinControl.GetPreferredSize(AWinControl, PreferredWidth, PreferredHeight, WithThemeSpace);
  if PreferredWidth < CocoaConfigComboBox.readOnly.minWidth then
    PreferredWidth:= CocoaConfigComboBox.readOnly.minWidth;
end;

class procedure TCocoaWSCustomComboBox.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  cmb: NSControl;
begin
  if (AWinControl.HandleAllocated) then
  begin
    cmb:= NSControl(AWinControl.Handle);
    cmb.setStringValue(NSString.string_);
    ControlSetTextWithChangeEvent(cmb, AText);
  end;
end;

class procedure TCocoaWSCustomComboBox.SetTextHint(
  const ACustomComboBox: TCustomComboBox; const ATextHint: string);
begin
  if NSAppKitVersionNumber <= NSAppKitVersionNumber10_10 then
    Exit;
  if (not Assigned(ACustomComboBox)) or (not ACustomComboBox.HandleAllocated) then
    Exit;
  TCocoaTextControlUtil.setTextHint(NSObject(ACustomComboBox.Handle), ATextHint);
end;

{ TCocoaWSToggleBox }

class function TCocoaWSToggleBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLHandle;
var
  btn: NSButton;
  cl: NSButtonCell;
begin
  btn := AllocButton(AWinControl, TLCLCheckBoxCallback, AParams,
           CocoaConfigToggleBox.bezelStyle, CocoaConfigToggleBox.buttonType);
  cl := NSButtonCell(NSButton(btn).cell);
  cl.setShowsStateBy(cl.showsStateBy or NSContentsCellMask);
  Result := TLCLHandle(btn);
end;

{ TCocoaWSScrollBar }

class function TCocoaWSScrollBar.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLHandle;
var
  scr : TCocoaScrollBar;
  prm : TCreateParams;
const
  ScrollBase = 15; // the shorter size of the scroller. There's a NSScroller class method for that
begin
  prm := AParams;
  // forcing the initial size to follow the designated kind of the scroll
  if (TCustomScrollBar(AWinControl).Kind = sbVertical) then begin
    prm.Width:=ScrollBase;
    prm.Height:=ScrollBase*4;
  end else
  begin
    prm.Width:=ScrollBase*4;
    prm.Height:=ScrollBase;
  end;

  // for the Scroller created separately through TCocoaWSScrollBar.CreateHandle(),
  // due to the lack of the control over the Layout by TCocoaManualScrollView,
  // only the Legacy Style can be used for compatibility.
  // it's the same logical relationship as NSScrollView and NSScroller.
  scr:= createLegacyScroller;
  scr.lclInitWithCreateParams(prm);
  scr.callback:=TLCLCommonCallback.Create(scr, AWinControl);

  // OnChange (scrolling) event handling
  scr.setTarget(scr);
  scr.setAction(objcselector('actionScrolling:'));

  scr.minInt:=TCustomScrollBar(AWinControl).Min;
  scr.maxInt:=TCustomScrollBar(AWinControl).Max;
  scr.pageInt:=TCustomScrollBar(AWinControl).PageSize;
  scr.largeInc:=abs(TCustomScrollBar(AWinControl).LargeChange);
  scr.smallInc:=abs(TCustomScrollBar(AWinControl).SmallChange);
  if scr.largeInc=0 then scr.largeInc:=1;
  if scr.smallInc=0 then scr.smallInc:=1;

  Result:=TLCLHandle(scr);

  scr.lclSetFrame( Bounds(AParams.X, AParams.Y, AParams.Width, AParams.Height));
end;

// vertical/horizontal in Cocoa is set automatically according to
// the geometry of the scrollbar, it cannot be forced to an unusual value
class procedure TCocoaWSScrollBar.SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean);
begin
  // the scroll type can be changed when creating a scroll.
  // since the size got changed, we have to create the handle
  RecreateWnd(AScrollBar);
end;

class procedure TCocoaWSScrollBar.SetParams(const AScrollBar:TCustomScrollBar);
var
  lScroller: TCocoaScrollBar;
  sz : integer;
begin
  if not Assigned(AScrollBar) then Exit;
  lScroller := TCocoaScrollBar(AScrollBar.Handle);
  if (lScroller = nil) then Exit;
  sz:=AScrollBar.Max - AScrollBar.PageSize;
  if sz > 0 then
  begin
    lScroller.setDoubleValue( AScrollBar.Position / sz );
    lScroller.setKnobProportion( AScrollBar.PageSize / AScrollBar.Max );
  end;
end;

{ TCocoaWSCustomGroupBox }

class function TCocoaWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  box: TCocoaGroupBox;
  cap: NSString;
  lGroupBoxContents: TCocoaCustomControl;
  ns: NSRect;
  //str: string;
begin
  box := NSView(TCocoaGroupBox.alloc).lclInitWithCreateParams(AParams);
  if Assigned(box) then
  begin
    box.callback := TLCLCommonCallback.Create(box, AWinControl);
    TLCLCommonCallback(box.callback.GetCallbackObject).BlockCocoaUpDown := true;
    cap := NSStringUTF8(AParams.Caption);
    box.setTitle(cap);
    cap.release;

    // set a content view in order to be able to customize drawing for labels/color
    ns := NSMakeRect(AParams.X, AParams.Y, AParams.Width, AParams.Height);
    lGroupBoxContents := TCocoaCustomControl.alloc.initWithFrame(ns);
    lGroupBoxContents.callback := box.callback; //TLCLCustomControlCallback.Create(lGroupBoxContents, AWinControl);
    //str := Format('%X=%X', [PtrUInt(box.callback), PtrUInt(lGroupBoxContents.callback)]);
    lGroupBoxContents.autorelease;
    box.setContentView(lGroupBoxContents);
  end;
  Result := TLCLHandle(box);
end;

class function TCocoaWSCustomGroupBox.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  box: TCocoaGroupBox;
begin
  box := TCocoaGroupBox(AWinControl.Handle);
  Result:=Assigned(box);
  if Result then
    AText := NSStringToString(box.title);
end;

class procedure TCocoaWSCustomGroupBox.SetText(const AWinControl: TWinControl; const AText: String);
var
  box: TCocoaGroupBox;
begin
  box := TCocoaGroupBox(AWinControl.Handle);
  box.setTitle(TCocoaControlUtil.toMacOSTitle(AText));
end;

class procedure TCocoaWSCustomGroupBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  box: TCocoaGroupBox;
  fn : NSFont;
begin
  if not AWinControl.HandleAllocated then Exit;
  box := TCocoaGroupBox(AWinControl.Handle);
  fn := TCocoaFont(AFont.Reference.Handle).Font;
  if AFont.Size = 0 then
    fn := NSFont.fontWithDescriptor_size(fn.fontDescriptor, NSFont.smallSystemFontSize);
  box.setTitleFont(fn);
end;

end.

