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
  CocoaGDIObjects, CocoaTextEdits,
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

  { TCocoaWSCustomEdit }

  TCocoaWSCustomEdit = class(TWSCustomEdit)
  public
    class function GetTextField(AWinControl: TWinControl): NSTextField;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;

    // WSControl functions
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont ); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;

    // WSEdit functions
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const NewAlignment: TAlignment); override;

    {class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;}
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string); override;
  end;
  
  { TCocoaMemoStrings }

  TCocoaMemoStrings = class(TCustomMemoStrings)
  private
    FTextView: TCocoaTextView;
  public
    class procedure GetLineStart(const s: AnsiString; LineIndex: Integer; var Offset, LinesSkipped: Integer);
  protected
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
  public
    constructor Create(ATextView: TCocoaTextView);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;

  { TCocoaWSCustomMemo }

  TCocoaWSCustomMemo = class(TWSCustomMemo)
  public
    class function GetTextView(AWinControl: TWinControl): TCocoaTextView;
    class function GetScrollView(AWinControl: TWinControl): TCocoaScrollView;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;

    // WSControl functions
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;

    // WSEdit functions
    //class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const NewAlignment: TAlignment); override;

    // WSMemo functions
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;

    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
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

// Sets the control text and then calls controls callback (if any)
// with TextChange (CM_TEXTCHANGED) event.
// Cocoa control do not fire a notification, if text is changed programmatically
// LCL expects a change notification in either way. (by software or by user)
procedure ControlSetTextWithChangeEvent(ctrl: NSControl; const text: string);
var
  cb: ICommonCallBack;
begin
  TCocoaControlUtil.setStringValue(ctrl, text);
  cb := ctrl.lclGetcallback;
  if Assigned(cb) then // cb.SendOnChange;
    cb.SendOnTextChanged;
end;

function AllocTextField(ATarget: TWinControl; const AParams: TCreateParams): TCocoaTextField;
begin
  Result := TCocoaTextField.alloc.lclInitWithCreateParams(AParams);
  if Assigned(Result) then
  begin
    if NOT Result.fixedInitSetting then
      Result.setFont(NSFont.systemFontOfSize(NSFont.systemFontSize));
    Result.callback := TLCLCommonCallback.Create(Result, ATarget);
    TCocoaControlUtil.setStringValue(Result, AParams.Caption);
  end;
end;

function AllocSecureTextField(ATarget: TWinControl; const AParams: TCreateParams): TCocoaSecureTextField;
begin
  Result := TCocoaSecureTextField.alloc.lclInitWithCreateParams(AParams);
  if Assigned(Result) then
  begin
    Result.setFont(NSFont.systemFontOfSize(NSFont.systemFontSize));
    Result.callback := TLCLCommonCallback.Create(Result, ATarget);
    TCocoaTextControlUtil.setStringValue(Result.currentEditor, AParams.Caption);
  end;
end;

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

{ TCocoaWSCustomEdit }

class function TCocoaWSCustomEdit.GetTextField(AWinControl: TWinControl): NSTextField;
begin
  if not Assigned(AWinControl) or (not AWinControl.HandleAllocated) or (AWinControl.Handle=0) then
  begin
    Exit(nil);
  end;

  if AWinControl is TCustomMemo then
  begin
    //raise Exception.Create('[TCocoaWSCustomEdit.GetTextField] Called for TMemo, but TMemo has no text field');
    Exit(nil);
  end;

  Result := NSTextField(AWinControl.Handle);
end;


type
  TCocoaVertCenterTextFieldCell = objcclass( NSTextFieldCell )
    function drawingRectForBounds(theRect: NSRect): NSRect; override;
  end;

function TCocoaVertCenterTextFieldCell.drawingRectForBounds(theRect: NSRect): NSRect;
var
  centerRect: NSRect;
begin
  centerRect.origin.x:= theRect.origin.x;
  centerRect.origin.y:= (theRect.size.height-cellSize.height)/2;
  centerRect.size.width:= theRect.size.width;
  centerRect.size.height:= cellSize.width;

  Result:= inherited drawingRectForBounds(centerRect);
end;

procedure SetTextFieldCell( const edit: TCustomEdit ; const field: NSTextField );
var
  cell: NSTextFieldCell;
begin
  if (field.respondsToSelector(ObjCSelector('cell'))) and Assigned(field.cell) then
  begin
    if CocoaConfigEdit.vertAlignCenter then begin
      cell:= TCocoaVertCenterTextFieldCell.new;
      field.setCell( cell );
      cell.release;
    end else begin
      cell:= NSTextFieldCell(field.cell);
    end;
    cell.setWraps(false);
    cell.setScrollable(true);
  end;

  if field.isKindOfClass(TCocoaTextField) and TCocoaTextField(field).fixedInitSetting then
    Exit;

  TCocoaTextControlUtil.setBorderStyle(field, edit.BorderStyle);
  TCocoaTextControlUtil.setAllignment(field, edit.Alignment);
  UpdateControlFocusRing( field, edit );
end;

class function TCocoaWSCustomEdit.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  field: NSTextField;
begin
  if TCustomEdit(AWinControl).PasswordChar=#0 then begin
    field:= AllocTextField(AWinControl, AParams);
  end else begin
    field:= AllocSecureTextField(AWinControl, AParams);
  end;
  SetTextFieldCell( TCustomEdit(AWinControl), field );
  Result:= TLCLHandle(field);
end;

class procedure TCocoaWSCustomEdit.SetColor(const AWinControl: TWinControl);
var
  field : NSTextField;

  // maybe the bug of macOS, especially on macOS 26
  // changing the background color while editing may not work.
  // see also: https://github.com/doublecmd/doublecmd/discussions/2683
  procedure ensureBackcolorApply;
  var
    w     : NSWindow;
    rsp   : NSResponder;
    ed    : TCocoaFieldEditor;
    selected: NSRange;
  begin
    w := NSView(AWinControl.Handle).window;
    if not Assigned(w) then
      Exit;

    rsp := w.firstResponder;
    if NOT Assigned(rsp) then
      Exit;
    if NOT (rsp.isKindOfClass(TCocoaFieldEditor)) then
      Exit;

    ed := TCocoaFieldEditor(rsp);
    if NSObject(ed.delegate) <> NSView(AWinControl.Handle) then
      Exit;

    selected:= ed.selectedRange;
    w.makeFirstResponder( field );
    ed.setSelectedRange( selected );
  end;

begin
  field := GetTextField(AWinControl);
  if not Assigned(field) then Exit;

  if (AWinControl.Color = clDefault) or (AWinControl.Color = clWindow) or (AWinControl.Color = clBackground)  then
    field.setBackgroundColor( NSColor.textBackgroundColor )
  else
    field.setBackgroundColor( TCocoaColorUtil.toColor(ColorToRGB(AWinControl.Color)));

  ensureBackcolorApply;
end;

class procedure TCocoaWSCustomEdit.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  field: NSTextField;
  editor: TCocoaFieldEditor;
begin
  TCocoaWSWinControl.SetFont(AWinControl, AFont);
  field:= self.GetTextField(AWinControl);
  if NOT Assigned(field) then
    Exit;
  editor:= TCocoaFieldEditor(field.currentEditor);
  editor.lclReviseCursorColor;
end;

class procedure TCocoaWSCustomEdit.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  field: NSTextField;
begin
  field:= GetTextField(AWinControl);
  if not Assigned(field) then Exit;
  {$ifdef BOOLFIX}
  field.setBordered_( ObjCBool(ABorderStyle <> bsNone) );
  field.setBezeled_( ObjCBool(ABorderStyle <> bsNone) );
  {$else}
  field.setBordered( ABorderStyle <> bsNone );
  field.setBezeled( ABorderStyle <> bsNone );
  {$endif}
  UpdateControlFocusRing( field, AWinControl );
end;

class function TCocoaWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  field : NSTextField;
  txt   :  NSText;
begin
  Result:=0;
  field := GetTextField(ACustomEdit);
  if not Assigned(field) then Exit;
  txt:=NSText(field.currentEditor);
  if not Assigned(txt) then Exit;

  Result:=txt.selectedRange.location;
end;

class function TCocoaWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  field : NSTextField;
  txt   : NSText;
begin
  Result:=0;
  field := GetTextField(ACustomEdit);
  if not Assigned(field) then Exit;
  txt:=NSText(field.currentEditor);
  if not Assigned(txt) then Exit;

  Result:=txt.selectedRange.length;
end;

class procedure TCocoaWSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const NewAlignment: TAlignment);
var
  field: NSTextField;
begin
  field := GetTextField(ACustomEdit);
  if not Assigned(field) then
    Exit;
  if field.isKindOfClass(TCocoaTextField) and TCocoaTextField(field).fixedInitSetting then
    Exit;
  TCocoaTextControlUtil.setAllignment(field, NewAlignment);
end;

class procedure TCocoaWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  field: NSTextField;
begin
  if not (ACustomEdit.HandleAllocated) then Exit;
  field := NSTextField(ACustomEdit.Handle);
  if not Assigned(field) then Exit;

  if NSObject(field).respondsToSelector( ObjCSelector('lclSetMaxLength:') ) then
    {%H-}NSTextField_LCLExt(field).lclSetMaxLength(NewLength);
end;

class procedure TCocoaWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  if (NewChar<>#0) xor TCocoaTextField(ACustomEdit.Handle).isKindOfClass_(NSSecureTextField) then
    RecreateWnd(ACustomEdit);
end;


class procedure TCocoaWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
var
  lHandle: NSTextField;
  w : NSWindow;
  t : NSText;
  isFocused: Boolean;
  r : Boolean;
  b : Boolean;
  rsp : NSResponder;
  ed  : TCocoaFieldEditor;
begin
  lHandle := GetTextField(ACustomEdit);
  if not Assigned(lHandle) then Exit;

  ed := nil; //if lHandle is "focused" then ed would be <> nil
  w := lHandle.window;
  if not Assigned(w) then t := nil
  else begin
    rsp := w.firstResponder;
    if (Assigned(rsp)) and (rsp.isKindOfClass(TCocoaFieldEditor)) then
    begin
      ed := TCocoaFieldEditor(rsp);
      if (NSObject(ed.delegate) = lHandle) then
      begin
        ed.retain;
        // the hack is needed to prevent infinite loop
        // on switching editable (ReadOnly) status.
        // without prevention of Editor focusing, AppKit goes into an infinite loop:
        // AppKit`-[_NSKeyboardFocusClipView removeFromSuperview] + 55
        // AppKit`-[NSWindow endEditingFor:] + 429
        // AppKit`-[NSView removeFromSuperview] + 78
        // AppKit`-[_NSKeyboardFocusClipView removeFromSuperview] + 55
        // AppKit`-[NSWindow endEditingFor:] + 429
        // AppKit`-[NSView removeFromSuperview] + 78
        // AppKit`-[_NSKeyboardFocusClipView removeFromSuperview] + 55
        ed.goingReadOnly := true;
      end
      else
        ed := nil; // someone else is focused
    end;
  end;

  {$ifdef BOOLFIX}
  lHandle.setEditable_(ObjCBool(not NewReadOnly));
  lHandle.setSelectable_(1); // allow to select read-only text (LCL compatible)
  {$ELSE}
  lHandle.setEditable( not NewReadOnly);
  lHandle.setSelectable(true); // allow to select read-only text (LCL compatible)
  {$ENDIF}

  if Assigned(ed) then begin
    ed.goingReadOnly := false;
    ed.release;
  end;
end;

class procedure TCocoaWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
var
  lHandle: NSTextField;
  curEditor:  NSText;
  lRange: NSRange;
begin
  lHandle := GetTextField(ACustomEdit);
  if not Assigned(lHandle) then Exit;
  curEditor := NSText(lHandle.currentEditor);
  if not Assigned(curEditor) then Exit;
  lRange := curEditor.selectedRange;
  lRange.location := NewStart;
  curEditor.setSelectedRange(lRange);
end;

class procedure TCocoaWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  lHandle: NSTextField;
  curEditor:  NSText;
  lRange: NSRange;
begin
  lHandle := GetTextField(ACustomEdit);
  if not Assigned(lHandle) then Exit;
  curEditor := NSText(lHandle.currentEditor);
  if not Assigned(curEditor) then Exit;
  lRange := curEditor.selectedRange;
  lRange.length := NewLength;
  curEditor.setSelectedRange(lRange);
end;

class procedure TCocoaWSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
begin
  if not Assigned(ACustomEdit) or not (ACustomEdit.HandleAllocated) then Exit;
  NSApplication(NSApp).sendAction_to_from(objcselector('cut:'), nil, id(ACustomEdit.Handle));
end;

class procedure TCocoaWSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
begin
  if not Assigned(ACustomEdit) or not (ACustomEdit.HandleAllocated) then Exit;
  NSApplication(NSApp).sendAction_to_from(objcselector('copy:'), nil, id(ACustomEdit.Handle));
end;

class procedure TCocoaWSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
begin
  if not Assigned(ACustomEdit) or not (ACustomEdit.HandleAllocated) then Exit;
  NSApplication(NSApp).sendAction_to_from(objcselector('paste:'), nil, id(ACustomEdit.Handle));
end;

class procedure TCocoaWSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  if not Assigned(ACustomEdit) or not (ACustomEdit.HandleAllocated) then Exit;
  NSApplication(NSApp).sendAction_to_from(objcselector('undo:'), nil, id(ACustomEdit.Handle));
end;

class procedure TCocoaWSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  txt : String;
  mxl : Integer;
begin
  if not AWinControl.HandleAllocated then exit;

  txt := AText;
  mxl := TCustomEdit(AWinControl).MaxLength;
  if (mxl > 0) and (UTF8Length(txt) > mxl) then
    txt := UTF8Copy(txt, 1, mxl);
  ControlSetTextWithChangeEvent(NSControl(AWinControl.Handle), txt);
end;

class procedure TCocoaWSCustomEdit.SetTextHint(const ACustomEdit: TCustomEdit;
  const ATextHint: string);
begin
  if NSAppKitVersionNumber <= NSAppKitVersionNumber10_10 then Exit;
  if (ACustomEdit.HandleAllocated) then
    TCocoaTextControlUtil.setTextHint(NSObject(ACustomEdit.Handle), ATextHint);
end;

{ TCocoaMemoStrings }

function LineBreaksToUnix(const src: string): string;
begin
  // todo: need more effecient replacement
  Result := StringReplace( StringReplace(
    StringReplace(src, #10#13, #10, [rfReplaceAll])
    , #13#10, #10, [rfReplaceAll])
    , #13, #10, [rfReplaceAll]);
end;

constructor TCocoaMemoStrings.Create(ATextView: TCocoaTextView);
begin
  inherited Create;
  FTextView := ATextView;
end;

function TCocoaMemoStrings.GetTextStr: string;
begin
  Result := NSStringToString(FTextView.string_);
end;

procedure TCocoaMemoStrings.SetTextStr(const Value: string);
begin
  TCocoaTextControlUtil.setStringValue(FTextView, LineBreaksToUnix(Value));

  FTextView.textDidChange(nil);
end;

class procedure TCocoaMemoStrings.GetLineStart(const s: AnsiString; LineIndex: Integer; var Offset, LinesSkipped: Integer);
var
  i : Integer;
begin
  i:=1;
  LinesSkipped:=0;
  while (LinesSkipped<>LineIndex) and (i<=length(s)) do begin
    if s[i] in [#10, #13] then begin
      inc(i);
      inc(LinesSkipped);
      if (i<=length(s)) and (s[i] in [#10,#13]) and (s[i-1]<>s[i]) then
        inc(i);
    end else
      inc(i);
  end;
  Offset:=i;
end;

function TCocoaMemoStrings.GetCount:Integer;
var
  s      : NSString;
  i      : LongWord;
  strLen : LongWord;
begin
  s := FTextView.string_;
  // it's a very nice example for Apple's docs
  // https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/TextLayout/Tasks/CountLines.html
  strLen := s.length;
  i := 0;
  Result := 0;
  while (i < strLen) do begin
    i := NSMaxRange(s.lineRangeForRange(NSMakeRange(i, 0)));
    inc(Result);
  end;
end;

function TCocoaMemoStrings.Get(Index:Integer):string;
var
  s     : AnsiString;
  ofs   : Integer;
  eofs  : Integer;
  t     : Integer;
begin
  s:=GetTextStr;
  t:=0;
  ofs:=0;
  GetLineStart(s, Index, ofs, t);
  eofs:=ofs;
  while (eofs<=length(s)) and not (s[eofs] in [#10,#13]) do
    inc(eofs);
  Result:=Copy(s, ofs, eofs-ofs);
end;

procedure TCocoaMemoStrings.Clear;
begin
  SetTextStr('');
end;

procedure TCocoaMemoStrings.Assign(Source: TPersistent);
begin
  if (Source=Self) or (Source=nil) then
     exit;
  if Source is TStrings then
    SetTextStr(TStrings(Source).Text)
  else
    inherited Assign(Source);
end;

procedure TCocoaMemoStrings.Delete(Index:Integer);
var
  s     : AnsiString;
  ofs   : Integer;
  eofs  : Integer;
  t     : Integer;
begin
  s:=GetTextStr;
  GetLineStart(s, Index, ofs, t);
  eofs:=ofs;
  while (eofs<=length(s)) and not (s[eofs] in [#10,#13]) do
    inc(eofs);
  if eofs<=length(s) then begin
    inc(eofs);
    if (eofs<=length(s)) and (s[eofs] in [#10,#13]) and (s[eofs-1]<>s[eofs]) then
      inc(eofs);
  end;
  System.Delete(s, ofs, eofs-ofs);
  SetTextStr(s);
end;

procedure TCocoaMemoStrings.Insert(Index:Integer;const S:string);
var
  rng   : NSRange;
  st,ed : NSUInteger;
  ced   : NSUInteger;
  ns    : NSString;
  idx   : integer;
  ro    : Boolean;
begin
  ns:=FTextView.string_;
  idx:=0;
  rng:=NSMakeRange(0,0);
  while (idx<Index) and (rng.location<ns.length) do begin
    ns.getLineStart_end_contentsEnd_forRange(nil, @st, @ced, rng);
    inc(idx);
    rng.location:=st;
    rng.length:=0;
  end;

  // using selectedRange in order to be consistent with Windows widgetset
  if rng.location>ns.length then rng.location:=ns.length;
  inc(FTextView.supressTextChangeEvent);

  ro := FTextView.isEditable; // checking for read-only flag;
  if not ro then FTextView.setEditable(true);

  FTextView.setSelectedRange(rng);

  if (rng.location>=ns.length) and (st=ced) and (ns.length>0) then
    FTextView.insertText( NSSTR_LINE_FEED );

  if S<>'' then
  begin
    FTextView.insertText( NSString.stringWithUTF8String( @S[1] ));
  end;

  dec(FTextView.supressTextChangeEvent);
  FTextView.insertText( NSSTR_LINE_FEED );

  if not ro then FTextView.setEditable(ro);

  FTextView.undoManager.removeAllActions;
end;

procedure TCocoaMemoStrings.LoadFromFile(const FileName: string);
var
  TheStream: TFileStream;
begin
  TheStream:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TCocoaMemoStrings.SaveToFile(const FileName: string);
var
  TheStream: TFileStream;
begin
  TheStream:=TFileStream.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

{ TCocoaWSCustomMemo }

class function TCocoaWSCustomMemo.GetTextView(AWinControl: TWinControl): TCocoaTextView;
var
  lScroll: TCocoaScrollView;
begin
  lScroll := GetScrollView(AWinControl);
  if not Assigned(lScroll) then
  begin
    Exit(nil);
  end;

  Result := TCocoaTextView(lScroll.documentView);
end;

class function TCocoaWSCustomMemo.GetScrollView(AWinControl: TWinControl): TCocoaScrollView;
begin
  if not Assigned(AWinControl) or (not AWinControl.HandleAllocated) or (AWinControl.Handle=0) then
  begin
    Exit(nil);
  end;

  Result := TCocoaScrollView(AWinControl.Handle);
end;

class function TCocoaWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams):TLCLHandle;
var
  txt: TCocoaTextView;
  ns: NSString;
  scr: TCocoaScrollView;
  nr:NSRect;
  r:TRect;
  layoutSize: NSSize;
  lcl: TLCLCommonCallback;
begin
  scr := TCocoaScrollView(NSView(TCocoaScrollView.alloc).lclInitWithCreateParams(AParams));

  nr.origin.x:=0;
  nr.origin.y:=0;
  nr.size.height:=0;
  nr.size.width:=AParams.Width;

  txt := TCocoaTextView.alloc.initwithframe(nr);
  txt.setAllowsUndo(true);
  // setting up a default system font (to be consistent with other widgetsets)
  txt.setFont( NSFont.systemFontOfSize( NSFont.systemFontSizeForControlSize(NSRegularControlSize) ));
  txt.setRichText(false);
  txt.setImportsGraphics(false);
  txt.setUsesRuler(false);

  // this is necessary for Ward Wrap disabled, so NSViewText
  // doesn't have a constraint to resize
  // Apple default maxsize is InitialWidth, 10000000
  // (MaxSize is also changed automatically, if NSViewText size is changed)
  txt.setMaxSize(NSMakeSize(10000000, 10000000));
  scr.setDocumentView(txt);

  scr.setHasVerticalScroller(VerticalScrollerVisible[TMemo(AWinControl).ScrollBars]);
  scr.setHasHorizontalScroller(HorizontalScrollerVisible[TMemo(AWinControl).ScrollBars]);
  scr.setAutohidesScrollers(ScrollerAutoHide[TMemo(AWinControl).ScrollBars]);
  scr.setDrawsBackground(false);

  ScrollViewSetBorderStyle(scr, TCustomMemo(AWinControl).BorderStyle);
  scr.setFocusRingType( NSFocusRingTypeExterior );

  nr:=scr.documentVisibleRect;
  txt.setFrame(nr);
  txt.lclSetEnabled(True);

  // ToDo: This should be made selectable in the LCL
  txt.setAutomaticQuoteSubstitutionEnabled(False);
  txt.setAutomaticLinkDetectionEnabled(False);
  // macOS 10.6 version
  if txt.respondsToSelector(objcselector('setAutomaticDataDetectionEnabled:')) then
    txt.setAutomaticDataDetectionEnabled(false);
  if txt.respondsToSelector(objcselector('setAutomaticTextReplacementEnabled:')) then
    txt.setAutomaticTextReplacementEnabled(False);
  if txt.respondsToSelector(ObjCSelector('setAutomaticDashSubstitutionEnabled:')) then
    txt.setAutomaticDashSubstitutionEnabled(False);
  if txt.respondsToSelector(ObjCSelector('setAutomaticSpellingCorrectionEnabled:')) then
    txt.setAutomaticSpellingCorrectionEnabled(False);

  // defaulting to System colors
  // This makes NSTextView to be responsive to theme color change (Mojave 10.14)
  txt.setTextColor(NSColor.textColor);
  txt.setBackgroundColor(NSColor.textBackgroundColor);
  UpdateControlFocusRing(txt, AWinControl);

  lcl := TLCLCommonCallback.Create(txt, AWinControl);
  lcl.ForceReturnKeyDown := true;
  txt.callback := lcl;
  txt.setDelegate(txt);

  TCocoaTextControlUtil.setStringValue(txt, AParams.Caption);

  scr.callback := txt.callback;

  TCocoaTextControlUtil.setWordWrap(txt, scr, TCustomMemo(AWinControl).WordWrap);
  TCocoaTextControlUtil.setAllignment(txt, TCustomMemo(AWinControl).Alignment);
  txt.wantReturns := TCustomMemo(AWinControl).WantReturns;
  txt.callback.SetTabSuppress(not TCustomMemo(AWinControl).WantTabs);
  txt.release;
  Result := TLCLHandle(scr);
end;

class procedure TCocoaWSCustomMemo.SetColor(const AWinControl: TWinControl);
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(AWinControl);
  if not Assigned(txt) then Exit;

  if (AWinControl.Color = clDefault) or (AWinControl.Color = clWindow) or (AWinControl.Color = clBackground) then
    txt.setBackgroundColor( NSColor.textBackgroundColor )
  else
    txt.setBackgroundColor( TCocoaColorUtil.toColor(ColorToRGB(AWinControl.Color)));
end;

class procedure TCocoaWSCustomMemo.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
var
  lRange: NSRange;
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomEdit);
  if not Assigned(txt) then Exit;

  lRange := txt.selectedRange;
  lRange.location := NewStart;
  txt.setSelectedRange(lRange);
  txt.scrollRangeToVisible(lRange);
end;

class procedure TCocoaWSCustomMemo.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  lRange: NSRange;
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomEdit);
  if not Assigned(txt) then Exit;

  lRange := txt.selectedRange;
  lRange.length := NewLength;
  txt.setSelectedRange(lRange);
end;

class procedure TCocoaWSCustomMemo.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  sv: TCocoaScrollView;
begin
  sv := GetScrollView(AWinControl);
  if not Assigned(sv) then Exit;

  ScrollViewSetBorderStyle(sv, ABorderStyle);
  UpdateControlFocusRing(sv.documentView, AWinControl);
end;

class function TCocoaWSCustomMemo.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
var
  txt: TCocoaTextView;
  lValue: NSValue;
  viewString: NSString;
  paraStart: NSUInteger = 0;
  paraEnd: NSUInteger = 0;
  contentsEnd: NSUInteger = 0;
  curLine: Integer = 0;
begin
  Result := Point(0, 0);
  txt := GetTextView(ACustomEdit);
  if not Assigned(txt) then Exit;
  lValue := NSValue(txt.selectedRanges.objectAtIndex(0));
  if lValue = nil then Exit;

  viewString := txt.string_;
  Result.X := lValue.rangeValue.location;

  // There is no simple function to do this in Cocoa :(
  while (paraEnd < viewString.length) do
  begin
    viewString.getLineStart_end_contentsEnd_forRange(@paraStart,
      @paraEnd, @contentsEnd, NSMakeRange(paraEnd, 0));

    if (lValue.rangeValue.location >= paraStart) and
       (lValue.rangeValue.location < paraEnd) then
    begin
      Break;
    end
    else
      Result.X := Result.X - (paraEnd - paraStart);

    Inc(curLine);
  end;
  Result.Y := curLine;

  {This doesn't work :/
  lineRange := viewString.lineRangeForRange(lValue.rangeValue);
  Result.X := lineRange.location;}
end;

class function TCocoaWSCustomMemo.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomEdit);
  if not Assigned(txt) then
  begin
    Result:=0;
    Exit;
  end;
  Result := txt.selectedRange.location;
end;

class function TCocoaWSCustomMemo.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  txt: TCocoaTextView;
  ns: NSArray;
begin
  txt := GetTextView(ACustomEdit);
  if not Assigned(txt) then
  begin
    Result:=0;
    Exit;
  end;
  Result := txt.selectedRange.length;
end;

class procedure TCocoaWSCustomMemo.SetAlignment(const ACustomEdit: TCustomEdit;
  const NewAlignment: TAlignment);
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomEdit);
  TCocoaTextControlUtil.setAllignment(txt, NewAlignment);
end;

class function TCocoaWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomMemo);
  if Assigned(txt) then
    Result := TCocoaMemoStrings.Create(txt)
  else
    Result := nil
end;

class procedure TCocoaWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo;
  const AText: string);
begin
  //todo:
end;

class procedure TCocoaWSCustomMemo.SetReadOnly(const ACustomEdit:TCustomEdit;
  NewReadOnly:boolean);
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomEdit);
  if Assigned(txt) then
    txt.setEditable(not NewReadOnly);
end;

class function TCocoaWSCustomMemo.GetTextLen(const AWinControl: TWinControl;
  var ALength: Integer): Boolean;
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(AWinControl);
  Result := Assigned(txt);
  if Result then
    ALength := txt.string_.lengthOfBytesUsingEncoding(NSUTF8StringEncoding);
end;

class procedure TCocoaWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
  ScrollViewSetScrollStyles(TCocoaScrollView(ACustomMemo.Handle), NewScrollbars);
end;

class procedure TCocoaWSCustomMemo.SetWantTabs(const ACustomMemo: TCustomMemo;
  const NewWantTabs: boolean);
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomMemo);
  if (not Assigned(txt)) then Exit;
  txt.callback.SetTabSuppress(not NewWantTabs);
end;


class procedure TCocoaWSCustomMemo.SetWantReturns(const ACustomMemo: TCustomMemo;
  const NewWantReturns: boolean);
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(ACustomMemo);
  if (not Assigned(txt)) then Exit;
  txt.wantReturns := NewWantReturns;
end;

class procedure  TCocoaWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
var
  txt: TCocoaTextView;
  lScroll: TCocoaScrollView;
begin
  txt := GetTextView(ACustomMemo);
  lScroll := GetScrollView(ACustomMemo);
  if (not Assigned(txt)) or (not Assigned(lScroll)) then Exit;

  TCocoaTextControlUtil.setWordWrap(txt, lScroll, NewWordWrap);
end;

class procedure TCocoaWSCustomMemo.SetText(const AWinControl:TWinControl;const AText:String);
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(AWinControl);
  if not Assigned(txt) then Exit;
  TCocoaTextControlUtil.setStringValue(txt, LineBreaksToUnix(AText));
end;

class function TCocoaWSCustomMemo.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  txt: TCocoaTextView;
begin
  txt := GetTextView(AWinControl);
  Result := Assigned(txt);
  if Result then
    AText := NSStringToString(txt.string_);
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

