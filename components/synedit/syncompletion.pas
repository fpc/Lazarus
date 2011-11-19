{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.
Portions created by Cyrille de Brebisson are Copyright (C) 1999
Cyrille de Brebisson. All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynCompletion;

{$I SynEdit.inc}
interface

uses
  LCLProc, LCLIntf, LCLType, LMessages, Classes, Graphics, Forms,
  Controls, StdCtrls, ExtCtrls, Menus, SysUtils,
  SynEditMiscProcs, SynEditKeyCmds, SynEdit, SynEditTypes;

type
  TSynBaseCompletionPaintItem =
    function(const AKey: string; ACanvas: TCanvas;
             X, Y: integer; Selected: boolean; Index: integer
            ): boolean of object;
  TSynBaseCompletionMeasureItem =
    function(const AKey: string; ACanvas: TCanvas;
      Selected: boolean; Index: integer): TPoint of object;
  TCodeCompletionEvent = procedure(var Value: string;
                                   SourceValue: string;
                                   var SourceStart, SourceEnd: TPoint;
                                   KeyChar: TUTF8Char;
                                   Shift: TShiftState) of object;
  TValidateEvent = procedure(Sender: TObject;
                             KeyChar: TUTF8Char;
                             Shift: TShiftState) of object;
  TSynBaseCompletionSearchPosition = procedure(var Position :integer) of object;
  
  TSynBaseCompletionForm = class;
  
  { TSynBaseCompletionHint }

  TSynBaseCompletionHint = class(THintWindow)
  private
    FCompletionForm: TSynBaseCompletionForm;
    FDisplayRect: TRect;
    FIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    procedure Paint; override;
    property Index: Integer read FIndex write FIndex;
    property DisplayRect: TRect read FDisplayRect write FDisplayRect;
  end;


  TSynCompletionLongHintType = (sclpNone,
                                sclpExtendRightOnly,
                                sclpExtendHalfLeft,
                                sclpExtendUnlimitedLeft
                               );

  { TSynBaseCompletionFormSizeDrag }

  TSynBaseCompletionFormSizeDrag = class(TPanel)
  private
    FMouseDownPos, FMouseLastPos, FWinSize: TPoint;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  end;

  { TSynBaseCompletionForm }

  TSynBaseCompletionForm = class(TForm)
    procedure SDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SDKeyPress(Sender: TObject; var Key: char);
    procedure SDUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  protected
    FCurrentString: string;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDelete: TNotifyEvent;
    FOnPaintItem: TSynBaseCompletionPaintItem;
    FItemList: TStrings;
    FPosition: Integer;
    FNbLinesInWindow: Integer;
    FFontHeight: integer;
    FResizeLock: Integer;
    Scroll: TScrollBar;
    SizeDrag: TSynBaseCompletionFormSizeDrag;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FClSelect: TColor;
    FCaseSensitive: boolean;
    FBackgroundColor: TColor;
    FOnSearchPosition: TSynBaseCompletionSearchPosition;
    FOnKeyCompletePrefix: TNotifyEvent;
    FOnKeyNextChar: TNotifyEvent;
    FOnKeyPrevChar: TNotifyEvent;
    FTextColor: TColor;
    FTextSelectedColor: TColor;
    FHint: TSynBaseCompletionHint;
    FHintTimer: TTimer;
    FLongLineHintTime: Integer;
    FLongLineHintType: TSynCompletionLongHintType;
    FMouseWheelAccumulator: Integer;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure SetCurrentString(const Value: string);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Paint; override;
    procedure AppDeactivated(Sender: TObject); // Because Form.Deactivate isn't called
    procedure Deactivate; override;
    procedure SelectPrec;
    procedure SelectNext;
    procedure ScrollChange(Sender: TObject);
    procedure ScrollGetFocus(Sender: TObject);
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SetItemList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure StringListChange(Sender: TObject);
    procedure DoOnResize; override;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure FontChanged(Sender: TObject); override;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
  private
    fCurrentEditor: TComponent;
    FDoubleClickSelects: Boolean;
    FDrawBorderWidth: Integer;
    FOnDragResized: TNotifyEvent;
    FOnMeasureItem: TSynBaseCompletionMeasureItem;
    FOnPositionChanged: TNotifyEvent;
    FShowSizeDrag: Boolean;
    FHintLock: Integer;
    procedure SetCurrentEditor(const AValue: TComponent);
    procedure SetDrawBorderWidth(const AValue: Integer);
    procedure SetLongLineHintTime(const AValue: Integer);
    procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetShowSizeDrag(const AValue: Boolean);
  protected
    procedure SetVisible(Value: Boolean); override;
    property DrawBorderWidth: Integer read FDrawBorderWidth write SetDrawBorderWidth;
    procedure IncHintLock;
    procedure DecHintLock;
    procedure DoOnDragResize(Sender: TObject);
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure ShowItemHint(AIndex: Integer);
    procedure OnHintTimer(Sender: TObject);
  published
    property CurrentString: string read FCurrentString write SetCurrentString;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDelete: TNotifyEvent read FOnKeyDelete write FOnKeyDelete;
    property OnPaintItem: TSynBaseCompletionPaintItem read FOnPaintItem
      write FOnPaintItem;
    property OnMeasureItem: TSynBaseCompletionMeasureItem read FOnMeasureItem
      write FOnMeasureItem;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property ItemList: TStrings read FItemList write SetItemList;
    property Position: Integer read FPosition write SetPosition;
    property NbLinesInWindow: Integer read FNbLinesInWindow
      write SetNbLinesInWindow;
    property ClSelect: TColor read FClSelect write FClSelect;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property CurrentEditor: TComponent read fCurrentEditor write SetCurrentEditor;
    property FontHeight:integer read FFontHeight;
    property OnSearchPosition:TSynBaseCompletionSearchPosition
      read FOnSearchPosition write FOnSearchPosition;
    property OnKeyCompletePrefix: TNotifyEvent read FOnKeyCompletePrefix write FOnKeyCompletePrefix;// e.g. Tab
    property OnKeyNextChar: TNotifyEvent read FOnKeyNextChar write FOnKeyNextChar;// e.g. arrow right
    property OnKeyPrevChar: TNotifyEvent read FOnKeyPrevChar write FOnKeyPrevChar;// e.g. arrow left
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property TextColor: TColor read FTextColor write FTextColor;
    property TextSelectedColor: TColor
      read FTextSelectedColor write FTextSelectedColor;
    property LongLineHintTime: Integer read FLongLineHintTime
             write SetLongLineHintTime default 0;
    property LongLineHintType: TSynCompletionLongHintType read FLongLineHintType
             write FLongLineHintType default sclpExtendRightOnly;
    property DoubleClickSelects: Boolean read FDoubleClickSelects write FDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read FShowSizeDrag write SetShowSizeDrag default False;
    property OnDragResized: TNotifyEvent read FOnDragResized write FOnDragResized;
  end;

  { TSynBaseCompletion }

  TSynBaseCompletion = class(TComponent)
  private
    Form: TSynBaseCompletionForm;
    FAddedPersistentCaret: boolean;
    FOnExecute: TNotifyEvent;
    FWidth: Integer;
    function GetCaseSensitive: boolean;
    function GetClSelect: TColor;
    function GetDoubleClickSelects: Boolean;
    function GetLongLineHintTime: Integer;
    function GetLongLineHintType: TSynCompletionLongHintType;
    function GetOnKeyDown: TKeyEvent;
    function GetOnMeasureItem: TSynBaseCompletionMeasureItem;
    function GetOnPositionChanged: TNotifyEvent;
    function GetShowSizeDrag: Boolean;
    procedure SetCaseSensitive(const AValue: boolean);
    procedure SetClSelect(const Value: TColor);
    function GetCurrentString: string;
    function GetItemList: TStrings;
    function GetNbLinesInWindow: Integer;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionPaintItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetDoubleClickSelects(const AValue: Boolean);
    procedure SetItemList(const Value: TStrings);
    procedure SetLongLineHintTime(const AValue: Integer);
    procedure SetLongLineHintType(const AValue: TSynCompletionLongHintType);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnMeasureItem(const AValue: TSynBaseCompletionMeasureItem);
    procedure SetOnPositionChanged(const AValue: TNotifyEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionPaintItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    function GetOnKeyDelete: TNotifyEvent;
    procedure SetOnKeyDelete(const Value: TNotifyEvent);
    procedure SetShowSizeDrag(const AValue: Boolean);
    procedure SetWidth(Value: Integer);
    function GetOnUTF8KeyPress: TUTF8KeyPressEvent;
    procedure SetOnUTF8KeyPress(const AValue: TUTF8KeyPressEvent);
    function GetFontHeight:integer;
    function GetOnSearchPosition:TSynBaseCompletionSearchPosition;
    procedure SetOnSearchPosition(NewValue :TSynBaseCompletionSearchPosition);
    function GetOnKeyCompletePrefix: TNotifyEvent;
    procedure SetOnKeyCompletePrefix(const AValue: TNotifyEvent);
    function GetOnKeyNextChar: TNotifyEvent;
    procedure SetOnKeyNextChar(const AValue: TNotifyEvent);
    function GetOnKeyPrevChar: TNotifyEvent;
    procedure SetOnKeyPrevChar(const AValue: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer); overload;
    procedure Execute(s: string; TopLeft: TPoint); overload;
    procedure Execute(s: string; TokenRect: TRect); overload; // Excute below or above the token // may be extended to adjust left corner too
    procedure Deactivate;
    function IsActive: boolean;
    function TheForm: TSynBaseCompletionForm;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnUTF8KeyPress
                                                write SetOnUTF8KeyPress;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyDelete: TNotifyEvent read GetOnKeyDelete write SetOnKeyDelete;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
  published
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnPaintItem: TSynBaseCompletionPaintItem
      read GetOnPaintItem write SetOnPaintItem;
    property OnMeasureItem: TSynBaseCompletionMeasureItem read GetOnMeasureItem
      write SetOnMeasureItem;
    property ItemList: TStrings read GetItemList write SetItemList;
    property Position: Integer read GetPosition write SetPosition;
    property NbLinesInWindow: Integer read GetNbLinesInWindow
                                      write SetNbLinesInWindow;
    property FontHeight: integer read GetFontHeight;
    property OnSearchPosition: TSynBaseCompletionSearchPosition
                             read GetOnSearchPosition write SetOnSearchPosition;
    property OnKeyCompletePrefix: TNotifyEvent read GetOnKeyCompletePrefix
                                               write SetOnKeyCompletePrefix;// e.g. Tab
    property OnKeyNextChar: TNotifyEvent read GetOnKeyNextChar
                                         write SetOnKeyNextChar;// e.g. arrow right
    property OnKeyPrevChar: TNotifyEvent read GetOnKeyPrevChar
                                         write SetOnKeyPrevChar;// e.g. arrow left
    property OnPositionChanged: TNotifyEvent read GetOnPositionChanged
                                             write SetOnPositionChanged;
    property ClSelect: TColor read GetClSelect write SetClSelect;
    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    property Width: Integer read FWidth write SetWidth;
    property LongLineHintTime: Integer read GetLongLineHintTime
             write SetLongLineHintTime default 0;
    property LongLineHintType: TSynCompletionLongHintType read GetLongLineHintType
             write SetLongLineHintType default sclpExtendRightOnly;
    property DoubleClickSelects: Boolean read GetDoubleClickSelects write SetDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read GetShowSizeDrag write SetShowSizeDrag default False;
  end;

  { TSynCompletion }

  TSynCompletion = class(TSynBaseCompletion)
  private
    FShortCut: TShortCut;
    fEditors: TList;
    fEditstuffs: TList;
    FEndOfTokenChr: string;
    FOnCodeCompletion: TCodeCompletionEvent;
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure backspace(Sender: TObject);
    procedure Cancel(Sender: TObject);
    procedure Validate(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure UTF8KeyPress(Sender: TObject; var Key: TUTF8Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure EditorUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    function GetPreviousToken(FEditor: TCustomSynEdit): string;
    function GetFEditor: TCustomSynEdit;
    function GetEditor(i: integer): TCustomSynEdit;
  protected
    procedure OnFormPaint(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetShortCut(Value: TShortCut);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Editors[i: integer]: TCustomSynEdit read GetEditor;
    procedure AddEditor(aEditor: TCustomSynEdit);
    function RemoveEditor(aEditor: TCustomSynEdit): boolean;
    function EditorsCount: integer;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property Editor: TCustomSynEdit read GetFEditor write SetEditor;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property OnCodeCompletion: TCodeCompletionEvent
      read FOnCodeCompletion write FOnCodeCompletion;
  end;

  { TSynAutoComplete }

  TSynAutoComplete = class(TComponent)
  private
    FShortCut: TShortCut;
    fEditors: TList;
    fEditstuffs: TList;
    fAutoCompleteList: TStrings;
    FEndOfTokenChr: string;
    procedure SetAutoCompleteList(List: TStrings);
    function GetEditor(i: integer): TCustomSynEdit;
    function GetEdit: TCustomSynEdit;
    procedure SetEdit(const Value: TCustomSynEdit);
  protected
    procedure SetShortCut(Value: TShortCut);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure EditorKeyPress(Sender: TObject; var Key: char); virtual;
    procedure EditorUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
      virtual;
    function GetPreviousToken(aEditor: TCustomSynEdit): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(token: string; aEditor: TCustomSynEdit);
    property Editors[i: integer]: TCustomSynEdit read GetEditor;
    procedure AddEditor(aEditor: TCustomSynEdit);
    function RemoveEditor(aEditor: TCustomSynEdit): boolean;
    function EditorsCount: integer;
    function GetTokenList: string;
    function GetTokenValue(Token: string): string; 
  published
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property Editor: TCustomSynEdit read GetEdit write SetEdit;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
  end;

procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);


implementation

{ TSynBaseCompletionFormSizeDrag }

procedure TSynBaseCompletionFormSizeDrag.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMouseDownPos.x := x + Left;
  FMouseDownPos.y := y + Top;
  FMouseLastPos.x := x + Left;
  FMouseLastPos.y := y + Top;
  FWinSize.x := TSynBaseCompletionForm(Owner).Width;
  FWinSize.y := TSynBaseCompletionForm(Owner).Height;
  TSynBaseCompletionForm(Owner).IncHintLock;
  MouseCapture := True;
end;

procedure TSynBaseCompletionFormSizeDrag.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: TSynBaseCompletionForm;
begin
  inherited MouseMove(Shift, X, Y);
  x := x + Left;
  y := y + Top;
  if (FMouseDownPos.y < 0) or
     ((FMouseLastPos.x = x) and (FMouseLastPos.y = y))
  then
    exit;
  FMouseLastPos.x := x;
  FMouseLastPos.y := y;

  F := TSynBaseCompletionForm(Owner);
  F.Width :=
    Max(FWinSize.x + x - FMouseDownPos.x, 100);
  F.NbLinesInWindow :=
    Max((FWinSize.y + y - FMouseDownPos.y) div F.FontHeight, 3);
end;

procedure TSynBaseCompletionFormSizeDrag.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseDownPos.y := -1;
  MouseCapture := False;
  TSynBaseCompletionForm(Owner).DecHintLock;

  if (FWinSize.x <> TSynBaseCompletionForm(Owner).Width) or
     (FWinSize.y <> TSynBaseCompletionForm(Owner).Height)
  then
    TSynBaseCompletionForm(Owner).DoOnDragResize(Owner);
end;

constructor TSynBaseCompletionFormSizeDrag.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMouseDownPos.y := -1;
end;

procedure TSynBaseCompletionFormSizeDrag.Paint;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clBtnShadow;
  Canvas.MoveTo(ClientRect.Right-2, ClientRect.Bottom-1);
  Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-2);
  Canvas.MoveTo(ClientRect.Right-5, ClientRect.Bottom-1);
  Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-5);
  Canvas.MoveTo(ClientRect.Right-8, ClientRect.Bottom-1);
  Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-8);
end;

{ TSynBaseCompletionForm }

constructor TSynBaseCompletionForm.Create(AOwner: TComponent);
begin
  FResizeLock := 1; // prevent DoResize (on Handle Creation) do reset LinesInWindow
  FDoubleClickSelects := True;
  FHintLock := 0;
  BeginFormUpdate;
  KeyPreview:= True;
  // we have no resource => must be constructed using CreateNew
  inherited CreateNew(AOwner, 1);
  FItemList := TStringList.Create;
  BorderStyle := bsNone;
  FormStyle := fsSystemStayOnTop;
  Scroll := TScrollBar.Create(self);
  Scroll.Kind := sbVertical;
  {$IFNDEF SYN_LAZARUS}
  Scroll.ParentCtl3D := False;
  {$ENDIF}
  Scroll.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollChange;
  Scroll.Parent := Self;
  Scroll.OnEnter := {$IFDEF FPC}@{$ENDIF}ScrollGetFocus;
  Scroll.OnScroll := {$IFDEF FPC}@{$ENDIF}ScrollScroll;
  Scroll.TabStop := False;
  Scroll.Visible := True;
  //Scroll.Align:=alRight;

  SizeDrag := TSynBaseCompletionFormSizeDrag.Create(Self);
  SizeDrag.Parent := Self;
  SizeDrag.BevelInner := bvNone;
  SizeDrag.BevelOuter := bvNone;
  SizeDrag.Caption := '';
  SizeDrag.AutoSize := False;
  SizeDrag.BorderStyle := bsNone;
  SizeDrag.Anchors := [akBottom, akRight, akLeft];
  SizeDrag.AnchorSideLeft.Side := asrTop;
  SizeDrag.AnchorSideLeft.Control := Scroll;
  SizeDrag.AnchorSideRight.Side := asrBottom;
  SizeDrag.AnchorSideRight.Control := Self;
  SizeDrag.AnchorSideBottom.Side := asrBottom;
  SizeDrag.AnchorSideBottom.Control := Self;
  SizeDrag.Height := Max(7, abs(Font.Height) * 2 div 3);
  SizeDrag.Cursor := crSizeNWSE;
  SizeDrag.Visible := False;

  SizeDrag.OnKeyPress:=@SDKeyPress;
  SizeDrag.OnKeyDown:=@SDKeyDown;
  SizeDrag.OnUTF8KeyPress:=@SDUtf8KeyPress;

  Scroll.Anchors:=[akTop,akRight, akBottom];
  Scroll.AnchorSide[akTop].Side := asrTop;
  Scroll.AnchorSide[akTop].Control := self;
  Scroll.AnchorSide[akRight].Side := asrBottom;
  Scroll.AnchorSide[akRight].Control := Self;
  Scroll.AnchorSide[akBottom].Side := asrTop;
  Scroll.AnchorSide[akBottom].Control := SizeDrag;

  DrawBorderWidth := 1;
  FTextColor:=clBlack;
  FTextSelectedColor:=clWhite;
  Caption:='Completion';
  Color:=clNone;
  FBackgroundColor:=clWhite;
  FHint := TSynBaseCompletionHint.Create(Self);
  FHint.FormStyle := fsSystemStayOnTop;;
  FHintTimer := TTimer.Create(nil);
  FHintTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnHintTimer;
  FHintTimer.Interval := 0;
  FLongLineHintTime := 0;
  FLongLineHintType := sclpExtendRightOnly;
  Visible := false;
  ClSelect := clHighlight;
  TStringList(FItemList).OnChange := {$IFDEF FPC}@{$ENDIF}StringListChange;
  FNbLinesInWindow := 6;
  FontChanged(Font);
  ShowHint := False;
  EndFormUpdate;
  FResizeLock := 0;
end;

procedure TSynBaseCompletionForm.Deactivate;
begin
  // completion box lost focus
  // this can happen when a hint window is clicked => ToDo
  Visible := False;
  FHintTimer.Enabled := False;
  FHint.Visible := False;
  if Assigned(OnCancel) then OnCancel(Self);
  if (FCurrentEditor<>nil) and (TCustomSynEdit(fCurrentEditor).HandleAllocated)
  then
    SetCaretRespondToFocus(TCustomSynEdit(FCurrentEditor).Handle,true);
end;

destructor TSynBaseCompletionForm.Destroy;
begin
  FreeAndNil(Scroll);
  FreeAndNil(SizeDrag);
  FItemList.Free;
  FHintTimer.Free;
  FHint.Free;
  inherited destroy;
end;

procedure TSynBaseCompletionForm.ShowItemHint(AIndex: Integer);
var
  R: TRect;
  P: TPoint;
  M: TMonitor;
  MinLeft: Integer;
begin
  FHintTimer.Enabled := False;
  if Visible and (AIndex >= 0) and (AIndex < ItemList.Count) and
     (FLongLineHintType <> sclpNone) and
     (FHintLock = 0)
  then begin
    // CalcHintRect uses the current index
    FHint.Index := AIndex;
    // calculate the size
    R := FHint.CalcHintRect(Monitor.Width, ItemList[AIndex], nil);

    if (R.Right <= Scroll.Left) then begin
      FHint.Hide;
      Exit;
    end;

    // calculate the position
    M := Monitor;
    P := ClientToScreen(Point(0, (AIndex - Scroll.Position) * FFontHeight));
    case FLongLineHintType of
      sclpExtendHalfLeft:      MinLeft := Max(M.Left,  P.X - ClientWidth div 2); // ClientWidth may be too much, if part of the ClientWidth extends to another screen.
      sclpExtendUnlimitedLeft: MinLeft := M.Left;
      else                     MinLeft := P.X;
    end;
    P.X := Max(MinLeft,
               Min(P.X,                              // Start at drop-down Left boundary
                   M.Left + M.Width - R.Right - 1    // Or push left, if hitting right Monitor border
                  )
              );
    P.Y := Max(M.Top, Min(P.Y, M.Top + M.Height - R.Bottom - 1));
    // actually Width and Height
    R.Right := Min(r.Right, M.Left + M.Width - 1 - P.X);
    R.Bottom := Min(r.Bottom, M.Top + M.Height - 1 - P.Y);

    FHint.DisplayRect := Bounds(P.X, P.Y, R.Right, R.Bottom);

    if (not FHint.IsVisible) and (FLongLineHintTime > 0) then
      FHintTimer.Enabled := True
    else
      OnHintTimer(nil);
  end
  else begin
    FHint.Hide;
  end;
end;

procedure TSynBaseCompletionForm.OnHintTimer(Sender: TObject);
begin
  FHintTimer.Enabled := False;
  FHint.ActivateHint(FHint.DisplayRect, ItemList[FHint.Index]);
  FHint.Invalidate;
end;

procedure TSynBaseCompletionForm.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  i: integer;
  Handled: Boolean;
begin
  //debugln('TSynBaseCompletionForm.KeyDown A Key=',dbgs(Key));
  inherited KeyDown(Key,Shift);
  if Key=VK_UNKNOWN then exit;
  Handled:=true;
  case Key of
// added the VK_XXX codes to make it more readable / maintainable
    VK_RETURN:
      if Assigned(OnValidate) then
        OnValidate(Self, '', Shift);
    VK_ESCAPE:
      if Assigned(OnCancel) then OnCancel(Self);
    // I do not think there is a worst way to do this, but laziness rules :-)
    VK_PRIOR:
      for i := 1 to NbLinesInWindow do
        SelectPrec;
    VK_NEXT:
      for i := 1 to NbLinesInWindow do
        SelectNext;
    VK_END:
      Position := ItemList.count - 1;
    VK_HOME:
      Position := 0;
    VK_UP:
      if ssCtrl in Shift then
        Position := 0
      else
        SelectPrec;
    VK_DOWN:
      if ssCtrl in Shift then
        Position := ItemList.count - 1
      else
        SelectNext;
    VK_BACK:
      if (Shift = []) and (Length(CurrentString) > 0) then begin
        if Assigned(OnKeyDelete) then OnKeyDelete(Self);
        CurrentString := UTF8Copy(CurrentString, 1, UTF8Length(CurrentString) - 1);
      end;
    VK_TAB:
      begin
        if Assigned(OnKeyCompletePrefix) then OnKeyCompletePrefix(Self);
      end;
    VK_LEFT:
      begin
        if (Shift = []) and (Length(CurrentString) > 0) then begin
          if Assigned(OnKeyPrevChar) then OnKeyPrevChar(Self);
        end;
      end;
    VK_Right:
      begin
        if Assigned(OnKeyNextChar) then OnKeyNextChar(Self);
      end;
  else
    Handled:=false;
  end;
  if Handled then Key:=VK_UNKNOWN;
  Invalidate;
end;

procedure TSynBaseCompletionForm.KeyPress(var Key: char);
begin
  debugln('TSynBaseCompletionForm.KeyPress A Key="',DbgStr(Key),'"');
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
  debugln('TSynBaseCompletionForm.KeyPress B Key="',DbgStr(Key),'"');
  if Key=#0 then exit;
  case key of //
    #33..'z':
      begin
        if Key<>#0 then
          CurrentString := CurrentString + key;
        Key:=#0;
      end;
    #8: ;
  else
    if (ord(key)>=32) and Assigned(OnValidate) then begin
      OnValidate(Self, Key, []);
      Key:=#0;
    end else begin
      if Assigned(OnCancel) then OnCancel(Self);
      Key:=#0;
    end;
  end; // case
  Invalidate;
  //debugln('TSynBaseCompletionForm.KeyPress END Key="',DbgStr(Key),'"');
end;

procedure TSynBaseCompletionForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  y := (y - 1) div FFontHeight;
  Position := Scroll.Position + y;
  if DoubleClickSelects and (ssDouble in Shift) and (Position = OldPosition) and
     Assigned(OnValidate)
  then
    OnValidate(Self, '', Shift);
end;

procedure TSynBaseCompletionForm.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if ((Scroll.Visible) and (x > Scroll.Left)) or
     (y  < DrawBorderWidth) or (y >= ClientHeight - DrawBorderWidth)
  then
    exit;
  Y := (Y - DrawBorderWidth) div FFontHeight;
  ShowItemHint(Scroll.Position + Y);
end;

procedure TSynBaseCompletionForm.Paint;
var
  i: integer;

  function Min(a, b: integer): integer;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;

begin
//Writeln('[TSynBaseCompletionForm.Paint]');

  // update scroll bar
  Scroll.Enabled := ItemList.Count > NbLinesInWindow;
  Scroll.Visible := (ItemList.Count > NbLinesInWindow) or ShowSizeDrag;

  if Scroll.Visible and Scroll.Enabled then
  begin
    Scroll.Max := ItemList.Count - 1;
    Scroll.LargeChange := NbLinesInWindow;
    Scroll.PageSize := NbLinesInWindow;
  end
  else
  begin
    Scroll.PageSize := 1;
    Scroll.Max := 0;
  end;

  //DebugLn(['TSynBaseCompletionForm.Paint NbLinesInWindow=',NbLinesInWindow,' ItemList.Count=',ItemList.Count]);
  for i := 0 to min(NbLinesInWindow - 1, ItemList.Count - Scroll.Position - 1) do
  begin
    if i + Scroll.Position = Position then
    begin
      Canvas.Brush.Color := clSelect;
      Canvas.Pen.Color := clSelect;
      Canvas.Rectangle(DrawBorderWidth, DrawBorderWidth+(FFontHeight * i),
                      Width-2*DrawBorderWidth, (FFontHeight * (i + 1))+1);
      Canvas.Pen.Color := clBlack;
      Canvas.Font.Color := TextSelectedColor;
      Hint := ItemList[Position];
    end
    else
    begin
      Canvas.Brush.Color := BackgroundColor;
      Canvas.Font.Color := TextColor;
      Canvas.FillRect(Rect(DrawBorderWidth, DrawBorderWidth+(FFontHeight * i),
                           Width-2*DrawBorderWidth, (FFontHeight * (i + 1))+1));
    end;

    //DebugLn(['TSynBaseCompletionForm.Paint ',i,' ',ItemList[Scroll.Position + i]]);
    if not Assigned(OnPaintItem) or
       not OnPaintItem(ItemList[Scroll.Position + i], Canvas,
        DrawBorderWidth, DrawBorderWidth+FFontHeight * i, i + Scroll.Position = Position,
        i + Scroll.Position
        ) then
    begin
      Canvas.TextOut(DrawBorderWidth+2, DrawBorderWidth+FFontHeight * i, ItemList[Scroll.Position + i]);
    end;
  end;
  // paint the rest of the background
  if NbLinesInWindow > ItemList.Count - Scroll.Position then
  begin
    Canvas.brush.color := color;
    i:=(FFontHeight * ItemList.Count)+1;
    Canvas.FillRect(Rect(0, i, Width, Height));
  end;
  // draw a rectangle around the window
  if DrawBorderWidth > 0 then begin
    Canvas.Pen.Color := TextColor;
    Canvas.Pen.Width := DrawBorderWidth;
    Canvas.Moveto(0, 0);
    Canvas.LineTo(Width - 1, 0);
    Canvas.LineTo(Width - 1, Height - 1);
    Canvas.LineTo(0, Height - 1);
    Canvas.LineTo(0, 0);
  end;
end;

procedure TSynBaseCompletionForm.AppDeactivated(Sender: TObject);
begin
  Deactivate;
end;

procedure TSynBaseCompletionForm.ScrollChange(Sender: TObject);
begin
  if Position < Scroll.Position then
    Position := Scroll.Position
  else 
  if Position > Scroll.Position + NbLinesInWindow - 1 then
    Position := Scroll.Position + NbLinesInWindow - 1;
  Invalidate;
end;

procedure TSynBaseCompletionForm.ScrollGetFocus(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynBaseCompletionForm.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if ScrollPos > (Scroll.Max - Scroll.PageSize) + 1 then
    ScrollPos := Scroll.Max - Scroll.PageSize + 1;
  FHint.Hide;
  ShowItemHint(Position);
end;

procedure TSynBaseCompletionForm.SelectNext;
begin
  if Position < ItemList.Count - 1 then
    Position := Position + 1;
end;

procedure TSynBaseCompletionForm.SelectPrec;
begin
  if Position > 0 then
    Position := Position - 1;
end;

procedure TSynBaseCompletionForm.SDKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(key,shift);
end;

procedure TSynBaseCompletionForm.SDKeyPress(Sender: TObject; var Key: char);
begin
  KeyPress(key);
end;

procedure TSynBaseCompletionForm.SDUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

procedure TSynBaseCompletionForm.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  //debugln('TSynBaseCompletionForm.UTF8KeyPress A UTF8Key="',DbgStr(UTF8Key),'" ',dbgsName(TObject(TMethod(OnUTF8KeyPress).Data)));
  if UTF8Key=#8 then
  begin
    // backspace
  end else
  begin
    if (Length(UTF8Key)>=1) and (not (UTF8Key[1] in ['a'..'z','A'..'Z','0'..'9','_'])) then
    begin
      // non identifier character

      // if it is special key then eat it
      if (Length(UTF8Key) = 1) and (UTF8Key[1] < #32) then
      begin
        if Assigned(OnCancel) then
          OnCancel(Self);
      end
      else
      if Assigned(OnValidate) then
        OnValidate(Self, UTF8Key, []);
      UTF8Key := '';
    end else
    if (UTF8Key<>'') then
    begin
      // identifier character
      CurrentString := CurrentString + UTF8Key;
      if Assigned(OnUTF8KeyPress) then
        OnUTF8KeyPress(Self, UTF8Key);
      UTF8Key := '';
    end;
  end;
  debugln('TSynBaseCompletionForm.UTF8KeyPress END UTF8Key="',DbgStr(UTF8Key),'"');
end;

procedure TSynBaseCompletionForm.SetCurrentString(const Value: string);
var
  i: integer;
begin
  FCurrentString := Value;
  //debugln('TSynBaseCompletionForm.SetCurrentString FCurrentString=',FCurrentString);
  if Assigned(FOnSearchPosition) then begin
    i:=Position;
    FOnSearchPosition(i);
    Position:=i;
  end else begin
    if FCaseSensitive then begin
      for i := 0 to Pred(ItemList.Count) do
        if 0 = CompareStr(fCurrentString,
          Copy(ItemList[i], 1, Length(fCurrentString)))
        then begin
          Position := i;
          break;
        end;
    end else begin
      for i := 0 to Pred(ItemList.Count) do
        if 0 = WideCompareText(UTF8Decode(fCurrentString),
                       UTF8Decode(Copy(ItemList[i], 1, Length(fCurrentString))))
        then begin
          Position := i;
          break;
        end;
    end;
  end;
end;

procedure TSynBaseCompletionForm.DoOnResize;
begin
  inherited DoOnResize;
  if ([csLoading,csDestroying]*ComponentState<>[]) or (Scroll=nil) then exit;
  if (fFontHeight > 0) and (FResizeLock = 0) then
  begin
    FNbLinesInWindow := (Height-2*DrawBorderWidth+(fFontHeight-1)) div fFontHeight;
    Invalidate;
  end;
end;

procedure TSynBaseCompletionForm.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Color := AValue;
    FHint.Color := AValue;
  end;
end;

procedure TSynBaseCompletionForm.FontChanged(Sender: TObject);
var
  TextMetric: TTextMetric;
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    inherited;
    FillChar(TextMetric,SizeOf(TextMetric),0);
    GetTextMetrics(Canvas.Handle, TextMetric);
    FFontHeight := TextMetric.tmHeight+2;
    SetNblinesInWindow(FNbLinesInWindow);
    SizeDrag.Height := Max(7, FFontHeight * 2 div 3);
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynBaseCompletionForm.WMMouseWheel(var Msg: TLMMouseEvent);
const
  WHEEL_DELTA = 120;
var
  WheelClicks: Integer;
begin
  Inc(FMouseWheelAccumulator, Msg.WheelDelta);
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator - WheelClicks * WHEEL_DELTA;
  WheelClicks := WheelClicks * Mouse.WheelScrollLines;
  Scroll.Position := Max(0, Min(FItemList.Count - NbLinesInWindow, Scroll.Position - WheelClicks));
end;

procedure TSynBaseCompletionForm.SetLongLineHintTime(const AValue: Integer);
begin
  if FLongLineHintTime = AValue then exit;
  FLongLineHintTime := AValue;
  FHintTimer.Interval := AValue;
end;

procedure TSynBaseCompletionForm.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scTopLine in Changes) and Assigned(OnCancel) then
    OnCancel(Self);
end;

procedure TSynBaseCompletionForm.SetShowSizeDrag(const AValue: Boolean);
begin
  if FShowSizeDrag = AValue then exit;
  FShowSizeDrag := AValue;
  SizeDrag.Visible := AValue;
end;

procedure TSynBaseCompletionForm.SetCurrentEditor(const AValue: TComponent);
begin
  if fCurrentEditor = AValue then exit;
  if fCurrentEditor <> nil then
    TCustomSynEdit(fCurrentEditor).UnRegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}EditorStatusChanged);
  fCurrentEditor := AValue;
  if (fCurrentEditor <> nil) and Visible then
    TCustomSynEdit(fCurrentEditor).RegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}EditorStatusChanged,
                                                          [scTopLine]);
end;

procedure TSynBaseCompletionForm.SetDrawBorderWidth(const AValue: Integer);
begin
  if FDrawBorderWidth = AValue then exit;
  FDrawBorderWidth := AValue;
  NbLinesInWindow := NbLinesInWindow;
  Scroll.BorderSpacing.Top := FDrawBorderWidth;
  Scroll.BorderSpacing.Right := FDrawBorderWidth;
  if SizeDrag.Visible then
    Scroll.BorderSpacing.Bottom := 0
  else
    Scroll.BorderSpacing.Bottom := FDrawBorderWidth;
  SizeDrag.BorderSpacing.Right := FDrawBorderWidth;
  SizeDrag.BorderSpacing.Bottom := FDrawBorderWidth;
end;

procedure TSynBaseCompletionForm.SetVisible(Value: Boolean);
begin
  if Visible = Value then exit;;
  inherited SetVisible(Value);
  if (fCurrentEditor <> nil) then begin
    if Visible then
      TCustomSynEdit(fCurrentEditor).RegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}EditorStatusChanged,
                                                            [scTopLine])
    else
      TCustomSynEdit(fCurrentEditor).UnRegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}EditorStatusChanged);
  end;
  if Value then
    Application.AddOnDeactivateHandler({$IFDEF FPC}@{$ENDIF}AppDeactivated)
  else
    Application.RemoveOnDeactivateHandler({$IFDEF FPC}@{$ENDIF}AppDeactivated);
end;

procedure TSynBaseCompletionForm.IncHintLock;
begin
  inc(FHintLock);
  FHint.Hide
end;

procedure TSynBaseCompletionForm.DecHintLock;
begin
  dec(FHintLock);
  if FHintLock = 0 then
    ShowItemHint(Position);
end;

procedure TSynBaseCompletionForm.DoOnDragResize(Sender: TObject);
begin
  if assigned(FOnDragResized) then
    FOnDragResized(Sender);
end;

procedure TSynBaseCompletionForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  if Position>=FItemList.Count then Position:=-1;
  Invalidate;
end;

procedure TSynBaseCompletionForm.SetNbLinesInWindow(
  const Value: Integer);
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    FNbLinesInWindow := Value;
    Height := fFontHeight * NbLinesInWindow + 2*DrawBorderWidth;
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynBaseCompletionForm.SetPosition(const Value: Integer);
begin
  if Value < ItemList.Count then begin
    if FPosition <> Value then begin
      FPosition := Value;
      if Position < Scroll.Position then
        Scroll.Position := Position
      else if Scroll.Position < Position - NbLinesInWindow + 1 then
        Scroll.Position := Position - NbLinesInWindow + 1;
      Invalidate;
      if Assigned(OnPositionChanged) then OnPositionChanged(Self);
    end;
  end;
  if Showing then
    ShowItemHint(Position);
end;

procedure TSynBaseCompletionForm.StringListChange(Sender: TObject);
begin
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  Position := Position;
end;

{ TSynBaseCompletion }

constructor TSynBaseCompletion.Create(AOwner: TComponent);
begin
  FWidth := 262;
  inherited Create(AOwner);
  Form := TSynBaseCompletionForm.Create(Self);
  Form.Width := FWidth;
end;

destructor TSynBaseCompletion.Destroy;
begin
  Form.Free;
  inherited Destroy;
end;

function TSynBaseCompletion.GetOnUTF8KeyPress: TUTF8KeyPressEvent;
begin
  Result:=Form.OnUTF8KeyPress;
end;

procedure TSynBaseCompletion.SetOnUTF8KeyPress(
  const AValue: TUTF8KeyPressEvent);
begin
  Form.OnUTF8KeyPress:=AValue;
end;

function TSynBaseCompletion.GetFontHeight:integer;
begin
  Result:=Form.FontHeight;
end;

function TSynBaseCompletion.GetOnSearchPosition:TSynBaseCompletionSearchPosition;
begin
  Result:=Form.OnSearchPosition;
end;

procedure TSynBaseCompletion.SetOnSearchPosition(
  NewValue :TSynBaseCompletionSearchPosition);
begin
  Form.OnSearchPosition:=NewValue;
end;

function TSynBaseCompletion.GetOnKeyCompletePrefix: TNotifyEvent;
begin
  Result:=Form.OnKeyCompletePrefix;
end;

procedure TSynBaseCompletion.SetOnKeyCompletePrefix(const AValue: TNotifyEvent);
begin
  Form.OnKeyCompletePrefix:=AValue;
end;

function TSynBaseCompletion.GetOnKeyNextChar: TNotifyEvent;
begin
  Result:=Form.OnKeyNextChar;
end;

procedure TSynBaseCompletion.SetOnKeyNextChar(const AValue: TNotifyEvent);
begin
  Form.OnKeyNextChar:=AValue;
end;

function TSynBaseCompletion.GetOnKeyPrevChar: TNotifyEvent;
begin
  Result:=Form.OnKeyPrevChar;
end;

procedure TSynBaseCompletion.SetOnKeyPrevChar(const AValue: TNotifyEvent);
begin
  Form.OnKeyPrevChar:=AValue;
end;

procedure TSynBaseCompletion.Execute(s: string; x, y: integer);
var
  CurSynEdit: TCustomSynEdit;
begin
  //writeln('TSynBaseCompletion.Execute ',Form.CurrentEditor.Name);

  //Todo: This is dangerous, if other plugins also change/changed the flag.
  FAddedPersistentCaret := False;

  CurrentString := s;
  if Assigned(OnExecute) then
    OnExecute(Self);
  if (ItemList.Count=1) and Assigned(OnValidate) then begin
    OnValidate(Form, '', []);
    exit;
  end;
  if (ItemList.Count=0) and Assigned(OnCancel) then begin
    OnCancel(Form);
    exit;
  end;

  if (Form.CurrentEditor is TCustomSynEdit) then begin
    CurSynEdit:=TCustomSynEdit(Form.CurrentEditor);
    FAddedPersistentCaret := not(eoPersistentCaret in CurSynEdit.Options);
    if FAddedPersistentCaret then
      CurSynEdit.Options:=CurSynEdit.Options+[eoPersistentCaret];
  end;
  Form.SetBounds(x,y,Form.Width,Form.Height);
  Form.Show;
  Form.Position := Form.Position;
end;

procedure TSynBaseCompletion.Execute(s: string; TopLeft: TPoint);
begin
  Execute(s, TopLeft.x, TopLeft.y);
end;

procedure TSynBaseCompletion.Execute(s: string; TokenRect: TRect);
var
  SpaceBelow, SpaceAbove: Integer;
  Mon: TMonitor;
begin
  Mon := Screen.MonitorFromPoint(TokenRect.TopLeft);
  if Mon <> nil then
    TokenRect.Left := Min(TokenRect.Left, Mon.Left + Mon.Width - Form.Width);

  SpaceBelow := Mon.Height - TokenRect.Bottom;
  SpaceAbove := TokenRect.Top - Mon.Top;
  if Form.Height < SpaceBelow then
    Execute(s, TokenRect.Left, TokenRect.Bottom)
  else
  if Form.Height < SpaceAbove then
    Execute(s, TokenRect.Left, TokenRect.Top - Form.Height)
  else
  begin
    if SpaceBelow > SpaceAbove then begin
      Form.NbLinesInWindow := Max(SpaceBelow div Form.FontHeight, 3); // temporary height
    Execute(s, TokenRect.Left, TokenRect.Bottom);
    end else begin
      Form.NbLinesInWindow := Max(SpaceAbove div Form.FontHeight, 3); // temporary height
      Execute(s, TokenRect.Left, TokenRect.Top - Form.Height);
    end;;
  end;
end;

function TSynBaseCompletion.GetCurrentString: string;
begin
  result := Form.CurrentString;
end;

function TSynBaseCompletion.GetItemList: TStrings;
begin
  result := Form.ItemList;
end;

function TSynBaseCompletion.GetNbLinesInWindow: Integer;
begin
  Result := Form.NbLinesInWindow;
end;

function TSynBaseCompletion.GetOnCancel: TNotifyEvent;
begin
  Result := Form.OnCancel;
end;

function TSynBaseCompletion.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;

function TSynBaseCompletion.GetOnPaintItem: TSynBaseCompletionPaintItem;
begin
  Result := Form.OnPaintItem;
end;

function TSynBaseCompletion.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;

function TSynBaseCompletion.GetPosition: Integer;
begin
  Result := Form.Position;
end;

procedure TSynBaseCompletion.SetCurrentString(const Value: string);
begin
  form.CurrentString := Value;
end;

procedure TSynBaseCompletion.SetDoubleClickSelects(const AValue: Boolean);
begin
  Form.DoubleClickSelects := AValue;
end;

procedure TSynBaseCompletion.SetItemList(const Value: TStrings);
begin
  form.ItemList := Value;
end;

procedure TSynBaseCompletion.SetLongLineHintTime(const AValue: Integer);
begin
  Form.LongLineHintTime := AValue;
end;

procedure TSynBaseCompletion.SetLongLineHintType(const AValue: TSynCompletionLongHintType);
begin
  Form.LongLineHintType := AValue;
end;

procedure TSynBaseCompletion.SetNbLinesInWindow(const Value: Integer);
begin
  form.NbLinesInWindow := Value;
end;

procedure TSynBaseCompletion.SetOnCancel(const Value: TNotifyEvent);
begin
  form.OnCancel := Value;
end;

procedure TSynBaseCompletion.SetOnKeyDown(const AValue: TKeyEvent);
begin
  Form.OnKeyDown:=AValue;
end;

procedure TSynBaseCompletion.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  form.OnKeyPress := Value;
end;

procedure TSynBaseCompletion.SetOnMeasureItem(
  const AValue: TSynBaseCompletionMeasureItem);
begin
  Form.OnMeasureItem := AValue;
end;

procedure TSynBaseCompletion.SetOnPositionChanged(const AValue: TNotifyEvent);
begin
  Form.OnPositionChanged :=  AValue;
end;

procedure TSynBaseCompletion.SetOnPaintItem(const Value:
  TSynBaseCompletionPaintItem);
begin
  form.OnPaintItem := Value;
end;

procedure TSynBaseCompletion.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;

procedure TSynBaseCompletion.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;

function TSynBaseCompletion.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;

function TSynBaseCompletion.GetDoubleClickSelects: Boolean;
begin
  Result := Form.DoubleClickSelects;
end;

function TSynBaseCompletion.GetLongLineHintTime: Integer;
begin
  Result := Form.LongLineHintTime;
end;

function TSynBaseCompletion.GetLongLineHintType: TSynCompletionLongHintType;
begin
  Result := Form.LongLineHintType;
end;

function TSynBaseCompletion.GetOnKeyDown: TKeyEvent;
begin
  Result:=Form.OnKeyDown;
end;

function TSynBaseCompletion.GetCaseSensitive: boolean;
begin
  Result := Form.CaseSensitive;
end;

function TSynBaseCompletion.GetOnMeasureItem: TSynBaseCompletionMeasureItem;
begin
  Result := Form.OnMeasureItem;
end;

function TSynBaseCompletion.GetOnPositionChanged: TNotifyEvent;
begin
  Result := Form.OnPositionChanged;
end;

function TSynBaseCompletion.GetShowSizeDrag: Boolean;
begin
  Result := Form.ShowSizeDrag;
end;

procedure TSynBaseCompletion.SetCaseSensitive(const AValue: boolean);
begin
  Form.CaseSensitive := AValue;
end;

procedure TSynBaseCompletion.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;

function TSynBaseCompletion.GetOnKeyDelete: TNotifyEvent;
begin
  result := Form.OnKeyDelete;
end;

procedure TSynBaseCompletion.SetOnKeyDelete(const Value: TNotifyEvent);
begin
  form.OnKeyDelete := Value;
end;

procedure TSynBaseCompletion.SetShowSizeDrag(const AValue: Boolean);
begin
  Form.ShowSizeDrag := AValue;
end;

procedure TSynBaseCompletion.SetWidth(Value: Integer);
begin
  FWidth := Value;
  Form.Width := FWidth;
  Form.SetNbLinesInWindow(Form.FNbLinesInWindow);
end;

procedure TSynBaseCompletion.Deactivate;
var
  CurSynEdit: TCustomSynEdit;
begin
  if FAddedPersistentCaret and
     (Form<>nil) and (Form.CurrentEditor is TCustomSynEdit)
  then begin
    CurSynEdit:=TCustomSynEdit(Form.CurrentEditor);
    CurSynEdit.Options:=CurSynEdit.Options-[eoPersistentCaret];
  end;
  if Assigned(Form) then Form.Deactivate;
end;

function TSynBaseCompletion.IsActive: boolean;
begin
  Result:=(Form<>nil) and (Form.Visible);
end;

function TSynBaseCompletion.TheForm: TSynBaseCompletionForm;
begin
  Result:=Form;
end;

procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);
var
  i: integer;
  OldFontColor: TColor;
  OldFontStyle: TFontStyles;
begin
  OldFontColor:=c.Font.Color;
  OldFontStyle:=c.Font.Style;
  c.Font.Style:=[];
  c.Font.Color:=clBlack;
  try
    i := 1;
    while i <= Length(s) do
      case s[i] of
        #1: begin
            C.Font.Color := (Ord(s[i + 3]) shl 8 + Ord(s[i + 2])) shl 8 + Ord(s[i + 1]);
            inc(i, 4);
          end;
        #2: begin
            C.Font.Color := (Ord(s[i + 3]) shl 8 + Ord(s[i + 2])) shl 8 + Ord(s[i + 1]);
            inc(i, 4);
          end;
        #3: begin
            case s[i + 1] of
              'B': c.Font.Style := c.Font.Style + [fsBold];
              'b': c.Font.Style := c.Font.Style - [fsBold];
              'U': c.Font.Style := c.Font.Style + [fsUnderline];
              'u': c.Font.Style := c.Font.Style - [fsUnderline];
              'I': c.Font.Style := c.Font.Style + [fsItalic];
              'i': c.Font.Style := c.Font.Style - [fsItalic];
            end;
            inc(i, 2);
          end;
      else
        C.TextOut(x, y, s[i]);
        x := x + c.TextWidth(s[i]);
        inc(i);
      end;
  except
  end;
  c.Font.Color:=OldFontColor;
  c.Font.Style:=OldFontStyle;
end;

{ TSynCompletion }

type
  TRecordUsedToStoreEachEditorVars = record
    KeyPress: TKeyPressEvent;
    KeyDown: TKeyEvent;
    UTF8KeyPress: TUTF8KeyPressEvent;
    NoNextKey: boolean;
  end;
  PRecordUsedToStoreEachEditorVars = ^TRecordUsedToStoreEachEditorVars;

procedure TSynCompletion.Backspace(Sender: TObject);
var
  F: TSynBaseCompletionForm;
begin
  F := Sender as TSynBaseCompletionForm;
  if F.CurrentEditor <> nil then begin
    (F.CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0,
      nil);
  end;
end;

procedure TSynCompletion.OnFormPaint(Sender: TObject);
begin

end;

procedure TSynCompletion.Cancel(Sender: TObject);
var
  F: TSynBaseCompletionForm;
begin
  F := Sender as TSynBaseCompletionForm;
  if F.CurrentEditor <> nil then begin
    if (F.CurrentEditor as TCustomSynEdit).Owner is TWinControl then
      TWinControl((F.CurrentEditor as TCustomSynEdit).Owner).SetFocus;
    (F.CurrentEditor as TCustomSynEdit).SetFocus;
  end;
end;

procedure TSynCompletion.Validate(Sender: TObject; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  F: TSynBaseCompletionForm;
  Value, CurLine: string;
  NewBlockBegin, NewBlockEnd: TPoint;
  LogCaret: TPoint;
begin
  //debugln('TSynCompletion.Validate ',dbgsName(Sender),' ',dbgs(Shift),' Position=',dbgs(Position));
  F := Sender as TSynBaseCompletionForm;
  // Note: Form.Visible can be false, for example when completion only contains one item
  if F.CurrentEditor is TCustomSynEdit then
    with TCustomSynEdit(F.CurrentEditor) do begin
      BeginUndoBlock;
      BeginUpdate;
      LogCaret := LogicalCaretXY;
      NewBlockBegin:=LogCaret;
      CurLine:=Lines[NewBlockBegin.Y - 1];
      while (NewBlockBegin.X>1) and (NewBlockBegin.X-1<=length(CurLine))
      and (CurLine[NewBlockBegin.X-1] in ['a'..'z','A'..'Z','0'..'9','_']) do
        dec(NewBlockBegin.X);
      //BlockBegin:=NewBlockBegin;
      if ssShift in Shift then begin
        // replace only prefix
        NewBlockEnd := LogCaret;
      end else begin
        // replace the whole word
        NewBlockEnd := LogCaret;
        CurLine:=Lines[NewBlockEnd.Y - 1];
        while (NewBlockEnd.X<=length(CurLine))
        and (CurLine[NewBlockEnd.X] in ['a'..'z','A'..'Z','0'..'9','_']) do
          inc(NewBlockEnd.X);
      end;
      //debugln('TSynCompletion.Validate B Position=',dbgs(Position));
      if Position>=0 then begin
        if Assigned(FOnCodeCompletion) then
        begin
          Value := ItemList[Position];
          FOnCodeCompletion(Value, TextBetweenPoints[NewBlockBegin, NewBlockEnd],
                            NewBlockBegin, NewBlockEnd, KeyChar, Shift);
          if (CompareCarets(NewBlockBegin, NewBlockEnd) <> 0) or (Value <> '') then
          begin
            TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := Value;
            TCustomSynEdit(F.CurrentEditor).SetFocus;
          end;
        end else begin
          TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := ItemList[Position];
          TCustomSynEdit(F.CurrentEditor).SetFocus;
        end;
      end
      else
      if (ItemList.Count = 0) then
        Cancel(Sender);
      EndUpdate;
      EndUndoBlock;
    end;
end;

procedure TSynCompletion.UTF8KeyPress(Sender: TObject; var Key: TUTF8Char);
var
  F: TSynBaseCompletionForm;
begin
  //debugln('TSynCompletion.UTF8KeyPress Key="',DbgStr(Key),'"');
  F := Sender as TSynBaseCompletionForm;
  if F.CurrentEditor <> nil then begin
    with F.CurrentEditor as TCustomSynEdit do begin
      CommandProcessor(ecChar, Key, nil);
    end;
  end;
end;

procedure TSynCompletion.SetEditor(const Value: TCustomSynEdit);
begin
  AddEditor(Value);
  Form.FCurrentEditor:=Value;
end;

procedure TSynCompletion.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (fEditors <> nil) then
    if (fEditors.IndexOf(AComponent) > -1) then
      RemoveEditor(AComponent as TCustomSynEdit);
end;

constructor TSynCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnUTF8KeyPress := @UTF8KeyPress;
  Form.OnKeyDelete := {$IFDEF FPC}@{$ENDIF}Backspace;
  Form.OnValidate := {$IFDEF FPC}@{$ENDIF}Validate;
  Form.OnCancel := {$IFDEF FPC}@{$ENDIF}Cancel;
  Form.OnPaint:=@OnFormPaint;
  FEndOfTokenChr := '()[].';
  fEditors := TList.Create;
  fEditstuffs := TList.Create;
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
end;

procedure TSynCompletion.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletion.EditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  p: TPoint;
  i: integer;
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  if Key=VK_UNKNOWN then exit;
  //debugln('TSynCompletion.EditorKeyDown A ',dbgs(Key));
  if (Form<>nil) and Form.Visible then begin
    // completion form is visible, but the synedit got a key
    // -> redirect to form
    Form.KeyDown(Key,Shift);
    //debugln('TSynCompletion.EditorKeyDown B ',dbgs(Key));
  end;

  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);
    if (Shift = ShortCutShift) and (Key = ShortCutKey) then
    with sender as TCustomSynEdit do begin
      if not ReadOnly and (Shift = ShortCutShift) and (Key = ShortCutKey) then begin
        p := ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight + 1));
        Form.CurrentEditor := Sender as TCustomSynEdit;
        Execute(GetPreviousToken(Sender as TCustomSynEdit), p.x, p.y);
        // eat it
        Key := VK_UNKNOWN;
      end;
    end;
    if Assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyDown) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyDown(sender, key, shift);
  end;
end;

function TSynCompletion.GetPreviousToken(FEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  if FEditor <> nil then begin
    s := FEditor.LineText;
    i := FEditor.CaretX - 1;
    if i > length(s) then
      result := ''
    else begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Begin
          dec(i);
        end;
      result := copy(s, i + 1, FEditor.CaretX - i - 1);
    end;
  end
  else
    result := '';
end;

procedure TSynCompletion.EditorKeyPress(Sender: TObject; var Key: char);
var
  i: integer;
begin
  //debugln(['TSynCompletion.EditorKeyPress ']);
  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyPress) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyPress(sender, key);
  end;
end;

procedure TSynCompletion.EditorUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  i: integer;
begin
  //debugln(['TSynCompletion.EditorUTF8KeyPress ']);
  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).UTF8KeyPress) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).UTF8KeyPress(sender,UTF8Key);
  end;
end;

destructor TSynCompletion.Destroy;
begin
  // necessary to get Notification called before fEditors is freed
  Form.Free;
  Form := nil;
  while fEditors.Count <> 0 do
    RemoveEditor(TCustomSynEdit(fEditors.last));
  FreeAndNil(fEditors);
  FreeAndNil(fEditstuffs);
  inherited;
end;

function TSynCompletion.GetFEditor: TCustomSynEdit;
begin
  Result:=TCustomSynEdit(Form.fCurrentEditor);
end;

procedure TSynCompletion.AddEditor(aEditor: TCustomSynEdit);
var
  p: PRecordUsedToStoreEachEditorVars;
begin
  if fEditors.IndexOf(aEditor) = -1 then begin
    fEditors.Add(aEditor);
    new(p);
    p^.KeyPress := aEditor.OnKeyPress;
    p^.UTF8KeyPress := aEditor.OnUTF8KeyPress;
    p^.KeyDown := aEditor.OnKeyDown;
    fEditstuffs.add(p);
    aEditor.FreeNotification(self);
    if not (csDesigning in ComponentState) then begin
      aEditor.OnKeyDown := {$IFDEF FPC}@{$ENDIF}EditorKeyDown;
      aEditor.OnKeyPress := {$IFDEF FPC}@{$ENDIF}EditorKeyPress;
      aEditor.OnUTF8KeyPress := {$IFDEF FPC}@{$ENDIF}EditorUTF8KeyPress;
    end;
  end;
end;

function TSynCompletion.EditorsCount: integer;
begin
  result := fEditors.count;
end;

function TSynCompletion.GetEditor(i: integer): TCustomSynEdit;
begin
  if (i < 0) or (i >= EditorsCount) then
    result := nil
  else
    result := TCustomSynEdit(fEditors[i]);
end;

function TSynCompletion.RemoveEditor(aEditor: TCustomSynEdit): boolean;
var
  i: integer;
  P : PRecordUsedToStoreEachEditorVars;
begin
  i := fEditors.Remove(aEditor);
  result := i <> -1;
  if result then
  begin
    p := fEditStuffs[i];  //shane
    dispose(p);           //shane
//    dispose(fEditstuffs[i]);  //commented out by shane
    fEditstuffs.delete(i);
  end;
end;

{ TSynAutoComplete }

procedure TSynAutoComplete.AddEditor(aEditor: TCustomSynEdit);
var
  p: PRecordUsedToStoreEachEditorVars;
begin
  if fEditors.IndexOf(aEditor) = -1 then begin
    fEditors.Add(aEditor);
    new(p);
    p^.KeyPress := aEditor.OnKeyPress;
    p^.KeyDown := aEditor.OnKeyDown;
    p^.UTF8KeyPress := aEditor.OnUTF8KeyPress;
    p^.NoNextKey := false;
    fEditstuffs.add(p);
    aEditor.FreeNotification(self);
    if not (csDesigning in ComponentState) then begin
      aEditor.OnKeyDown := {$IFDEF FPC}@{$ENDIF}EditorKeyDown;
      aEditor.OnKeyPress := {$IFDEF FPC}@{$ENDIF}EditorKeyPress;
      aEditor.OnUTF8KeyPress := {$IFDEF FPC}@{$ENDIF}EditorUTF8KeyPress;
    end;
  end;
end;

constructor TSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited;
  fEditors := TList.Create;
  fEditstuffs := TList.Create;
  FEndOfTokenChr := '()[].';
  fAutoCompleteList := TStringList.Create;
  fShortCut := Menus.ShortCut(Ord(' '), [ssShift]);
end;

procedure TSynAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynAutoComplete.destroy;
begin
  while feditors.count <> 0 do
{$IFDEF FPC}
    RemoveEditor(TCustomSynEdit(feditors.last));
{$ELSE}
    RemoveEditor(feditors.last);
{$ENDIF}
  FreeAndNil(fEditors);
  FreeAndNil(fEditstuffs);
  FreeAndNil(fAutoCompleteList);
  inherited;
end;

procedure TSynAutoComplete.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: integer;
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);

  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if (Shift = ShortCutShift) and (Key = ShortCutKey) and
      not (Sender as TCustomSynEdit).ReadOnly
    then begin
      Execute(GetPreviousToken(Sender as TCustomSynEdit), Sender as TCustomSynEdit);
      Key := 0;
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey := true;
    end;
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyDown) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyDown(sender, key, Shift);
  end;
end;

procedure TSynAutoComplete.EditorKeyPress(Sender: TObject; var Key: char);
var
  i: integer;
begin
  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey then begin
      key := #0;
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey := false;
    end;
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyPress) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).KeyPress(sender, key);
  end;
end;

procedure TSynAutoComplete.EditorUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  i: integer;
begin
  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey then begin
      UTF8Key := #0;
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey := false;
    end;
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).UTF8KeyPress) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).UTF8KeyPress(sender, UTF8Key);
  end;
end;

function TSynAutoComplete.EditorsCount: integer;
begin
  result := fEditors.count;
end;

procedure TSynAutoComplete.Execute(token: string; aEditor: TCustomSynEdit);
var
  Temp: string;
  i, j, prevspace: integer;
  StartOfBlock: tpoint;
begin
//Writeln('[TSynAutoComplete.Execute] Token is "',Token,'"');
  i := AutoCompleteList.IndexOf(token);
  if i <> -1 then begin
    TRecordUsedToStoreEachEditorVars(
                     fEditstuffs[fEditors.IndexOf(aEditor)]^).NoNextKey := true;
    for j := 1 to length(token) do
      aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    inc(i);
    StartOfBlock := Point(-1, -1);
    PrevSpace := 0;
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      for j := 0 to PrevSpace - 1 do
        aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      Temp := AutoCompleteList[i];
      PrevSpace := 0;
      while (length(temp) >= PrevSpace + 2) and (temp[PrevSpace + 2] <= ' ') do
        inc(PrevSpace);
      for j := 2 to length(Temp) do begin
        aEditor.CommandProcessor(ecChar, Temp[j], nil);
        if Temp[j] = '|' then
          StartOfBlock := aEditor.CaretXY
      end;
      inc(i);
      if (i < AutoCompleteList.Count) and
        (length(AutoCompleteList[i]) > 0) and
        (AutoCompleteList[i][1] = '=') then
        aEditor.CommandProcessor(ecLineBreak, ' ', nil);
    end;
    if (StartOfBlock.x <> -1) and (StartOfBlock.y <> -1) then begin
      aEditor.CaretXY := StartOfBlock;
      aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    end;
  end;
end;

function TSynAutoComplete.GetEdit: TCustomSynEdit;
begin
  if EditorsCount > 0 then
    result := Editors[0]
  else
    result := nil;
end;

function TSynAutoComplete.GetEditor(i: integer): TCustomSynEdit;
begin
  if (i < 0) or (i >= EditorsCount) then
    result := nil
  else
    result := TCustomSynEdit(fEditors[i]);
end;

function TSynAutoComplete.GetPreviousToken(aEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  if aEditor <> nil then begin
    s := aEditor.LineText;
    i := aEditor.CaretX - 1;
    if i > length(s) then
      result := ''
    else begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        dec(i);
      result := copy(s, i + 1, aEditor.CaretX - i - 1);
    end;
  end
  else
    result := '';
end;

procedure TSynAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (fEditors <> nil) then
    if fEditors.indexOf(AComponent) <> -1 then
      RemoveEditor(AComponent as TCustomSynEdit);
end;

function TSynAutoComplete.RemoveEditor(aEditor: TCustomSynEdit): boolean;
var
  i: integer;
  P : PRecordUsedToStoreEachEditorVars;

begin
  i := fEditors.Remove(aEditor);
  result := i <> -1;
  if result then begin
    p := fEditStuffs[i];  //shane
    dispose(p);           //shane
//    dispose(fEditstuffs[i]);  //commented out by shane
    fEditstuffs.delete(i);
  end;
end;

procedure TSynAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynAutoComplete.SetEdit(const Value: TCustomSynEdit);
begin
  AddEditor(Value);
end;

function TSynAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

{ TSynBaseCompletionHint }

procedure TSynBaseCompletionHint.Paint;
var
  R: TRect;
begin
  if FCompletionForm.Position = FIndex then
    Canvas.Brush.Color := FCompletionForm.ClSelect
  else
    Canvas.Brush.Color := Color;
    
  Canvas.Pen.Width := 1;
  R := ClientRect;
  Canvas.FillRect(R);
  DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT);
  
  Canvas.Font.Color := FCompletionForm.TextColor;
  
  if not Assigned(FCompletionForm.OnPaintItem)
  or not FCompletionForm.OnPaintItem(Caption, Canvas, 1, 1,
                                     FCompletionForm.Position = FIndex, FIndex)
  then begin
    Canvas.TextOut(2, 2, Caption);
  end;
end;

constructor TSynBaseCompletionHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Brush.Style := bsSolid;
  FCompletionForm := AOwner as TSynBaseCompletionForm;
  Color := FCompletionForm.BackgroundColor;
  AutoHide := False;
  Visible := False;
end;

function TSynBaseCompletionHint.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
var
  P: TPoint;
begin
  if Assigned(FCompletionForm.OnMeasureItem) then
  begin
    Result.TopLeft := Point(0, 0);
    P := FCompletionForm.OnMeasureItem(AHint, Canvas,
                                     FCompletionForm.Position = FIndex, FIndex);
    Result.Bottom := P.Y + 2;
    Result.Right := P.X + 4;
  end
  else
    Result := Rect(0, 0, Canvas.TextWidth(AHint) + 4, FCompletionForm.FontHeight);
end;

end.

