{
 ***************************************************************************
                              groupededit.pas
                              ---------------

 Provied a base class where a TCustomMaskedit (derived) control is
 grouped inside a TCustomControl with a TControl derived class.

 The objective of this baseclass is to forward all relevant methods and
 properties of TCustomMaskEdit.
 Grouping the controls inside a TCustomControl is done, so that the
 resulting control will properly align and anchor.

 Initial implementation 2016 by Bart Broersma


               Component Library Extended dialogs Controls


 ***************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************


}
unit GroupedEdit;

{$mode objfpc}{$H+}

//{$I lcl_defines.inc}

interface

uses
  Classes, SysUtils, LCLProc, LResources, LCLStrConsts, Types, LCLType, LMessages,
  Graphics, Controls, Forms, LazFileUtils, Dialogs, StdCtrls, Buttons, Menus,
  MaskEdit;

type

  { TGEEdit }

  TGEEdit = class(TCustomMaskedit)
  public
    function PerformTab(ForwardTab: boolean): boolean; override;
  end;

  TGEEditClass = class of TGEEdit;

  { TCustomAbstractGroupedEdit }

  TCustomAbstractGroupedEdit = class(TCustomControl)
  private
    FAutoSizeHeightIsEditHeight: Boolean;
    FBuddy: TControl;
    FDirectInput: Boolean;
    FEdit: TGEEdit;
    FFocusOnBuddyClick: Boolean;
    FInitialColor: TColor;
    FIsReadOnly: Boolean;
    FLayout: TLeftRight;
    FSpacing: Integer;
    //Forwarded events from FBuddy
    FOnBuddyClick: TNotifyEvent;
    //Forwarded events from FEdit
    FOnEditClick: TNotifyEvent;
    FOnEditChange: TNotifyEvent;
    FOnEditDblClick: TNotifyEvent;
    FOnEditDragDrop: TDragDropEvent;
    FOnEditDragOver: TDragOverEvent;
    FOnEditEditingDone: TNotifyEvent;
    FOnEditEndDrag: TEndDragEvent;
    FOnEditExit: TNotifyEvent;
    FOnEditKeyDown: TKeyEvent;
    FOnEditKeyPress: TKeyPressEvent;
    FOnEditEnter: TNotifyEvent;
    FOnEditKeyUp: TKeyEvent;
    FOnEditMouseDown: TMouseEvent;
    FOnEditMouseUp: TMouseEvent;
    FOnEditMouseEnter: TNotifyEvent;
    FOnEditMouseLeave: TNotifyEvent;
    FOnEditMouseMove: TMouseMoveEvent;
    FOnEditMouseWheel: TMouseWheelEvent;
    FOnEditMouseWheelUp: TMouseWheelUpDownEvent;
    FOnEditMouseWheelDown: TMouseWheelUpDownEvent;
    FOnEditStartDrag: TStartDragEvent;
    FOnEditUtf8KeyPress: TUtf8KeyPressEvent;

    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetAutoSelected: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretPos: TPoint;
    function GetCharCase: TEditCharCase;
    function GetColor: TColor;
    function GetDirectInput: Boolean;
    function GetEchoMode: TEchoMode;
    function GetEditMask: String;
    function GetEditText: string;
    function GetHideSelection: Boolean;
    function GetIsMasked: Boolean;
    function GetMaxLength: Integer;
    function GetModified: Boolean;
    function GetNumbersOnly: Boolean;
    function GetParentColor: Boolean;
    function GetPasswordChar: char;
    function GetReadOnly: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: String;
    function GetTabStop: Boolean;
    function GetTextHint: TTranslateString;

    procedure InternalOnBuddyClick(Sender: TObject);
    procedure InternalOnEditClick(Sender: TObject);
    procedure InternalOnEditDblClick(Sender: TObject);
    procedure InternalOnEditChange(Sender: TObject);
    procedure InternalOnEditDragDrop(Sender, Source: TObject; X,Y: Integer);
    procedure InternalOnEditDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure InternalOnEditEditingDone(Sender: TObject);
    procedure InternalOnEditEnter(Sender: TObject);
    procedure InternalOnEditExit(Sender: TObject);
    procedure InternalOnEditEndDrag(Sender, Target: TObject; X,Y: Integer);
    procedure InternalOnEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure InternalOnEditKeyPress(Sender: TObject; var Key: char);
    procedure InternalOnEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure InternalOnEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InternalOnEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InternalOnEditMouseEnter(Sender: TObject);
    procedure InternalOnEditMouseLeave(Sender: TObject);
    procedure InternalOnEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure InternalOnEditMouseWheel(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure InternalOnEditMouseWheelUp(Sender: TObject;
          Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure InternalOnEditMouseWheelDown(Sender: TObject;
          Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure InternalOnEditUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure InternalOnEditStartDrag(Sender: TObject; var DragObject: TDragObject);

    procedure SetAlignment(AValue: TAlignment);
    procedure SetAutoSelect(AValue: Boolean);
    procedure SetAutoSelected(AValue: Boolean);
    procedure SetCaretPos(AValue: TPoint);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetEchoMode(AValue: TEchoMode);
    procedure SetEditMask(AValue: String);
    procedure SetEditText(AValue: string);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetLayout(AValue: TLeftRight);
    procedure SetMaxLength(AValue: Integer);
    procedure SetModified(AValue: Boolean);
    procedure SetNumbersOnly(AValue: Boolean);
    procedure SetParentColor(AValue: Boolean);
    procedure SetPasswordChar(AValue: char);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSelLength(AValue: Integer);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelText(AValue: String);
    procedure SetSpacing(const Value: integer);
    procedure SetTabStop(AValue: Boolean);
    procedure SetTextHint(AValue: TTranslateString);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                WithThemeSpace: Boolean); override;
    function CreateBuddy: TControl; virtual;
    function CreateEditor: TGEEdit; virtual;

    procedure FocusAndMaybeSelectAll;
    function GetEditorClassType: TGEEditClass; virtual;
    function GetBuddyClassType: TControlClass; virtual; abstract;
    class function GetControlClassDefaultSize: TSize; override;
    procedure SetDirectInput(AValue: Boolean); virtual;
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;

    function GetEditPopupMenu: TPopupMenu;
    function GetBuddyCaption: TCaption;
    function GetBuddyCursor: TCursor;
    function GetBuddyHint: TTranslateString;
    function GetBuddyWidth: Integer;
    function GetBuddyVisible: Boolean;
    procedure SetBuddyCaption(AValue: TCaption);
    procedure SetBuddyCursor(AValue: TCursor);
    procedure SetBuddyHint(AValue: TTranslateString);
    procedure SetBuddyWidth(AValue: Integer);
    procedure SetBuddyVisible(AValue: Boolean);

    procedure BuddyClick; virtual;

    procedure DoEnter; override;

    procedure EditChange; virtual;
    procedure EditClick; virtual;
    procedure EditDblClick; virtual;
    procedure EditDragDrop(Source: TObject; X,Y: Integer);  virtual;
    procedure EditDragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); virtual;
    procedure EditEditingDone; virtual;
    procedure EditEndDrag(Target: TObject; X,Y: Integer); virtual;
    procedure EditEnter; virtual;
    procedure EditExit; virtual;
    procedure EditKeyDown(var Key: word; Shift: TShiftState); virtual;
    procedure EditKeyPress( var Key: char); virtual;
    procedure EditKeyUp(var Key: word; Shift: TShiftState); virtual;
    procedure EditMouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseEnter; virtual;
    procedure EditMouseLeave; virtual;
    procedure EditMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure EditMouseWheel(Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure EditMouseWheelUp(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure EditMouseWheelDown(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure EditUtf8KeyPress(var UTF8Key: TUTF8Char); virtual;
    procedure EditStartDrag(var DragObject: TDragObject); virtual;

    procedure UpdateSpacing;
    procedure CheckCursor;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    function  EditCanModify: Boolean; virtual;
    procedure GetSel(out _SelStart: Integer; out _SelStop: Integer);
    function GetSpacing: Integer; virtual;
    procedure SetSel(const _SelStart: Integer; _SelStop: Integer);
    procedure Loaded; override;
    procedure Reset; virtual;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure SetColor(AValue: TColor); reintroduce;
    procedure SetCursor(AValue: TCursor); override;
    procedure ShouldAutoAdjust(var AWidth, AHeight: Boolean); override;

    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property AutoSelected: Boolean read GetAutoSelected write SetAutoSelected;
    property Buddy: TControl read FBuddy;
    property BuddyCaption: TCaption read GetBuddyCaption write SetBuddyCaption;
    property BuddyCursor: TCursor read GetBuddyCursor write SetBuddyCursor default crDefault;
    property BuddyHint: TTranslateString read GetBuddyHint write SetBuddyHint;
    property BuddyWidth: Integer read GetBuddyWidth write SetBuddyWidth;
    property DirectInput : Boolean read GetDirectInput write SetDirectInput default True;
    property BaseEditor: TGEEdit read FEdit;
    property EditMask: String read GetEditMask write SetEditMask;
    property EditText: string read GetEditText write SetEditText;
    property FocusOnBuddyClick: Boolean read FFocusOnBuddyClick write FFocusOnBuddyClick default False;
    property IsMasked: Boolean read GetIsMasked;
    property Layout: TLeftRight read FLayout write SetLayout default taLeftJustify;
    property Spacing: Integer read GetSpacing write SetSpacing default 0;

    //Derived classes should implement there own (readonly) Edit property, so that it will have the correct classtype
    //Derived classes should implement a (readonly) property that returns the buddy with the correct classtype (e.g. TSpeedButton)

    property OnBuddyClick: TNotifyEvent read FOnBuddyClick write FOnBuddyClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocus; override;
    function Focused: Boolean; override;
    procedure Clear;
    procedure ClearSelection; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure SelectAll;
    procedure Undo; virtual;
    procedure ValidateEdit; virtual;

    property Autosize default True;
    property AutoSizeHeightIsEditHeight: Boolean read FAutoSizeHeightIsEditHeight write FAutoSizeHeightIsEditHeight default True;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property Color: TColor read GetColor write SetColor stored True default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property ParentColor: Boolean read GetParentColor write SetParentColor default False;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode default emNormal;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default False;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property Modified: Boolean read GetModified write SetModified;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly default False;
    property PasswordChar: char read GetPasswordChar write SetPasswordChar;
    property PopupMenu: TPopupMenu read GetEditPopupMenu write SetPopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
    property Text;
    property TextHint: TTranslateString read GetTextHint write SetTextHint;

    property OnChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnDblClick: TNotifyEvent read FOnEditDblClick write FOnEditDblClick;
    property OnDragDrop: TDragDropEvent read FOnEditDragDrop write FOnEditDragDrop;
    property OnDragOver: TDragOverEvent read FOnEditDragOver write FOnEditDragOver;
    property OnEditingDone: TNotifyEvent read FOnEditEditingDone write FOnEditEditingDone;
    property OnEndDrag: TEndDragEvent read FOnEditEndDrag write FOnEditEndDrag;
    property OnEnter: TNotifyEvent read FOnEditEnter write FOnEditEnter;
    property OnExit: TNotifyEvent read FOnEditExit write FOnEditExit;
    property OnMouseDown: TMouseEvent read FOnEditMouseDown write FOnEditMouseDown;
    property OnKeyPress: TKeyPressEvent read FOnEditKeyPress write FOnEditKeyPress;
    property OnKeyDown: TKeyEvent read FOnEditKeyDown write FOnEditKeyDown;
    property OnKeyUp: TKeyEvent read FOnEditKeyUp write FOnEditKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnEditMouseEnter write FOnEditMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnEditMouseLeave write FOnEditMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnEditMouseMove write FOnEditMouseMove;
    property OnMouseWheel: TMouseWheelEvent read FOnEditMouseWheel write FOnEditMouseWheel;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnEditMouseWheelUp write FOnEditMouseWheelUp;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnEditMouseWheelDown write FOnEditMouseWheelDown;
    property OnMouseUp: TMouseEvent read FOnEditMouseUp write FOnEditMouseUp;
    property OnStartDrag: TStartDragEvent read FOnEditStartDrag write FOnEditStartDrag;
    property OnUtf8KeyPress: TUtf8KeyPressEvent read FOnEditUtf8KeyPress write FOnEditUtf8KeyPress;

  end;

 { TAbstractGroupedEdit }

  TAbstractGroupedEdit = class(TCustomAbstractGroupedEdit)
  public
    property AutoSelected;
  published
    property NumbersOnly;
    property Action;
    property AutoSelect;
    property AutoSizeHeightIsEditHeight;
    property AutoSize default True;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property BuddyCaption;
    property BuddyCursor;
    property BuddyHint;
    property BuddyWidth;
    property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DirectInput;
    property EchoMode;
    property Enabled;
    property FocusOnBuddyClick;
    property Font;
//    property HideSelection;
    property Hint;
    property Layout;
    property MaxLength;
    //property OnBuddyClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnContextPopup;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
  end;


implementation


{ TGEEdit }


function TGEEdit.PerformTab(ForwardTab: boolean): boolean;
begin
  //if not Forward then inherited PerFormTab will set focus to the owning
  //TCustomAbstractGroupedEdit, which immediately transfers the focus back to the TGEEdit
  //so let TCustomAbstractGroupedEdit do the Performtab in this case
  if ForwardTab then
    Result := inherited PerformTab(ForwardTab)
  else
  begin
    if Owner is TCustomAbstractGroupedEdit then
      Result :=  TCustomAbstractGroupedEdit(Owner).PerformTab(ForwardTab)
    else
      Result := False;
  end;
end;

{ TCustomAbstractGroupedEdit }

procedure TCustomAbstractGroupedEdit.InternalOnBuddyClick(Sender: TObject);
begin
  BuddyClick;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    EditChange;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditClick(Sender: TObject);
begin
  EditClick;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditDblClick(Sender: TObject);
begin
  EditDblClick;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  EditDragDrop(Source, X, Y);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  EditDragOver(Source, X, Y, State, Accept);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditEditingDone(Sender: TObject);
begin
  EditEditingDone;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  EditEndDrag(Target, X, Y);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditEnter(Sender: TObject);
begin
  EditEnter;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditExit(Sender: TObject);
begin
  EditExit;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  EditKeyDown(Key, Shift);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditKeyPress(Sender: TObject; var Key: char);
begin
  EditKeyPress(Key);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  EditKeyUp(Key, Shift);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EditMouseDown(Button, Shift, X, Y);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EditMouseUp(Button, Shift, X, Y);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseEnter(Sender: TObject);
begin
  EditMouseEnter;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseLeave(Sender: TObject);
begin
  EditMouseLeave;
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  EditMouseMove(Shift, X, Y);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  EditMouseWheel(Shift, WheelDelta, MousePos, Handled);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  EditMouseWheelUp(Shift, MousePos, Handled);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  EditMouseWheelDown(Shift, MousePos, Handled);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  EditUtf8KeyPress(UTF8Key);
end;

procedure TCustomAbstractGroupedEdit.InternalOnEditStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  EditStartDrag(DragObject);
end;

function TCustomAbstractGroupedEdit.GetCanUndo: Boolean;
begin
  Result := FEdit.CanUndo;
end;

function TCustomAbstractGroupedEdit.GetCaretPos: TPoint;
begin
  Result := FEdit.CaretPos;
end;

function TCustomAbstractGroupedEdit.GetEditPopupMenu: TPopupMenu;
begin
  Result := FEdit.PopupMenu;
end;

procedure TCustomAbstractGroupedEdit.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  EditPreferredHeight: integer;
begin
  EditPreferredHeight := 0;
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  FEdit.CalculatePreferredSize(PreferredWidth, EditPreferredHeight, WithThemeSpace);

  if FAutoSizeHeightIsEditHeight then
    PreferredHeight := EditPreferredHeight;
  PreferredWidth := 0;
end;

function TCustomAbstractGroupedEdit.GetReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

function TCustomAbstractGroupedEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

function TCustomAbstractGroupedEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

function TCustomAbstractGroupedEdit.GetSelText: String;
begin
  Result := FEdit.SelText;
end;

function TCustomAbstractGroupedEdit.GetSpacing: Integer;
begin
  Result := FSpacing;
end;

function TCustomAbstractGroupedEdit.GetTabStop: Boolean;
begin
  Result := FEdit.TabStop;
end;

function TCustomAbstractGroupedEdit.RealGetText: TCaption;
begin
  Result := FEdit.Text;
end;

function TCustomAbstractGroupedEdit.GetTextHint: TTranslateString;
begin
  Result := FEdit.TextHint;
end;

procedure TCustomAbstractGroupedEdit.FocusAndMaybeSelectAll;
begin
  FEdit.SetFocus;
  if AutoSelect then
    FEdit.SelectAll
  else
    FEdit.SelStart := MaxInt;
end;

function TCustomAbstractGroupedEdit.GetAlignment: TAlignment;
begin
  Result := FEdit.Alignment;
end;

function TCustomAbstractGroupedEdit.GetAutoSelect: Boolean;
begin
  Result := FEdit.AutoSelect;
end;

function TCustomAbstractGroupedEdit.GetAutoSelected: Boolean;
begin
  Result := FEdit.AutoSelected;
end;

function TCustomAbstractGroupedEdit.GetBuddyCaption: TCaption;
begin
  Result := FBuddy.Caption;
end;

function TCustomAbstractGroupedEdit.GetBuddyCursor: TCursor;
begin
  Result := FBuddy.Cursor;
end;

function TCustomAbstractGroupedEdit.GetBuddyHint: TTranslateString;
begin
  Result := FBuddy.Hint;
end;

function TCustomAbstractGroupedEdit.GetBuddyWidth: Integer;
begin
  Result := FBuddy.Width;
end;

function TCustomAbstractGroupedEdit.GetBuddyVisible: Boolean;
begin
  Result := FBuddy.Visible;
end;

procedure TCustomAbstractGroupedEdit.SetBuddyHint(AValue: TTranslateString);
begin
  FBuddy.Hint := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetBuddyWidth(AValue: Integer);
begin
  FBuddy.Width := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetBuddyVisible(AValue: Boolean);
begin
  FBuddy.Visible := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetCaretPos(AValue: TPoint);
begin
  FEdit.CaretPos := AValue;
end;

function TCustomAbstractGroupedEdit.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;

function TCustomAbstractGroupedEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TCustomAbstractGroupedEdit.GetEchoMode: TEchoMode;
begin
  Result := FEdit.EchoMode;
end;

function TCustomAbstractGroupedEdit.GetEditMask: String;
begin
  Result := FEdit.EditMask
end;

function TCustomAbstractGroupedEdit.GetEditText: string;
begin
  Result := FEdit.EditText;
end;

function TCustomAbstractGroupedEdit.GetColor: TColor;
begin
  Result := FEdit.Color;
end;

function TCustomAbstractGroupedEdit.GetHideSelection: Boolean;
begin
  Result := FEdit.HideSelection;
end;

function TCustomAbstractGroupedEdit.GetIsMasked: Boolean;
begin
  Result := FEdit.IsMasked;
end;

function TCustomAbstractGroupedEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

function TCustomAbstractGroupedEdit.GetModified: Boolean;
begin
  Result := FEdit.Modified;
end;

function TCustomAbstractGroupedEdit.GetNumbersOnly: Boolean;
begin
  Result := FEdit.NumbersOnly;
end;

function TCustomAbstractGroupedEdit.GetParentColor: Boolean;
begin
  Result := FEdit.ParentColor;
end;

function TCustomAbstractGroupedEdit.GetPasswordChar: char;
begin
  Result := FEdit.PasswordChar;
end;

procedure TCustomAbstractGroupedEdit.SetAlignment(AValue: TAlignment);
begin
  FEdit.Alignment := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetAutoSelect(AValue: Boolean);
begin
  FEdit.AutoSelect := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetAutoSelected(AValue: Boolean);
begin
  FEdit.AutoSelected := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetAutoSize(AValue: Boolean);
begin
  if AutoSize = AValue then
    Exit;
  inherited SetAutosize(AValue);
  //FButton.AutoSize := AValue;
  FEdit.AutoSize := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetBuddyCaption(AValue: TCaption);
begin
  FBuddy.Caption := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetBuddyCursor(AValue: TCursor);
begin
  FBuddy.Cursor := AValue;
end;

class function TCustomAbstractGroupedEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 80 + 23; //as TCustomEdit + TCustomSpeedButton
  Result.CY := 23;  //as TCustomEdit
end;

procedure TCustomAbstractGroupedEdit.SetCharCase(AValue: TEditCharCase);
begin
  FEdit.CharCase := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetDirectInput(AValue: Boolean);
begin
  FDirectInput := AValue;
  FEdit.ReadOnly := ((not FDirectInput) or (FIsReadOnly));
end;

procedure TCustomAbstractGroupedEdit.SetEchoMode(AValue: TEchoMode);
begin
  FEdit.EchoMode := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetEditMask(AValue: String);
begin
  FEdit.EditMask := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetEditText(AValue: string);
begin
  FEdit.EditText := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetColor(AValue: TColor);
begin
  if (csLoading in ComponentState) then
    FInitialColor := AValue
  else
    FEdit.Color := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetCursor(AValue: TCursor);
begin
  if Cursor = AValue then
    Exit;
  inherited SetCursor(AValue);
  FEdit.Cursor := AValue;
end;

procedure TCustomAbstractGroupedEdit.ShouldAutoAdjust(var AWidth,
  AHeight: Boolean);
begin
  AWidth := True;
  AHeight := not AutoSize;
end;

procedure TCustomAbstractGroupedEdit.SetFocus;
begin
  FEdit.SetFocus;
end;

procedure TCustomAbstractGroupedEdit.SetHideSelection(AValue: Boolean);
begin
  FEdit.HideSelection := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetLayout(AValue: TLeftRight);
begin
  if (FLayout = AValue) then
    Exit;
  FLayout := AValue;
  DisableAlign;
  UpdateSpacing;
  case FLayout of
    taLeftJustify : FBuddy.Align := alRight;
    taRightJustify: FBuddy.Align := alLeft;
  end;
  EnableAlign;
end;

procedure TCustomAbstractGroupedEdit.SetMaxLength(AValue: Integer);
begin
  FEdit.MaxLength := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetModified(AValue: Boolean);
begin
  FEdit.Modified := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetParentColor(AValue: Boolean);
begin
  FEdit.ParentColor := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetPasswordChar(AValue: char);
begin
  FEdit.PasswordChar := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetPopupMenu(AValue: TPopupMenu);
begin
  FEdit.PopupMenu := AValue;
end;

procedure TCustomAbstractGroupedEdit.RealSetText(const AValue: TCaption);
begin
  FEdit.Text := AValue;
end;


procedure TCustomAbstractGroupedEdit.BuddyClick;
begin
  //debugln(['TCustomAbstractGroupedEdit.BuddyClick: Assigned(FOnBuddyClick)=',Assigned(FOnBuddyClick)]);
  if ReadOnly then
    Exit;
  if Assigned(FOnBuddyClick) then
    FOnBuddyClick(Self);
  //derived controls that override BuddyClick typically run a dialog after calling inherited,
  //in that case selecting the text now does not make sense at all (and looks silly)
  //it's up to the derived control to implement this focus and select if wanted
  if TMethod(@Self.BuddyClick).Code = Pointer(@TCustomAbstractGroupedEdit.BuddyClick) then
  begin
    if FocusOnBuddyClick then FocusAndMaybeSelectAll;
  end;
end;

procedure TCustomAbstractGroupedEdit.DoEnter;
begin
  inherited DoEnter;
  if not (csDesigning in ComponentState) then
    FEdit.SetFocus;
end;

procedure TCustomAbstractGroupedEdit.EditChange;
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

procedure TCustomAbstractGroupedEdit.EditClick;
begin
  if Assigned(FOnEditClick) then FOnEditClick(Self);
end;

procedure TCustomAbstractGroupedEdit.EditDblClick;
begin
  if Assigned(FOnEditDblClick) then FOnEditDblClick(Self);
end;

procedure TCustomAbstractGroupedEdit.EditDragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(FOnEditDragDrop) then FOnEditDragDrop(Self, Source, X, Y);
end;

procedure TCustomAbstractGroupedEdit.EditDragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnEditDragOver) then FOnEditDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TCustomAbstractGroupedEdit.EditEditingDone;
begin
  if Assigned(FOnEditEditingDone) then FOnEditEditingDone(Self);
end;

procedure TCustomAbstractGroupedEdit.EditEndDrag(Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEditEndDrag) then FOnEditEndDrag(Self, Target, X, Y);
end;

procedure TCustomAbstractGroupedEdit.EditEnter;
begin
  if Assigned(FOnEditEnter) then FOnEditEnter(Self);
end;

procedure TCustomAbstractGroupedEdit.EditExit;
begin
  if Assigned(FOnEditExit) then FOnEditExit(Self);
end;

procedure TCustomAbstractGroupedEdit.EditKeyDown(var Key: word; Shift: TShiftState);
begin
  if Assigned(FOnEditKeyDown) then FOnEditKeyDown(Self, Key, Shift);
end;

procedure TCustomAbstractGroupedEdit.EditKeyPress(var Key: char);
begin
  if Assigned(FOnEditKeyPress) then FOnEditKeyPress(Self, Key);
end;

procedure TCustomAbstractGroupedEdit.EditKeyUp(var Key: word; Shift: TShiftState);
begin
  if Assigned(FOnEditKeyUp) then FOnEditKeyUp(Self, Key, Shift);
end;

procedure TCustomAbstractGroupedEdit.EditMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseDown) then FOnEditMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomAbstractGroupedEdit.EditMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseUp) then FOnEditMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCustomAbstractGroupedEdit.EditMouseEnter;
begin
  if Assigned(FOnEditMouseEnter) then FOnEditMouseEnter(Self);
end;

procedure TCustomAbstractGroupedEdit.EditMouseLeave;
begin
  if Assigned(FOnEditMouseLeave) then FOnEditMouseLeave(Self);
end;

procedure TCustomAbstractGroupedEdit.EditMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseMove) then FOnEditMouseMove(Self, Shift, X, Y);
end;

procedure TCustomAbstractGroupedEdit.EditMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOnEditMouseWheel) then FOnEditMouseWheel(Self, Shift, WheelDelta, MousePos, Handled);
end;

procedure TCustomAbstractGroupedEdit.EditMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOnEditMouseWheelUp) then FOnEditMouseWheelUp(Self, Shift, MousePos, Handled);
end;

procedure TCustomAbstractGroupedEdit.EditMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOnEditMouseWheelDown) then FOnEditMouseWheelDown(Self, Shift, MousePos, Handled);
end;

procedure TCustomAbstractGroupedEdit.EditUtf8KeyPress(var UTF8Key: TUTF8Char);
begin
  if Assigned(FOnEditUtf8KeyPress) then FOnEditUtf8KeyPress(Self, Utf8Key);
end;

procedure TCustomAbstractGroupedEdit.EditStartDrag(var DragObject: TDragObject);
begin
  if Assigned(FOnEditStartDrag) then FOnEditStartDrag(Self, DragObject);
end;

procedure TCustomAbstractGroupedEdit.CheckCursor;
begin
  FEdit.CheckCursor;
end;

procedure TCustomAbstractGroupedEdit.CMParentColorChanged(var Message: TLMessage);
begin
  if inherited ParentColor then
  begin
    inherited SetColor(Parent.Color);
    inherited ParentColor := True;
  end;
end;

function TCustomAbstractGroupedEdit.EditCanModify: Boolean;
begin
  Result := FEdit.EditCanModify;
end;

procedure TCustomAbstractGroupedEdit.GetSel(out _SelStart: Integer; out _SelStop: Integer);
begin
  FEdit.GetSel(_SelStart, _SelStop);
end;

procedure TCustomAbstractGroupedEdit.SetSel(const _SelStart: Integer; _SelStop: Integer);
begin
  FEdit.SetSel(_SelStart, _SelStop);
end;

procedure TCustomAbstractGroupedEdit.Loaded;
begin
  inherited Loaded;
  {
    inherited Loaded sends a CM_PARENTFONTCHANGED message, which then
    also sets FEdit's color, which is undesired.
  }
  UpdateSpacing;
  if GetColor <> FInitialColor then SetColor(FInitialColor);
end;


procedure TCustomAbstractGroupedEdit.Reset;
begin
  FEdit.Reset;
end;


procedure TCustomAbstractGroupedEdit.SetReadOnly(AValue: Boolean);
begin
  FIsReadOnly := AValue;
  FEdit.ReadOnly := AValue or (not DirectInput);
  FBuddy.Enabled := not FIsReadOnly and Enabled;
end;

procedure TCustomAbstractGroupedEdit.SetSelLength(AValue: Integer);
begin
  FEdit.SelLength := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetSelStart(AValue: Integer);
begin
  FEdit.SelStart := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetSelText(AValue: String);
begin
  FEdit.SelText := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetSpacing(const Value: integer);
begin
  if (Value = FSpacing) then Exit;
  FSpacing := Value;
  if not (csLoading in ComponentState) then UpdateSpacing;
end;

procedure TCustomAbstractGroupedEdit.SetTabStop(AValue: Boolean);
begin
  FEdit.TabStop := AValue;
end;

procedure TCustomAbstractGroupedEdit.SetTextHint(AValue: TTranslateString);
begin
  FEdit.TextHint := AValue;
end;

procedure TCustomAbstractGroupedEdit.UpdateSpacing;
begin
  if (FBuddy=nil) or not FBuddy.Visible then
  begin
    FEdit.BorderSpacing.Right := 0;
    FEdit.BorderSpacing.Left := 0;
  end
  else
  if (FLayout = taLeftJustify) then
  begin
    FEdit.BorderSpacing.Right := FSpacing;
    FEdit.BorderSpacing.Left := 0;
  end
  else
  begin
    FEdit.BorderSpacing.Right := 0;
    FEdit.BorderSpacing.Left := FSpacing;
  end;
end;


function TCustomAbstractGroupedEdit.CreateBuddy: TControl;
begin
  Result := GetBuddyClassType.Create(Self);
end;

function TCustomAbstractGroupedEdit.CreateEditor: TGEEdit;
begin
  Result := GetEditorClassType.Create(Self);
end;

function TCustomAbstractGroupedEdit.GetEditorClassType: TGEEditClass;
begin
  Result := TGEEdit;
end;

constructor TCustomAbstractGroupedEdit.Create(AOwner: TComponent);
var
  B: TBitmap;
begin
  AutoSizeHeightIsEditHeight := True;
  FEdit := CreateEditor;
  FBuddy := CreateBuddy;
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoFocus];
  FEdit.ParentColor := False;
  FInitialColor := {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
  BorderStyle := bsNone;
  FLayout := taLeftjustify;
  FDirectInput := True;
  FIsReadOnly := False;
  TabStop := True;
  inherited TabStop := False;
  FocusOnBuddyClick := False;
  FSpacing := 0;
  SetInitialBounds(0, 0, GetControlClassDefaultSize.CX, GetControlClassDefaultSize.CY);

  FBuddy.Align := alRight;
  FBuddy.OnClick := @InternalOnBuddyClick;
  FBuddy.Parent := Self;

  with FEdit do
  begin
    Align := alClient;
    ParentColor := False;
    ParentFont := True;

    AutoSelect := True;
    Alignment := taLeftJustify;
    ReadOnly := False;

    OnChange := @InternalOnEditChange;
    OnClick := @InternalOnEditClick;
    OnDblClick := @InternalOnEditDblClick;
    OnDragDrop := @InternalOnEditDragDrop;
    OnDragOver := @InternalOnEditDragOver;
    OnEditingDone := @InternalOnEditEditingDone;
    OnEndDrag := @InternalOnEditEndDrag;
    OnExit := @InternalOnEditExit;
    OnEnter := @InternalOnEditEnter;
    OnKeyDown := @InternalOnEditKeyDown;
    OnKeyPress := @InternalOnEditKeyPress;
    OnKeyUp := @InternalOnEditKeyUp;
    OnMouseDown := @InternalOnEditMouseDown;
    OnMouseUp := @InternalOnEditMouseUp;
    OnMouseEnter := @InternalOnEditMouseEnter;
    OnMouseLeave := @InternalOnEditMouseLeave;
    OnMouseMove := @InternalOnEditMouseMove;
    OnMouseWheel := @InternalOnEditMouseWheel;
    OnMouseWheelUp := @InternalOnEditMouseWheelUp;
    OnMouseWheelDown := @InternalOnEditMouseWheelDown;
    OnStartDrag := @InternalOnEditStartDrag;
    OnUtf8KeyPress := @InternalOnEditUtf8KeyPress;
  end;
  FEdit.Parent := Self;
  AutoSize := True;
  Color := {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
  inherited ParentColor := True; //don't want to see the container if Parent's color changes
end;

destructor TCustomAbstractGroupedEdit.Destroy;
begin
  inherited Destroy;
end;

function TCustomAbstractGroupedEdit.Focused: Boolean;
begin
  Result := FEdit.Focused;
end;

procedure TCustomAbstractGroupedEdit.Clear;
begin
  FEdit.Clear;
end;

procedure TCustomAbstractGroupedEdit.ClearSelection;
begin
  FEdit.ClearSelection;
end;

procedure TCustomAbstractGroupedEdit.CopyToClipboard;
begin
  FEdit.CopyToClipboard;
end;

procedure TCustomAbstractGroupedEdit.CutToClipboard;
begin
  FEdit.CutToClipBoard;
end;

procedure TCustomAbstractGroupedEdit.PasteFromClipboard;
begin
  FEdit.PasteFromClipBoard;
end;

procedure TCustomAbstractGroupedEdit.SelectAll;
begin
  FEdit.SelectAll;
end;


procedure TCustomAbstractGroupedEdit.Undo;
begin
  FEdit.Undo;
end;

procedure TCustomAbstractGroupedEdit.ValidateEdit;
begin
  FEdit.ValidateEdit;
end;

initialization
  RegisterPropertyToSkip(TCustomAbstractGroupedEdit, 'TextHintFontColor','Used in a previous version of Lazarus','');
  RegisterPropertyToSkip(TCustomAbstractGroupedEdit, 'TextHintFontStyle','Used in a previous version of Lazarus','');
end.
