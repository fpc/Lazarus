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
{$IFDEF SYN_LAZARUS}
  LCLLinux, LMessages,
  LCLType, GraphType,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, Messages, Graphics, Forms, Controls, StdCtrls, Menus,
  SysUtils, SynEditTypes, SynEditKeyCmds, SynEditHighlighter,
  {$IFDEF SYN_LAZARUS}SynEditTextBuffer,{$ENDIF} SynEdit;

type
  TSynBaseCompletionPaintItem =
    function(
      {$IFDEF SYN_LAZARUS}const {$ENDIF}AKey: string; ACanvas: TCanvas;
      X, Y: integer
      {$IFDEF SYN_LAZARUS}; Selected: boolean; Index: integer{$ENDIF}
      ): boolean of object;
  TCodeCompletionEvent = procedure(var Value: string; Shift: TShiftState)
    of object;
  TValidateEvent = procedure(Sender: TObject; Shift: TShiftState) of object;
  TSynBaseCompletionSearchPosition = procedure(var Position :integer) of object;

  TSynBaseCompletionForm = class(TForm)
  protected
    FCurrentString: string;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDelete: TNotifyEvent;
    FOnPaintItem: TSynBaseCompletionPaintItem;
    FItemList: TStrings;
    FPosition: Integer;
    FNbLinesInWindow: Integer;
    FFontHeight: integer;
    Scroll: TScrollBar;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FClSelect: TColor;
    FAnsi: boolean;
    {$IFDEF SYN_LAZARUS}
    FOnSearchPosition:TSynBaseCompletionSearchPosition;
    FTextColor: TColor;
    FTextSelectedColor: TColor;
    {$ENDIF}
    procedure SetCurrentString(const Value: string);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Paint; override;
    procedure ScrollGetFocus(Sender: TObject);
    procedure Deactivate; override;
    procedure SelectPrec;
    procedure SelectNext;
    procedure ScrollChange(Sender: TObject);
    procedure SetItemList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure StringListChange(Sender: TObject);
    {$IFDEF SYN_LAZARUS}
    procedure SetFontHeight(NewFontHeight :integer);
    {$ENDIF}
  private
    Bitmap: TBitmap; // used for drawing
    fCurrentEditor: TComponent;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property CurrentString: string read FCurrentString write SetCurrentString;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDelete: TNotifyEvent read FOnKeyDelete write FOnKeyDelete;
    property OnPaintItem: TSynBaseCompletionPaintItem read FOnPaintItem
      write FOnPaintItem;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property ItemList: TStrings read FItemList write SetItemList;
    property Position: Integer read FPosition write SetPosition;
    property NbLinesInWindow: Integer read FNbLinesInWindow
      write SetNbLinesInWindow;
    property ClSelect: TColor read FClSelect write FClSelect;
    property ffAnsi: boolean read fansi write fansi;
    property CurrentEditor: TComponent read fCurrentEditor write fCurrentEditor;
    {$IFDEF SYN_LAZARUS}
    property FontHeight:integer read FFontHeight write SetFontHeight;
    property OnSearchPosition:TSynBaseCompletionSearchPosition
      read FOnSearchPosition write FOnSearchPosition;
    property TextColor: TColor read FTextColor write FTextColor;
    property TextSelectedColor: TColor
      read FTextSelectedColor write FTextSelectedColor;
    {$ENDIF}
  end;

  TSynBaseCompletion = class(TComponent)
  private
    Form: TSynBaseCompletionForm;
    FOnExecute: TNotifyEvent;
    FWidth: Integer;
    RFAnsi: boolean;
    SFAnsi: boolean;
    function GetClSelect: TColor;
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
    procedure SetItemList(const Value: TStrings);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionPaintItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    function GetOnKeyDelete: TNotifyEvent;
    procedure SetOnKeyDelete(const Value: TNotifyEvent);
    procedure SetWidth(Value: Integer);
    {$IFDEF SYN_LAZARUS}
    function GetFontHeight:integer;
    procedure SetFontHeight(NewFontHeight :integer);
    function GetOnSearchPosition:TSynBaseCompletionSearchPosition;
    procedure SetOnSearchPosition(NewValue :TSynBaseCompletionSearchPosition);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer);
    procedure Deactivate;
    {$IFDEF SYN_LAZARUS}
    function IsActive: boolean;
    function TheForm: TSynBaseCompletionForm;
    {$ENDIF}
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyDelete: TNotifyEvent read GetOnKeyDelete write SetOnKeyDelete;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
  published
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnPaintItem: TSynBaseCompletionPaintItem
      read GetOnPaintItem write SetOnPaintItem;
    property ItemList: TStrings read GetItemList write SetItemList;
    property Position: Integer read GetPosition write SetPosition;
    property NbLinesInWindow: Integer read GetNbLinesInWindow
      write SetNbLinesInWindow;
    {$IFDEF SYN_LAZARUS}
    property FontHeight: integer read GetFontHeight write SetFontHeight;
    property OnSearchPosition: TSynBaseCompletionSearchPosition
      read GetOnSearchPosition write SetOnSearchPosition;
    {$ENDIF}
    property ClSelect: TColor read GetClSelect write SetClSelect;
    property AnsiStrings: boolean read SFAnsi write RFAnsi;
    property Width: Integer read FWidth write SetWidth;
  end;

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
    procedure Validate(Sender: TObject; Shift: TShiftState);
    procedure KeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    function GetPreviousToken(FEditor: TCustomSynEdit): string;
    function GetFEditor: TCustomSynEdit;
    function GetEditor(i: integer): TCustomSynEdit;
  protected
    procedure OnFormPaint(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
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
    function GetPreviousToken(aEditor: TCustomSynEdit): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
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


uses
  SynEditStrConst;

{ TSynBaseCompletionForm }

constructor TSynBaseCompletionForm.Create(AOwner: TComponent);
begin
{$IFDEF SYN_CPPB_1}
  CreateNew(AOwner, 0);
{$ELSE}
  {$IFDEF FPC}
  CreateNew(AOwner,0);
  {$ELSE}
  CreateNew(AOwner);
  {$ENDIF}
{$ENDIF}
  FItemList := TStringList.Create;
  BorderStyle := bsNone;
  Scroll := TScrollBar.Create(self);
  Scroll.Kind := sbVertical;
  Scroll.ParentCtl3D := False;
  Scroll.OnChange := {$IFDEF FPC}@{$ENDIF}ScrollChange;
  Scroll.Parent := self;
  Scroll.OnEnter := {$IFDEF FPC}@{$ENDIF}ScrollGetFocus;
  Scroll.Width := 10;
  {$IFDEF SYN_LAZARUS}
  Scroll.Visible := True;
  FTextColor:=clBlack;
  FTextSelectedColor:=clWhite;
  {$ENDIF}
  Visible := false;
  FFontHeight := Canvas.TextHeight('Cyrille de Brebisson');
  Color := clWindow;
  ClSelect := clHighlight;
  TStringList(FItemList).OnChange := {$IFDEF FPC}@{$ENDIF}StringListChange;
  bitmap := TBitmap.Create;
  NbLinesInWindow := 6;
  ShowHint := True;
end;

procedure TSynBaseCompletionForm.Deactivate;
begin
  Visible := False;
  SetCaretRespondToFocus(TCustomSynEdit(FCurrentEditor).Handle,true);
end;

destructor TSynBaseCompletionForm.Destroy;
begin
  bitmap.free;
  Scroll.Free;
  FItemList.Free;
  inherited destroy;
end;

procedure TSynBaseCompletionForm.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  i: integer;
begin
  case Key of
// added the VK_XXX codes to make it more readable / maintainable
    VK_RETURN:
      if Assigned(OnValidate) then OnValidate(Self, Shift);
    VK_ESCAPE{$IFNDEF SYN_LAZARUS}, VK_SPACE{$ENDIF}:
      if Assigned(OnCancel) then OnCancel(Self);
    // I do not think there is a worst way to do this, but laziness rules :-)
    VK_PRIOR:
      for i := 1 to NbLinesInWindow do
        SelectPrec;
    VK_NEXT:
      for i := 1 to NbLinesInWindow do
        SelectNext;
    VK_END:
      if ssCtrl in Shift then Position := ItemList.count - 1;
    VK_HOME:
      if ssCtrl in Shift then Position := 0;
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
        CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);
      end;
  end;
  {$ifdef SYN_LAZARUS}
  Invalidate;
  {$ENDIF}
end;

procedure TSynBaseCompletionForm.KeyPress(var Key: char);
begin
  case key of //
    #33..'z': begin
        if Assigned(OnKeyPress) then
          OnKeyPress(self, Key);
        {$ifdef SYN_LAZARUS}
        if Key in [#33..'z'] then
          CurrentString := CurrentString + key;
        {$else}
        CurrentString := CurrentString + key;
        {$ENDIF}
      end;
    #8:
      if Assigned(OnKeyPress) then OnKeyPress(self, Key); 
  else if Assigned(OnCancel) then
    OnCancel(Self);
  end; // case
  {$ifdef SYN_LAZARUS}
  Invalidate;
  {$ENDIF}
end;

procedure TSynBaseCompletionForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  y := (y - 1) div FFontHeight;
  Position := Scroll.Position + y;
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
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  {$IFNDEF SYN_LAZARUS}  
  Position := Position;
  {$ENDIF}
  Scroll.LargeChange := NbLinesInWindow;

  {$IFNDEF SYN_LAZARUS}
  // canvas.draw is unfinished in lcl
  with bitmap do begin
    canvas.pen.color := color;
    canvas.brush.color := color;
    canvas.Rectangle(0, 0, Width, Height);
  {$ENDIF}
    for i := 0 to min(NbLinesInWindow - 1, ItemList.Count - 1) do begin
      if i + Scroll.Position = Position then begin
        Canvas.Brush.Color := clSelect;
        Canvas.Pen.Color := clSelect;
        Canvas.Rectangle(0, FFontHeight * i, width, FFontHeight * (i + 1));
        Canvas.Pen.Color := clBlack;
        {$IFDEF SYN_LAZARUS}
        Canvas.Font.Color := TextSelectedColor;
        {$ELSE}
        Canvas.Font.Color := clWhite;
        {$ENDIF}
        Hint := ItemList[Position];
      end
      else
        Begin
          Canvas.Brush.Color := Color;
          {$IFDEF SYN_LAZARUS}
          Canvas.Font.Color := TextColor;
          Canvas.FillRect(Rect(0, FFontHeight * i, width, FFontHeight * (i + 1)));
          {$ELSE}
          Canvas.Font.Color := clBlack;
          {$ENDIF}
        end;
        
      if not Assigned(OnPaintItem)
      or not OnPaintItem(ItemList[Scroll.Position + i], Canvas,
          {$IFDEF SYN_LAZARUS}
          1, FFontHeight * i +1, i + Scroll.Position = Position,
          i + Scroll.Position
          {$ELSE}
          0, FFontHeight * i
          {$ENDIF}
          )
      then
        Begin
          Canvas.TextOut(2, FFontHeight * i, ItemList[Scroll.Position + i]);
        end;
    end;
  {$IFNDEF SYN_LAZARUS}
  end;
  Canvas.Draw(1, 1, bitmap);
  // draw a rectangle around the window
  {$ENDIF}
  {$IFDEF SYN_LAZARUS}
  Canvas.Pen.Color := TextColor;
  {$ELSE}
  Canvas.Pen.Color := clBlack;
  {$ENDIF}
  Canvas.Moveto(0, 0);
  Canvas.LineTo(Width - 1, 0);
  Canvas.LineTo(Width - 1, Height - 1);
  Canvas.LineTo(0, Height - 1);
  Canvas.LineTo(0, 0);
end;

procedure TSynBaseCompletionForm.ScrollChange(Sender: TObject);
begin
  if Position < Scroll.Position then
    Position := Scroll.Position
  else if Position > Scroll.Position + NbLinesInWindow - 1 then
    Position := Scroll.Position + NbLinesInWindow - 1;
//writeln('TSynBaseCompletionForm.ScrollChange');
  Invalidate;
end;

procedure TSynBaseCompletionForm.ScrollGetFocus(Sender: TObject);
begin
  ActiveControl := nil;
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

procedure TSynBaseCompletionForm.SetCurrentString(const Value: string);
var
  i: integer;
begin
  FCurrentString := Value;
  {$IFDEF SYN_LAZARUS}
  if Assigned(FOnSearchPosition) then begin
    i:=Position;
    FOnSearchPosition(i);
    Position:=i;
  end else begin
  {$ENDIF}
    if ffAnsi then begin
      for i := 0 to Pred(ItemList.Count) do
        if 0 = CompareText(fCurrentString,
          Copy(ItemList[i], 1, Length(fCurrentString)))
        then begin
          Position := i;
          break;
        end;
    end else begin
      for i := 0 to Pred(ItemList.Count) do
        if 0 = CompareStr(fCurrentString,
          Copy(ItemList[i], 1, Length(fCurrentString)))
        then begin
          Position := i;
          break;
        end;
    end;
  {$IFDEF SYN_LAZARUS}
  end;
  {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
procedure TSynBaseCompletionForm.SetFontHeight(NewFontHeight:integer);
begin
  if NewFontHeight<>FFontHeight then begin
    FFontHeight:=NewFontHeight;
    SetNblinesInWindow(FNbLinesInWindow);
  end;
end;
{$ENDIF}

procedure TSynBaseCompletionForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  {$IFDEF SYN_LAZARUS}
  if Position>=FItemList.Count then Position:=-1;
  Invalidate;
  {$ENDIF}
end;

procedure TSynBaseCompletionForm.SetNbLinesInWindow(
  const Value: Integer);
begin
  FNbLinesInWindow := Value;
  Height := fFontHeight * NbLinesInWindow + 2;
  Scroll.Top := 1;
  Scroll.Left := ClientWidth - Scroll.Width - 1;
  Scroll.Height := Height - 2;
  Bitmap.Width := Scroll.Left;
  Bitmap.Height := Height - 2;
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
    end;
  end;
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

{$IFDEF SYN_LAZARUS}
function TSynBaseCompletion.GetFontHeight:integer;
begin
  Result:=Form.FontHeight;
end;

procedure TSynBaseCompletion.SetFontHeight(NewFontHeight :integer);
begin
  Form.FontHeight:=NewFontHeight;
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
{$ENDIF}

procedure TSynBaseCompletion.Execute(s: string; x, y: integer);
begin
  SetCaretRespondToFocus(TCustomSynEdit(Form.CurrentEditor).Handle,false);
  CurrentString := s;
  if Assigned(OnExecute) then
    OnExecute(Self);
  {$IFDEF SYN_LAZARUS}
  if (ItemList.Count=1) and Assigned(OnValidate) then begin
    OnValidate(Form, []);
    exit;
  end;
  if (ItemList.Count=0) and Assigned(OnCancel) then begin
    OnCancel(Form);
    exit;
  end;
  
  Form.SetBounds(x,y,Form.Width,Form.Height);
  Form.Color:=clNone;
  {$ELSE}
  Form.Left:=x;
  Form.Top:=y;
  {$ENDIF}
  Form.Show;
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

procedure TSynBaseCompletion.SetItemList(const Value: TStrings);
begin
  form.ItemList := Value;
end;

procedure TSynBaseCompletion.SetNbLinesInWindow(const Value: Integer);
begin
  form.NbLinesInWindow := Value;
end;

procedure TSynBaseCompletion.SetOnCancel(const Value: TNotifyEvent);
begin
  form.OnCancel := Value;
end;

procedure TSynBaseCompletion.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  form.OnKeyPress := Value;
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

procedure TSynBaseCompletion.SetWidth(Value: Integer);
begin
//writeln('TSynBaseCompletion.SetWidth START ',Value);
  FWidth := Value;
  Form.Width := FWidth;
  Form.SetNbLinesInWindow(Form.FNbLinesInWindow);
//writeln('TSynBaseCompletion.SetWidth END ',Value);
end;

procedure TSynBaseCompletion.Deactivate;
begin
  if Assigned(Form) then Form.Deactivate;
end;

{$IFDEF SYN_LAZARUS}
function TSynBaseCompletion.IsActive: boolean;
begin
  Result:=(Form<>nil) and (Form.Visible);
end;

function TSynBaseCompletion.TheForm: TSynBaseCompletionForm;
begin
  Result:=Form;
end;
{$ENDIF}

procedure PrettyTextOut(c: TCanvas; x, y: integer; s: string);
var
  i: integer;
  {$IFNDEF SYN_LAZARUS}
  b: TBrush;
  f: TFont;
  {$ELSE}
  OldFontColor: TColor;
  OldFontStyle: TFontStyles;
  {$ENDIF}
begin
  {$IFDEF SYN_LAZARUS}
  OldFontColor:=c.Font.Color;
  OldFontStyle:=c.Font.Style;
  c.Font.Style:=[];
  c.Font.Color:=clBlack;
  {$ELSE}
  b := TBrush.Create;
  b.Assign(c.Brush);
  f := TFont.Create;
  f.Assign(c.Font);
  {$ENDIF}
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
  {$IFDEF SYN_LAZARUS}
  c.Font.Color:=OldFontColor;
  c.Font.Style:=OldFontStyle;
  {$ELSE}
  c.Font.Assign(f);
  f.Free;
  c.Brush.Assign(b);
  b.Free;
  {$ENDIF}
end;

{ TSynCompletion }

type
  TRecordUsedToStoreEachEditorVars = record
    kp: TKeyPressEvent;
    kd: TKeyEvent;
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
//writeln('TSynCompletion.OnFormShow A');
  //LCLLinux.ShowCaret(Editor.Handle);
//writeln('TSynCompletion.OnFormShow END');
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

procedure TSynCompletion.Validate(Sender: TObject; Shift: TShiftState);
var
  F: TSynBaseCompletionForm;
  Value, CurLine: string;
  {$IFDEF SYN_LAZARUS}
  NewCaretXY: TPoint;
  {$Else}
  Pos: TPoint;
  {$ENDIF}
begin
  F := Sender as TSynBaseCompletionForm;
  if F.CurrentEditor <> nil then
    with F.CurrentEditor as TCustomSynEdit do begin
      BeginUndoBlock;
      BlockBegin := Point(CaretX - length(CurrentString), CaretY);
      {$IFDEF SYN_LAZARUS}
      if ssShift in Shift then begin
        // replace only prefix
        BlockEnd := Point(CaretX, CaretY);
      end else begin
        // replace the whole word
        NewCaretXY:=CaretXY;
        CurLine:=TSynEditStringList(Lines).ExpandedStrings[NewCaretXY.Y - 1];
        while (NewCaretXY.X<=length(CurLine))
        and (CurLine[NewCaretXY.X] in ['a'..'z','A'..'Z','0'..'9','_']) do
          inc(NewCaretXY.X);
        BlockEnd := NewCaretXY;
      end;
      {$ELSE}
      BlockEnd := Point(CaretX, CaretY);
      {$ENDIF}
      if Position>=0 then begin
        if Assigned(FOnCodeCompletion) then begin
          Value := ItemList[Position];
          FOnCodeCompletion(Value, Shift);
          SelText := Value;
        end else
          SelText := ItemList[Position];
      end;       
      {$IFNDEF SYN_LAZARUS}
      with Editor do begin
        Pos := CaretXY;
        Perform(LM_MBUTTONDOWN, 0, 0);
        Application.ProcessMessages;
        CaretXY := Pos;
      end;
      SetFocus;
      {$ENDIF}
      EndUndoBlock;
    end;
end;

procedure TSynCompletion.KeyPress(Sender: TObject; var Key: Char);
var
  F: TSynBaseCompletionForm;
begin
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
  if (Operation = opRemove) and (fEditors.IndexOf(AComponent) > -1) then
    RemoveEditor(AComponent as TCustomSynEdit);
end;

constructor TSynCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnKeyPress := {$IFDEF FPC}@{$ENDIF}KeyPress;
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
  {$IFDEF SYN_LAZARUS}
  if (Form<>nil) and (Form.Visible) then begin
    // completion form is visible, but the synedit got a key
    // -> redirect to form
    Form.KeyDown(Key,Shift);
    // eat it
    Key:=VK_UNKNOWN;
  end;
  {$ENDIF}
  ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);
  if (Shift <> ShortCutShift) or (Key <> ShortCutKey) then exit;

  i := fEditors.IndexOf(Sender);
  if i <> -1 then
    with sender as TCustomSynEdit do begin
      if not ReadOnly and (Shift = ShortCutShift) and (Key = ShortCutKey) then begin
        p := ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight));
        Form.CurrentEditor := Sender as TCustomSynEdit;
        Execute(GetPreviousToken(Sender as TCustomSynEdit), p.x, p.y);
        {$IFDEF SYN_LAZARUS}
        // eat it
        {$ENDIF}
        Key := VK_UNKNOWN;
        TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey := true;
      end;
      if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kd) then
        TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kd(sender, key, shift);
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
  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey then begin
      key := #0;
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey := false;
    end;
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kp) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kp(sender, key);
  end;
end;

destructor TSynCompletion.Destroy;
begin
  // necessary to get Notification called before fEditors is freed
  Form.Free;
  Form := nil;
  while fEditors.Count <> 0 do
    RemoveEditor(TCustomSynEdit(fEditors.last));
  fEditors.Free;
  fEditstuffs.free;
  inherited;
end;

function TSynCompletion.GetFEditor: TCustomSynEdit;
begin
  {$IFDEF SYN_LAZARUS}
  Result:=TCustomSynEdit(Form.fCurrentEditor);
  {$ELSE}
  if EditorsCount > 0 then
    result := Editors[0]
  else
    result := nil;
  {$ENDIF}
end;

procedure TSynCompletion.AddEditor(aEditor: TCustomSynEdit);
var
  p: PRecordUsedToStoreEachEditorVars;
begin
  if fEditors.IndexOf(aEditor) = -1 then begin
    fEditors.Add(aEditor);
    new(p);
    p^.kp := aEditor.OnKeyPress;
    p^.kd := aEditor.OnKeyDown;
    p^.NoNextKey := false;
    fEditstuffs.add(p);
    aEditor.FreeNotification(self);
    if not (csDesigning in ComponentState) then begin
      aEditor.OnKeyDown := {$IFDEF FPC}@{$ENDIF}EditorKeyDown;
      aEditor.OnKeyPress := {$IFDEF FPC}@{$ENDIF}EditorKeyPress;
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
    p^.kp := aEditor.OnKeyPress;
    p^.kd := aEditor.OnKeyDown;
    p^.NoNextKey := false;
    fEditstuffs.add(p);
    aEditor.FreeNotification(self);
    if not (csDesigning in ComponentState) then begin
      aEditor.OnKeyDown := {$IFDEF FPC}@{$ENDIF}EditorKeyDown;
      aEditor.OnKeyPress := {$IFDEF FPC}@{$ENDIF}EditorKeyPress;
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
  fEditors.free;
  fEditstuffs.free;
  fAutoCompleteList.free;
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
//inserted by shane
  ShortCutKey := word(' ');
  ShortCutShift := [ssCtrl];
//shane

  i := fEditors.IndexOf(Sender);
  if i <> -1 then begin
    if (Shift = ShortCutShift) and (Key = ShortCutKey) and
      not (Sender as TCustomSynEdit).ReadOnly
    then begin
      Execute(GetPreviousToken(Sender as TCustomSynEdit), Sender as TCustomSynEdit);
      Key := 0;
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).NoNextKey := true;
    end;
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kd) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kd(sender, key, Shift);
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
    if assigned(TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kp) then
      TRecordUsedToStoreEachEditorVars(fEditstuffs[i]^).kp(sender, key);
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
  if (Operation = opRemove) and (fEditors.indexOf(AComponent) <> -1) then
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

end.

