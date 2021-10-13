{ Visual component TCheckComboBox

  Copyright (C) 2014 Vojtěch Čihák, e-mail: cihakvjtch@seznam.cz

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public License as published by the Free Software Foundation; either version
  2 of the License, or (at your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this
  library with independent modules to produce an executable, regardless of the license terms of
  these independent modules,and to copy and distribute the resulting executable under terms of
  your choice, provided that you also meet, for each linked independent module, the terms and
  conditions of the license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this exception to your
  version of the library, but you are not obligated to do so. If you do not wish to do so, delete
  this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this
  library; if not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth
  Floor, Boston, MA 02110-1335, USA.
}

unit CheckCombo;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  LCLIntf, LCLType, LMessages, LResources, LazLoggerBase,
  ImgList, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics, GraphUtil,
  Themes, Forms;

type
  { Events }
  TCheckItemChange = procedure(Sender: TObject; AIndex: Integer) of object;

  { TCheckComboItemState }
  TCheckComboItemState = class
  public
    State: TCheckBoxState;
    Enabled: Boolean;
    Data: TObject;
  end;

  { TCustomCheckCombo }
  TCustomCheckCombo = class(TCustomComboBox)
  private
    FAllowGrayed: Boolean;
    FOnItemChange: TCheckItemChange;
    procedure AsyncCheckItemStates(Data: PtrInt);
    function GetChecked(AIndex: Integer): Boolean;
    function GetCount: Integer;
    function GetItemEnabled(AIndex: Integer): Boolean;
    function GetObject(AIndex: Integer): TObject;
    function GetState(AIndex: Integer): TCheckBoxState;
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetItemEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetObject(AIndex: Integer; AValue: TObject);
    procedure SetState(AIndex: Integer; AValue: TCheckBoxState);
  protected
    FCheckHighlight: Boolean;
    FCheckSize: TSize;
    FDropped: Boolean;
    FHilightedIndex: Integer;
    FHiLiteLeft: Integer;
    FHiLiteRight: Integer;
    FNeedMeasure: Boolean;
    FRejectDropDown: Boolean;
    FRejectToggleOnSelect: Boolean;
    FRightToLeft: Boolean;
    FTextHeight: SmallInt;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure ClearItemStates;
    procedure CloseUp; override;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure DropDown; override;
    procedure FontChanged(Sender: TObject); override;
    procedure InitializeWnd; override;
    procedure InitItemStates;
    procedure CheckItemStates;
    procedure QueueCheckItemStates;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetItemHeight(const AValue: Integer); override;
    procedure SetItems(const Value: TStrings); override;
    procedure Select; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const AItem: string; AState: TCheckBoxState; AEnabled: Boolean = True); reintroduce;
    procedure AssignItems(AItems: TStrings);
    procedure Clear; override;
    procedure DeleteItem(AIndex: Integer);
    procedure CheckAll(AState: TCheckBoxState; AAllowGrayed: Boolean = True; AAllowDisabled: Boolean = True);
    procedure Toggle(AIndex: Integer);
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Count: Integer read GetCount;
    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property Objects[AIndex: Integer]: TObject read GetObject write SetObject;
    property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
    property OnItemChange: TCheckItemChange read FOnItemChange write FOnItemChange;
  end;

  { TCheckComboBox }
  TCheckComboBox = class(TCustomCheckCombo)
  published
    property Align;
    property AllowGrayed;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property Count;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnItemChange;
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
    property OnSelect;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
  end;


implementation

{ TCustomCheckCombo }

constructor TCustomCheckCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TStringList(Items).Duplicates:=dupIgnore;
  Style:=csOwnerDrawFixed;
  FNeedMeasure:=True;
  FRejectToggleOnSelect:=True;
end;

destructor TCustomCheckCombo.Destroy;
begin
  ClearItemStates;
  inherited Destroy;
end;

procedure TCustomCheckCombo.AddItem(const AItem: string; AState: TCheckBoxState; AEnabled: Boolean);
var pItemState: TCheckComboItemState;
begin
  pItemState:=TCheckComboItemState.Create;
  pItemState.State:=aState;
  pItemState.Enabled:=AEnabled;
  pItemState.Data:=nil;
  inherited AddItem(AItem, pItemState);
end;

procedure TCustomCheckCombo.AssignItems(AItems: TStrings);
begin
  ClearItemStates;
  Items.Assign(AItems);
  InitItemStates;
end;

procedure TCustomCheckCombo.CheckAll(AState: TCheckBoxState; AAllowGrayed: Boolean;
  AAllowDisabled: Boolean);
var i: Integer;
begin
  for i:=0 to Items.Count-1 do
  begin
    if (AAllowGrayed or (State[i]<>cbGrayed)) and (AAllowDisabled or ItemEnabled[i])
      then State[i]:=AState;
  end;
end;

procedure TCustomCheckCombo.Clear;
begin
  ClearItemStates;
  inherited Clear;
end;

procedure TCustomCheckCombo.ClearItemStates;
var i: Integer;
begin
  for i:=0 to Items.Count-1 do
  begin
    Items.Objects[i].Free;
    Items.Objects[i]:=nil;
  end;
end;

procedure TCustomCheckCombo.CloseUp;
begin
  FDropped:=False;
  if FRejectDropDown then
  begin
    FRejectDropDown:=False;
    Update;
  end else
    inherited CloseUp;
end;

procedure TCustomCheckCombo.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FRightToLeft:=IsRightToLeft;
  FNeedMeasure:=True;
  Invalidate;
end;

procedure TCustomCheckCombo.DeleteItem(AIndex: Integer);
begin
  if (AIndex>=0) and (AIndex<Items.Count) then
  begin
    Items.Objects[AIndex].Free;
    Items.Delete(AIndex);
  end;
end;

procedure TCustomCheckCombo.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
                            { Enabled, State, Highlighted }
const caCheckThemes: array [Boolean, TCheckBoxState, Boolean] of TThemedButton =
                           { normal, highlighted }
        (((tbCheckBoxUncheckedDisabled, tbCheckBoxUncheckedDisabled),  { disabled, unchecked }
          (tbCheckBoxCheckedDisabled, tbCheckBoxCheckedDisabled),      { disabled, checked }
          (tbCheckBoxMixedDisabled, tbCheckBoxMixedDisabled)),         { disabled, greyed }
         ((tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot),         { enabled, unchecked }
          (tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot),             { enabled, checked }
          (tbCheckBoxMixedNormal, tbCheckBoxMixedHot)));               { enabled, greyed }
      cCheckIndent: SmallInt = 2;
      cTextIndent: SmallInt = 5;
var aDetail: TThemedElementDetails;
    aDropped: Boolean;
    aEnabled: Boolean;
    aFlags: Cardinal;
    aFocusedEditableMainItemNoDD: Boolean;  { combo has edit-like line edit in csDropDownList (Win) and is closed (not DroppedDown }
    aGray: Byte;
    anyRect: TRect;
    aState: TCheckBoxState;
    ItemState: TCheckComboItemState;
begin  { do not call inherited ! }
  ItemState:=TCheckComboItemState(Items.Objects[Index]);
  if not (ItemState is TCheckComboItemState) then
    QueueCheckItemStates;
  aDropped:=DroppedDown;
  if aDropped and FRejectDropDown then
    begin
      DroppedDown:=False;
      exit;  { Exit! }
    end;
  aEnabled:=IsEnabled;
  if not (csDesigning in ComponentState) then
    aEnabled:= (aEnabled and ItemState.Enabled);
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  aFocusedEditableMainItemNoDD := (Focused and (ARect.Left>0) and not aDropped);
  {$ELSE}
  aFocusedEditableMainItemNoDD := False;
  {$ENDIF}
  if (ARect.Left=0) or aFocusedEditableMainItemNoDD then
    begin
      if odSelected in State then
        begin
          if not aEnabled then
            begin
              aGray:=ColorToGray(Canvas.Brush.Color);
              Canvas.Brush.Color:=RGBToColor(aGray, aGray, aGray);
            end;
        end else
        Canvas.Brush.Color:=clWindow;
      Canvas.Brush.Style:=bsSolid;
      Canvas.FillRect(ARect);
    end;
  if not (csDesigning in ComponentState)
    then aState:=ItemState.State
    else aState:=cbUnchecked;
  aDetail:=ThemeServices.GetElementDetails(caCheckThemes
    [aEnabled, aState, not aDropped and FCheckHighlight]);
  if FNeedMeasure then
    begin
      FCheckSize:=ThemeServices.GetDetailSize(aDetail);
      FTextHeight:=Canvas.TextHeight('ŠjÁÇ');
      if not aDropped then
        begin
          if not FRightToLeft then
            begin
              FHiLiteLeft:=-1;
              FHiLiteRight:=ARect.Right;
            end else
            begin
              FHiLiteLeft:=ARect.Left;
              FHiLiteRight:=ARect.Right;
            end;
          FNeedMeasure := False;
        end;
    end;
  if not FRightToLeft
    then anyRect.Left:=ARect.Left+cCheckIndent
    else anyRect.Left:=ARect.Right-cCheckIndent-FCheckSize.cx;
  anyRect.Right:=anyRect.Left+FCheckSize.cx;
  anyRect.Top:=(ARect.Bottom+ARect.Top-FCheckSize.cy) div 2;
  anyRect.Bottom:=anyRect.Top+FCheckSize.cy;
  ThemeServices.DrawElement(Canvas.Handle, aDetail, anyRect);
  Canvas.Brush.Style:=bsClear;
  if (not (odSelected in State) or not aDropped) and not aFocusedEditableMainItemNoDD
    then Canvas.Font.Color:=clWindowText
    else begin
       Canvas.Font.Color:=clHighlightText;
       FHilightedIndex:=Index;
    end;
  if aFocusedEditableMainItemNoDD then
    begin
      LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
    end;
  aFlags:=DT_END_ELLIPSIS+DT_VCENTER+DT_SINGLELINE+DT_NOPREFIX;
  if not FRightToLeft then
    begin
      anyRect.Left:=ARect.Left+cCheckIndent+FCheckSize.cx+cTextIndent;
      anyRect.Right:=ARect.Right;
    end else
    begin
      anyRect.Right:=anyRect.Left-cTextIndent;
      anyRect.Left:=ARect.Left;
      aFlags:=aFlags or DT_RIGHT or DT_RTLREADING;
    end;
  anyRect.Top:=(ARect.Top+ARect.Bottom-FTextHeight) div 2;
  anyRect.Bottom:=anyRect.Top+FTextHeight;
  DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), anyRect, aFlags);
end;

procedure TCustomCheckCombo.DropDown;
{$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
{$ELSE}
var aCursorPos: TPoint;
    aRect: TRect;
{$ENDIF}
begin
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  FRejectDropDown:=False;
  {$ELSE}
  aCursorPos:=ScreenToControl(Mouse.CursorPos);
  aRect:=Rect(FHiLiteLeft, 0, FHiLiteRight, Height);
  FRejectDropDown:=PtInRect(aRect, aCursorPos);
  {$ENDIF}
  FDropped:=True;
  if not FRejectDropDown then
    begin
      inherited DropDown;
      FRejectToggleOnSelect:=False;
    end else
    if (ItemIndex>=0) and ItemEnabled[ItemIndex] then Toggle(ItemIndex);
end;

procedure TCustomCheckCombo.FontChanged(Sender: TObject);
begin
  FNeedMeasure:=True;
  inherited FontChanged(Sender);
end;

procedure TCustomCheckCombo.InitializeWnd;
begin
  InitItemStates;
  inherited InitializeWnd;
  CheckItemStates;
  FRightToLeft:=IsRightToLeft;
end;

procedure TCustomCheckCombo.InitItemStates;
var i: Integer;
    pItemState: TCheckComboItemState;
begin
  for i:=0 to Items.Count-1 do
    if Items.Objects[i]=nil then begin
      pItemState:=TCheckComboItemState.Create;
      pItemState.Enabled:=True;
      pItemState.State:=cbUnchecked;
      pItemState.Data:=nil;
      Items.Objects[i]:=pItemState;
    end else if not (Items.Objects[i] is TCheckComboItemState) then
      raise Exception.Create(DbgSName(Self)+': Item '+IntToStr(i)+' is not a TCheckComboItemState');
end;

procedure TCustomCheckCombo.CheckItemStates;
var
  i: Integer;
begin
  for i:=0 to Items.Count-1 do
    if not (Items.Objects[i] is TCheckComboItemState) then
      raise Exception.Create(DbgSName(Self)+': Item '+IntToStr(i)+' is not a TCheckComboItemState');
end;

procedure TCustomCheckCombo.QueueCheckItemStates;
begin
  Application.QueueAsyncCall(@AsyncCheckItemStates,0);
end;

procedure TCustomCheckCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      if FDropped then
        if (ItemIndex=FHilightedIndex) and ItemEnabled[ItemIndex] then Toggle(ItemIndex);
    VK_SPACE:
      if DroppedDown then
        if (ItemIndex>=0) and ItemEnabled[ItemIndex] then
        begin
          if ItemIndex<>FHilightedIndex then
          begin
            ItemIndex:=FHilightedIndex;
            inherited Select;
          end;
          Toggle(ItemIndex);
          DroppedDown:=False;
        end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomCheckCombo.Loaded;
begin
  inherited Loaded;
  InitItemStates;
end;

procedure TCustomCheckCombo.MouseLeave;
begin
  FCheckHighlight:=False;
  inherited MouseLeave;
end;

procedure TCustomCheckCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
var aHighlight: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  aHighlight:=((X>FHiLiteLeft) and (X<FHiLiteRight));
  if aHighlight<>FCheckHighlight then
    begin
      FCheckHighlight:=aHighlight;
      Invalidate;
    end;
end;

procedure TCustomCheckCombo.Select;
begin
  inherited Select;
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  if DroppedDown then FRejectToggleOnSelect:=True;
  {$ENDIF}
  if not FRejectToggleOnSelect then
    begin
      if (ItemIndex >= 0) and ItemEnabled[ItemIndex] then Toggle(ItemIndex);
      FRejectToggleOnSelect:=True;
    end;
  FDropped:=False;
end;

procedure TCustomCheckCombo.SetItemHeight(const AValue: Integer);
begin
  inherited SetItemHeight(AValue);
  FNeedMeasure:=True;
end;

procedure TCustomCheckCombo.SetItems(const Value: TStrings);
begin
  ClearItemStates;
  inherited SetItems(Value);
  InitItemStates;
end;

procedure TCustomCheckCombo.Toggle(AIndex: Integer);
const caNewStateMap: array [TCheckBoxState, Boolean] of TCheckBoxState =
  { False (AllowGrayed) True }
  ((cbChecked, cbGrayed),       { cbUnchecked }
   (cbUnChecked, cbUnChecked),  { cbChecked }
   (cbChecked, cbChecked));     { cbGrayed }
begin
  State[AIndex]:=caNewStateMap[State[AIndex], AllowGrayed];
end;

{ TCustomCheckCombo.Getters and Setters }

function TCustomCheckCombo.GetChecked(AIndex: Integer): Boolean;
begin
  Result:=(TCheckComboItemState(Items.Objects[AIndex]).State=cbChecked);
end;

procedure TCustomCheckCombo.AsyncCheckItemStates(Data: PtrInt);
begin
  CheckItemStates;
end;

function TCustomCheckCombo.GetCount: Integer;
begin
  Result:=Items.Count;
end;

function TCustomCheckCombo.GetItemEnabled(AIndex: Integer): Boolean;
begin
  Result:=TCheckComboItemState(Items.Objects[AIndex]).Enabled;
end;

function TCustomCheckCombo.GetObject(AIndex: Integer): TObject;
begin
  Result:=TCheckComboItemState(Items.Objects[AIndex]).Data;
end;

function TCustomCheckCombo.GetState(AIndex: Integer): TCheckBoxState;
begin
  Result:=TCheckComboItemState(Items.Objects[AIndex]).State;
end;

procedure TCustomCheckCombo.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  if AValue=(TCheckComboItemState(Items.Objects[AIndex]).State=cbChecked) then exit;
  if AValue
    then TCheckComboItemState(Items.Objects[AIndex]).State:=cbChecked
    else TCheckComboItemState(Items.Objects[AIndex]).State:=cbUnchecked;
  if Assigned(FOnItemChange) then
    FOnItemChange(Self, AIndex);
  if AIndex=ItemIndex then
    Invalidate;
end;

procedure TCustomCheckCombo.SetItemEnabled(AIndex: Integer; AValue: Boolean);
begin
  if TCheckComboItemState(Items.Objects[AIndex]).Enabled=AValue then exit;
  TCheckComboItemState(Items.Objects[AIndex]).Enabled:=AValue;
  if AIndex=ItemIndex then
    Invalidate;
end;

procedure TCustomCheckCombo.SetObject(AIndex: Integer; AValue: TObject);
begin
  TCheckComboItemState(Items.Objects[AIndex]).Data:=AValue;
end;

procedure TCustomCheckCombo.SetState(AIndex: Integer; AValue: TCheckBoxState);
begin
  if TCheckComboItemState(Items.Objects[AIndex]).State=AValue then exit;
  TCheckComboItemState(Items.Objects[AIndex]).State:=AValue;
  if Assigned(FOnItemChange) then
    FOnItemChange(self, AIndex);
  if AIndex=ItemIndex then
    Invalidate;
end;


end.


