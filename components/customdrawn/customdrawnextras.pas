{
  Copyright (C) 2010 Felipe Monteiro de Carvalho

  License: The same modifying LGPL with static linking exception as the LCL

  This unit should be a repository for various custom drawn components,
  which are not in the unit customdrawncontrols,
  and also property editors for the customdrawnextras
}
unit customdrawnextras;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
  Math, types, customdrawnutils, contnrs, componenteditors, LMessages, Messages,
  LCLProc, PropEdits, ExtCtrls, ImgList, Forms, Menus,
  customdrawncontrols,
  // fpimage
  fpcanvas, fpimgcanv, fpimage
  {$ifdef CUSTOMDRAWN_USE_FREETYPE}
  // font support
  , ftfont
  {$endif}  ;

type

  // commented items are not yet supported
  TBitmappedButtonOption = (bboUseImageForSelection
    {bboUseImageForMouseOver, bboDrawFocusRectangle,}
    (*bboCheckable,*));

  TBitmappedButtonOptions = set of TBitmappedButtonOption;

  TBitmappedButtonState = (bbsNormal, bbsDown, bbsFocused, bbsMouseOver);

  { TCustomBitmappedButton }

  TCustomBitmappedButton = class(TCustomControl)
  private
    FOnChange: TNotifyEvent;
    BmpBuffer: TBitmap;
    StrBuffer: TStringList;
  protected
    FImageBtn: TPicture;
    FImageBtnDown: TPicture;
    FImageBtnMouseOver: TPicture;
    FImageBtnFocused: TPicture;
    FImageBtnChecked: TPicture;
    FOptions: TBitmappedButtonOptions;
    FState: TBitmappedButtonState;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    // button state change
    procedure DoButtonDown();
    procedure DoButtonUp();
    // inherited
    procedure RealSetText(const Value: TCaption); override;
  protected
    // Properties
    property ImageBtn: TPicture read FImageBtn;
    property ImageBtnDown: TPicture read FImageBtnDown;
    property ImageBtnFocused: TPicture read FImageBtnFocused;
    property Options: TBitmappedButtonOptions read FOptions write FOptions;
    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    function GetStateBitmap(): TBitmap;
  end;

  {@@
    TBitmappedButton is a simple custom drawn button which bases it's drawing
    on provided raster images. Currently the following states are supported:
    normal, down and focused.

    The Caption of this button may have multiple lines of text, separated by any
    line separator. The text is drawn centralized in the button.

    Some work was done trying to achieve alpha blending for the button, but this
    wasn't successfull. It would work like this: The button may be drawn flat
    or alpha blended using a separate image for the Alpha channel. While pixels
    in the alpha channel will result in the button pixel being fully drawn,
    while black pixels represent pixels which aren't drawn. grey pixels are
    alpha blended.
  }

  TBitmappedButton = class(TCustomBitmappedButton)
  published
    // LCL properties and events
    property Action;
    property Anchors;
    property AnchorSide;
    //    property BidiMode;
    //    property BorderSpacing;
    //    property Cancel;
    property Caption;
    property Constraints;
    //    property Default;
    //    property DragCursor;
    //    property DragKind;
    //    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBidiMode;
    //    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
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
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    // Specific properties
    property ImageBtn;
    property ImageBtnDown;
    property ImageBtnFocused;
    property Options;
  end;

  { TCDPageControlEditor }

  TCDPageControlEditor = class(TDefaultComponentEditor)
  private
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
    procedure PrepareItem(Index: integer; const AnItem: TMenuItem); override;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    function PControl: TCDPageControl; virtual;
  end;

procedure Register;

implementation

uses
  ObjInspStrConsts;

const
  INT_BitmappedButton_LineSpacing = 2;
  MaskBaseColor = $00111111;

resourcestring
  sNEXT_PAGE = 'Ne&xt Page';
  sPREV_PAGE = '&Previouse Page';

procedure Register;
begin
  RegisterComponents('Custom Drawn', [TCDButton, TCDTrackBar, TCDTabControl,
    TCDPageControl, TCDGroupBox]);
  RegisterComponentEditor(TCDPageControl, TCDPageControlEditor);
  RegisterComponentEditor(TCDTabSheet, TCDPageControlEditor);
  RegisterNoIcon([TCDTabSheet]);
  RegisterClasses([TCDTabSheet]);
end;

{ TCustomBitmappedButton }

procedure TCustomBitmappedButton.DoEnter;
begin
  DoButtonUp();

  inherited DoEnter;
end;

procedure TCustomBitmappedButton.DoExit;
begin
  DoButtonUp();

  inherited DoExit;
end;

procedure TCustomBitmappedButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_SPACE then
    DoButtonDown();
end;

procedure TCustomBitmappedButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  DoButtonUp();

  inherited KeyUp(Key, Shift);
end;

procedure TCustomBitmappedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DoButtonDown();

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomBitmappedButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DoButtonUp();

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomBitmappedButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCustomBitmappedButton.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TCustomBitmappedButton.DoButtonDown();
var
  NewState: TBitmappedButtonState;
begin
  NewState := bbsDown;

  case FState of
    bbsNormal, bbsFocused: NewState := bbsDown;
    //  bbsChecked, bbsCheckedSelected: NewState := bbsCheckedDown;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBitmappedButton.DoButtonUp();
var
  NewState: TBitmappedButtonState;
begin
  if Focused then
    NewState := bbsFocused
  else
    NewState := bbsNormal;

{  case FState of
  bbsCheckedDown:
  begin
    if Focused then NewState := bbsCheckedSelected
    else NewState := bbsChecked;
  end;
  end;}

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBitmappedButton.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);

  // Makes sure that caption changes are drawn
  Invalidate;
end;

constructor TCustomBitmappedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BmpBuffer := TBitmap.Create;
  StrBuffer := TStringList.Create;

  FImageBtn := TPicture.Create;
  FImageBtnDown := TPicture.Create;
  FImageBtnMouseOver := TPicture.Create;
  FImageBtnFocused := TPicture.Create;
  FImageBtnChecked := TPicture.Create;

  FOptions := [{bboDrawSelectionRectangle}];

  TabStop := True;
end;

destructor TCustomBitmappedButton.Destroy;
begin
  if Assigned(FImageBtn) then
    FImageBtn.Free;
  if Assigned(FImageBtnDown) then
    FImageBtnDown.Free;
  if Assigned(FImageBtnMouseOver) then
    FImageBtnMouseOver.Free;
  if Assigned(FImageBtnFocused) then
    FImageBtnFocused.Free;
  if Assigned(FImageBtnChecked) then
    FImageBtnChecked.Free;

  BmpBuffer.Free;
  StrBuffer.Free;

  inherited Destroy;
end;

procedure TCustomBitmappedButton.EraseBackground(DC: HDC);
begin
  // The correct implementation is doing nothing
end;

procedure TCustomBitmappedButton.Paint;
var
  lTextX, lTextY, lTextCX, lTextCY, lTmp, lTextHeightPlusLineSpacing: integer;
  i: integer;
begin
  // First draw the button image
  BmpBuffer.Width := Width;
  BmpBuffer.Height := Height;
  BmpBuffer.Canvas.Draw(0, 0, GetStateBitmap());

  // Now measure the text position

  BmpBuffer.Canvas.Font.Assign(Self.Font);
  BmpBuffer.Canvas.Brush.Style := bsClear;

  StrBuffer.Text := Caption;

  lTextCX := 0;
  for i := 0 to StrBuffer.Count - 1 do
  begin
    lTmp := BmpBuffer.Canvas.TextWidth(StrBuffer.Strings[i]);
    lTextCX := Max(lTextCX, lTmp);
  end;

  lTextHeightPlusLineSpacing :=
    BmpBuffer.Canvas.TextHeight(Caption) + INT_BitmappedButton_LineSpacing;
  lTextCY := BmpBuffer.Canvas.TextHeight(Caption) * StrBuffer.Count +
    INT_BitmappedButton_LineSpacing * (StrBuffer.Count - 1);

  lTextX := Width div 2 - lTextCX div 2;
  lTextY := Height div 2 - lTextCY div 2;

  // Draw the text

  for i := 0 to StrBuffer.Count - 1 do
  begin
    BmpBuffer.Canvas.TextOut(lTextX, lTextY + lTextHeightPlusLineSpacing * i,
      StrBuffer.Strings[i]);
  end;

  // And flush the buffer to the screen
  Canvas.Draw(0, 0, BmpBuffer);
end;

function TCustomBitmappedButton.GetStateBitmap(): TBitmap;
begin
  case FState of
    bbsDown: Result := FImageBtnDown.Bitmap;
    //  bbsMouseOver: Result := FImageBtnMouseOver;
    bbsFocused:
    begin
      if bboUseImageForSelection in Options then
        Result := FImageBtnFocused.Bitmap
      else
        Result := FImageBtn.Bitmap;
    end;
      //  bbsChecked:   Result := FImageBtnChecked;
    else
      Result := FImageBtn.Bitmap;
  end;
end;

{ TCDPageControlEditor }

procedure TCDPageControlEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem := TMenuItem(Sender);
  if (AMenuItem = nil) or (not (AMenuItem is TMenuItem)) then
    exit;
  NewPageIndex := AMenuItem.MenuIndex;
  if (NewPageIndex < 0) or (NewPageIndex >= PControl.PageCount) then
    exit;
  PControl.PageIndex := NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(TComponent(PControl.Tabs.Objects[PControl.PageIndex]));
end;

procedure TCDPageControlEditor.ExecuteVerb(Index: integer);
var
  NewPage: TCDTabSheet;
  Hook: TPropertyEditorHook;
  PageComponent: TPersistent;
  OldPage: longint;
begin
  if not GetHook(Hook) then exit;

  case Index of
    0:
    begin  //  New Page
      NewPage := PControl.AddPage('');
      Hook.PersistentAdded(NewPage, True);
    end;
    1:
    begin // Insert Page
      NewPage := PControl.InsertPage(PControl.PageIndex, '');
      Hook.PersistentAdded(NewPage, True);
    end;
    2:
    begin  //  Delete Page
      //WriteLn('Delete 1');
      NewPage := PControl.ActivePage;
      if NewPage = nil then Exit;
      //WriteLn('Delete 2');
      PControl.RemovePage(PControl.PageIndex);
      Hook.PersistentDeleting(NewPage);
    end;
    3:
    begin  //  Next Page
      PControl.ActivePage := PControl.FindNextPage(PControl.ActivePage, True, False);
    end;
    4:
    begin  //  Previous Page
      PControl.ActivePage := PControl.FindNextPage(PControl.ActivePage, False, False);
    end;
  end;
  Modified;
  if Designer <> nil then Designer.Modified;
  PControl.Invalidate;
end;

function TCDPageControlEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := nbcesAddPage;
    1: Result := nbcesInsertPage;
    2: Result := nbcesDeletePage;
    3: Result := sNEXT_PAGE;
    4: Result := sPREV_PAGE;
    5: Result := nbcesShowPage;
  end;
end;

function TCDPageControlEditor.GetVerbCount: integer;
begin
  Result := 6;
end;

procedure TCDPageControlEditor.PrepareItem(Index: integer; const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    0: ;
    1: AnItem.Enabled := PControl.PageIndex >= 0;
    2: AnItem.Enabled := PControl.PageIndex >= 0;
    3: AnItem.Enabled := PControl.PageIndex < PControl.PageCount - 1;
    4: AnItem.Enabled := PControl.PageIndex > 0;
    5: AddMenuItemsForPages(AnItem);
  end;
end;

procedure TCDPageControlEditor.AddMenuItemsForPages(ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
  TabPage: TCDTabSheet;
begin
  ParentMenuItem.Enabled := PControl.PageCount > 0;
  for i := 0 to PControl.PageCount - 1 do
  begin
    TabPage := PControl.GetPage(i);
    NewMenuItem := TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name := 'ShowPage' + IntToStr(i);
    NewMenuItem.Caption := TabPage.Name + ' "' + TabPage.Caption + '"';
    NewMenuItem.OnClick := @ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

function TCDPageControlEditor.PControl: TCDPageControl;
begin
  if Component is TCDPageControl then
    Result := TCDPageControl(Component)
  else if Component is TCDTabSheet then
    Result := TCDPageControl(TCDTabSheet(Component).Parent);
end;

end.

