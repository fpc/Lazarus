{%MainUnit ../dialogs.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

type

  { TPromptDialog }

  TPromptDialog = class(TForm)
    procedure PromptDialogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    function CreateButtons(AVerticalLayout: Boolean; ASpacing: Integer): Integer;
  public
    IsSmallDevice: Boolean;
  
    TheDefaultIndex : Longint;

    FBitmap: TCustomBitmap;
    MSG : AnsiString;
    NumButtons : Longint;
    Buttons : PLongint;

    TextBox : TRect;
    TextStyle : TTextStyle;

    procedure LayoutDialog;
    procedure LayoutDialogSmallDevice;
    procedure Paint; override;
    constructor CreateMessageDialog(const ACaption, aMsg: string;
      DialogType: Longint; TheButtons: PLongint; ButtonCount, DefaultIndex : Longint);
    destructor Destroy; override;
  end;

procedure TPromptDialog.PromptDialogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_Escape) then
    ModalResult := -1;
end;

function TPromptDialog.CreateButtons(AVerticalLayout: Boolean;
 ASpacing: Integer): Integer;
var
  curBtn      : Longint; // variable to loop through TMsgDlgButtons
  ButtonIndex : integer;
  CurButton: TBitBtn;
  DefaultButton: TBitBtn;
begin
  Result := 0;

  ButtonIndex := -1;
  DefaultButton := nil;
  for curBtn := 0 to NumButtons - 1 do
  begin
    if (Buttons[curBtn] >= Low(DialogButtonKind)) and
      (Buttons[curBtn] <= High(DialogButtonKind))
    then
    begin
      inc(ButtonIndex);

      CurButton := TBitBtn.Create(Self);
      with CurButton do
      begin
        Parent:= Self;
        Layout  := blGlyphLeft;
        OnKeyDown := @PromptDialogKeyDown;

        Kind := DialogButtonKind[Buttons[curBtn]];

        if Height < Glyph.Height + 5 then
          Height := Glyph.Height + 5;

        if ButtonIndex = TheDefaultIndex then
          DefaultButton := CurButton;

        Inc(Result, ASpacing);

        if AVerticalLayout then Inc(Result, Height)
        else
        begin
{          CurBtnSize:=GetButtonSize(CurButton);
          Inc(Result, CurBtnSize.X);}
        end;
      end;
    end;
  end;
  if DefaultButton <> nil then
    DefaultButton.Default := True;
end;

procedure TPromptDialog.Paint;
var
  UseMaskHandle: HBitmap;
begin
  inherited Paint;
  
  // Draws the text
  Canvas.Brush := Brush;
  Canvas.TextRect(TextBox, TextBox.Left, TextBox.Top, MSG, TextStyle);

  // Draws the icon
  if Assigned (FBitmap) and not IsSmallDevice then
  begin
    UseMaskHandle := FBitmap.MaskHandle;
    MaskBlt(Canvas.GetUpdatedHandle([csHandleValid]),
            cBitmapX, cBitmapY, FBitmap.Width, FBitmap.Height,
            FBitmap.Canvas.GetUpdatedHandle([csHandleValid]),
            0, 0, UseMaskHandle, 0, 0);
  end;
end;

constructor TPromptDialog.CreateMessageDialog(const ACaption, aMsg: string;
  DialogType : longint; TheButtons: PLongint; ButtonCount, DefaultIndex : Longint);
begin
  inherited CreateNew(nil, 1);

  IsSmallDevice := (Screen.Width <= 300);

  AutoScroll := False;
  OnKeyDown := @PromptDialogKeyDown;
  //debugln('TPromptDialog.CreateMessageDialog A ButtonCount=',dbgs(ButtonCount));

  ControlStyle:= ControlStyle-[csSetCaption];
  PopupMode := pmAuto;
  BorderStyle := bsDialog;
  Position    := poScreenCenter;
  SetInitialBounds(0,0,200,100);
  MSG         := ConvertLineEndings(AMSG);
  Buttons     := nil;
  FBitmap := nil;
  case DialogType of
    idDialogConfirm,
    idDialogInfo,
    idDialogWarning,
    idDialogError :
      begin
        FBitmap := GetDialogIcon(DialogType);

        if ACaption <> '' then
          Caption := ACaption
        else
          Caption := GetDialogCaption(DialogType);
      end;
    else
      begin
        FBitmap := GetDialogIcon(idDialogInfo);
        if ACaption <> '' then
          Caption := ACaption
        else
          Caption := Application.Title;
      end
  end;
  NumButtons := ButtonCount;
  Buttons := TheButtons;

  if (DefaultIndex >= ButtonCount) or (DefaultIndex < 0) then
    TheDefaultIndex := 0
  else
    TheDefaultIndex := DefaultIndex;

  // Assures a minimum text size
  if MSG = '' then MSG := '   ';

  // Initialize TextStyle
  FillChar(TextStyle, SizeOf(TTextStyle), 0);

  with TextStyle do
  begin
    Clipping   := True;
    Wordbreak  := True;
    SystemFont := True;
    Opaque     := False;
  end;

  if IsSmallDevice then
    LayoutDialogSmallDevice
  else
    LayoutDialog;
end;

destructor TPromptDialog.Destroy;
begin
  FBitmap.Free;
  inherited destroy;
end;

procedure TPromptDialog.LayoutDialog;
const
  cBtnCalcWidth  = 50;
  cBtnCalcHeight = 13;
  cBtnCalcSpace   = 4;
  cBtnCalcBorder = 4;
  cBtnDist = 10;
var
  curBtn      : Longint; // variable to loop through TMsgDlgButtons
  cMinLeft,
  ButtonLeft  : integer;    // left position of button(s)
  TextLeft    : integer;    // left position of text
  reqBtnWidth : integer;    // width neccessary to display buttons
  reqWidth, reqHeight : integer;    // width and height neccessary to display all
  i        : integer;
  ButtonIndex : integer;
  MinBtnWidth: Integer; // minimum width for a single button
  MinBtnHeight: Integer; // minimum height for a single button
  CurButton: TBitBtn;
  ButtonTop: Integer;
  CurBtnSize: TPoint;
  DefaultButton: TBitBtn;

  function GetButtonSize(AButton: TBitBtn): TPoint;
  begin
    AButton.HandleNeeded;
    TBitBtnAccess(AButton).CalculatePreferredSize(Result.x, Result.y, True);
    if MinBtnHeight < Result.y then
      MinBtnHeight := Result.y
    else
    if Result.y < MinBtnHeight then
      Result.y := MinBtnHeight;
    if Result.x < MinBtnWidth then
      Result.x := MinBtnWidth;
  end;
  
begin
  // calculate the width & height we need to display the Message

  // calculate the needed size for the text
  TextBox := Rect(0, 0, Screen.Width div 2, Screen.Height - 100);
  SelectObject(Canvas.Handle, Screen.SystemFont.Reference.Handle);
  DrawText(Canvas.Handle, PChar(MSG), Length(MSG),
    TextBox, DT_WORDBREAK or DT_CALCRECT);

  // calculate the width we need to display the buttons
  MinBtnWidth:=Max(25,MinimumDialogButtonWidth);
  MinBtnHeight:=Max(15,MinimumDialogButtonHeight);
  reqBtnWidth := 0;

  // create the buttons, without positioning
  ButtonIndex := -1;
  DefaultButton := nil;
  for curBtn := 0 to NumButtons - 1 do
  begin
    if (Buttons[curBtn] >= Low(DialogButtonKind)) and
       (Buttons[curBtn] <= High(DialogButtonKind)) then
    begin
      inc(ButtonIndex);

      CurButton := TBitBtn.Create(Self);
      with CurButton do
      begin
        Parent:= Self;
        Layout  := blGlyphLeft;
        OnKeyDown := @PromptDialogKeyDown;
        Kind := DialogButtonKind[Buttons[curBtn]];
        if Height < Glyph.Height + 5 then
          Height := Glyph.Height + 5;

        if ButtonIndex = TheDefaultIndex then
          DefaultButton := CurButton;

        CurBtnSize:=GetButtonSize(CurButton);
        if reqBtnWidth > 0 then inc(reqBtnWidth, cBtnDist);
        Inc(reqBtnWidth, CurBtnSize.X);
      end;
    end;
  end;
  if DefaultButton <> nil then
    DefaultButton.Default := True;

  // calculate the minimum text offset from left
  if FBitmap <> nil then
    cMinLeft := cBitmapX + max(32, FBitmap.Width) + cLabelSpacing
  else
    cMinLeft := 0;

  // calculate required width for the text
  reqWidth := cMinLeft + TextBox.Right;

  // if buttons require more space than the text, center the text
  // as much as possible
  if reqWidth < reqBtnWidth then
  begin
    reqWidth := reqBtnWidth;
    TextLeft := max(cMinLeft, cLabelSpacing + (reqWidth - TextBox.Right) div 2);
  end
  else
    TextLeft := (cMinLeft + reqWidth - TextBox.Right) div 2;

  // position the text
  OffsetRect(TextBox, TextLeft, cLabelSpacing);

  // calculate the height of the text+icon
  reqHeight:= max(TextBox.Bottom, 32);
  if (FBitmap <> nil) and (FBitmap.Height > reqHeight) then
    reqHeight := FBitmap.Height;

  // set size of form
  SetBounds(Left, Top, reqWidth + 2 * cLabelSpacing,
        3 * cLabelSpacing + reqHeight + MinBtnHeight);

  // calculate the left of the buttons
  ButtonLeft := ((reqWidth - reqBtnWidth) div 2) + cLabelSpacing;
  ButtonTop := reqHeight + 2*cLabelSpacing;

  // position buttons and activate default
  for i := 0 to ComponentCount-1 do
  begin
    if (Components[i] is TBitBtn) then
    begin
      CurButton := TBitBtn(Components[i]);
      CurBtnSize := GetButtonSize(CurButton);
      CurButton.SetBounds(ButtonLeft, ButtonTop, CurBtnSize.X, CurBtnSize.Y);
      inc(ButtonLeft, CurButton.Width + cBtnDist);
      
      if (CurButton.Default) then
      begin
        ActiveControl := CurButton;
        DefaultControl := CurButton;
      end;
    end;
  end;
end;

{
  Executes the layout of TPromptDialog for small devices

  Two layout options are present, both to optimize space if
  only 1 button is present and also to allow many buttons.

  If only 1 button is present:

  ====================
  Caption
  ====================
        Button 1        > button in the middle
   Some text and so on
   ...       ...
   ...       ...
  ====================

  If more buttons are present:

  ====================
  Caption
  ====================
   Icon      Button 1
   Some text Button 2
   ...       ...
   ...       ...
  ====================

   <       >           cTextWidth
             <      >  cButtonWidth
            <>         cSpacing
}
procedure TPromptDialog.LayoutDialogSmallDevice;
Const
  cDialogWidth = 200;
  cDialogHalfWidth = cDialogWidth div 2;
  cTextWidth = 100;
  cHorizontalSpacing = 5;
  cVerticalSpacing = 5;
  cButtonWidth = cDialogWidth - cTextWidth - 3 * cHorizontalSpacing;
  cOneButtonTextWidth = cTextWidth + cHorizontalSpacing + cButtonWidth;
var
  i        : integer;
  CurButton: TBitBtn;

  ButtonLeft, ButtonTop: Integer;

  MinHeightForText,
   MinHeightForButtons,
   reqHeight: Integer;
begin
  // Create buttons without positioning and
  // Calculate the minimum size for the buttons
  // First thing so that ComponentCount is updated
  MinHeightForButtons := CreateButtons(True, cVerticalSpacing);

  // calculate the width & height we need to display the Message

  // calculate the needed size for the text
  if ComponentCount = 1 then { one button layout }
    TextBox := Rect(0, 0, cOneButtonTextWidth, Screen.Height - 100)
  else
    TextBox := Rect(0, 0, cTextWidth, Screen.Height - 100);

  DrawText(Canvas.Handle, PChar(MSG), Length(MSG),
    TextBox, DT_WORDBREAK or DT_CALCRECT);

  // calculate the height of the text+icon
  MinHeightForText := TextBox.Bottom;

  if ComponentCount = 1 then { one button layout }
  begin
    TextBox.Top := 2*cVerticalSpacing + MinHeightForButtons;
    Inc(TextBox.Bottom, TextBox.Top);
    TextBox.Left := cHorizontalSpacing;
    TextBox.Right := cOneButtonTextWidth + TextBox.Left;
  end
  else
  begin
    TextBox.Top := cVerticalSpacing;
    Inc(TextBox.Bottom, TextBox.Top);
    TextBox.Left := cHorizontalSpacing;
    TextBox.Right := cTextWidth + TextBox.Left;
  end;

  reqHeight := Max(MinHeightForText, MinHeightForButtons);

  if ComponentCount = 1 then { one button layout }
  begin
    // set size of form
    Height := (TextBox.Bottom - TextBox.Top) + 3*cVerticalSpacing + MinHeightForButtons;
    Width := 200;

    // position buttons and activate default
    if (Components[0] is TBitBtn) then
    begin
      CurButton:=TBitBtn(Components[0]);
      CurButton.Left := cDialogHalfWidth - cButtonWidth div 2;
      CurButton.Top := cVerticalSpacing;
      CurButton.Width := cButtonWidth;
      if (CurButton.Default) then
      begin
        ActiveControl:=CurButton;
        DefaultControl:=CurButton;
      end;
    end;
  end
  else
  begin
    // set size of form
    Height := reqHeight + cVerticalSpacing;
    Width := 200;

    // calculate the left of the buttons
    ButtonLeft := cTextWidth + 2 * cHorizontalSpacing;
    ButtonTop := cVerticalSpacing;

    // position buttons and activate default
    for i:=0 to ComponentCount-1 do
    begin
      if (Components[i] is TBitBtn) then
      begin
        CurButton:=TBitBtn(Components[i]);
        CurButton.Left := ButtonLeft;
        CurButton.Top := ButtonTop;
        CurButton.Width := cButtonWidth;

        inc(ButtonTop, CurButton.Height + cVerticalSpacing);

        if (CurButton.Default) then
        begin
          ActiveControl:=CurButton;
          DefaultControl:=CurButton;
        end;
      end;
    end;
  end;

  // We need to avoid a too high dialog which would go out
  // of the screen and be maybe impossible to close
  if Height > Screen.Height - 50 then
    Height := Screen.Height - 50;
end;


function ShowPromptDialog(const DialogCaption,
  DialogMessage: String;
  DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint;
  UseDefaultPos: boolean;
  X, Y: Longint): Longint;
var
  theModalResult: longint;
begin
  with TPromptDialog.CreateMessageDialog (DialogCaption, DialogMessage,
    DialogType, Buttons, ButtonCount, DefaultIndex)
  do
    try
      if not UseDefaultPos then
      begin
        Position := poDesigned;
        Left := X;
        Top := Y;
      end;
      theModalResult := ShowModal;
      case theModalResult of
        -1 : Result := EscapeResult
      else
        Result := DialogResult[theModalResult];
      end;
    finally
      Free;
    end;
end;

function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TForm;
var
  PDlg: TPromptDialog;
  Btns: PLongInt;
  CancelValue, DefaultIndex, ButtonCount: Longint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount, False, mbOk);
  PDlg := TPromptDialog.CreateMessageDialog('', Msg, DialogIds[DlgType], Btns, ButtonCount, DefaultIndex);
  Result := TForm(PDlg);
  ReallocMem(Btns, 0);
end;

type

  { TQuestionDlg }

  TQuestionDlg = class(TForm)
  private
    FButtons: TList;
    FBitmap: TCustomBitmap;
    FBitmapX, FBitmapY: Integer;
  public
    TextBox : TRect;
    TextStyle : TTextStyle;
    MessageTxt: String;
    constructor CreateQuestionDlg(const aCaption, aMsg: string;
      DlgType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint);
    destructor Destroy; override;
    procedure Paint; override;
    procedure LayoutDialog;
    function ShowModal: TModalResult; override;
  end;

{ TQuestionDlg }

procedure TQuestionDlg.Paint;
var
  UseMaskHandle: HBitmap;
begin
  inherited Paint;
  // draw the text
  Canvas.Brush := Brush;
  Canvas.TextRect(TextBox, TextBox.Left, TextBox.Top, MessageTxt, TextStyle);
  // draw the icon
  if Assigned (FBitmap) then
  begin
    UseMaskHandle := FBitmap.MaskHandle;
    MaskBlt(Canvas.GetUpdatedHandle([csHandleValid]),
            cBitmapX, cBitmapY, FBitmap.Width, FBitmap.Height,
            FBitmap.Canvas.GetUpdatedHandle([csHandleValid]),
            0, 0, UseMaskHandle, 0, 0);
  end;
end;

procedure TQuestionDlg.LayoutDialog;
const
  cBtnDist = 10; // distance between buttons
  cLabelSpacing = 8; // space around label
var
  Flags: Cardinal;
  i: Integer;
  CurButton: TBitBtn;
  reqBtnWidth: Integer;
  reqWidth: LongInt;
  cMinLeft: Integer;
  ButtonLeft: Integer;
  reqHeight: LongInt;
  CurBtnPos: Integer;
  CurBtnSize: TPoint;
  MinBtnWidth: Integer; // minimum width for a single button
  MinBtnHeight: Integer; // minimum height for a single button

  function GetButtonSize(AButton: TBitBtn): TPoint;
  begin
    AButton.HandleNeeded;
    TBitBtnAccess(AButton).CalculatePreferredSize(Result.x, Result.y, True);
    if MinBtnHeight < Result.y then
      MinBtnHeight := Result.y
    else
    if Result.y < MinBtnHeight then
      Result.y := MinBtnHeight;
    if Result.x < MinBtnWidth then
      Result.x := MinBtnWidth;
  end;

begin
  FillChar(TextStyle, SizeOf(TTextStyle), 0);

  with TextStyle do
  begin
    Clipping   := True;
    Wordbreak  := True;
    SystemFont := True;
    Opaque     := False;
  end;

  // calculate the width & height we need to display the Message
  if MessageTxt = '' then
    MessageTxt := '   ';
  TextBox := Rect(0, 0, Screen.Width div 2, Screen.Height - 100);
  Flags := DT_CalcRect or DT_WordBreak;
  SelectObject(Canvas.Handle, Screen.SystemFont.Reference.Handle);
  DrawText(Canvas.Handle, PChar(MessageTxt), Length(MessageTxt), TextBox, Flags);

  // calculate the width we need to display the buttons
  MinBtnWidth:=Max(25,MinimumDialogButtonWidth);
  MinBtnHeight:=Max(15,MinimumDialogButtonHeight);
  reqBtnWidth := 0;

  if FButtons <> nil then
    for i := 0 to FButtons.Count - 1 do
    begin
      CurButton := TBitBtn(FButtons[i]);
      CurBtnSize:=GetButtonSize(CurButton);
      if i > 0 then Inc(reqBtnWidth, cBtnDist);
      Inc(reqBtnWidth, CurBtnSize.X);
    end;

  // calculate the width of the dialog
  if FBitmap <> nil then
    cMinLeft := cLabelSpacing + max(20, FBitmap.Width) + cLabelSpacing
  else
    cMinLeft := cLabelSpacing;
  reqWidth:= reqBtnWidth + 2 * cBtnDist;
  if reqWidth < (TextBox.Right + cMinLeft + cLabelSpacing) then
    reqWidth:= TextBox.Right + cMinLeft + cLabelSpacing;
  ButtonLeft := ((reqWidth - reqBtnWidth) div 2);

  // calculate the height of the dialog
  reqHeight:= TextBox.Bottom;
  if (FBitmap <> nil) and (FBitmap.Height > reqHeight) then
    reqHeight := FBitmap.Height;
  inc(reqHeight, CurBtnSize.Y + 3 * cLabelSpacing);

  // calculate the text position
  OffsetRect(TextBox,
             ((reqWidth-cMinLeft-TextBox.Right-cLabelSpacing) div 2) + cMinLeft,
             cLabelSpacing);

  // calculate the icon position
  if FBitmap <> nil then
  begin
    FBitmapX := cLabelSpacing;
    FBitmapY := (reqHeight - CurBtnSize.Y - FBitmap.Height - cLabelSpacing) div 2;
  end;

  // set size of form
  SetBounds((Screen.Width - reqWidth-10) div 2, (Screen.Height - reqHeight-50) div 2,
         reqWidth, reqHeight);

  // position buttons
  CurBtnPos := ButtonLeft;
  if FButtons <> nil then
    for i := 0 to FButtons.Count-1 do
    begin
      CurButton := TBitBtn(Components[i]);
      CurBtnSize := GetButtonSize(CurButton);
      CurButton.SetBounds(CurBtnPos, ClientHeight - CurBtnSize.Y - cLabelSpacing,
                          CurBtnSize.X, CurBtnSize.Y);
      inc(CurBtnPos, CurButton.Width + cBtnDist);
    end;
end;

function TQuestionDlg.ShowModal: TModalResult;
begin
  LayoutDialog;
  Result := inherited ShowModal;
end;

constructor TQuestionDlg.CreateQuestionDlg(const aCaption, aMsg: string;
  DlgType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint);
var
  i: integer;
  CurBtn: TDialogButton;
  NewButton: TBitBtn;
  NewKind: TBitBtnKind;
  NewCaption: String;
begin
  inherited CreateNew(nil, 1);
  PopupMode := pmAuto;
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  MessageTxt := ConvertLineEndings(aMsg);
  HelpContext := HelpCtx;

  // Initialize TextStyle
  FillChar(TextStyle, SizeOf(TTextStyle), 0);
  with TextStyle do
  begin
    Clipping   := True;
    Wordbreak  := True;
    SystemFont := True;
    Opaque     := False;
  end;

  FBitmap := nil;
  NewCaption := ACaption;
  case DlgType of
    idDialogConfirm, idDialogInfo, idDialogWarning, idDialogError:
      begin
        FBitmap := GetDialogIcon(DlgType);
        if NewCaption = '' then
          NewCaption := GetDialogCaption(DlgType);
      end;
    else
      FBitmap := GetDialogIcon(idDialogInfo);
  end;
  if NewCaption = '' then
    NewCaption := Application.Title;
  Caption := NewCaption;

  for i := 0 to Buttons.Count - 1 do
  begin
    CurBtn := Buttons[i];
    // get button kind
    case CurBtn.ModalResult of
      mrOk:       NewKind := bkOK;
      mrCancel:   NewKind := bkCancel;
      mrYes:      NewKind := bkYes;
      mrNo:       NewKind := bkNo;
      mrAbort:    NewKind := bkAbort;
      mrRetry:    NewKind := bkRetry;
      mrIgnore:   NewKind := bkIgnore;
      mrAll:      NewKind := bkAll;
      mrNoToAll:  NewKind := bkNoToAll;
      mrYesToAll: NewKind := bkYesToAll;
    else
      NewKind := bkCustom;
    end;

    NewButton := TBitBtn.Create(Self);
    with NewButton do
    begin
      AutoSize := False;
      Anchors := [akLeft, akBottom];
      ModalResult := CurBtn.ModalResult;
      Layout := blGlyphLeft;
      Kind := NewKind;
      Caption := CurBtn.Caption;
      Parent := Self;
      Default := CurBtn.Default;
      Cancel := CurBtn.Cancel;
    end;
    if FButtons = nil then
      FButtons := TList.Create;
    FButtons.Add(NewButton);
  end;
end;

destructor TQuestionDlg.Destroy;
begin
  FreeAndNil(FButtons);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;


function ShowQuestionDialog(const aCaption, aMsg: string; DlgType: LongInt;
  Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
{ Show a dialog with aCaption as Title, aMsg as Text, DlgType as Icon,
  HelpCtx as Help context and Buttons to define the shown buttons and their
  TModalResult.
  Buttons is a list of TModalResult and strings. For each number a button is
  created. To set a custom caption, add a string after a button.
  The default TModalResults defined in controls.pp (mrNone..mrLast) don't need
  a caption. The default captions will be used.

  Examples for Buttons:
    [mrOk,mrCancel,'Cancel now',mrIgnore,300,'Do it','IsDefault']
    This will result in 4 buttons:
      'Ok' returning mrOk
      'Cancel now' returning mrCancel
      'Ignore' returning mrIgnore
      'Do it' returning 300. This will be the default button (focused)

}
var
  QuestionDialog: TQuestionDlg;
begin
  QuestionDialog := TQuestionDlg.CreateQuestionDlg(aCaption, aMsg, DlgType, Buttons, HelpCtx);
  try
    Result := QuestionDialog.ShowModal;
  finally
    QuestionDialog.Free;
  end;
end;

function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: array of const; HelpCtx: Longint): TModalResult;
var
  DialogButtons: TDialogButtons;
  i: integer;
  CurBtnValue: TModalResult;
  CurBtnCaption, CurOptions: String;
  HasOptions: Boolean;
  IsDefault: Boolean;
  NewButton: TDialogButton;
begin
  DialogButtons := TDialogButtons.Create(TDialogButton);
  try
    i := Low(Buttons);
    while i <= High(Buttons) do
    begin
      if Buttons[i].VType <> vtInteger then
        raise Exception.Create('TQuestionDlg.CreateQuestionDlg integer expected at '
          +IntToStr(i)+' but VType='+IntToStr(ord(Buttons[i].VType))+' found.');
      if Buttons[i].VType = vtInteger then
      begin
        // get TModalResult
        CurBtnValue := Buttons[i].VInteger;
        inc(i);

        // get button caption
        CurBtnCaption := '';
        if (i <= High(Buttons)) then
        begin
          case Buttons[i].VType of
            vtString: CurBtnCaption := Buttons[i].VString^;
            vtAnsiString: CurBtnCaption := AnsiString(Buttons[i].VAnsiString);
            vtChar: CurBtnCaption := Buttons[i].VChar;
            vtPChar: CurBtnCaption := Buttons[i].VPChar;
            vtPWideChar: CurBtnCaption := Buttons[i].VPWideChar;
            vtWideChar: CurBtnCaption := AnsiString(Buttons[i].VWideChar);
            vtWidestring: CurBtnCaption := AnsiString(WideString(Buttons[i].VWideString));
          else
            dec(i); // prevent the following inc(i)
          end;
          inc(i);
        end;

        // get options
        CurOptions := '';
        IsDefault := False;
        if (i <= High(Buttons)) then
        begin
          HasOptions := True;
          case Buttons[i].VType of
            vtString: CurOptions := Buttons[i].VString^;
            vtAnsiString: CurOptions := AnsiString(Buttons[i].VAnsiString);
            vtChar: CurOptions := Buttons[i].VChar;
            vtPChar: CurOptions := Buttons[i].VPChar;
            vtPWideChar: CurOptions := Buttons[i].VPWideChar;
            vtWideChar: CurOptions := AnsiString(Buttons[i].VWideChar);
            vtWidestring: CurOptions := AnsiString(WideString(Buttons[i].VWideString));
          else
            HasOptions := False;
          end;
          if HasOptions then
          begin
            if SysUtils.CompareText(CurOptions,'isdefault')<>0 then
              raise Exception.Create('TQuestionDlg.CreateQuestionDlg option expected at '
                +IntToStr(i)+' but "'+CurOptions+'" found.');
            if DialogButtons.DefaultButton <> nil then
              raise Exception.Create('TQuestionDlg.CreateQuestionDlg only one button can be default');
            IsDefault := True;
            inc(i);
          end;
        end;

        if CurBtnCaption = '' then
        begin
          // find default caption
          case CurBtnValue of
            mrOk       : CurBtnCaption := rsmbOk;
            mrCancel   : CurBtnCaption := rsmbCancel;
            mrYes      : CurBtnCaption := rsmbYes;
            mrNo       : CurBtnCaption := rsmbNo;
            mrAbort    : CurBtnCaption := rsmbAbort;
            mrRetry    : CurBtnCaption := rsmbRetry;
            mrIgnore   : CurBtnCaption := rsmbIgnore;
            mrAll      : CurBtnCaption := rsmbAll;
            mrYesToAll : CurBtnCaption := rsmbYesToAll;
            mrNoToAll  : CurBtnCaption := rsmbNoToAll;
          end;
        end;
        if CurBtnCaption = '' then
          raise Exception.Create('TQuestionDlg.Create: missing Button caption '+dbgs(i-1));
        NewButton := DialogButtons.Add;
        with NewButton do
        begin
          Caption := CurBtnCaption;
          ModalResult := CurBtnValue;
        end;
        if IsDefault then
          DialogButtons.DefaultButton := NewButton;
      end
      else
        raise Exception.Create('TQuestionDlg.Create: invalid Buttons parameter '+dbgs(i));
    end;
    if DialogButtons.DefaultButton = nil then
      DialogButtons.DefaultButton := DialogButtons.FindButton([mrYes, mrOk, mrYesToAll, mrAll, mrRetry, mrCancel, mrNo, mrNoToAll, mrAbort, mrIgnore]);
    DialogButtons.CancelButton := DialogButtons.FindButton([mrAbort, mrCancel, mrNo, mrIgnore, mrNoToAll, mrYes, mrOk, mrRetry, mrAll, mrYesToAll]);
    Result := WidgetSet.AskUser(aCaption, aMsg, DialogIds[DlgType], DialogButtons, HelpCtx);
  finally
    DialogButtons.Free;
  end;
end;

function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: array of const; const HelpKeyword: string): TModalResult;
begin
  // TODO: handle HelpKeyword
  Result := QuestionDlg(aCaption, aMsg, DlgType, Buttons, 0);
end;

// included by dialogs.pp
