// included by dialogs.pp

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
type
  TPromptDialog = class(TForm)
    procedure PromptDialogKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
  public
    TheDefaultIndex : Longint;

    FBitmap : TBitmap;
    FLabel : TLabel;
    MSG : AnsiString;
    NumButtons : Longint;
    Buttons : PLongint;

    TextBox : TRect;
    TextStyle : TTextStyle;

    procedure LayoutDialog;
    procedure Paint; override;
    constructor CreateMessageDialog(const ACaption, aMsg: string;
      DialogType : longint; TheButtons: PLongint; ButtonCount, DefaultIndex : Longint);
    Destructor Destroy; Override;
  end;

procedure TPromptDialog.PromptDialogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  OldFocusControl, NewFocusControl: TWinControl;
  i: integer;
begin
  if (Key=VK_Escape) then
    ModalResult := -1;
  if (Key=VK_LEFT) or (Key=VK_RIGHT) then begin
    // focus the next button to the left or right
    
    // search old focused button
    OldFocusControl:=FindControl(LCLLinux.GetFocus);
    if (OldFocusControl=nil) or (GetParentForm(OldFocusControl)<>Self)
    or (not (OldFocusControl is TButton)) then
    begin
      OldFocusControl:=nil;
      for i:=0 to ComponentCount-1 do
        if (Components[i] is TButton) and (TButton(Components[i]).Default) then
        begin
          OldFocusControl:=TButton(Components[i]);
          break;
        end;
    end;
    
    // find next focused button
    if (OldFocusControl<>nil) then begin
      i:=ComponentCount-1;
      while i>=0 do begin
        if Components[i]=OldFocusControl then
          break
        else
          dec(i);
      end;
      if i<0 then exit;
      NewFocusControl:=nil;
      repeat
        if Key=VK_LEFT then begin
          dec(i);
          if i<0 then i:=ComponentCount-1;
        end else begin
          inc(i);
          if i>=ComponentCount then i:=0;
        end;
        if Components[i] is TButton then begin
          NewFocusControl:=TWinControl(Components[i]);
          break;
        end;
      until false;
      if NewFocusControl.HandleAllocated then begin
        LCLLinux.SetFocus(NewFocusControl.Handle);
        Key:=VK_UNKNOWN;
      end;
    end;
  end;
end;

const
  DialogResult : Array[mrNone..mrYesToAll] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll);

  DialogButtonKind : Array[idButtonOK..idButtonNoToAll] of TBitBtnKind = (
    bkOk, bkCancel, bkHelp, bkYes, bkNo, bkClose, bkAbort, bkRetry,
    bkIgnore, bkAll, bkCustom, bkCustom);

  DialogButtonText : Array[idButtonOK..idButtonNoToAll] of String = (
    rsmbOk, rsmbCancel, rsmbHelp, rsmbYes, rsmbNo, rsmbClose, rsmbAbort,
    rsmbRetry, rsmbIgnore, rsmbAll, rsmbYesToAll, rsmbNoToAll);

  DialogCaption : Array[idDialogWarning..idDialogConfirm] of String = (
    rsMtWarning, rsMtError, rsMtInformation, rsMtConfirmation);


procedure TPromptDialog.Paint;
begin
  Inherited Paint;
  Canvas.TextRect(TextBox, 0, 0, MSG, TextStyle);
  if assigned (FBitmap) then
    Canvas.CopyRect(Bounds(cBitmapX, cBitmapY,FBitmap.Width,FBitmap.Height),
                    FBitmap.Canvas, Rect(0,0,FBitmap.Width,FBitmap.Height));
end;

constructor TPromptDialog.CreateMessageDialog(const ACaption, aMsg: string;
  DialogType : longint; TheButtons: PLongint; ButtonCount, DefaultIndex : Longint);
begin
  inherited Create (Application);

  OnKeyDown :=@PromptDialogKeyDown;

  ControlStyle:= ControlStyle-[csSetCaption];
  BorderStyle := bsDialog;
  Position    := poScreenCenter;
  Width       := 200;
  Height      := 100;
  MSG         := AMSG;
  Buttons     := nil;
  FBitmap := nil;
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := '';

  Case DialogType of
    idDialogConfirm,
    idDialogInfo,
    idDialogWarning,
    idDialogError :
      begin
        FBitmap := TBitmap.Create;
        FBitmap.Handle := LoadStockPixmap(DialogType);
        If ACaption <> '' then
          Caption := ACaption
        else
          Caption := DialogCaption[DialogType];
      end;
    else begin
      FBitmap := TBitmap.Create;
      FBitmap.Handle := LoadStockPixmap(idDialogInfo);
      If ACaption <> '' then
        Caption := ACaption
      else
        Caption := Application.Title;
    end
  end;
  NumButtons := ButtonCount;
  Buttons := TheButtons;

  If (DefaultIndex >= ButtonCount) or
    (DefaultIndex < 0)
  then
    TheDefaultIndex := 0
  else
    theDefaultIndex := DefaultIndex;

  LayoutDialog;
end;

Destructor TPromptDialog.Destroy;
begin
  FBitmap.Free;
  inherited destroy;
end;

procedure TPromptDialog.LayoutDialog;
Const
  AVGBuffer : PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890()|_ ';
  cBtnCalcWidth  = 50;
  cBtnCalcHeight = 13;
  cBtnCalcSpace   = 4;
var
  curBtn      : Longint; // variable to loop through TMsgDlgButtons
  cBtnWidth,
  cBtnHeight,
  curBtnWidth,
  cMinLeft,
  cBtnDist,
  ButtonLeft  : integer;    // left position of button(s)
  reqBtnWidth : integer;    // width neccessary to display buttons
  reqWidth, reqHeight : integer;    // width and height neccessary to display all
  i        : integer;
  ButtonIndex : integer;
  Avg : TPoint;
begin
  FillChar(TextStyle, SizeOf(TextStyle), 0);

  With TextStyle do begin
    Clipping   := True;
    Wordbreak  := True;
    SystemFont := True;
  end;

  // calculate the width & height we need to display the Message
  If MSG = '' then
    MSG := '   ';
  TextBox := Rect(0,0, Screen.Width div 2,Screen.Height - 100);
  SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FONT));
  DrawText(Canvas.Handle, PChar(MSG), Length(MSG),
    TextBox, DT_WORDBREAK or DT_INTERNAL or DT_CALCRECT);

  // calculate the width we need to display the buttons
  SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FONT));
  GetTextExtentPoint(Canvas.Handle,AVGBuffer,StrLen(AVGBuffer),TSize(AVG));
  AVG.X := AVG.X div 52;
  reqBtnWidth := 0;
  cBtnWidth := (cBtnCalcWidth*Avg.X) div 5;
  cBtnHeight := (cBtnCalcHeight*AVG.Y) div 8;
  cBtnDist := (cBtnCalcSpace * Avg.X) div 4;
  for curBtn := 0 to NumButtons - 1 do
  begin
    If (Buttons[curBtn] >= Low(DialogButtonKind)) and
      (Buttons[curBtn] <= High(DialogButtonKind))
    then begin
      curBtnWidth := Canvas.TextWidth(DialogButtonText[Buttons[curBtn]]);
      if curBtnWidth > cBtnWidth then
        cBtnWidth := curBtnWidth;
      Inc(reqBtnWidth, cBtnWidth + cBtnDist)
    end;
  end;
  if reqBtnWidth > 0 then Dec(reqBtnWidth, cBtnDist);
  Inc(cBtnDist, cBtnWidth);

  // patch positions to center label and buttons
  reqWidth:= reqBtnWidth;

  If FBitmap <> nil then
    cMinLeft := cBitmapX + 32 + cLabelSpacing
  else
    cMinLeft := cLabelSpacing;

  if reqWidth < (TextBox.Right + cMinLeft) then reqWidth:= TextBox.Right + cMinLeft;

  ButtonLeft := ((reqWidth - reqBtnWidth) div 2) + cLabelSpacing;

  reqHeight:= TextBox.Bottom;

  if reqHeight < 32 then reqHeight := 32;

  OffsetRect(TextBox, ((reqWidth - cMinLeft - TextBox.Right) div 2) + cMinLeft, cLabelSpacing);

  // set size of form
  SetBounds(Left, Top, reqWidth + 2 * cLabelSpacing,
        3 * cLabelSpacing + reqHeight + cBtnHeight);

  // create the buttons
  ButtonIndex := -1;
  for curBtn := 0 to NumButtons - 1 do begin
    If (Buttons[curBtn] >= Low(DialogButtonKind)) and
      (Buttons[curBtn] <= High(DialogButtonKind))
    then begin
      inc(ButtonIndex);

      with TBitBtn.Create(Self) do begin
        Parent:= Self;
        SetBounds (ButtonLeft, 2 * cLabelSpacing + reqHeight, cBtnWidth, cBtnHeight);
        inc(ButtonLeft, cBtnDist);
        Layout  := blGlyphLeft;
        OnKeyDown := @PromptDialogKeyDown;
        Case Buttons[curBtn] of
          idButtonYesToAll,
          idButtonNoToAll :
            begin
              Glyph.Handle := LoadStockPixmap(Buttons[curBtn]);
              If Buttons[curBtn] = idButtonYesToAll then begin
                ModalResult := mrYesToAll;
                Caption := rsmbYesToAll;
              end
              else begin
                ModalResult := mrNoToAll;
                Caption := rsmbNoToAll;
              end;
            end;
          else
            Kind := DialogButtonKind[Buttons[curBtn]];
        end;
        if ButtonIndex = TheDefaultIndex then Default := true;
        Visible:=true;
      end;
    end;
  end;

  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TBitBtn) and (TBitBtn(Components[i]).Default) then begin
      TBitBtn(Components[i]).SetFocus;
      break;
    end;
  end;
end;

Function ShowPromptDialog(const DialogCaption,
  DialogMessage : String;
  DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint;
  UseDefaultPos: boolean;
  X, Y : Longint) : Longint;
var
  theModalResult : longint;
begin
  with TPromptDialog.CreateMessageDialog (DialogCaption, DialogMessage,
    DialogType, Buttons, ButtonCount, DefaultIndex)
  do
    try
      if not UseDefaultPos then begin
        Position := poDesigned;
        Left := X;
        Top := Y;
      end;
      theModalResult := ShowModal;
      Case theModalResult of
        -1 : Result := EscapeResult
        else
          Result := DialogResult[theModalResult];
      end;
    finally
      Free;
    end;
end;


// included by dialogs.pp

{
   $Log$
   Revision 1.2  2002/11/05 21:21:36  lazarus
   MG: fixed moving button with LEFT and RIGHT in messagedlgs

   Revision 1.1  2002/10/25 10:06:34  lazarus
   MG: broke interfacebase uses circles

}
