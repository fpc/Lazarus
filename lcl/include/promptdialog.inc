{%MainUnit ../dialogs.pp}

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
const
  DialogResult : Array[mrNone..mrYesToAll] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll);

  DialogButtonKind : Array[idButtonOK..idButtonNoToAll] of TBitBtnKind = (
    bkOk, bkCancel, bkHelp, bkYes, bkNo, bkClose, bkAbort, bkRetry,
    bkIgnore, bkAll, bkCustom, bkCustom);

  {DialogButtonText : Array[idButtonOK..idButtonNoToAll] of String = (
    rsmbOk, rsmbCancel, rsmbHelp, rsmbYes, rsmbNo, rsmbClose, rsmbAbort,
    rsmbRetry, rsmbIgnore, rsmbAll, rsmbYesToAll, rsmbNoToAll);}

  {DialogCaption : Array[idDialogWarning..idDialogConfirm] of String = (
    rsMtWarning, rsMtError, rsMtInformation, rsMtConfirmation);}

type
  TPromptDialog = class(TForm)
    procedure PromptDialogKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);

    function GetDialogCaption(idDiag : Integer) : String;
    function GetDialogButtonText(idBut : Integer): string;
  public
    TheDefaultIndex : Longint;

    FBitmap : TBitmap;
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
    OldFocusControl:=FindOwnerControl(LCLIntf.GetFocus);
    if (OldFocusControl=nil) or (GetParentForm(OldFocusControl)<>Self)
    or (not (OldFocusControl is TCustomButton)) then
    begin
      OldFocusControl:=nil;
      for i:=0 to ComponentCount-1 do
        if (Components[i] is TCustomButton)
        and (TCustomButton(Components[i]).Default) then
        begin
          OldFocusControl:=TCustomButton(Components[i]);
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
        if Components[i] is TCustomButton then begin
          NewFocusControl:=TWinControl(Components[i]);
          break;
        end;
      until false;
      ActiveControl:=NewFocusControl;
      Key:=VK_UNKNOWN;
    end;
  end;
end;

{** Return the localized or not title of dialog}
function TPromptDialog.GetDialogCaption(idDiag: Integer): String;
begin
  Result:='?';
  Case idDiag of
    idDialogWarning : Result:=rsMtWarning;
    idDialogError   : Result:=rsMtError;
    idDialogInfo    : Result:=rsMtInformation;
    idDialogConfirm : Result:=rsMtConfirmation;
  end;
end;

{** Return the text associed a an standard button}
function TPromptDialog.GetDialogButtonText(idBut: Integer): string;
begin
  Result:='';
  Case idBut of
    idButtonOk       : Result:=rsmbOk;
    idButtonCancel   : Result:=rsmbCancel;
    idButtonHelp     : Result:=rsmbHelp;
    idButtonYes      : Result:=rsmbYes;
    idButtonNo       : Result:=rsmbNo;
    idButtonClose    : Result:=rsmbClose;
    idButtonAbort    : Result:=rsmbAbort;
    idButtonRetry    : Result:=rsmbRetry;
    idButtonIgnore   : Result:=rsmbIgnore;
    idButtonAll      : Result:=rsmbAll;
    idButtonYesToAll : Result:=rsmbYesToAll;
    idButtonNoToAll  : Result:=rsmbNoToAll;
  end;
end;

procedure TPromptDialog.Paint;
begin
  Inherited Paint;
  Canvas.Brush := Brush;
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
          Caption := GetDialogCaption(DialogType);
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
  FillChar(TextStyle, SizeOf(TTextStyle), 0);

  With TextStyle do
  begin
    Clipping   := True;
    Wordbreak  := True;
    SystemFont := True;
    Opaque     := False;
  end;

  // calculate the width & height we need to display the Message
  If MSG = '' then
    MSG := '   ';
  TextBox := Rect(0,0, Screen.Width div 2,Screen.Height - 100);
  SelectObject(Canvas.Handle, GetStockObject(DEFAULT_GUI_FONT));
  DrawText(Canvas.Handle, PChar(MSG), Length(MSG),
    TextBox, DT_WORDBREAK or DT_INTERNAL or DT_CALCRECT);

  // calculate the width we need to display the buttons
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
    then
    begin
      curBtnWidth := Canvas.TextWidth(GetDialogButtonText(Buttons[curBtn]));
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
    cMinLeft := cBitmapX + max(32,FBitmap.Width) + cLabelSpacing
  else
    cMinLeft := cLabelSpacing;

  if reqWidth < (TextBox.Right + cMinLeft) then reqWidth:= TextBox.Right + cMinLeft;

  ButtonLeft := ((reqWidth - reqBtnWidth) div 2) + cLabelSpacing;

  reqHeight:= max(TextBox.Bottom, 32);

  if (FBitmap <> nil) and (FBitmap.Height > reqHeight) then
    reqHeight := FBitmap.Height;

  OffsetRect(TextBox, ((reqWidth - cMinLeft - TextBox.Right) div 2) + cMinLeft, cLabelSpacing);

  // set size of form
  SetBounds(Left, Top, reqWidth + 2 * cLabelSpacing,
        3 * cLabelSpacing + reqHeight + cBtnHeight);

  // create the buttons
  ButtonIndex := -1;
  for curBtn := 0 to NumButtons - 1 do
  begin
    If (Buttons[curBtn] >= Low(DialogButtonKind)) and
      (Buttons[curBtn] <= High(DialogButtonKind))
    then
    begin
      inc(ButtonIndex);

      with TBitBtn.Create(Self) do
      begin
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
              If Buttons[curBtn] = idButtonYesToAll then
              begin
                ModalResult := mrYesToAll;
                Caption := rsmbYesToAll;
              end
              else
              begin
                ModalResult := mrNoToAll;
                Caption := rsmbNoToAll;
              end;
            end;
          else
            Kind := DialogButtonKind[Buttons[curBtn]];
        end;
	if Height < Glyph.Height + 5 then
	  Height := Glyph.Height + 5;
        if ButtonIndex = TheDefaultIndex then Default := true;
        Visible:=true;
      end;
    end;
  end;

  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TCustomBitBtn)
    and (TCustomBitBtn(Components[i]).Default) then
    begin
      ActiveControl:=TCustomBitBtn(Components[i]);
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
   Revision 1.16  2004/07/15 10:43:38  mattias
   added TCustomButton, TCustomBitBtn, TCustomSpeedButton

   Revision 1.15  2004/04/20 09:18:44  micha
   reserve enough space for glyph on button in message dialogs

   Revision 1.14  2004/04/10 17:58:57  mattias
   implemented mainunit hints for include files

   Revision 1.13  2004/03/06 17:12:19  mattias
   fixed CreateBrushIndirect

   Revision 1.12  2004/03/06 16:11:27  mattias
   fixed TextStyle in dialogs

   Revision 1.11  2004/02/17 00:32:25  mattias
   fixed TCustomImage.DoAutoSize fixing uninitialized vars

   Revision 1.10  2003/10/16 16:43:57  ajgenius
   fix opaque brush

   Revision 1.9  2003/10/15 20:33:37  ajgenius
   add csForm, start fixing Style matching for syscolors and fonts

   Revision 1.8  2003/09/18 09:21:03  mattias
   renamed LCLLinux to LCLIntf

   Revision 1.7  2003/08/27 08:14:37  mattias
   fixed system fonts for win32 intf

   Revision 1.6  2003/07/04 10:30:02  mattias
   removed unused label from Micha

   Revision 1.5  2003/06/23 09:42:09  mattias
   fixes for debugging lazarus

   Revision 1.4  2003/03/25 10:45:41  mattias
   reduced focus handling and improved focus setting

   Revision 1.3  2003/03/04 09:21:09  mattias
   added localization for env options from Olivier

   Revision 1.2  2002/11/05 21:21:36  lazarus
   MG: fixed moving button with LEFT and RIGHT in messagedlgs

   Revision 1.1  2002/10/25 10:06:34  lazarus
   MG: broke interfacebase uses circles

}
