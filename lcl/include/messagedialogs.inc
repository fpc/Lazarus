{******************************************************************************
                                  MessageDialogs
 ******************************************************************************

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
  current design flaws:

  - The actual design ignores any features provided by the underlying 
    widgetset. This is because the GTK libraries do not provide an
    abstract dialog class
	   
  Delphi compatibility:

   - the interface is almost like in delphi 5
     
  TODO:
    - calculate the size required for the dialog based on the 
      current font and the length of the text to be displayed
    - remove all those nasty constants (cBitmapWidth...)
    - use better strategy to set default button in function SetButtons 
    - Help-button    

}
type

  { TMessageBox
   
    Internal class used to build a MessageBox. 
  }
  TMessageBox = class(TForm)
    procedure MessageBoxKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
  private
    FBitmap : TBitmap;
    FLabel : TLabel;
    FDlgType : TMsgDlgType;
    FButtons : TMsgDlgButtons;
    FRelayoutNeeded: boolean;
    FUpdateCounter: integer;
    FMsgLines: TStringList;
    procedure SetMessage(const value : string); 
    function GetMessage: string; 
    procedure SetDialogType (const value : TMsgDlgType);
    procedure SetButtons (const value : TMsgDlgButtons);
    procedure ReLayout;
    function LineWidth(Index: integer): integer;
    function LineHeight: integer;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure FormActivate(Sender: TObject);
    property TheMessage : string read GetMessage write SetMessage;
    property DialogType : TMsgDlgType write SetDialogType;
    property Buttons : TMsgDlgButtons write SetButtons;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

const
  //
  //TODO: all the constants below should be replaced in the future
  //      their only purpose is to overcome some current design flaws &
  //      missing features in the GTK libraries
  //
  cBitmapX  = 10;      // x-position for bitmap in messagedialog
  cBitmapY  = 10;      // y-position for bitmap in messagedialog
  cBitmapWidth = 32;   // width of the dialogs icon
  cBitmapHeight= 32;   // height of the dialogs icon
  cLabelSpacing= 10;   // distance between icon & label

{------------------------------------------------------------------------------
  Method: TMessageBox.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for a MessageBox
 ------------------------------------------------------------------------------}
constructor TMessageBox.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
   
  FMsgLines   := TStringList.Create;
  FLabel      := TLabel.Create(Self);
  FLabel.Alignment:= taLeftJustify;
  FLabel.Parent:= Self;
  FLabel.Visible:= true;
  
  ControlStyle:= ControlStyle-[csSetCaption];
  BorderStyle := bsDialog;
  Position    := poScreenCenter;
  Width       := 200;
  Height      := 100;
  FDlgType    := mtInformation;
  FBitmap     := nil;
  FButtons    := [mbOk];
  FRelayoutNeeded := false;
  FUpdateCounter := 0;
  OnKeyDown:=@MessageBoxKeyDown;
  OnActivate:=@FormActivate;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.Destroy
  Params:  ---
  Returns: Nothing

  Destructor for a MessageBox
 ------------------------------------------------------------------------------}
destructor TMessageBox.Destroy;
begin
  FLabel.Free;
  FBitmap.Free;
  FMsgLines.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.FormActivate
  Params:  ---
  Returns: Nothing

  
 ------------------------------------------------------------------------------}
procedure TMessageBox.FormActivate(Sender: TObject);
var i: integer;
begin
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TBitBtn) and (TBitBtn(Components[i]).Default) then begin
  //    TBitBtn(Components[i]).SetFocus;
      break;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.Paint
  Params:  ---
  Returns: Nothing

  Draw the icon in the messagebox.
 ------------------------------------------------------------------------------}
procedure TMessageBox.Paint;
begin
  inherited Paint;
  if assigned (FBitmap) then
    Canvas.CopyRect(Bounds(cBitmapX, cBitmapY,cBitmapWidth,cBitmapHeight),
                    FBitmap.Canvas,
                    Rect(0,0,cBitmapWidth,cBitmapHeight));
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.LineWidth
  Params:  Index - index of the line to check the width
  Returns: Text width of the line
 ------------------------------------------------------------------------------}
function TMessageBox.LineWidth(Index: integer): integer;
begin
  if (Index>=0) and (Index<FMsgLines.Count) then
    Result:=Canvas.TextWidth(FMsgLines[Index])
  else
    Result:=0;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.LineHeight
  Params:  nothing
  Returns: Text height of the line
 ------------------------------------------------------------------------------}
function TMessageBox.LineHeight: integer;
begin
  Result:=Canvas.TextHeight('ABCDEFGHIJKLMNOPQRSTUVWXYZgp09') + 4;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.MessageBoxKeyDown

  OnKeyDown Event handler for messages dialogs.
  The 'Escape' key has the same meaning as a Cancel or Abort click.
 ------------------------------------------------------------------------------}
procedure TMessageBox.MessageBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_Escape) then begin
    if mbCancel in FButtons then
      ModalResult:=mrCancel
    else if mbAbort in FButtons then
      ModalResult:=mrAbort;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.SetMessage
  Params:  value - text to be displayed in the message box
  Returns: Nothing

  Sets the Text in MessageBox
 ------------------------------------------------------------------------------}
procedure TMessageBox.SetMessage(const Value : string);
begin
  if Value=FMsgLines.Text then exit;
  FMsgLines.Text:=Value;
  FRelayoutNeeded:=true;
  ReLayout;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.GetMessage
  Params: Nothing
  Returns: Message Text

  Sets the Text in MessageBox
 ------------------------------------------------------------------------------}
function TMessageBox.GetMessage: string;
begin
  if FMsgLines<>nil then
    Result:=FMsgLines.Text
  else
    Result:='';
end;
{------------------------------------------------------------------------------
  Method: TMessageBox.SetDialogType
  Params:  value - the type of dialog to be shown
  Returns: Nothing

  Sets the type of dialog.
 ------------------------------------------------------------------------------}
procedure TMessageBox.SetDialogType (const Value : TMsgDlgType);
var DefaultCaption: boolean;
begin
  if (value = FDlgType) and (assigned (FBitmap)) then exit;
  DefaultCaption:=(Caption=cMtCaption[FDlgType]) or (Caption='');
  FDlgType := value;
  FBitmap.Free;
  FBitmap := TBitmap.Create; 
  FBitmap.Handle := CreatePixmapIndirect(@mtImages[FDlgType],
                         ColorToRGB(clBtnFace));
  if DefaultCaption then
    Caption := cMtCaption[FDlgType];
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.SetButtons
  Params:  value - the set of buttons required
  Returns: Nothing

  Depending on "value" this method inserts some buttons into the dialog.
  The more buttons required, the wider the dialog will be.
 ------------------------------------------------------------------------------}
procedure TMessageBox.SetButtons(const value : TMsgDlgButtons);
begin
  if FButtons = Value then exit;
  FButtons := Value;
  if FButtons=[] then FButtons:=[mbOk];
  FRelayoutNeeded:=true;
  ReLayout;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.ReLayout
  Params:  nothing
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TMessageBox.ReLayout;
const
  cBtnWidth  = 90;     //TODO: Take into account different languages; button width may vary!
  cBtnHeight = 25;
  cBtnDist   = cBtnWidth + 4;
  cMinLeft   = cBitmapX + cBitmapWidth + cLabelSpacing;
var
  aButton     : TBitBtn;    // temp. variable to create buttons
  aBitmap     : TBitmap;    // temp. variable to create bitmaps for buttons
  curBtn      : TMsgDlgBtn; // variable to loop through TMsgDlgButtons
  ButtonLeft  : integer;    // left position of button(s)
  LabelLeft   : integer;    // left position of labels
  reqBtnWidth : integer;    // width neccessary to display buttons
  reqWidth, reqHeight : integer;    // width and height neccessary to display all
  LabelWidth  : integer;    // max width of labels
  LabelHeight : integer;
  i, j        : integer;
  ButtonIndex : integer;
begin
  if FUpdateCounter>0 then exit;
  
  // calculate label width
  LabelWidth:= 0;
  for i:=0 to FMsgLines.Count-1 do begin
    j:=LineWidth(i);
    if j > LabelWidth then LabelWidth:= j;
  end;
  LabelHeight:= LineHeight * FMsgLines.Count;
  
  // destroy old BitBtns
  for i:=ComponentCount-1 downto 0 do
    if Components[i] is TBitBtn then
      Components[i].Free;

  // calculate the width we need to display the buttons
  reqBtnWidth := 0;
  for curBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if curBtn in FButtons then inc(reqBtnWidth, cBtnDist);
  if reqBtnWidth > 0 then Dec(reqBtnWidth, 4);
    
  // patch positions to center label and buttons
  reqWidth:= reqBtnWidth;
  if reqWidth < (LabelWidth + cMinLeft) then reqWidth:= LabelWidth + cMinLeft;
  
  LabelLeft := ((reqWidth - cMinLeft - LabelWidth) div 2) + cMinLeft;
  ButtonLeft := ((reqWidth - reqBtnWidth) div 2) + cLabelSpacing;

  reqHeight:= LabelHeight;
  if reqHeight < cBitmapHeight then reqHeight:= cBitmapHeight;

  // set size of form
  SetBounds(Left, Top, reqWidth + 2 * cLabelSpacing,
        3 * cLabelSpacing + reqHeight + cBtnHeight);
  
  // set up labels
  FLabel.SetBounds(LabelLeft, cLabelSpacing, LabelWidth, LabelHeight);
  Flabel.Caption:= Trim(FMsgLines.Text);
  
  // create the buttons
  ButtonIndex := -1;
  for curBtn := low(TMsgDlgBtn) to high(TMsgDlgBtn) do begin
    if curBtn in FButtons then begin
      inc(ButtonIndex);
      aButton := TBitBtn.Create(self);
      With aButton do begin
        Parent  := Self;
        SetBounds (ButtonLeft, 2 * cLabelSpacing + reqHeight, cBtnWidth, cBtnHeight);
        inc(ButtonLeft, cBtnDist);
        Layout  := blGlyphLeft;
        // ToDo: when TBitmap streaming is working, load image from resource 
        aBitmap := TBitmap.Create;
        aBitmap.Handle := CreatePixmapIndirect(@mbImages [curBtn],
                ColorToRGB(clBtnFace));
        Glyph   := aBitmap;
        ModalResult := cMbResult[curBtn];
        Caption := cMbCaption[curBtn];
        if ButtonIndex=0 then Default := true;
        if curbtn in [mbOK, mbYes] then Default := true;
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
  
  FRelayoutNeeded:=false;
end;

procedure TMessageBox.BeginUpdate;
begin
  inc(FUpdateCounter);
end;

procedure TMessageBox.EndUpdate;
begin
  dec(FUpdateCounter);
  if FUpdateCounter<0 then FUpdateCounter:=0;
  if (FUpdateCounter=0) and (FRelayoutNeeded) then
    ReLayout;
end;

// ---------------------------------------------------------------------------

function CreateMessageDialog(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TForm;
var
  msgbox : TMessageBox;
begin
  msgbox := TMessageBox.Create (application);
  msgbox.BeginUpdate;
  msgbox.theMessage := aMsg;
  msgBox.DialogType := DlgType;
  msgBox.Buttons    := buttons;
  msgbox.EndUpdate;
  Result := msgbox;
end;

function CreateMessageDialogWithCap(const aCaption, aMsg: string;
  DlgType: TMsgDlgType;  Buttons: TMsgDlgButtons): TForm;
var
  msgbox : TMessageBox;
begin
  msgbox := TMessageBox.Create (Application);
  msgbox.BeginUpdate;
  msgbox.theMessage := aMsg;
  msgBox.DialogType := DlgType;
  msgBox.Buttons    := buttons;
  msgBox.Caption    := aCaption;
  msgbox.EndUpdate;
  Result := msgbox;
end;

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  aDialog : TForm;
begin
  aDialog := CreateMessageDialog (aMsg, DlgType, buttons);
  try
    Result := aDialog.ShowModal;
  finally
    aDialog.Free;
  end;
end;

function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  aDialog : TForm;
begin
  aDialog := CreateMessageDialogWithCap(aCaption, aMsg, DlgType, buttons);
  try
    Result := aDialog.ShowModal;
  finally
    aDialog.Free;
  end;
end;

function MessageDlgPos(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
var
  aDialog : TForm;
begin
  aDialog := CreateMessageDialog (aMsg, DlgType, buttons);
  aDialog.Position:=poDefault;
  aDialog.Left := x;
  aDialog.Top := y;
  Result := aDialog.ShowModal;
end;

function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer;
begin
  writeln ('MessageDlgPosHelp ****** NOT YET FULLY IMPLEMENTED ********');
//TODO: set helpcontext and helpfile
  result := MessageDlg (aMsg, DlgType, buttons, helpctx);
end;

procedure ShowMessage(const aMsg: string);
begin
  MessageDlg (aMsg, mtInformation, [mbOK], 0);
end;

procedure ShowMessageFmt(const aMsg: string; Params: array of const);
begin
  MessageDlg (Format (aMsg, Params), mtInformation, [mbOK], 0);
end;

procedure ShowMessagePos(const aMsg: string; X, Y: Integer);
begin
  writeln ('ShowMessagePos ****** NOT YET FULLY IMPLEMENTED ********');
  MessageDlg (aMsg, mtInformation, [mbOK], 0);
end;

{
   $Log$
   Revision 1.12  2002/08/30 10:06:07  lazarus
   Fixed alignment of multiline TLabel.
   Simplified and prettified MessageBoxen.

   Revision 1.11  2002/07/29 13:39:07  lazarus
   MG: removed ambigious TBitmap from LCLType and added Escape key to MessageDlgs

   Revision 1.10  2002/06/06 07:23:24  lazarus
   MG: small fixes to reduce form repositioing

   Revision 1.9  2002/05/10 06:05:53  lazarus
   MG: changed license to LGPL

   Revision 1.8  2001/12/10 22:39:37  lazarus
   MG: added perl highlighter

   Revision 1.7  2001/10/16 10:51:10  lazarus
   MG: added clicked event to TButton, MessageDialog reacts to return key

   Revision 1.6  2001/10/07 07:35:28  lazarus
   MG: minor fix

   Revision 1.4  2001/07/31 18:40:24  lazarus
   MG: added unit info, arrow xpms, and many changes from jens arm

   Revision 1.3  2001/06/14 14:57:58  lazarus
   MG: small bugfixes and less notes

   Revision 1.1  2001/03/03 00:50:34  lazarus
   + added support for message dialogs (messagedialogs.inc)
   + added some pixmaps for message dialogs(messagedialogpixmaps.inc)
   stoppok

}   
