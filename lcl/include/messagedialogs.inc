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
  FLabel.ShowAccelChar:= false;
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
    Result:= Canvas.TextWidth(FMsgLines[Index])
  else
    Result:= 0;
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
  FBitmap.Handle := LoadStockPixmap(mtImages[FDlgType]);
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
Function GetAvgCharSize(Handle : hDC) : TPoint;
var
  I : integer;
  Buffer : Array[0..51] of Char;
begin
  For I := 0 to 25 do Buffer[I] := chr(I + Ord('A'));
  For I := 0 to 25 do Buffer[I + 26] := chr(I + Ord('a'));
  GetTextExtentPoint(Handle,Buffer,52,TSize(Result));
  Result.X := Result.X div 52;
end;

procedure TMessageBox.ReLayout;
const
  cBtnCalcWidth  = 50;
  cBtnCalcHeight = 13;
  cBtnCalcSpace   = 4;
  cMinLeft   = cBitmapX + cBitmapWidth + cLabelSpacing;
var
  aBitmap     : TBitmap;    // temp. variable to create bitmaps for buttons
  curBtn      : TMsgDlgBtn; // variable to loop through TMsgDlgButtons
  cBtnWidth,
  cBtnHeight,
  curBtnWidth,
  cBtnDist,
  ButtonLeft  : integer;    // left position of button(s)
  reqBtnWidth : integer;    // width neccessary to display buttons
  reqWidth, reqHeight : integer;    // width and height neccessary to display all
  i        : integer;
  ButtonIndex : integer;
  Avg : TPoint;
  TextBox : TRect;
  TextStyle : TTextStyle;
begin
  if FUpdateCounter > 0 then exit;

  FillChar(TextStyle, SizeOf(TextStyle), 0);
  
  With TextStyle do begin
    Clipping   := True;
    Wordbreak  := True;
    SystemFont := True;
  end;

  // calculate the width & height we need to display the Message
  TextBox := Rect(0,0, Screen.Width div 2,Screen.Height - 100);
  SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FONT));
  DrawText(Canvas.Handle, PChar(FMsgLines.Text), Length(FMsgLines.Text),
    TextBox, DT_WORDBREAK or DT_INTERNAL or DT_CALCRECT);

  // destroy old BitBtns
  for i:=ComponentCount-1 downto 0 do
    if Components[i] is TBitBtn then
      Components[i].Free;

  // calculate the width we need to display the buttons
  SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FONT));
  Avg := GetAvgCharSize(Canvas.Handle);
  reqBtnWidth := 0;
  cBtnWidth := (cBtnCalcWidth*Avg.X) div 4;
  cBtnHeight := (cBtnCalcHeight*AVG.Y) div 8;
  cBtnDist := (cBtnCalcSpace * Avg.X) div 4;
  for curBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if curBtn in FButtons then
    begin
      curBtnWidth := Canvas.TextWidth(cMbCaption[curBtn]) + 8;
      if curBtnWidth > cBtnWidth then
        cBtnWidth := curBtnWidth;
      Inc(reqBtnWidth, cBtnWidth + cBtnDist)
    end;
  if reqBtnWidth > 0 then Dec(reqBtnWidth, cBtnDist);
  Inc(cBtnDist, cBtnWidth);

  // patch positions to center label and buttons
  reqWidth:= reqBtnWidth;
  if reqWidth < (TextBox.Right + cMinLeft) then reqWidth:= TextBox.Right + cMinLeft;
  
  ButtonLeft := ((reqWidth - reqBtnWidth) div 2) + cLabelSpacing;

  reqHeight:= TextBox.Bottom;
  if reqHeight < cBitmapHeight then reqHeight:= cBitmapHeight;

  OffsetRect(TextBox, ((reqWidth - cMinLeft - TextBox.Right) div 2) + cMinLeft,cLabelSpacing);

  // set size of form
  SetBounds(Left, Top, reqWidth + 2 * cLabelSpacing,
        3 * cLabelSpacing + reqHeight + cBtnHeight);
  
  // set up labels
  FLabel.SetBounds(TextBox.Left, TextBox.Top, TextBox.Right, TextBox.Bottom);
  Flabel.Caption:= Trim(FMsgLines.Text);

  // create the buttons
  ButtonIndex := -1;
  for curBtn := low(TMsgDlgBtn) to high(TMsgDlgBtn) do begin
    if curBtn in FButtons then begin
      inc(ButtonIndex);
      
      with TBitBtn.Create(Self) do begin
        Parent:= Self;
        SetBounds (ButtonLeft, 2 * cLabelSpacing + reqHeight, cBtnWidth, cBtnHeight);
        inc(ButtonLeft, cBtnDist);
        Layout  := blGlyphLeft;
        // ToDo: when TBitmap streaming is working, load image from resource 
        aBitmap := TBitmap.Create;
        aBitmap.Handle := LoadStockPixmap(mbImages[curBtn]);
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
  if FUpdateCounter<0 then FUpdateCounter:= 0;
  if (FUpdateCounter = 0) and FRelayoutNeeded then ReLayout;
end;

// ---------------------------------------------------------------------------

function CreateMessageDialog(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TForm;
var
  msgbox : TMessageBox;
begin
  msgbox:= TMessageBox.Create(Application);
  msgbox.BeginUpdate;
  msgbox.theMessage := aMsg;
  msgBox.DialogType := DlgType;
  msgBox.Buttons    := buttons;
  msgbox.EndUpdate;
  Result:= msgbox;
end;

function CreateMessageDialogWithCap(const aCaption, aMsg: string;
  DlgType: TMsgDlgType;  Buttons: TMsgDlgButtons): TForm;
var
  msgbox : TMessageBox;
begin
  msgbox:= TMessageBox.Create(Application);
  msgbox.BeginUpdate;
  msgbox.theMessage := aMsg;
  msgBox.DialogType := DlgType;
  msgBox.Buttons    := buttons;
  msgBox.Caption    := aCaption;
  msgbox.EndUpdate;
  Result:= msgbox;
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

function MessageDlgPos(const aMsg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Helpctx : Longint; X,Y : Integer): Integer;
begin
  with CreateMessageDialog (aMsg, DlgType, Buttons) do
    try
      Position := poDesigned;
      Left := X;
      Top := Y;
      //HelpContext := Helpctx;
      Result := ShowModal;
    finally
      Close;
    end;
end;

function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer;
begin
  writeln ('MessageDlgPosHelp ****** NOT YET FULLY IMPLEMENTED ********');
//TODO: set helpcontext and helpfile
  result := MessageDlgPos(aMsg, DlgType, buttons, helpctx, X, Y);
end;

procedure ShowMessage(const aMsg: string);
begin
  NotifyUser(aMsg, idDialogInfo);
end;

procedure ShowMessageFmt(const aMsg: string; Params: array of const);
begin
  NotifyUser(Format(aMsg, Params), idDialogInfo);
end;

procedure ShowMessagePos(const aMsg: string; X, Y: Integer);
begin
  NotifyUserAtXY(aMsg, idDialogInfo, X, Y);
end;

//----------------------------------------------------------------------------//
//-----------------------Prompt User For Information--------------------------//
Function InputBox(const ACaption, APrompt, ADefault : String) : String;
begin
  Result := ADefault;
  InputQuery(ACaption, APrompt, Result);
end;

Function PasswordBox(const ACaption, APrompt : String) : String;
begin
  Result := '';
  InputQuery(ACaption, APrompt, True, Result);
end;

Function InputQuery(const ACaption, APrompt : String; MaskInput : Boolean;
  var Value : String) : Boolean;
begin
  Result := LCLLinux.RequestInput(ACaption, APrompt, MaskInput, Value);
end;

Function InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
begin
  Result := InputQuery(ACaption, APrompt, False, Value);
end;

{
   $Log$
   Revision 1.17  2002/10/23 14:36:52  lazarus
   AJ:Fixes to PromptUser;Switched ShowMessage* to use NotifyUser*;
      fixed TGraphicPropertyEditor for when Property is nil.

   Revision 1.16  2002/10/11 16:00:39  lazarus
   AJ: made InputQuery Interface Dependant

   Revision 1.15  2002/10/10 13:29:08  lazarus
   AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

   Revision 1.14  2002/09/27 20:52:23  lazarus
   MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

   Here is the run down of what it includes -

    -Vasily Volchenko's Updated Russian Localizations

    -improvements to GTK Styles/SysColors
    -initial GTK Palette code - (untested, and for now useless)

    -Hint Windows and Modal dialogs now try to stay transient to
     the main program form, aka they stay on top of the main form
     and usually minimize/maximize with it.

    -fixes to Form BorderStyle code(tool windows needed a border)

    -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
     when flat

    -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
     and to match GTK theme better. It works most of the time now,
     but some themes, noteably Default, don't work.

    -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
     mode.

    -misc other cleanups/ fixes in gtk interface

    -speedbutton's should now draw correctly when flat in Win32

    -I have included an experimental new CheckBox(disabled by
     default) which has initial support for cbGrayed(Tri-State),
     and WordWrap, and misc other improvements. It is not done, it
     is mostly a quick hack to test DrawFrameControl
     DFCS_BUTTONCHECK, however it offers many improvements which
     can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

    -fixes Message Dialogs to more accurately determine
     button Spacing/Size, and Label Spacing/Size based on current
     System font.
    -fixes MessageDlgPos, & ShowMessagePos in Dialogs
    -adds InputQuery & InputBox to Dialogs

    -re-arranges & somewhat re-designs Control Tabbing, it now
     partially works - wrapping around doesn't work, and
     subcontrols(Panels & Children, etc) don't work. TabOrder now
     works to an extent. I am not sure what is wrong with my code,
     based on my other tests at least wrapping and TabOrder SHOULD
     work properly, but.. Anyone want to try and fix?

    -SynEdit(Code Editor) now changes mouse cursor to match
     position(aka over scrollbar/gutter vs over text edit)

    -adds a TRegion property to Graphics.pp, and Canvas. Once I
     figure out how to handle complex regions(aka polygons) data
     properly I will add Region functions to the canvas itself
     (SetClipRect, intersectClipRect etc.)

    -BitBtn now has a Stored flag on Glyph so it doesn't store to
     lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
     bkOk, bkCancel, etc.) This should fix most crashes with older
     GDKPixbuf libs.

   Revision 1.13  2002/09/03 20:02:01  lazarus
   Intermediate UI patch to show a bug.

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
