{******************************************************************************
                                  MessageDialogs
 ******************************************************************************}
{ 
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
    - Help-button will not work

  Bugs:

    - TBitmap seems to have a bug, the size of the bitmap is always 0
      in the Paint function.
}
type

   { TMessageBox
   
     Internal class used to build a MessageBox. 
   }
   TMessageBox = class (TForm)
   private
      FBitmap : TBitmap;
      FLabel  : TLabel;
      FDlgType : TMsgDlgType;
      FButtons : TMsgDlgButtons;
      procedure SetText (const value : string); 
      procedure SetDialogType (const value : TMsgDlgType);
      procedure SetButtons (const value : TMsgDlgButtons);
   public
      constructor Create (AOwner : TComponent); override;
      destructor Destroy; override;
      procedure Paint; override;
      property TheMessage : string write SetText;
      property DialogType : TMsgDlgType write SetDialogType;
      property Buttons : TMsgDlgButtons write SetButtons;
   end;

const
   //
   //TODO: all the constants below should be replaced in the future
   //      their only purpose id to overcome some current design flaws &
   //      missing features in the GTK libraries
   //
   cBitmapX  = 10;      // x-position for bitmap in messagedialog
   cBitmapY  = 10;      // y-position for bitmap in messagedialog
   cBitmapWidth = 32;   // width of the dialogs icon
   cBitmapHeight= 32;   // height of the dialogs icon
   cLabelSpacing= 10;   // distance between icon & label
   cLabelHeight = 40;   // height of the (multiline) label
   cLabelWidth  = 130;  // width of the label

{------------------------------------------------------------------------------
  Method: TMessageBox.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for a MessageBox
 ------------------------------------------------------------------------------}
constructor TMessageBox.Create (AOwner : TComponent);
begin
   inherited Create (AOwner);
   
   BorderStyle := bsDialog;
   Position    := poMainFormCenter;
   width       := 200;
   height      := 100;
   FDlgType := mtInformation;
   Caption  := cMtCaption [FDlgType];
   FBitmap  := nil;
   FLabel   := nil;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.Destroy
  Params:  ---
  Returns: Nothing

  Destructor for a MessageBox
 ------------------------------------------------------------------------------}
destructor TMessageBox.Destroy;
begin
   FBitmap.Free;
   inherited Destroy;
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
//TODO: use real with of the icon (Currently FBitmap.Width /Height are always 0, seems to be a bug somewhere)
   if assigned (FBitmap)
      then Canvas.Copyrect(Bounds(cBitmapX, cBitmapY, cBitmapWidth{FBitmap.width+1}, cBitmapHeight{FBitmap.Height+1}),
                     FBitmap.Canvas, Rect(0,0, FBitmap.Width, FBitmap.Height));
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.SetText
  Params:  value - text to be displayed in the message box
  Returns: Nothing

  Creates a label to display the text.
 ------------------------------------------------------------------------------}
procedure TMessageBox.SetText (const value : string);
begin
   if not (assigned (FLabel)) then
   begin
      FLabel := TLabel.Create(self);
      FLabel.Parent := self;
      FLabel.Font.Size := 5;
      FLabel.top       := 10;
      FLabel.left      := cBitmapX + cBitmapWidth + cLabelSpacing;
      FLabel.Height    := cLabelHeight;
      FLabel.Width     := cLabelWidth;
      FLabel.WordWrap  := true;
   end;
   FLabel.Show;
   FLabel.Caption := value;
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.SetDialogType
  Params:  value - the type of dialog to be shown
  Returns: Nothing

  Sets the type of dialog.
 ------------------------------------------------------------------------------}
procedure TMessageBox.SetDialogType (const value : TMsgDlgType);
begin
   if (value = FDlgType) and (assigned (FBitmap)) then exit;
   FDlgType := value;
   FBitmap.Free;
   FBitmap := TBitmap.Create; 
   FBitmap.Handle := CreatePixmapIndirect(@mtImages[FDlgType], ColorToRGB(clBtnFace));
   Caption := cMtCaption [FDlgType];
end;

{------------------------------------------------------------------------------
  Method: TMessageBox.SetButtons
  Params:  value - the set of buttons required
  Returns: Nothing

  Depending on "value" this method inserts some buttons into the dialog.
  The more buttons required, the wider the dialog will be.
 ------------------------------------------------------------------------------}
procedure TMessageBox.SetButtons (const value : TMsgDlgButtons);
const
   cBtnWidth  = 85;     //TODO: Take into account different languages; button width may vary!
   cBtnDist   = cBtnWidth + 25;
   cBtnHeight = 32;
   cMinLeft   = cBtnDist - cBtnWidth;
var
   aButton    : TBitBtn;         // temp. variable to create buttons
   aBitmap    : TBitmap;         // temp. variable to create bitmaps for buttons
   curBtn     : TMsgDlgBtn;      // variable to loop through TMsgDlgButtons
   ButtonLeft : integer;         // left position of button(s)
   LabelLeft  : integer;         // left position of label
   reqBWidth  : integer;         // width neccessary to display all buttons
   reqLWidth  : integer;         // width neccessary to display the label
begin
   FButtons := value;

   // calculate the width we need to display the buttons
   reqBWidth := cMinLeft;
   curBtn := low (TMsgDlgBtn);
   while curBtn < high (TMsgDlgBtn) do
   begin
      if curBtn in FButtons then reqBWidth := reqBWidth + cBtnDist;
      inc (curBtn);
   end;

   //TODO: calculate the width we need for the label
   reqLWidth := 200;

   // set size of form
   // patch positions to center label and buttons
   if reqBWidth >= reqLWidth then
   begin
      width     := reqBWidth;
      LabelLeft := round ((width - reqLWidth) / 2);
      ButtonLeft := cMinLeft;
   end
   else begin
      width      := reqLWidth;
      ButtonLeft := round ((width - reqBWidth + 2*cMinLeft) / 2);
      writeln ('ButtonLeft :', ButtonLeft, ' RequestedWidth : ',reqBWidth, 'Formwidth : ', width);
      LabelLeft  := cMinLeft + 4*FBitmap.Width;
   end;

//   FLabel.left := LabelLeft;
   // create the buttons
//   ButtonLeft := cMinLeft;
   curBtn := low (TMsgDlgBtn);
//Todo -cmaybe 4:clear any buttons (only for general purpose use)!!!
   while curBtn < high (TMsgDlgBtn) do
   begin
      if curBtn in value then
      begin
         aButton := TBitBtn.Create(self);
         With aButton do
         begin
            Parent  := self;
            SetBounds (ButtonLeft, 60, cBtnWidth, cBtnHeight);
            ButtonLeft := ButtonLeft + cBtnDist;
            layout  := blGlyphLeft;
            aBitmap := TBitmap.Create;
            aBitmap.Handle := CreatePixmapIndirect(@mbImages [curBtn], ColorToRGB(clBtnFace));
            Glyph   := aBitmap;
            ModalResult := cMbResult [curBtn];
            caption := cMbCaption [curBtn];
            if curbtn in [mbOK, mbYes] then Default := true;
            Show;
         end;
      end;
      inc (curBtn);
   end;
end;


// ---------------------------------------------------------------------------

function CreateMessageDialog(const aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TForm;
var
   msgbox : TMessageBox;
begin
   msgbox := TMessageBox.Create (application);
   msgbox.theMessage := aMsg;
   msgBox.DialogType := DlgType;
   msgBox.Buttons    := buttons;
   result := msgbox;
end;

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
   aDialog : TForm;
begin
   aDialog := CreateMessageDialog (aMsg, DlgType, buttons);
   result := aDialog.ShowModal;
   aDialog.Free;
end;

function MessageDlgPos(const aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
begin
   writeln ('****** NOT YET FULLY IMPLEMENTED ********');
//TODO: set helpcontext and x/y coordinates
   result := MessageDlg (aMsg, DlgType, buttons, helpctx);
end;


function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer;
begin
   writeln ('****** NOT YET FULLY IMPLEMENTED ********');
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
   writeln ('****** NOT YET FULLY IMPLEMENTED ********');
   MessageDlg (aMsg, mtInformation, [mbOK], 0);
end;
{
   $Log$
   Revision 1.1  2001/03/03 00:50:34  lazarus
   + added support for message dialogs (messagedialogs.inc)
   + added some pixmaps for message dialogs(messagedialogpixmaps.inc)
   stoppok

}   
