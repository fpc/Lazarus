(******************************************************************************
                                     TFONT
 ******************************************************************************)


{------------------------------------------------------------------------------
  Method:  TFont.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TFont.Create;
begin
  inherited Create;
  with FFontData do
  begin
    Handle := 0;
    Charset := ANSI_CHARSET; 
    Pitch := fpDefault;
  end;

  FColor := clWindowText;
  FPixelsPerInch := ScreenInfo.PixelsPerInchX;
end;

{------------------------------------------------------------------------------
  Method: TFont.Assign
  Params: Source: Another font
  Returns:  nothing

  Copies the source font to itself
 ------------------------------------------------------------------------------}
Procedure TFont.Assign(Source : TPersistent);
begin
  if Source is TFont then begin
    //TODO:lock;
    try
      //TODO: TFont(Source).Lock;
      try
        Height := TFont(Source).Height;
        Color := TFont(Source).Color;
        Name := TFont(Source).Name;
      finally
        //TODO: TFont(Source).UnLock;
      end;
    finally
      //TODO: UnLock;
    end;
    Exit;
  end;
  inherited Assign(Source); 
end;

{------------------------------------------------------------------------------
  Method: TFont.SetSize
  Params: Value: the new value
  Returns:  nothing

  Converts the size in points to pixels and sets the height
 ------------------------------------------------------------------------------}
procedure TFont.SetSize(value : Integer);
begin
  Height := -(Value * FPixelsPerInch) div 72;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetSize
  Params: none
  Returns:  The font size

  Calculates the size based on height
 ------------------------------------------------------------------------------}
function TFont.GetSize : Integer;
begin
  Result := -(Height * 72) div FPixelsPerInch;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetPitch
  Params: Value: the new value
  Returns:  nothing

  Sets the pitch of a font
 ------------------------------------------------------------------------------}
Procedure TFont.SetPitch(Value : TFOntPitch);
Begin
  if FFontData.Pitch <> Value
  then begin
    FreeHandle;
    FFontData.Pitch := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetHeight
  Params: Value: the new value
  Returns:  nothing

  Sets the height of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetHeight(Value: Integer);
begin
  if FFontData.Height <> Value
  then begin
    FreeHandle;
    FFontData.Height := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetStyle(value : TFontStyles);
begin
  if FFontData.Style <> Value 
  then begin
    FreeHandle;
    FFontData.Style := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the pencolor of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetColor(Value : TColor);
begin
  if FColor <> Value 
  then begin
    FColor := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetName
  Params: none
  Returns:  The font name

  Returns the name of the font
 ------------------------------------------------------------------------------}
function TFont.GetName: TFontName;
begin
  Result := FFontdata.Name;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetName
  Params: Value: the new value
  Returns:  nothing

  Sets the name of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetName(const Value : TFontName);
begin
  if FFontData.Name <> Value 
  then begin
    FreeHandle;
    FFontData.Name := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TFont.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetHandle
  Params:   a font handle
  Returns:  nothing

  sets the font to an external created font 
 ------------------------------------------------------------------------------}
procedure TFont.SetHandle(const Value: HFONT);
begin
  if FFontData.Handle <> Value 
  then begin
    FreeHandle;
    FFontData.Handle := Value;
    //TODO: query new parameters
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetHandle
  Params:   none
  Returns:  a handle to a font gdiobject

  Creates a font if needed
 ------------------------------------------------------------------------------}
function TFont.GetHandle: HFONT;
const
  LF_BOOL: array[Boolean] of Byte = (0, 255);
  LF_WEIGHT: array[Boolean] of Integer = (FW_NORMAL, FW_BOLD);
var
  LogFont: TLogFont;
begin
  if FFontData.Handle = 0 
  then with LogFont do 
  begin
    lfHeight := Height;
    lfWidth := 0; 
    lfEscapement := 0; 
    lfOrientation := 0; 
    lfWeight := LF_WEIGHT[fsBold in Style];
    lfItalic := LF_BOOL[fsItalic in Style];
    lfUnderline := LF_BOOL[fsUnderline in Style];
    lfStrikeOut := LF_BOOL[fsStrikeOut in Style];
    lfCharSet := Byte(FFontData.Charset);
    if AnsiCompareText(Name, 'Default') = 0 
    then StrPCopy(lfFaceName, 'helvetica') //TODO: change this to a more flexible default
    else StrPCopy(lfFaceName, Name);

    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
    
    FFontData.Handle := CreateFontIndirect(LogFont);
  end;
  
  Result := FFontData.Handle;
  
end;

{------------------------------------------------------------------------------
  Method:  TFont.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a fonthandle if needed
 ------------------------------------------------------------------------------}
procedure TFont.FreeHandle;
begin
  if FFontData.Handle <> 0 
  then begin
    //TODO: what if a font is currently selected
    DeleteObject(FFontData.Handle);
    FFontData.Handle := 0;
  end;
end;


{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.15  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.14  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.13  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.12  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.11  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.10  1999/11/09 17:16:44  lazarus
  Added PITCH to TFONT>
  Doesn't do anything yet.
  Shane

  Revision 1.9  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.8  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.7  1999/08/16 20:48:02  lazarus
  Added a changed event for TFOnt and code to get the average size of the font.  Doesn't seem to work very well yet.
  The "average size" code is found in gtkobject.inc.

  Revision 1.6  1999/08/16 18:45:39  lazarus
  Added a TFont Dialog plus minor additions.

  Shane Aug 16th 1999  14:07 CST

  Revision 1.5  1999/08/07 17:59:19  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.4  1999/08/01 21:46:25  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.3  1999/07/31 06:39:25  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
