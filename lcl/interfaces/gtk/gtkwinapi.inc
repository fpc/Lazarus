// included by gtkint.pp

{******************************************************************************
                         All GTK Winapi implementations.
                   Initial Revision  : Sat Nov 13 12:53:53 1999


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
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
}
{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$EndIf}

const
  BOOL_TEXT: array[Boolean] of string = ('False', 'True');

//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  Method:   Arc
  Params:   x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.Arc(DC: HDC;
  x,y,width,height,angle1,angle2 : Integer): Boolean;
var
  DCOrigin: TPoint;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.Arc] Uninitialized GC');
      Result := False;
    end
    else begin
      // Draw outline
      SelectGDKPenProps(DC);

      If (dcfPenSelected in DCFlags) then begin
        Result := True;
        if (CurrentPen^.IsNullPen) then exit;

        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        inc(X,DCOrigin.X);
        inc(Y,DCOrigin.Y);
        {$IFDEF DebugGDKTraps}BeginGDKErrorTrap;{$ENDIF}
        gdk_draw_arc(Drawable, GC, 0, X, Y, Width, Height,
                     Angle1 shl 2, Angle2 shl 2);
        {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
      end else
        Result:=false;
    end;
  end;
end;

 {------------------------------------------------------------------------------
  Method:   AngleChord
  Params:   DC,x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use AngleChord to draw a filled Chord-shape on the canvas. The angles angle1
  and angle2 are 1/16th of a degree. For example, a full circle equals 5760
  16*360). Positive values of Angle and AngleLength mean counter-clockwise while
  negative values mean clockwise direction. Zero degrees is at the 3'o clock
  position.

------------------------------------------------------------------------------}
function TGtkWidgetSet.AngleChord(DC: HDC;
  x,y,width,height,angle1,angle2 : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.AngleChord] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited AngleChord(DC, x, y, width, height, angle1, angle2);
  end;
end;

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:  
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc;
var
{$IFDEF Gtk1}
  Widget: PGtkWidget;
  TargetObject: TObject;
  PaintWidget: Pointer;
{$ENDIF}
  IsDoubleBuffered: Boolean;
begin
  {$IFDEF Gtk1}
  Widget:=PGtkWidget(Handle);
  TargetObject:=GetNearestLCLObject(Widget);
  IsDoubleBuffered:=(TargetObject is TWinControl)
                    and TWinControl(TargetObject).DoubleBuffered;
  // check if Handle is the paint widget of the LCL component
  if IsDoubleBuffered then begin
    PaintWidget:=GetFixedWidget(PGtkWidget(TWinControl(TargetObject).Handle));
    IsDoubleBuffered:=(PaintWidget=Widget);
    //if not IsDoubleBuffered then begin
    //  DebugLn('TGtkWidgetSet.BeginPaint Not the paint widget: ',
    //    TWinControl(TargetObject).Name,':',TWinControl(TargetObject).ClassName,
    //    ' PaintWidget=',GetWidgetClassName(PaintWidget),
    //    ' Widget=',GetWidgetClassName(Widget));
    //end;
  end;
  {$IFNDEF UseGTKDoubleBuf}
  IsDoubleBuffered:=false;
  {$ENDIF}
  {$ELSE}
  IsDoubleBuffered:=false;
  {$ENDIF}
  if IsDoubleBuffered then
    PS.hDC:=GetDoubleBufferedDC(Handle)
  else
    PS.hDC:=GetDC(Handle);
  Result := PS.hDC;
end;

{------------------------------------------------------------------------------
  Function: BitBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           Rop:                   The raster operation to be performed
  Returns: True if succesful

  The BitBlt function copies a bitmap from a source context into a destination
  context using the specified raster operation.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width,
                       Height, ROP);
end;

{------------------------------------------------------------------------------
  Function: BringWindowToTop
  Params: hWnd:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.BringWindowToTop(hWnd : HWND): Boolean;
var
  {$IFDEF VerboseFocus}
  LCLObject: TControl;
  {$ENDIF}
  GdkWindow: PGdkWindow;
  AForm: TCustomForm;
begin
  {$IFDEF VerboseFocus}
  DbgOut('TGtkWidgetSet.BringWindowToTop hWnd=',HexStr(Cardinal(hWnd),8));
  LCLObject:=TControl(GetLCLObject(Pointer(hWnd)));
  if LCLObject<>nil then
    DebugLn(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    DebugLn(' LCLObject=nil');
  {$ENDIF}
  Result := GtkWidgetIsA(PGtkWidget(hWnd),GTK_TYPE_WINDOW);
  if Result then begin
    GdkWindow:=GetControlWindow(PgtkWidget(hwnd));
    if GdkWindow<>nil then begin
      AForm:=TCustomForm(GetLCLObject(PgtkWidget(hwnd)));
      if (AForm<>nil) and (AForm is TCustomForm) and (AForm.Parent=nil) then
        Screen.MoveFormToZFront(AForm);
      {$IFDEF DebugGDKTraps}
      BeginGDKErrorTrap;
      {$ENDIF}
      gdk_window_raise(GdkWindow);
      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}
      // how to set the keyboard focus to the raised window?
{$ifdef gtk2}
      gtk_window_present(PGtkWindow(hWnd));
{$endif gtk2}
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: CallNextHookEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam: WParam; lParam : LParam) : Integer;
begin
  Result := 0;
  //TODO: Does anything need to be done here?
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
  Assert(False, 'Trace:TODO: CALLNEXTHOOKEX in gtkwinapi.inc');
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
  Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
end;

{------------------------------------------------------------------------------
  Function: CallWindowProc
  Params: lpPrevWndFunc:
          Handle:
          Msg:
          wParam:
          lParam:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND;
  Msg : UINT; wParam: WParam; lParam : LParam) : Integer;
var
  Proc : TWndMethod;
  Mess : TLMessage;
  P : Pointer;
begin
  Result := -1;
  if Handle = 0 then Exit;
  Result := -1;
  P := nil;
  P := gtk_object_get_data(pgtkobject(Handle),'WNDPROC');
  if P <> nil then
    Proc := TWndMethod(P^)
  else
    Exit;
  Mess.msg := msg;
  Mess.LParam := LParam;
  Mess.WParam := WParam;
  Proc(Mess);
  Result := Mess.Result;
end;

{------------------------------------------------------------------------------
  Function: ClientToScreen
  Params:  Handle : HWND; var P : TPoint
  Returns: true on success

  Converts the client-area coordinates of P to screen coordinates.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.ClientToScreen(Handle : HWND; var P : TPoint) : Boolean;
var
  Position: TPoint;
Begin
  if Handle = 0
  then begin
    Position.X := 0;
    Position.Y := 0;
  end
  else begin
    Position:=GetWidgetClientOrigin(PGtkWidget(Handle));
  end;

  // Todo: calculate offset, since platform specific
  Inc(P.X, Position.X);
  Inc(P.Y, Position.Y);

  Assert(False, Format('Trace:  [GTKObject.ClientToScreen] Handle: 0x%x --> (%d, %d)', [Integer(Handle), P.X, P.y]));
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: ClipboardFormatToMimeType
  Params:  FormatID - a registered format identifier (0 is invalid)
  Returns: the corresponding mime type as string
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ClipboardFormatToMimeType(
  FormatID: TClipboardFormat): string;
var p: PChar;
begin
  if FormatID<>0 then begin
    p:=gdk_atom_name(FormatID);
    Result:=StrPas(p);
    g_free(p);
  end else
    Result:='';
end;

{------------------------------------------------------------------------------
  Function: ClipboardGetData
  Params:  ClipboardType
           FormatID - a registered format identifier (0 is invalid)
           Stream - If format is available, it will be appended to this stream
  Returns: true on success
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
type
  PGdkAtom = ^TGdkAtom;
var FormatAtom, FormatTry: Cardinal;
  SupportedCnt, i: integer;
  SupportedFormats: PGdkAtom;
  SelData: TGtkSelectionData;
  CompoundTextList: PPGChar;
  CompoundTextCount: integer;

  function IsFormatSupported(Format: cardinal): boolean;
  var a: integer;
     AllID: cardinal;
  begin
    if Format=0 then begin
      Result:=false;
      exit;
    end;
    if SupportedCnt<0 then begin
      Result:=false;
      AllID:=gdk_atom_intern('TARGETS',GdkFalse);
      SelData:=RequestSelectionData(ClipboardWidget,ClipboardType,AllID);
      {DebugLn('IsFormatSupported A ',HexStr(Cardinal(SelData.Selection),8),
      ' ',HexStr(Cardinal(ClipboardTypeAtoms[ClipboardType]),8),
      ' SelData.Target=',SelData.Target,' AllID=',AllID,
      ' SelData.TheType=',SelData.TheType,' ',gdk_atom_intern('ATOM',0),
      ' SelData.Length=',SelData.Length,
      ' SelData.Format=',SelData.Format
      );}
      if (SelData.Selection<>ClipboardTypeAtoms[ClipboardType])
      or (SelData.Target<>AllID)
      or (SelData.{$IfDef GTK2}_Type{$Else}theType{$EndIf}<>gdk_atom_intern('ATOM',GdkFalse)) then begin
        SupportedCnt:=0;
        exit;
      end;
      SupportedCnt:=SelData.Length div (SelData.Format shr 3);
      SupportedFormats:=PGdkAtom(SelData.Data);
    end;
    a:=SupportedCnt-1;
    while (a>=0) and (SupportedFormats[a]<>Format) do dec(a);
    Result:=(a>=0);
  end;

begin
  {$IfDef DEBUG_CLIPBOARD}
  DebugLn('[TGtkWidgetSet.ClipboardGetData] A ClipboardWidget=',HexStr(Cardinal(ClipboardWidget),8),' Format=',ClipboardFormatToMimeType(FormatID),' Now=',Now);
  {$EndIf}
  Result:=false;
  if (FormatID=0) or (Stream=nil) then exit;
  if not (ClipboardType in [ctPrimarySelection,ctSecondarySelection,ctClipboard])
  then exit;
  // request the data from the selection owner
  SupportedCnt:=-1;
  SupportedFormats:=nil;
  try

    FormatAtom:=FormatID;
    if (FormatAtom=gdk_atom_intern('text/plain',GdkTrue)) then begin
      // text/plain is supported in various formats in gtk
      // The COMPOUND_TEXT format supports internationalization and is therefore
      // preferred even before 'text/plain'
      FormatAtom:=0;
      FormatTry:=gdk_atom_intern('COMPOUND_TEXT',GdkTrue);
      if IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      if (SupportedCnt=0) then
        FormatAtom:=gdk_atom_intern('COMPOUND_TEXT',GdkTrue);
      // then check for simple text format 'text/plain'
      FormatTry:=gdk_atom_intern('text/plain',GdkTrue);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      // then check for simple text format STRING
      FormatTry:=gdk_atom_intern('STRING',GdkTrue);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      // check for some other formats that can be interpreted as text
      FormatTry:=gdk_atom_intern('FILE_NAME',GdkTrue);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      FormatTry:=gdk_atom_intern('HOST_NAME',GdkTrue);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      FormatTry:=gdk_atom_intern('USER',GdkTrue);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      // the TEXT format is not reliable, but it should be supported
      FormatTry:=gdk_atom_intern('TEXT',GdkTrue);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
    end;

    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('[TGtkWidgetSet.ClipboardGetData] B  Format=',ClipboardFormatToMimeType(FormatAtom),' Now=',Now);
    {$EndIf}
    if FormatAtom=0 then exit;

    // request data from owner
    SelData:=RequestSelectionData(ClipboardWidget,ClipboardType,FormatAtom);
    try
      {$IfDef DEBUG_CLIPBOARD}
      DebugLn('[TGtkWidgetSet.ClipboardGetData] C  Length=',SelData.Length,' Now=',Now);
      {$EndIf}
      if (SelData.Selection<>ClipboardTypeAtoms[ClipboardType])
      or (SelData.Target<>FormatAtom) then
        exit;

      // write data to stream
      if (SelData.Data<>nil) and (SelData.Length>0) then begin
        if (FormatID=gdk_atom_intern('text/plain',GdkTrue)) then begin
          // the lcl expects the return format as simple text
          // transform if necessary
          if FormatAtom=gdk_atom_intern('COMPOUND_TEXT',GdkTrue) then begin
            CompoundTextCount:=gdk_text_property_to_text_list(SelData.{$IfDef GTK2}_Type{$Else}theType{$EndIf},
              SelData.Format,SelData.Data,SelData.Length,{$IfDef GTK1}@{$EndIf}CompoundTextList);
            {$IfDef DEBUG_CLIPBOARD}
            DebugLn('[TGtkWidgetSet.ClipboardGetData] D  CompoundTextCount=',CompoundTextCount,' Now=',Now);
            {$EndIf}
            for i:=0 to CompoundTextCount-1 do
              if (CompoundTextList[i]<>nil) then
                Stream.Write(CompoundTextList[i]^,StrLen(CompoundTextList[i]));
            gdk_free_text_list(CompoundTextList);
          end else
            Stream.Write(SelData.Data^,SelData.Length);
        end else begin
          Stream.Write(SelData.Data^,SelData.Length);
        end;
      end;

      {$IfDef DEBUG_CLIPBOARD}
      DebugLn('[TGtkWidgetSet.ClipboardGetData] END ',' Now=',Now);
      {$EndIf}
    finally
      if SelData.Data<>nil then FreeMem(SelData.Data);
    end;
    Result:=true;
  finally
    if SupportedFormats<>nil then FreeMem(SupportedFormats);
  end;
end;

{------------------------------------------------------------------------------
  Function: ClipboardGetFormats
  Params:  ClipboardType
  Returns: true on success
           Count contains the number of supported formats
           List is an array of TClipboardType

  ! List will be created. You must free it yourself with FreeMem(List) !
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
type
  PGdkAtom = ^TGdkAtom;
var AllID: cardinal;
  FormatAtoms: PGdkAtom;
  Cnt, i: integer;
  AddTextPlain: boolean;
  SelData: TGtkSelectionData;

  function IsFormatSupported(Format: cardinal): boolean;
  var a: integer;
  begin
    if Format<>0 then begin
      for a:=0 to Cnt-1 do begin
        {$IfDef DEBUG_CLIPBOARD}
        DebugLn('  IsFormatSupported ',Format,'  ',FormatAtoms[a]);
        {$EndIf}
        if FormatAtoms[a]=Format then begin
          Result:=true;
          exit;
        end;
      end;
    end;
    Result:=false;
  end;

  function IsFormatSupported(Formats: TGtkClipboardFormats): boolean;
  var Format: TGtkClipboardFormat;
  begin
    for Format:=Low(TGtkClipboardFormat) to High(TGtkClipboardFormat) do
      if (Format in Formats)
      and (IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[Format]),GdkTrue)))
      then begin
        Result:=true;
        exit;
      end;
    Result:=false;
  end;


begin
  {$IfDef DEBUG_CLIPBOARD}
  DebugLn('[TGtkWidgetSet.ClipboardGetFormats] A ClipboardWidget=',HexStr(Cardinal(ClipboardWidget),8),' Now=',Now);
  {$EndIf}
  Result:=false;
  Count:=0;
  List:=nil;
  if not (ClipboardType in [ctPrimarySelection,ctSecondarySelection,ctClipboard])
  then exit;
  // request the list of supported formats from the selection owner
  AllID:=gdk_atom_intern('TARGETS',GdkFalse);

  SelData:=RequestSelectionData(ClipboardWidget,ClipboardType,AllID);

  try
    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('[TGtkWidgetSet.ClipboardGetFormats] Checking TARGETS answer ',
      ' selection: ',SelData.Selection,'=',ClipboardTypeAtoms[ClipboardType],
                ' "',gdk_atom_name(SelData.Selection),'"',
      ' target: ',SelData.Target,'=',AllID,
                ' "',gdk_atom_name(SelData.Target),'"',
      ' theType: ',SelData.TheType,'=',gdk_atom_intern('ATOM',GdkFalse),
                ' "',gdk_atom_name(SelData.theType),'"',
      ' Length=',SelData.Length,
      ' Format=',SelData.Format,
      ' Data=',HexStr(Cardinal(SelData.Data),8),
      ' Now=',Now
      );
    {$EndIf}
    if (SelData.Selection<>ClipboardTypeAtoms[ClipboardType])
    or (SelData.Target<>AllID)
    or (SelData.Format<=0)
    or ((SelData.{$IfDef GTK2}_Type{$Else}TheType{$EndIf}<>gdk_atom_intern('ATOM',GdkFalse))
         and (SelData.{$IfDef GTK2}_Type{$Else}TheType{$EndIf}<>AllID))
    then
      exit;
    Cnt:=SelData.Length div (SelData.Format shr 3);
    if (SelData.Data<>nil) and (Cnt>0) then begin
      Count:=Cnt;
      FormatAtoms:=PGdkAtom(SelData.Data);
      // add transformable lcl formats
      // for example: the lcl expects text as 'text/plain', but gtk applications
      // also know 'TEXT' and 'STRING'. These formats can automagically
      // transformed into the lcl format, so the lcl format is also supported
      // and will be added to the list

      AddTextPlain:=false;
      if (not IsFormatSupported(gdk_atom_intern('text/plain',GdkTrue)))
      and (IsFormatSupported([gfCOMPOUND_TEXT,gfTEXT,gfSTRING,gfFILE_NAME,
        gfHOST_NAME,gfUSER]))
      then begin
        AddTextPlain:=true;
        inc(Count);
      end;

      // copy normal supported formats
      GetMem(List,SizeOf(TClipboardFormat)*Count);
      i:=0;
      while (i<Cnt) do begin
        {$IfDef DEBUG_CLIPBOARD}
        DebugLn('[TGtkWidgetSet.ClipboardGetFormats] Supported formats: ',
                i,'/',Cnt,':  ',FormatAtoms[i]);
        DebugLn('  MimeType="',ClipboardFormatToMimeType(FormatAtoms[i]),'"');
        {$EndIf}
        List[i]:=FormatAtoms[i];
        inc(i);
      end;

      // add all lcl formats that the gtk-interface can transform from the
      // supported formats
      if AddTextPlain then begin
        List[i]:=gdk_atom_intern('text/plain',GdkFalse);
        inc(i);
      end;
    end;
  finally
    if SelData.Data<>nil then FreeMem(SelData.Data);
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: ClipboardGetOwnerShip
  Params:  ClipboardType
           OnRequestProc - TClipboardRequestEvent is defined in LCLIntf.pp
                           If OnRequestProc is nil the onwership will end.
           FormatCount - number of formats
           Formats - array of TClipboardFormat. The supported formats the owner
                      provides.

  Returns: true on success

  Sets the supported formats and requests ownership for the clipboard.
  Each time the clipboard is read the OnRequestProc will be executed.
  If someone else requests the ownership, the OnRequestProc will be executed
  with the invalid FormatID 0 to notify the old owner of the lost of ownership.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
var TargetEntries: PGtkTargetEntry;

  function IsFormatSupported(FormatID: cardinal): boolean;
  var i: integer;
  begin
    if FormatID=0 then begin
      Result:=false;
      exit;
    end;
    i:=FormatCount-1;
    while (i>=0) and (Formats[i]<>FormatID) do dec(i);
    Result:=(i>=0);
  end;

  procedure AddTargetEntry(var Index: integer; const FormatName: string);
  begin
    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('  AddTargetEntry ',FormatName);
    {$EndIf}
    TargetEntries[Index].Target := StrAlloc(Length(FormatName) + 1);
    StrPCopy(TargetEntries[Index].Target, FormatName);
    TargetEntries[Index].Info:=Index;
    inc(Index);
  end;

{function TGtkWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;}
var
  TargetEntriesSize, i: integer;
  gtkFormat: TGtkClipboardFormat;
  ExpFormatCnt: integer;
  OldClipboardWidget: PGtkWidget;
begin
  if ClipboardType in [ctPrimarySelection, ctSecondarySelection, ctClipboard] then
  begin
    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('[TGtkWidgetSet.ClipboardGetOwnerShip] A');
    {$EndIf}
    ClipboardHandler[ClipboardType]:=nil;
    Result:=false;
    if (ClipboardWidget=nil) or (FormatCount=0) or (Formats=nil) then
    begin
      // end ownership
      if (ClipBoardWidget <> nil)
      and (GetControlWindow(ClipboardWidget)<>nil)
      and (gdk_selection_owner_get(ClipboardTypeAtoms[ClipboardType]) =
           GetControlWindow(ClipboardWidget))
      then begin
        gtk_selection_owner_set(nil,ClipboardTypeAtoms[ClipboardType],0);
      end;
      Result:=true;
      exit;
    end;

    // registering targets

    FreeClipboardTargetEntries(ClipboardType);

    // the gtk-interface adds automatically some gtk formats the lcl does not
    // know
    ExpFormatCnt:=FormatCount;
    for gtkFormat:=Low(TGtkClipboardFormat) to High(TGtkClipboardFormat) do
      ClipboardExtraGtkFormats[ClipboardType][gtkFormat]:=false;
    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('[TGtkWidgetSet.ClipboardGetOwnerShip] B');
    {$EndIf}
    if IsFormatSupported(gdk_atom_intern('text/plain',GdkTrue)) then
    begin
      // lcl provides 'text/plain' and the gtk-interface will automatically
      // provide some more text formats
      ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]:=
          not IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[gfCOMPOUND_TEXT]),GdkFalse));
      ClipboardExtraGtkFormats[ClipboardType][gfSTRING]:=not IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[gfSTRING]),GdkFalse));
      ClipboardExtraGtkFormats[ClipboardType][gfTEXT]:=not IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[gfTEXT]),GdkFalse));
    end;

    for gtkFormat:=Low(TGtkClipboardFormat) to High(TGtkClipboardFormat) do
      if ClipboardExtraGtkFormats[ClipboardType][gtkFormat] then
        inc(ExpFormatCnt);

    // build TargetEntries
    TargetEntriesSize:=SizeOf(TGtkTargetEntry) * ExpFormatCnt;
    GetMem(TargetEntries,TargetEntriesSize);
    FillChar(TargetEntries^,TargetEntriesSize,0);
    i:=0;
    while i<FormatCount do
      AddTargetEntry(i,ClipboardFormatToMimeType(Formats[i]));
    for gtkFormat:=Low(TGtkClipboardFormat) to High(TGtkClipboardFormat) do
      if ClipboardExtraGtkFormats[ClipboardType][gtkFormat] then
        AddTargetEntry(i,GtkClipboardFormatName[gtkFormat]);

    // set the supported formats
    ClipboardTargetEntries[ClipboardType]:=TargetEntries;
    ClipboardTargetEntryCnt[ClipboardType]:=ExpFormatCnt;

    // reset the clipboard widget (this will set the new target list)
    OldClipboardWidget:=ClipboardWidget;
    SetClipboardWidget(nil);
    SetClipboardWidget(OldClipboardWidget);

    // taking the ownership
    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('[TGtkWidgetSet.ClipboardGetOwnerShip] C');
    {$EndIf}
    if gtk_selection_owner_set(ClipboardWidget,
      ClipboardTypeAtoms[ClipboardType],0)=GdkFalse
    then begin
      {$IfDef DEBUG_CLIPBOARD}
      DebugLn('[TGtkWidgetSet.ClipboardGetOwnerShip] D FAILED');
      {$EndIf}
      exit;
    end;

    {$IfDef DEBUG_CLIPBOARD}
    DebugLn('[TGtkWidgetSet.ClipboardGetOwnerShip] YEAH, got it!');
    {$EndIf}
    ClipboardHandler[ClipboardType]:=OnRequestProc;

    Result:=true;
  end else
    { the gtk does not support this kind of clipboard, so the application can
     have the ownership at any time. The TClipboard in clipbrd.pp has an
     internal cache system, so that an application can use all types of
     clipboards even if the underlying platform does not support it.
     Of course this will only be a local clipboard, invisible to other
     applications. }
    Result:=true;
end;

{------------------------------------------------------------------------------
  Function: ClipboardRegisterFormat
  Params:  AMimeType
  Returns: the registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ClipboardRegisterFormat(
  const AMimeType:String): TClipboardFormat;
var AtomName: PChar;
begin
  if Assigned(Application) then begin
    AtomName:=PChar(AMimeType);
    Result:=gdk_atom_intern(AtomName,GdkFalse);
  end else
    RaiseException(
      'ERROR: TGtkWidgetSet.ClipboardRegisterFormat gdk not initialized');
end;


{------------------------------------------------------------------------------
  Function: CreateBitmap
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  GdiObject: PGdiObject;
  DefGdkWindow: PGdkWindow;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.CreateBitmap] Width: %d, Height: %d, Planes: %d, BitCount: %d, BitmapBits: 0x%x', [Width, Height, Planes, BitCount, Longint(BitmapBits)]));

  if (BitCount < 1) or (Bitcount > 32)
  then begin
    Result := 0;
    DebugLn(Format('ERROR: [TGtkWidgetSet.CreateBitmap] Illegal depth %d', [BitCount]));
    Exit;
  end;

//write('TGtkWidgetSet.CreateBitmap->');
  GdiObject := NewGDIObject(gdiBitmap);

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}

  // if the bitcount is 1 then create a gdkbitmap
  // else create a gdkpixmap

  {if BitCount > 1
  then begin
    Assert(False, Format('Trace:  [TGtkWidgetSet.CreateBitmap] gbPixmap', [])); }
    DefGdkWindow := nil;
    If BitCount = 1 then begin
      GdiObject^.GDIBitmapType := gbBitmap;
      GdiObject^.GDIBitmapObject := CreateGdkBitmap(DefGdkWindow,Width,Height);
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIBitmapObject);
    end
    else begin
      GdiObject^.GDIBitmapType := gbPixmap;
      GdiObject^.GDIPixmapObject :=
        gdk_pixmap_new(DefGdkWindow, Width, Height, BitCount);
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);
    end;
    gdk_visual_ref(GdiObject^.Visual);
    GdiObject^.SystemVisual := False;

    // the visual is created only when needed
    {If GdiObject^.Visual <> nil then
      gdk_visual_ref(GdiObject^.Visual)
    else begin
      GdiObject^.Visual := gdk_visual_get_best_with_depth(BitCount);
      if GdiObject^.Visual=nil then begin
        DebugLn('Warning: [TGtkWidgetSet.CreateBitmap] No visual for depth ',
                BitCount,'. Using default.');
        GdiObject^.Visual := gdk_visual_get_system;
      end;
    end;}

    // the colormap is only created if needed
    //GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, GdkTrue);
    {$IFDEF DebugGDKTraps}
    EndGDKErrorTrap;
    {$ENDIF}

    If BitmapBits <> nil then
      LoadFromPixbufData(hBitmap(GdiObject), BitmapBits);

  {end
  else if Bitcount = 1
  then begin
    Assert(False, Format('Trace:  [TGtkWidgetSet.CreateBitmap] gbBitmap', []));
    GdiObject^.GDIBitmapType := gbBitmap;
    GdiObject^.GDIBitmapObject := gdk_pixmap_new(nil, Width, Height, BitCount);

    GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIBitmapObject);
    If GdiObject^.Visual = nil then
      GdiObject^.Visual := gdk_visual_get_best_with_depth(BitCount)
    else
       gdk_visual_ref(GdiObject^.Visual);

    GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, 1)
  end;
  else begin
    Assert(False, Format('Trace:  [TGtkWidgetSet.CreateBitmap] gbImage', []));
    GdiObject^.GDIBitmapType := gbImage;
    GdiObject^.GDI_RGBImageObject := NewGDI_RGBImage(Width, Height, BitCount);
    GdiObject^.Visual := gdk_visual_get_best_with_depth(BitCount);
    GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, 1);
  end;}

  Result := HBITMAP(GdiObject);
//DebugLn('[TGtkWidgetSet.CreateBitmap] ',HexStr(Result,8));
  Assert(False, Format('Trace:< [TGtkWidgetSet.CreateBitmap] --> 0x%x', [Integer(Result)]));
end;


{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
    var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): Boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
var
  GdiObject: PGDIObject;
  DefGDkWindow: PGdkWindow;
  GDkWindow: PGdkWindow;
  GC: PGdkGC;
  ImgData: Pointer;
  ImgWidth: Cardinal;
  ImgHeight: Cardinal;
  ImgDepth: Cardinal;
  Visual: PGdkVisual;
  GdkImage: PGdkImage;
  ImgDataSize: Cardinal;
begin
  Result:=false;
  Bitmap:=0;
  MaskBitmap:=0;

  if (RawImage.Description.Width=0) or (RawImage.Description.Height=0) then
    exit;

  try
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage A ',
      ' AlwaysCreateMask='+dbgs(AlwaysCreateMask),
      ' Depth='+dbgs(RawImage.Description.Depth),
      ' Width='+dbgs(RawImage.Description.Width),
      ' Height='+dbgs(RawImage.Description.Height),
      ' Data='+HexStr(Cardinal(RawImage.Data),8),
      ' DataSize='+dbgs(RawImage.DataSize)+
      ' Mask='+HexStr(Cardinal(RawImage.Mask),8)+
      ' MaskSize='+dbgs(RawImage.MaskSize)+
      ' Palette='+HexStr(Cardinal(RawImage.Palette),8)+
      ' PaletteSize='+dbgs(RawImage.PaletteSize)+
      ' BitsPerPixel='+dbgs(RawImage.Description.BitsPerPixel)+
      '');
    {$ENDIF}

    // ToDo: check description

    DefGdkWindow := nil;
    GdiObject := NewGDIObject(gdiBitmap);
    GdiObject^.GDIBitmapType := gbPixmap;

    // create Pixmap from data
    ImgWidth:=RawImage.Description.Width;
    ImgHeight:=RawImage.Description.Height;
    ImgDepth:=RawImage.Description.Depth;
    ImgData:=RawImage.Data;
    ImgDataSize:=RawImage.DataSize;
    
    if ImgDepth=1 then begin
      // create a GdkBitmap
      if RawImage.Data<>nil then begin
        GDkWindow:=gdk_bitmap_create_from_data(DefGdkWindow,ImgData,
                                               ImgWidth,ImgHeight);
      end else begin
        GDkWindow := CreateGdkBitmap(DefGdkWindow,ImgWidth,ImgHeight);
      end;
      GdiObject^.GDIBitmapObject := GDkWindow;
      GdiObject^.GDIBitmapType := gbBitmap;
    end else begin
      // create a GdkPixmap
      if RawImage.Data<>nil then begin

        { The gdk_pixmap_create_from_data seems to be buggy.
          It only creates pixmaps of Depth 1
            gdk_pixmap_create_from_data(DefGdkWindow,PGChar(RawImage.Data),
                          RawImage.Description.Width, RawImage.Description.Height,
                          RawImage.Description.Depth, @fg,@bg);}
        GdkWindow:=gdk_pixmap_new(DefGdkWindow,ImgWidth,ImgHeight,ImgDepth);
        // Create a GdkImage, copy our data into it and create a pixmap from it
        Visual:=gdk_visual_get_best_with_depth(ImgDepth);
        GdkImage:=gdk_image_new(GDK_IMAGE_FASTEST,Visual,ImgWidth,ImgHeight);
        {$IFDEF VerboseRawImage}
        DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage GdkImage: ',
          ' BytesPerLine=',dbgs(GdkImage^.bpl),
          ' BytesPerPixel=',dbgs(GdkImage^.bpp),
          ' ByteOrder=',dbgs(GdkImage^.byte_order),
          '');
        {$ENDIF}
        if (RawImage.Description.BitsPerPixel<>(cardinal(GdkImage^.bpp) shl 3))
        then begin
          RaiseGDBException('TGtkWidgetSet.CreateBitmapFromRawImage Incompatible BitsPerPixel');
        end;
        if (ImgDataSize<>GdkImage^.bpl*ImgHeight) then begin
          RaiseGDBException('TGtkWidgetSet.CreateBitmapFromRawImage Incompatible DataSize');
        end;
        System.Move(ImgData^,GdkImage^.mem^,ImgDataSize);
        GC:=gdk_gc_new(GDkWindow);
        gdk_draw_image(PGDKDrawable(GdkWindow),GC,
                       GdkImage,0,0,0,0,ImgWidth,ImgHeight);
        gdk_gc_unref(GC);
        gdk_image_destroy(GdkImage);
      end else begin
        GDkWindow := gdk_pixmap_new(DefGdkWindow,
                         RawImage.Description.Width,RawImage.Description.Height,
                         RawImage.Description.Depth);
      end;
      GdiObject^.GDIPixmapObject := GDkWindow;
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);
      gdk_visual_ref(GdiObject^.Visual);
      GdiObject^.SystemVisual := False;
    end;

    // if we are here the bitmap was created successfully
    Bitmap:=HBITMAP(GdiObject);

    // create mask
    if (AlwaysCreateMask or (not RawImageMaskIsEmpty(@RawImage,true)))
    and (RawImage.Mask<>nil) then begin
      {$IFDEF VerboseRawImage}
      DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage creating mask .. ');
      {$ENDIF}
      GdiObject^.GDIBitmapMaskObject :=
          gdk_bitmap_create_from_data(DefGdkWindow,PGChar(RawImage.Mask),
                       RawImage.Description.Width, RawImage.Description.Height);
    end;

  except
    if Bitmap<>0 then DeleteObject(Bitmap);
    Bitmap:=0;
    if MaskBitmap<>0 then DeleteObject(MaskBitmap);
    MaskBitmap:=0;
    exit;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
const
  //HATCH_NULL      : array[0..7] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00);
  HATCH_BDIAGONAL : array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  HATCH_CROSS     : array[0..7] of Byte = ($08, $08, $08, $FF, $08, $08, $08, $08);
  {This is too fine for a Cross Hatch ($22, $22, $FF, $22, $22, $22, $FF, $22);}
  HATCH_DIAGCROSS : array[0..7] of Byte = ($81, $42, $24, $18, $18, $24, $42, $81);
  HATCH_FDIAGONAL : array[0..7] of Byte = ($01, $02, $04, $08, $10, $20, $40, $80);
  HATCH_HORIZONTAL: array[0..7] of Byte = ($00, $00, $00, $FF, $00, $00, $00, $00);
  HATCH_VERTICAL  : array[0..7] of Byte = ($08, $08, $08, $08, $08, $08, $08, $08);
var
  GObject: PGdiObject;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.CreateBrushIndirect]  Style: %d, Color: %8x', [LogBrush.lbStyle, LogBrush.lbColor]));

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}

  //write('CreateBrushIndirect->');
  GObject := NewGDIObject(gdiBrush);
  try
    {$IFDEF DebugGDIBrush}
    DebugLn('[TGtkWidgetSet.CreateBrushIndirect] ',HexStr(Cardinal(GObject),8));
    {$ENDIF}
    GObject^.IsNullBrush := False;
    with LogBrush do
    begin
      case lbStyle of
   //     BS_HOLLOW,         // Hollow brush.
        BS_NULL:           // Same as BS_HOLLOW.
          begin
            //GObject^.GDIBrushFill := GDK_STIPPLED;
            //GObject^.GDIBrushPixmap :=
            //  gdk_bitmap_create_from_data(nil, @HATCH_NULL, 8, 8);
            GObject^.IsNullBrush := True;
          end;

        BS_SOLID:          // Solid brush.
          begin
            GObject^.GDIBrushFill := GDK_SOLID;
          end;

        BS_HATCHED:        // Hatched brush.
          begin
            GObject^.GDIBrushFill := GDK_STIPPLED;
            case lbHatch of
              HS_BDIAGONAL:
                GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(
                   nil, @HATCH_BDIAGONAL, 8, 8);
              HS_CROSS:
                GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(
                   nil, @HATCH_CROSS, 8, 8);
              HS_DIAGCROSS:
                GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(
                   nil, @HATCH_DIAGCROSS, 8, 8);
              HS_FDIAGONAL:
                GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(
                   nil, @HATCH_FDIAGONAL, 8, 8);
              HS_HORIZONTAL:
                GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(
                   nil, @HATCH_HORIZONTAL, 8, 8);
              HS_VERTICAL:
                GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(
                   nil, @HATCH_VERTICAL, 8, 8);
              else
                RaiseGDBException('invalid lbHatch');
            end;
          end;

        BS_DIBPATTERN,     // A pattern brush defined by a device-independent
             // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERN, the
             // lbHatch member contains a handle to a packed DIB.Windows 95:
             // Creating brushes from bitmaps or DIBs larger than 8x8 pixels
             // is not supported. If a larger bitmap is given, only a portion
             // of the bitmap is used.
        BS_DIBPATTERN8X8,  // Same as BS_DIBPATTERN.
        BS_DIBPATTERNPT,   // A pattern brush defined by a device-independent
             // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERNPT, the
             // lbHatch member contains a pointer to a packed DIB.
        BS_PATTERN,        // Pattern brush defined by a memory bitmap.
        BS_PATTERN8X8:     // Same as BS_PATTERN.
          begin
            GObject^.GDIBrushFill := GDK_TILED;
            if IsValidGDIObject(lbHatch)
            and (PGdiObject(lbHatch)^.GDIType = gdiBitmap)
            then
              GObject^.GDIBrushPixmap := PGdiObject(lbHatch)^.GDIBitmapObject
            else
              RaiseGDBException('unsupported bitmap');
          end;

        else
          RaiseGDBException(Format('unsupported Style %d',[lbStyle]));
      end;

      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}

      If not GObject^.IsNullBrush then
        SetGDIColorRef(GObject^.GDIBrushColor,lbColor);
    end;
    Result := HBRUSH(GObject);
  except
    Result:=0;
    DisposeGDIObject(GObject);
    DebugLn('TGtkWidgetSet.CreateBrushIndirect failed');
  end;
  Assert(False, Format('Trace:< [TGtkWidgetSet.CreateBrushIndirect] Got --> %x', [Result]));
end;

{------------------------------------------------------------------------------
  Function: CreateCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap;
  Width, Height: Integer): Boolean;
var
  GTKObject: PGTKObject;
  BMP: PGDKPixmap;
begin
  Assert(False, 'Trace:TODO: [TGtkWidgetSet.CreateCaret] Finish');

  GTKObject := PGTKObject(Handle);
  Result := GTKObject <> nil;

  if Result then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      if IsValidGDIObjectType(Bitmap, gdiBitmap) then
        BMP := PGdiObject(Bitmap)^.GDIBitmapObject
      else
        BMP := nil;
      GTKAPIWidget_CreateCaret(PGTKAPIWidget(GTKObject), Width, Height, BMP);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else Assert(False, 'Trace:WARNING: [TGtkWidgetSet.CreateCaret] Got null HWND');
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleBitmap
  Params: DC:
          Width:
          Height:
  Returns:

  Creates a bitmap compatible with the specified device context.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateCompatibleBitmap(DC: HDC;
  Width, Height: Integer): HBITMAP;
var
  Depth : Longint;
  GDIObject: PGdiObject;
  DefGdkWindow: PGDkWindow;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.CreateCompatibleBitmap] DC: 0x%x, W: %d, H: %d', [DC, Width, Height]));

  Depth := -1;

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}
  if (IsValidDC(DC) and (TDeviceContext(DC).Drawable <> nil)) then begin
    DefGdkWindow := TDeviceContext(DC).Drawable;
    Depth := gdk_drawable_get_depth(TDeviceContext(DC).Drawable);
  end else
    DefGdkWindow:=nil;
  If Depth = -1 then
    Depth := gdk_visual_get_system^.Depth;

  if Depth <> -1 then begin
    if (Depth < 1) or (Depth > 32)
    then begin
      Result := 0;
      DebugLn(Format('ERROR: [TGtkWidgetSet.CreateCompatibleBitmap] Illegal depth %d', [Depth]));
      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}
      Exit;
    end;

    GdiObject := NewGDIObject(gdiBitmap);

    If Depth = 1 then begin
      GdiObject^.GDIBitmapType := gbBitmap;
      GdiObject^.GDIBitmapObject :=
        gdk_pixmap_new(DefGdkWindow, Width, Height, Depth);
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);
    end
    else begin
      GdiObject^.GDIBitmapType := gbPixmap;
      GdiObject^.GDIPixmapObject :=
        gdk_pixmap_new(DefGdkWindow, Width, Height, Depth);
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);
    end;

    If GdiObject^.Visual = nil then begin
      GdiObject^.Visual := gdk_visual_get_best_with_depth(Depth);
      If GdiObject^.Visual = nil then 
        GdiObject^.Visual := gdk_visual_get_system;
      GdiObject^.SystemVisual := True;
    end
    else begin
      gdk_visual_ref(GdiObject^.Visual);
      GdiObject^.SystemVisual := False;
    end;

    GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, GdkTrue);

    {$IFDEF DebugGDKTraps}
    EndGDKErrorTrap;
    {$ENDIF}

    Result := HBITMAP(GdiObject);

  end else
    Result := 0;

  Assert(False, Format('Trace:< [TGtkWidgetSet.CreateCompatibleBitmap] DC: 0x%x --> 0x%x', [DC, Result]));
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
var
  pNewDC: TDeviceContext;
begin
  Result := 0;
  pNewDC := NewDC;

  // dont copy
  // In a compatible DC you have to select a bitmap into it
(*
  if IsValidDC(DC) then
    with TDeviceContext(DC)^ do
    begin
      pNewDC^.hWnd := hWnd;
      pNewDC^.Drawable := Drawable;
      pNewDC^.GC := gdk_gc_new(Drawable);
    end
  else begin
    // We can't do anything yet
    // Wait till a bitmap get selected
  end;
*)

  pNewDC.CurrentFont := CreateDefaultFont;
  pNewDC.CurrentBrush := CreateDefaultBrush;
  pNewDC.CurrentPen := CreateDefaultPen;

  Result := HDC(pNewDC);

  Assert(False,Format('trace:  [TGtkWidgetSet.CreateCompatibleDC] DC: 0x%x --> 0x%x', [Integer(DC), Integer(Result)]));
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result:=CreateFontIndirectEx(LogFont,'');
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirectEx
  Params:  const LogFont: TLogFont; const LongFontName: string
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
{$IfDef GTK2}
begin
  DebugLn('ToDo: TGtkWidgetSet.CreateFontIndirectEx');
  Result:=0;
end;
{$Else}
var
  GdiObject: PGdiObject;
  FontNameRegistry, Foundry, FamilyName, WeightName,
  Slant, SetwidthName, AddStyleName, PixelSize,
  PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
  CharSetRegistry, CharSetCoding: string;
  n: Integer;
  CachedFont: TGdkFontCacheDescriptor;

  function LoadFont: boolean;
  var
    S: string;
    Desc: TGdkFontCacheDescriptor;
  begin
    S:=FontNameRegistry+'-'+Foundry+'-'+FamilyName+'-'+WeightName
       +'-'+Slant+'-'+SetwidthName+'-'+AddStyleName+'-'+PixelSize
       +'-'+PointSize+'-'+ResolutionX+'-'+ResolutionY+'-'+Spacing+'-'+AverageWidth
       +'-'+CharSetRegistry+'-'+CharSetCoding;
    { MG: heaptrc gets corrupted heap using the construction below:
    S := Format('%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s',
      [FontNameRegistry, Foundry, FamilyName, WeightName,
       Slant, SetwidthName, AddStyleName, PixelSize,
       PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
       CharSetRegistry, CharSetCoding
      ]);}

    //DebugLn('  Trying "',S,'"');
    {S:=FontNameRegistry+','+Foundry+','+FamilyName+','+WeightName
       +','+Slant+','+SetwidthName+','+AddStyleName+','+PixelSize
       +','+PointSize+','+ResolutionX+','+ResolutionY+','+Spacing+','+AverageWidth
       +','+CharSetRegistry+','+CharSetCoding;
    DebugLn('  Trying B "',S,'"');}

    GdiObject^.GDIFontObject := gdk_font_load(PChar(s));
    Result:=GdiObject^.GDIFontObject<>nil;
    
    if Result then begin
      Desc:=FontCache.Add(GdiObject^.GDIFontObject,LogFont,LongFontName);
      if Desc<>nil then
        Desc.xlfd:=s;
    end;
    
    {$IFDEF VerboseFonts}
    DebugLn('  Tried "',S,'" Success=',dbgs(GdiObject^.GDIFontObject<>nil));
    {$ENDIF}
  end;

  procedure LoadDefaultFont;
  begin
    DisposeGDIObject(GdiObject);
    GdiObject:=CreateDefaultFont;
    {$IFDEF VerboseFonts}
    DebugLn('TGtkWidgetSet.CreateFontIndirectEx.LoadDefaultFont');
    {$ENDIF}
  end;
  
  function GetDefaultFontFamilyName: string;
  begin
    Result:=GetDefaultFontName;
    if IsFontNameXLogicalFontDesc(Result) then
      Result := ExtractXLFDItem(LongFontName,2);
    if Result='' then Result:='*';
  end;
  
  function FamilyNameExists: boolean;
  var
    AFont: PGdkFont;
    S: String;
  begin
    S := '*-*-'+FamilyName+'*-*-*-*-*-*-*-*-*-*-*-*-*';
    AFont:=gdk_font_load(PChar(s));
    Result:=AFont<>nil;
    if Result then gdk_font_unref(AFont);
  end;
  
  function CheckFontNameIsMangledXLogicalFontDesc(const ALongFontName: string
    ): boolean;
  var
    c: Integer;
    i: Integer;
  begin
    c:=0;
    for i:=1 to length(ALongFontName) do
      if ALongFontName[i]='-' then inc(c);
    Result:=(c>5) and (c<>14);
    if Result then
      debugln('WARNING: Fontnamt "',ALongFontName,'" seems to be a XLFD fontname, but has 14<>',dbgs(c),' minus signs');
  end;
  

begin
  // For info about xlfd see:
  //    http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html
  // Lets fill in all the xlfd parts. Assume we have scalable fonts.

  {$IFDEF VerboseFonts}
  DebugLn('TGtkWidgetSet.CreateFontIndirectEx A Name=',LogFont.lfFaceName,' Height=',dbgs(LogFont.lfHeight),' LongName=',LongFontName);
  {$ENDIF}

  Result := 0;
  GDIObject := NewGDIObject(gdiFont);

  try
    GdiObject^.LogFont := LogFont;
    
    CachedFont:=FontCache.FindGDKFontDesc(LogFont,LongFontName);
    if CachedFont<>nil then begin
      CachedFont.Item.IncreaseRefCount;
      GdiObject^.GDIFontObject := TGdkFontCacheItem(CachedFont.Item).GdkFont;
      exit;
    end;

    // set default values
    FontNameRegistry := '*';
    Foundry := '*';
    FamilyName := '*';
    WeightName := '*';
    Slant := '*';
    SetwidthName := '*';
    AddStyleName := '*';
    PixelSize := '*';
    PointSize := '*';
    ResolutionX := '*';
    ResolutionY := '*';
    Spacing := '*';
    AverageWidth := '*';
    CharSetRegistry := '*';
    CharSetCoding := '*';

    // check if LongFontName is in XLFD format and get nicer defaults
    // This way, the user can set X fonts that are not supported by TFont.

    {$IFDEF VerboseFonts}
    DebugLn('TGtkWidgetSet.CreateFontIndirectEx Name="',LogFont.lfFaceName,'"',
    ' Long="',LongFontName,'" IsXLFD=',dbgs(IsFontNameXLogicalFontDesc(LongFontName))
    ,' ',dbgs(ord(LogFont.lfFaceName[0])));
    {$ENDIF}
    if IsFontNameXLogicalFontDesc(LongFontName) then begin
      FontNameRegistry := ExtractXLFDItem(LongFontName,0);
      Foundry          := ExtractXLFDItem(LongFontName,1);
      FamilyName       := ExtractXLFDItem(LongFontName,2);
      WeightName       := ExtractXLFDItem(LongFontName,3);
      Slant            := ExtractXLFDItem(LongFontName,4);
      SetWidthName     := ExtractXLFDItem(LongFontName,5);
      AddStyleName     := ExtractXLFDItem(LongFontName,6);
      PixelSize        := ExtractXLFDItem(LongFontName,7);
      PointSize        := ExtractXLFDItem(LongFontName,8);
      ResolutionX      := ExtractXLFDItem(LongFontName,9);
      ResolutionY      := ExtractXLFDItem(LongFontName,10);
      Spacing          := ExtractXLFDItem(LongFontName,11);
      AverageWidth     := ExtractXLFDItem(LongFontName,12);
      CharSetRegistry  := ExtractXLFDItem(LongFontName,13);
      CharSetCoding    := ExtractXLFDItem(LongFontName,14);
    end else if CheckFontNameIsMangledXLogicalFontDesc(LongFontName) then begin
    end;

    with LogFont do
    begin

      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [TGtkWidgetSet.CreateFontIndirectEx] No fontname');
        Exit;
      end;

      FamilyName := StrPas(lfFaceName); //StringReplace(FaceName, ' ', '*');
      
      if (AnsiCompareText(FamilyName,'default')<>0)
      and (not FamilyNameExists) then begin
        FamilyName:='default';
      end;
      
      if AnsiCompareText(FamilyName,'default')=0 then begin
        {$IFDEF VerboseFonts}
        DebugLn('TGtkWidgetSet.CreateFontIndirectEx FamilyName="',FamilyName,'" PixelSize=',PixelSize,' LogFont.lfHeight=',LogFont.lfHeight);
        {$ENDIF}
        if (LogFont.lfHeight=0) then begin
          LoadDefaultFont;
          exit;
        end else begin
          FamilyName:=GetDefaultFontFamilyName;
          Foundry:='*';
        end;
      end;

      Assert(False, Format('trace:  [TGtkWidgetSet.CreateFontIndirectEx] Name: %s, Height: %d', [FamilyName, lfHeight]));

      // calculate weight offset.
      //        API                 XLFD
      // --------------------- --------------
      // Weight=400 --> normal normal
      // Weight=700 --> bold   normal+4000 (or bold in non scalable fonts)
      //
      // So in API the offset for normal = 400 and an increase of 300 equals to
      // an offset of 4000
      if WeightName='*' then begin
        case lfWeight of
          FW_DONTCARE   : WeightName := '*';
          FW_LIGHT      : WeightName := 'light';
          FW_NORMAL     : WeightName := 'normal';
          FW_MEDIUM     : WeightName := 'medium';
          FW_SEMIBOLD   : WeightName := 'demi bold';
          FW_BOLD       : WeightName := 'bold';

          else begin
            n := ((lfWeight - FW_NORMAL) * 4000) div (FW_BOLD - FW_NORMAL);
            if n = 0
            then WeightName := 'normal'
            else if n > 0
            then WeightName := Format('normal+%d', [n])
            else WeightName := Format('normal%d', [n]);
          end;
        end;
      end;

      if Slant='*' then begin
        // TODO: find out if escapement has something to do with slant
        if lfItalic = 0 then Slant := 'r' else Slant := 'i';
      end;

      // SetWidthName := '*';

      if AddStyleName='*' then begin
        // calculate Style name extentions (=rotation)
        //        API                 XLFD
        // --------------------- --------------
        // Orientation 1/10 deg  1/64 deg
        if lfOrientation = 0
        then AddStyleName := '*'
        else begin
          n := (lfOrientation * 64) div 10;
          if n >= 0
          then AddStyleName := Format('+%d', [n])
          else AddStyleName := Format('+%d', [n]);
        end;
      end;

      if (PixelSize='*') and (PointSize='*') then begin
        // TODO: make more accurate (implement the meaning of
        //       positive and negative height values.
        PixelSize := IntToStr(Abs(lfHeight));

        // Since we use pixelsize, it isn't allowed to give a value here
        PointSize := '*';

        // Use the default
        ResolutionX := '*';
        ResolutionY := '*';
      end;

      if Spacing='*' then begin
        // spacing
        if (FIXED_PITCH and lfPitchAndFamily)>0 then
          Spacing := 'm'   // mono spaced
        else if (VARIABLE_PITCH and lfPitchAndFamily)>0 then
          Spacing := 'p'   // proportional spaced
        else
          Spacing := '*';
      end;

      if AverageWidth='*' then begin
        // calculate AverageWidth
        //        API                 XLFD
        // --------------------- --------------
        // Width pixel             1/10 pixel
        if lfWidth = 0
        then AverageWidth := '*'
        else AverageWidth := InttoStr(lfWidth * 10);
      end;

      if CharSetCoding =  '*' then begin
        case lfCharset of
        FCS_ISO_10646_1: begin
            CharSetRegistry:='iso10646';
            CharSetCoding:='1';
          end;
        fcs_ISO_8859_1: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='1';
          end;
        fcs_ISO_8859_2: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='2';
          end;
        fcs_ISO_8859_3: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='3';
          end;
        fcs_ISO_8859_4: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='4';
          end;
        fcs_ISO_8859_5: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='5';
          end;
        fcs_ISO_8859_6: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='6';
          end;
        fcs_ISO_8859_7: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='7';
          end;
        fcs_ISO_8859_8: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='8';
          end;
        fcs_ISO_8859_9: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='9';
          end;
        fcs_ISO_8859_10: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='10';
          end;
        fcs_ISO_8859_15: begin
            CharSetRegistry:='iso8859';
            CharSetCoding:='15';
          end;
        end;
      end;
    end;
     

    {$IFDEF VerboseFonts}
    write('CreateFontIndirect->');
    {$ENDIF}
    if LoadFont then exit;

    if (WeightName='normal') then begin
      WeightName:='medium';
      if LoadFont then exit;
    end else if (WeightName='bold') then begin
      WeightName:='black';
      if LoadFont then exit;
    end;

    if (WeightName='medium') then begin
      WeightName:='regular';
      if LoadFont then exit;
    end else if (WeightName='black') then begin
      WeightName:='demi bold';
      if LoadFont then exit;
    end;

    // try all weights
    WeightName := '*';
    if LoadFont then exit;

    // try one height lower
    PixelSize := IntToStr(Abs(LogFont.lfHeight)-1);
    if LoadFont then exit;

    // try one height higher
    PixelSize := IntToStr(Abs(LogFont.lfHeight)+1);
    if LoadFont then exit;

    PixelSize := IntToStr(Abs(LogFont.lfHeight));

    // try instead of mono spaced -> character cell spaced
    if (Spacing='m') then begin
      Spacing:='c';
      if LoadFont then exit;
    end;

    // try instead of italic -> oblique
    if (Slant='i') then begin
      Slant := 'o';
      if LoadFont then exit;
    end;

    // try all slants
    Slant := '*';
    if LoadFont then exit;

    // try all spacings
    if spacing<>'*' then begin 
      Spacing := '*';
      if LoadFont then exit;
    end; 

    if charSetCoding<>'*' then begin 
      charsetCoding := '*';
      charSetRegistry:= '*'; 
      if LoadFont then exit;
    end; 

    if (Foundry<>'*') then begin
      // try all Families
      PixelSize := IntToStr(Abs(LogFont.lfHeight));
      FamilyName := '*';
      if LoadFont then exit;
    end;
    
    // nothing exists -> use default
    LoadDefaultFont;

  finally
    if GdiObject^.GDIFontObject = nil
    then begin
      {$IFDEF VerboseFonts}
      DebugLn('[TGtkWidgetSet.CreateFontIndirect] ',HexStr(Cardinal(GdiObject),8),'  ',FGDIObjects.Count);
      {$ENDIF}
      DisposeGDIObject(GdiObject);
      Result := 0;
    end
    else begin
      Result := HFONT(GdiObject);
    end;

    if Result = 0
    then DebugLn('WARNING: [TGtkWidgetSet.CreateFontIndirectEx] NOT found XLFD: <'+LongFontName+'> Fontname="'+LogFont.lfFaceName+'"')
    else Assert(False, Format('Trace:  [TGtkWidgetSet.CreateFontIndirectEx] found XLFD: <%s>', [LongFontName]));
  end;
end;
{$EndIf}

{------------------------------------------------------------------------------
  Function: CreatePalette
  Params:  LogPalette
  Returns: a handle to the Palette created


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreatePalette(const LogPalette: TLogPalette): HPALETTE;
var
  GObject: PGdiObject;
begin
  Assert(False, 'trace:[TGtkWidgetSet.CreatePalette]');
  GObject := NewGDIObject(gdiPalette);

  with LogPalette, GObject^ do
  begin
    SystemPalette := False;

    PaletteRealized := False;

    VisualType := GDK_VISUAL_PSEUDO_COLOR;

    PaletteVisual := nil;

    {$IFDEF DebugGDKTraps}
    BeginGDKErrorTrap;
    {$ENDIF}

    PaletteVisual := gdk_visual_get_best_with_type(VisualType);
    If PaletteVisual = nil then begin
      PaletteVisual := GDK_Visual_Get_System;
      GDK_Visual_Ref(PaletteVisual);
    end;
    PaletteColormap := GDK_Colormap_new(PaletteVisual, GdkTrue);

    {$IFDEF DebugGDKTraps}
    EndGDKErrorTrap;
    {$ENDIF}

    RGBTable := TDynHashArray.Create(-1);
    RGBTable.OnGetKeyForHashItem:=@GetRGBAsKey;
    IndexTable := TDynHashArray.Create(-1);
    IndexTable.OnGetKeyForHashItem:=@GetIndexAsKey;
    InitializePalette(GObject, LogPalette.palPalEntry,
      MemSize(Pointer(LogPalette.palPalEntry)) div SizeOf(tagRGBQuad));
  end;

  Result := HPALETTE(GObject);
end;

{------------------------------------------------------------------------------
  Function: CreatePenIndirect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  GObject: PGdiObject;
begin
  Assert(False, 'trace:[TGtkWidgetSet.CreatePenIndirect]');
//write('CreatePenIndirect->');
  GObject := NewGDIObject(gdiPen);

  with LogPen do
  begin
    GObject^.GDIPenStyle := lopnStyle;
    GObject^.GDIPenWidth := lopnWidth.X;
    SetGDIColorRef(GObject^.GDIPenColor,lopnColor);
  end;

  Result := HPEN(GObject);
end;

{------------------------------------------------------------------------------
  Function: CreatePixmapIndirect
  Params:  Data: Raw pixmap data (PPGChar of xpm file,
                               You can use graphics.XPMToPPChar to create this)
  Returns: Handle to LCL bitmap

  Creates a bitmap from raw pixmap data.
  If TransColor < 0 the transparency mask will be automatically gnerated.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreatePixmapIndirect(const Data: Pointer;
  const TransColor: Longint): HBITMAP;
var
  GdiObject: PGdiObject;
  GDKColor: TGDKColor;
  Window: PGdkWindow;
  ColorMap: PGdkColormap;
  P: Pointer;
  Depth : Longint;
begin
  GdiObject := NewGDIObject(gdiBitmap);
  if TransColor >= 0 then begin
    GDKColor := AllocGDKColor(TransColor);
    p := @GDKColor;
  end else
    p:=nil; // automatically create transparency mask
  Window:=nil; // use the X root window for colormap

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}

  if Window<>nil then
    ColorMap:=gdk_window_get_colormap(Window)
  else
    ColorMap:=gdk_colormap_get_system;

  try
    GdiObject^.GDIPixmapObject :=
      gdk_pixmap_colormap_create_from_xpm_d(Window,Colormap,
             GdiObject^.GDIBitmapMaskObject, p, Data);

    Depth := gdk_drawable_get_depth(GdiObject^.GDIPixmapObject);
    If GdiObject^.Visual <> nil then
      GDK_Visual_UnRef(GdiObject^.Visual);

    GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);

    If GdiObject^.Visual = nil then begin
      GdiObject^.Visual := gdk_visual_get_best_with_depth(Depth);
      If GdiObject^.Visual = nil then 
        GdiObject^.Visual := gdk_visual_get_system;
      GdiObject^.SystemVisual := True;
    end
    else begin
      gdk_visual_ref(GdiObject^.Visual);
      GdiObject^.SystemVisual := False;
    end;

    If GdiObject^.Colormap <> nil then
      GDK_Colormap_UnRef(GdiObject^.Colormap);

    GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, GdkFalse);

    GdiObject^.GDIBitmapType:=gbPixmap;
  except
    DisposeGDIObject(GdiObject);
    GdiObject:=nil;
  end;
  {$IFDEF DebugGDKTraps}
  EndGDKErrorTrap;
  {$ENDIF}
  
  Result := HBITMAP(GdiObject);
end;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points, NumPts, FillMode
  Returns: the handle to the region

  Creates a Polygon, a closed many-sided shaped region. The Points parameter is
  an array of points that give the vertices of the polygon. FillMode=Winding
  determines what points are going to be included in the region. When Winding
  is True, points are selected by using the Winding fill algorithm. When Winding
  is False, points are selected by using using the even-odd (alternative) fill
  algorithm. NumPts indicates the number of points to use.
  The first point is always connected to the last point.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
var
  i: integer;
  PointArray: PGDKPoint;
  GObject: PGdiObject;
  fr : TGDKFillRule;
begin
  Result := 0;
  if NumPts<=0 then exit;
  GObject := NewGDIObject(gdiRegion);

  GetMem(PointArray,SizeOf(TGdkPoint)*NumPts);
  for i:=0 to NumPts-1 do begin
    PointArray[i].x:=Points[i].x;
    PointArray[i].y:=Points[i].y;
  end;

  If FillMode=Winding then
    fr := GDK_WINDING_RULE
  else
    fr := GDK_EVEN_ODD_RULE;

  GObject^.GDIRegionObject := gdk_region_polygon(PointArray, NumPts, fr);

  FreeMem(PointArray);

  Result := HRGN(GObject);
end;

{------------------------------------------------------------------------------
  Function: CreateRectRgn
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
var
  R: TGDKRectangle;
  RRGN: PGDKRegion;
  GObject: PGdiObject;
  RegionObj: PGdkRegion;
begin
  GObject := NewGDIObject(gdiRegion);
  if X1<=X2 then begin
    R.X := gint16(X1);
    R.Width := X2 - X1;
  end else begin
    R.X := gint16(X2);
    R.Width := X1 - X2;
  end;
  if Y1<=Y2 then begin
    R.Y := gint16(Y1);
    R.Height := Y2 - Y1;
  end else begin
    R.Y := gint16(Y2);
    R.Height := Y1 - Y1;
  end;

  RRGN := gdk_region_new;
  RegionObj:=PGdkRegion(gdk_region_union_with_rect(RRGN,@R));
  GObject^.GDIRegionObject := RegionObj;
  gdk_region_destroy(RRGN);

  Result := HRGN(GObject);
  //DebugLn('TGtkWidgetSet.CreateRectRgn A ',GDKRegionAsString(RegionObj));
end;

{------------------------------------------------------------------------------
  Function: CombineRgn
  Params:  Dest, Src1, Src2, fnCombineMode
  Returns: longint

  Combine the 2 Source Regions into the Destination Region using the specified
  Combine Mode. The Destination must already be initialized. The Return value
  is the Destination's Region type, or ERROR.

  The Combine Mode can be one of the following:
      RGN_AND  : Gets a region of all points which are in both source regions

      RGN_COPY : Gets an exact copy of the first source region

      RGN_DIFF : Gets a region of all points which are in the first source
                 region but not in the second.(Source1 - Source2)

      RGN_OR   : Gets a region of all points which are in either the first
                 source region or in the second.(Source1 + Source2)

      RGN_XOR  : Gets all points which are in either the first Source Region
                 or in the second, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.CombineRgn(Dest, Src1, Src2 : HRGN;
  fnCombineMode : Longint) : Longint;
var
  Continue : Boolean;
  D, S1, S2 : PGDKRegion;
  DObj, S1Obj, S2Obj : PGDIObject;
begin
  Result := SIMPLEREGION;
  DObj := PGdiObject(Dest);
  S1Obj := PGdiObject(Src1);
  S2Obj := PGdiObject(Src2);
  Continue := IsValidGDIObject(Dest) and IsValidGDIObject(Src1)
              and IsValidGDIObject(Src2);
  If Not Continue then begin
    DebugLn('WARNING: [TGtkWidgetSet.CombineRgn] Invalid HRGN');
    Result := Error;
  end
  else begin
    If DObj^.GDIRegionObject <> nil then begin
      gdk_region_destroy(DObj^.GDIRegionObject);
      DObj^.GDIRegionObject:=nil;
    end;
    S1 := S1Obj^.GDIRegionObject;
    S2 := S2Obj^.GDIRegionObject;
    //DebugLn('TGtkWidgetSet.CombineRgn A fnCombineMode=',fnCombineMode);
    Case fnCombineMode of
      RGN_AND :
        D := PGDKRegion(gdk_region_intersect(S1, S2));
      RGN_COPY :
        D := gdk_region_copy(S1);
      RGN_DIFF :
        D := PGDKRegion(gdk_region_subtract(S1, S2));
      RGN_OR :
        D := PGDKRegion(gdk_region_union(S1, S2));
      RGN_XOR :
        D := PGDKRegion(gdk_region_xor(S1, S2));
      else begin
        Result:= ERROR;
        D := nil;
      end;
    end;
    DObj^.GDIRegionObject := D;
    Result := RegionType(D);
    //DebugLn('TGtkWidgetSet.CombineRgn B Mode=',fnCombineMode,
    //  ' S1=',GDKRegionAsString(S1),' S2=',GDKRegionAsString(S2),' D=',GDKRegionAsString(D),'');
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.ComboBoxDropDown(Handle: HWND;
                                       DropDown: boolean): boolean; override;
------------------------------------------------------------------------------}
function TGtkWidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean;

  procedure gtk_combo_get_pos(combo : PGtkCombo; var x : gint; var  y : gint;
    var height : gint; var width : gint);
  var
    popwin : PGtkbin;
    widget : PGtkWidget;
    popup : PGtkScrolledwindow;
    real_height : gint;
    list_requisition : PGtkRequisition;
    show_hscroll : gboolean;
    show_vscroll : gboolean;
    avail_height : gint;
    min_height : gint;
    alloc_width : gint;
    work_height : gint;
    old_height : gint;
    old_width : gint;
    okay_to_exit : boolean;
  const
    EMPTY_LIST_HEIGHT = 15;
  begin
    show_hscroll := False;
    show_vscroll := False;

    widget := GTK_WIDGET(combo);
    popup  := GTK_SCROLLED_WINDOW (combo^.popup);
    popwin := GTK_BIN (combo^.popwin);

    gdk_window_get_origin (combo^.entry^.window, @x, @y);

    real_height := MIN (combo^.entry^.requisition.height,
                    combo^.entry^.allocation.height);
    y := y + real_height;

    avail_height := gdk_screen_height () - y;

    New(list_requisition);
    gtk_widget_size_request (combo^.list, list_requisition);

    min_height := MIN (list_requisition^.height,popup^.vscrollbar^.requisition.height);
    if  GTK_LIST (combo^.list)^.children = nil then
      list_requisition^.height := list_requisition^.height + EMPTY_LIST_HEIGHT;

    alloc_width := (widget^.allocation.width -
      2 * cardinal(gtk_widget_get_xthickness(gtk_bin_get_child(popwin))) -
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(popwin))^) -
      2 * border_width(GTK_CONTAINER (combo^.popup)^) -
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(PGTKBin(popup)))^) -
      2 * cardinal(gtk_widget_get_xthickness(gtk_bin_get_child(PGTKBin(popup)))));

    work_height := (2 * cardinal(gtk_widget_get_ythickness(gtk_bin_get_child(popwin))) +
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(popwin))^) +
      2 * border_width(GTK_CONTAINER (combo^.popup)^) +
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(PGTKBin(popup)))^) +
      2 * cardinal(gtk_widget_get_xthickness(gtk_bin_get_child(PGTKBin(popup)))));

    repeat
      okay_to_exit := True;
      old_width := alloc_width;
      old_height := work_height;

      if ((not show_hscroll) and (alloc_width < list_requisition^.width)) then
      begin
           work_height := work_height +  popup^.hscrollbar^.requisition.height +
          GTK_SCROLLED_WINDOW_CLASS(gtk_object_get_class(combo^.popup))^.scrollbar_spacing;
        show_hscroll := TRUE;
        okay_to_exit := False;
      end;
      if ((not show_vscroll) and (work_height + list_requisition^.height > avail_height)) then
      begin
        if ((work_height + min_height > avail_height) and (y - real_height > avail_height)) then
        begin
          y := y - (work_height + list_requisition^.height + real_height);
          break;
        end;
        alloc_width := alloc_width -
          popup^.vscrollbar^.requisition.width +
          GTK_SCROLLED_WINDOW_CLASS(gtk_object_get_class(combo^.popup))^.scrollbar_spacing;
        show_vscroll := TRUE;
        okay_to_exit := False;
      end;
    until ((old_width <> alloc_width) or (old_height <> work_height) or okay_to_exit);

    width := widget^.allocation.width;
    if (show_vscroll) then
      height := avail_height
    else
      height := work_height + list_requisition^.height;
    if (x < 0) then
      x := 0;

    Dispose(list_requisition);
  end;

var
  ComboWidget: PGtkCombo;
  height, width, x, y : gint;
  old_width, old_height : gint;
begin
  Result:=false;
  if Handle=0 then exit;
  ComboWidget:=PGtkCombo(Handle);
  if DropDown<>GTK_WIDGET_VISIBLE(ComboWidget^.popwin) then begin
    if DropDown then begin
      old_width := ComboWidget^.popwin^.allocation.width;
      old_height := ComboWidget^.popwin^.allocation.height;
      gtk_combo_get_pos(ComboWidget,x,y,height,width);
      if ((old_width <> width) or (old_height <> height)) then
      begin
        gtk_widget_hide (GTK_SCROLLED_WINDOW(ComboWidget^.popup)^.hscrollbar);
        gtk_widget_hide (GTK_SCROLLED_WINDOW(ComboWidget^.popup)^.vscrollbar);
      end;
      gtk_widget_set_uposition (comboWidget^.popwin,x, y);
      gtk_widget_set_usize(ComboWidget^.popwin,width ,height);
      gtk_widget_realize(ComboWidget^.popwin);

      {$IFDEF DebugGDKTraps}
      BeginGDKErrorTrap;
      {$ENDIF}
      gdk_window_resize(ComboWidget^.popwin^.window,width,height);
      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}

      gtk_widget_show (ComboWidget^.popwin);
      gtk_widget_grab_focus(ComboWidget^.popwin);
    end else
      gtk_widget_hide (ComboWidget^.popwin);
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: DeleteDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  // TODO:
  // for now it's just the same, however CreateDC/FreeDC
  // and GetDC/ReleaseDC are couples
  // we should use gdk_new_gc for create and gtk_new_gc for Get
  Result:= (ReleaseDC(0, hDC) = 1);
end;

{------------------------------------------------------------------------------
  Function: DeleteObject
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;

  procedure RaiseInvalidGDIObject;
  begin
    RaiseGDBException('TGtkWidgetSet.DeleteObject invalid GdiObject='+HexStr(Cardinal(GdiObject),8));
  end;

var
  GDIObjectExists: boolean;
begin
  if GDIObject=0 then begin
    Result:=true;
    exit;
  end;
  // Find out if we want to release internal GDI object
  GDIObjectExists:=FGDIObjects.Contains(PGDIObject(GDIObject));
  Result:=GDIObjectExists;
  if not GDIObjectExists then begin
    RaiseInvalidGDIObject;
  end;
  with PGdiObject(GDIObject)^ do
  begin
    case GDIType of
      gdiFont:
        begin
          if GDIFontObject<>nil then begin
            {$Ifdef GTK2}
            pango_font_description_free(GDIFontObject);
            {$Else}
            FontCache.Unreference(GDIFontObject);
            {$EndIf}
          end;
        end;
      gdiBrush:
        begin
          {$IFDEF DebugGDKTraps}
          BeginGDKErrorTrap;
          {$ENDIF}
          {$IFDEF DebugGDIBrush}
          debugln('TGtkWidgetSet.DeleteObject gdiBrush: ',HexStr(Cardinal(GdiObject),8));
          //if Cardinal(GdiObject)=$404826F4 then RaiseGDBException('');
          {$ENDIF}
          if (GDIBrushPixmap <> nil)
          then gdk_bitmap_unref(GDIBrushPixmap);
          {$IFDEF DebugGDKTraps}
          EndGDKErrorTrap;
          {$ENDIF}

          FreeGDIColor(@GDIBrushColor);
        end;
      gdiBitmap:
        begin
          {$IFDEF DebugGDKTraps}
          BeginGDKErrorTrap;
          {$ENDIF}
          if GDIBitmapObject <> nil then
            gdk_bitmap_unref(GDIBitmapObject);
          If (Visual <> nil) and (not SystemVisual) then
            gdk_visual_unref(Visual);
          If Colormap <> nil then
            gdk_colormap_unref(Colormap);
          {$IFDEF DebugGDKTraps}
          EndGDKErrorTrap;
          {$ENDIF}
        end;
      gdiPen:
        begin
          FreeGDIColor(@GDIPenColor);
        end;
      gdiRegion:
        begin
          if (GDIRegionObject <> nil) then
            gdk_region_destroy(GDIRegionObject);
        end;
      gdiPalette:
        begin
          {$IFDEF DebugGDKTraps}
          BeginGDKErrorTrap;
          {$ENDIF}
          If PaletteVisual <> nil then
            gdk_visual_unref(PaletteVisual);
          If PaletteColormap <> nil then
            gdk_colormap_unref(PaletteColormap);
          {$IFDEF DebugGDKTraps}
          EndGDKErrorTrap;
          {$ENDIF}

          RGBTable.Free;
          IndexTable.Free;
        end;
      else begin
        Result:= false;
        DebugLn('[TGtkWidgetSet.DeleteObject] TODO : Unimplemented GDI type');
        Assert(False, 'Trace:TODO : Unimplemented GDI object in delete object');
      end;
    end;
  end;

  { Dispose of the GDI object }
  //DebugLn('[TGtkWidgetSet.DeleteObject] ',Result,'  ',HexStr(GDIObject,8),'  ',FGDIObjects.Count);
  DisposeGDIObject(PGDIObject(GDIObject));
end;

{------------------------------------------------------------------------------
  Function: DestroyCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DestroyCaret(Handle: HWND): Boolean;
var
  GTKObject: PGTKObject;
begin
  GTKObject := PGTKObject(Handle);
  Result := true;

  if GTKObject<>nil then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_DestroyCaret(PGTKAPIWidget(GTKObject));
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else Assert(False, 'Trace:WARNING: [TGtkWidgetSet.DestroyCaret] Got null HWND');
end;

{------------------------------------------------------------------------------
  Function: DrawFrameControl
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DrawFrameControl(DC: HDC; var Rect : TRect;
  uType, uState : Cardinal) : Boolean;
{const
  ADJUST_FLAG: array[Boolean] of Integer = (0, BF_ADJUST);
  PUSH_EDGE_FLAG: array[Boolean] of Integer = (EDGE_RAISED, EDGE_SUNKEN);
  PUSH_EDGE_FLAG2: array[Boolean] of Integer = (0, BF_FLAT);}
var
  Widget: PGtkWidget;

  procedure DrawButtonPush;
  var
    State: TGtkStateType;
    Shadow: TGtkShadowType;
    aStyle : PGTKStyle;
    aDC: TDeviceContext;
    DCOrigin: TPoint;
  begin
    //if Widget<>nil then begin

    // use the gtk paint functions to draw a widget style dependent button

    //writeln('DrawButtonPush ',
    //  ' DFCS_BUTTONPUSH=',uState and DFCS_BUTTONPUSH,
    //  ' DFCS_PUSHED=',uState and DFCS_PUSHED,
    //  ' DFCS_INACTIVE=',uState and DFCS_INACTIVE,
    //  ' DFCS_FLAT=',uState and DFCS_FLAT,
    //  '');
    // set State (the interior filling style)
    if (DFCS_INACTIVE and uState)<>0 then begin
      // button disabled
      State:=GTK_STATE_INSENSITIVE;
    end else begin
      if (DFCS_PUSHED and uState)<>0 then begin
        // button enabled, down
        if (DFCS_CHECKED and uState)<>0 then begin
          // button enabled, down, special (e.g. mouse over)
          State:=GTK_STATE_ACTIVE;
        end else begin
          // button enabled, down, normal
          State:=GTK_STATE_SELECTED;
        end;
      end else begin
        // button enabled, up
        if (DFCS_CHECKED and uState)<>0 then begin
          // button enabled, up, special (e.g. mouse over)
          State:=GTK_STATE_PRELIGHT;
        end else begin
          // button enabled, up, normal
          State:=GTK_STATE_NORMAL;
        end;
      end;
    end;
    // set Shadow (the border style)
    if (DFCS_PUSHED and uState)<>0 then begin
      // button down
      Shadow:=GTK_SHADOW_IN;
    end else begin
      if (((DFCS_FLAT+DFCS_CHECKED) and uState)=DFCS_FLAT) then begin
        // button up, flat, no special
        Shadow:=GTK_SHADOW_ETCHED_OUT;
        //Shadow:=GTK_SHADOW_NONE;
      end else begin
        // button up
        Shadow:=GTK_SHADOW_OUT;
      end;
    end;

    aDC:=TDeviceContext(DC);
    DCOrigin:=GetDCOffset(aDC);

    aStyle := GetStyle(lgsButton);
    If aStyle = nil then
      aStyle := gtk_widget_get_style(Widget)
    else begin
      If State = GTK_STATE_SELECTED then
        State := GTK_STATE_ACTIVE;
      // MG: You can't assign a style to any window. Why it is needed anyway?
      //aStyle := gtk_style_attach(gtk_style_ref(aStyle),aDC.Drawable);
    end;

    if aStyle<>nil then begin
      If (Shadow=GTK_SHADOW_NONE) then
        gtk_paint_flat_box(aStyle,aDC.Drawable,
           State,
           Shadow,
           nil,
           Widget,
           'button',
           Rect.Left+DCOrigin.X,Rect.Top+DCOrigin.Y,
           Rect.Right-Rect.Left,Rect.Bottom-Rect.Top)
      else
        gtk_paint_box(aStyle,aDC.Drawable,
           State,
           Shadow,
           nil,
           Widget,
           'button',
           Rect.Left+DCOrigin.X,Rect.Top+DCOrigin.Y,
           Rect.Right-Rect.Left,Rect.Bottom-Rect.Top);
    end;
    Result := True;
  end;

  procedure DrawCheck;
  var
    State: TGtkStateType;
    Shadow: TGtkShadowType;
    aDC: TDeviceContext;
    DCOrigin: TPoint;
    Style : PGTKStyle;
    Widget : PGTKWidget;
  begin
    // use the gtk paint functions to draw a widget style dependent check(box)

    if (DFCS_PUSHED and uState)<>0 then begin
      STATE := GTK_STATE_ACTIVE;//button checked(GTK ignores disabled)
      Shadow := GTK_SHADOW_IN;//checked style
    end
    else begin
      Shadow := GTK_SHADOW_OUT; //unchecked style
      if (DFCS_INACTIVE and uState)<>0 then begin
        State:=GTK_STATE_INSENSITIVE;//button disabled
      end else
        if (DFCS_CHECKED and uState)<>0 then begin
          // button enabled, special (e.g. mouse over)
          State:=GTK_STATE_PRELIGHT;
        end else begin
          // button enabled, normal
          State:=GTK_STATE_NORMAL;
        end;
    end;

    aDC:=TDeviceContext(DC);
    DCOrigin:=GetDCOffset(aDC);

    Style := GetStyle(lgsCheckbox);

    If Style = nil then
      Style := GetStyle(lgsGTK_Default);

    If Style <> nil then
      Style := gtk_style_attach(gtk_style_ref(Style),aDC.Drawable);

    Widget := GetStyleWidget(lgsCheckbox);
    If Widget = nil then
      Widget := GetStyleWidget(lgsDefault);
    If (Widget <> nil) and (Style <> nil) then begin
      Widget^.Window := aDC.Drawable;
      if Style<>nil then
        gtk_paint_check(Style,aDC.Drawable, State,
          Shadow, nil, Widget, 'checkbutton',
          Rect.Left+DCOrigin.X,Rect.Top+DCOrigin.Y,
          Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);
      Result := True;
    end
    else begin
      {$IfNDef Win32}
      if Style<>nil then
        gtk_draw_check(Style,aDC.Drawable, State,
          Shadow, Rect.Left+DCOrigin.X,Rect.Top+DCOrigin.Y,
          Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);
      {$EndIf}
      Result := True;
    end;
  end;

var ClientWidget: PGtkWidget;
begin
  Result := False;
  if IsValidDC(DC) then begin
    Widget:=PGtkWidget(TDeviceContext(DC).Wnd);
    ClientWidget:=GetFixedWidget(Widget);
    if ClientWidget<>nil then
      Widget:=ClientWidget;
  end else
    Widget:=nil;

  case uType of
    DFC_CAPTION:
      begin  //all draw CAPTION commands here
      end;
    DFC_MENU:
      begin

      end;
    DFC_SCROLL:
      begin
      end;
    DFC_BUTTON:
      begin
        Assert(False, Format('Trace:  [TGtkWidgetSet.DrawFrameControl] DFC_BUTTON --> draw rect = %d,%d,%d,%d',[Rect.Left,Rect.Top,REct.Right,REct.Bottom]));
        //figure out the style first
        case uState and $1F of
          DFCS_BUTTONRADIOIMAGE:
            begin
              Assert(False, 'Trace:State ButtonRadioImage');
            end;
          DFCS_BUTTONRADIOMASK:
            begin
              Assert(False, 'Trace:State ButtonRadioMask');
            end;
          DFCS_BUTTONRADIO:
            begin
              Assert(False, 'Trace:State ButtonRadio');
            end;
          DFCS_BUTTON3STATE:
            begin
              Assert(False, 'Trace:State Button3State');
            end;
          DFCS_BUTTONPUSH:
            begin
              Assert(False, 'Trace:DFCS_BUTTONPUSH in uState');
              DrawButtonPush;
            end;
          DFCS_BUTTONCHECK:
            begin
              Assert(False, 'Trace:State ButtonCheck');
              DrawCheck;
            end;
        else
          DebugLn(Format('ERROR: [TGtkWidgetSet.DrawFrameControl] Unknown State 0x%x', [uState]));
        end;
      end;
  else
    DebugLn(Format('ERROR: [TGtkWidgetSet.DrawFrameControl] Unknown type %d', [uType]));
  end;
end;

{------------------------------------------------------------------------------
  Function: DrawEdge
  Params:   DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal
  Returns:  Boolean

  Draws one or more edges of a rectangle. The rectangle is the area
  Left to Right-1 and Top to Bottom-1.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DrawEdge(DC: HDC; var ARect: TRect; Edge: Cardinal;
  grfFlags: Cardinal): Boolean;
  
  procedure DrawEdges(var R: TRect; GC: pgdkGC; Drawable:PGdkDrawable;
    const TopLeftColor, BottomRightColor: TGDKColor);
  begin
    gdk_gc_set_foreground(GC, @TopLeftColor);
    if (grfFlags and BF_TOP) = BF_TOP then begin
      gdk_draw_line(Drawable, GC, R.Left, R.Top, R.Right, R.Top);
      inc(R.Top);
    end;
    if (grfFlags and BF_LEFT) = BF_LEFT then begin
      gdk_draw_line(Drawable, GC, R.Left, R.Top, R.Left, R.Bottom);
      inc(R.Left);
    end;

    gdk_gc_set_foreground(GC, @BottomRightColor);
    if (grfFlags and BF_BOTTOM) = BF_BOTTOM then begin
      gdk_draw_line(Drawable, GC, R.Left, R.Bottom-1, R.Right, R.Bottom-1);
      dec(R.Bottom);
    end;
    if (grfFlags and BF_RIGHT) = BF_RIGHT then begin
      gdk_draw_line(Drawable, GC, R.Right-1, R.Top, R.Right-1, R.Bottom);
      dec(R.Right);
    end;
  end;
  
Var
  InnerTL, OuterTL,
  InnerBR, OuterBR: TGDKColor;
  BInner, BOuter: Boolean;
  Width, Height: Integer;
  R: TRect;
  DCOrigin: TPoint;
begin
  //DebugLn('TGtkWidgetSet.DrawEdge Edge=',HexStr(Cardinal(Edge),8),' grfFlags=',HexStr(Cardinal(grfFlags),8));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      Assert(False, 'Trace:[TGtkWidgetSet.DrawEdge] Uninitialized GC');
      Result := False;
    end
    else begin
      R := ARect;
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      OffsetRect(R,DCOrigin.X,DCOrigin.Y);

      // try to use the gdk functions, so that the current theme is used
      BInner := False;
      BOuter := False;

      // TODO: change this to real colors
      if (edge and BDR_RAISEDINNER) = BDR_RAISEDINNER
      then begin
        InnerTL := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
        InnerBR := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
        BInner := True;
      end;
      if (edge and BDR_SUNKENINNER) = BDR_SUNKENINNER
      then begin
        InnerTL := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
        InnerBR := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
        BInner := True;
      end;
      if (edge and BDR_RAISEDOUTER) = BDR_RAISEDOUTER
      then begin
        OuterTL := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
        OuterBR := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
        BOuter := True;
      end;
      if (edge and BDR_SUNKENOUTER) = BDR_SUNKENOUTER
      then begin
        OuterTL := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
        OuterBR := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
        BOuter := True;
      end;

      gdk_gc_set_fill(GC, GDK_SOLID);
      SelectedColors := dcscCustom;

      // Draw outer rect
      if Bouter then
        DrawEdges(R,GC,Drawable,OuterTL,OuterBR);

      // Draw inner rect
      if BInner then
        DrawEdges(R,GC,Drawable,InnerTL,InnerBR);

//      gdk_colormap_free_colors(gdk_colormap_get_system, @OuterTL, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @OuterBR, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @InnerTL, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @InnerBR, 1);

      //Draw interiour
      if ((grfFlags and BF_MIDDLE) = BF_MIDDLE) and
        not CurrentBrush^.IsNullBrush
      then begin
        Width := R.Right - R.Left + 1;
        Height := R.Bottom - R.Top + 1;
        SelectGDKBrushProps(DC);
        If not CurrentBrush^.IsNullBrush then
          if (CurrentBrush^.GDIBrushFill = GDK_SOLID)
          and (IsBackgroundColor(TColor(CurrentBrush^.GDIBrushColor.ColorRef)))
          then
            StyleFillRectangle(Drawable, GC, CurrentBrush^.GDIBrushColor.ColorRef,
                               R.Left, R.Top, Width, Height)
          else
            gdk_draw_rectangle(Drawable, GC, 1, R.Left, R.Top, Width, Height);
      end;

      // adjust rect if needed
      if (grfFlags and BF_ADJUST) = BF_ADJUST
      then ARect := R;

      Result := True;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
var
  TM : TTextmetric;
  theRect     : TRect;
  Lines : PPChar;
  I, NumLines : Longint;
  TempDC,
  TempPen,
  TempBrush : Longint;

  Function LeftOffset : Longint;
  begin
    If (Flags and DT_Right) = DT_Right then
      Result := DT_Right
    else
      If (Flags and DT_CENTER) = DT_CENTER then
        Result := DT_CENTER
    else
      Result := DT_LEFT;
  end;

  Function TopOffset : Longint;
  begin
    If (Flags and DT_BOTTOM) = DT_BOTTOM then
      Result := DT_BOTTOM
    else
      If (Flags and DT_VCENTER) = DT_VCENTER then
        Result := DT_VCENTER
    else
      Result := DT_Top;
  end;

  Function CalcRect : Boolean;
  begin
    Result := (Flags and DT_CalcRect) = DT_CalcRect;
  end;

  Procedure DoCalcRect;
  var
    AP : TSize;
    J, MaxLength,
    LineWidth : Integer;
  begin
    theRect := Rect;

    MaxLength := theRect.Right - theRect.Left;

    If (Flags and DT_SingleLine) = DT_SingleLine then begin
      GetTextExtentPoint(DC, Str, Count, AP);
      theRect.Right := theRect.Left + Min(MaxLength, AP.cX);
      theRect.Bottom := theRect.Top + TM.tmHeight;

      If not CalcRect then
        Case TopOffset of
          DT_VCENTER :
            OffsetRect(theRect, 0, (Rect.Bottom - theRect.Bottom) div 2);
          DT_Bottom :
            OffsetRect(theRect, 0, Rect.Bottom - theRect.Bottom);
        end;
    end
    else begin
      If (Flags and DT_WordBreak) <> DT_WordBreak then
        MaxLength := Count*TM.tmMaxCharWidth;
      Self.WordWrap(DC, Str, MaxLength, Lines, NumLines);

      If (Lines = nil) or (NumLines = 0) then
        exit;

      LineWidth := 0;

      For J := 0 to NumLines - 1 do begin
        GetTextExtentPoint(DC, Lines[J], StrLen(Lines[J]), AP);
        LineWidth := Max(LineWidth, AP.cX);
      end;

      LineWidth := Min(MaxLength, LineWidth);

      theRect.Right := theRect.Left + LineWidth;
      theRect.Bottom := theRect.Top + NumLines*TM.tmHeight;
    end;

    If not CalcRect then
      Case LeftOffset of
        DT_CENTER :
          OffsetRect(theRect, (Rect.Right - theRect.Right) div 2, 0);
        DT_Right :
          OffsetRect(theRect, Rect.Right - theRect.Right, 0);
      end;
  end;

  Procedure DrawLine(theLine : PChar; LineLength, TopPos : Longint);
  var
    Points : Array[0..1] of TSize;
    LogP : TLogPen;
    pIndex : Longint;
    AStr : String;
    LeftPos : Longint;
  begin
    AStr := Copy(String(theLine), 1, LineLength);

    If (Flags and DT_NoPrefix) <> DT_NoPrefix then
      pIndex := DeleteAmpersands(aStr)
    else
      pIndex := -1;

    If TempBrush = -1 then
      TempBrush := SelectObject(DC, GetStockObject(NULL_BRUSH));

    If LeftOffset <> DT_Left then
      GetTextExtentPoint(DC, PChar(aStr), Length(aStr), Points[0]);

    Case LeftOffset of
      DT_Left:
        LeftPos := theRect.Left;
      DT_Center:
        LeftPos := theRect.Left + (theRect.Right - theRect.Left) div 2
                 - Points[0].cX div 2;
      DT_Right:
        LeftPos := theRect.Right - Points[0].cX;
    end;

    {Draw line of Text}
    TextOut(DC, LeftPos, TopPos, PChar(aStr), Length(aStr));

    {Draw Prefix}
    If pIndex > 0 then begin
      {Create & select pen of font color}
      If TempPen = -1 then begin
        LogP.lopnStyle := PS_SOLID;
        LogP.lopnWidth.X := 1;
        LogP.lopnColor := GetTextColor(DC);
        TempPen := SelectObject(DC, CreatePenIndirect(LogP));
      end;

      {Get prefix line position}
      GetTextExtentPoint(DC, PChar(aStr), pIndex - 1, Points[0]);
      Points[0].cX := LeftPos + Points[0].cX;
      Points[0].cY := TopPos + tm.tmHeight - TM.tmDescent + 1;

      GetTextExtentPoint(DC, @aStr[pIndex], 1, Points[1]);
      Points[1].cX := Points[0].cX + Points[1].cX;
      Points[1].cY := Points[0].cY;

      {Draw prefix line}
      Polyline(DC, @Points[0], 2);
    end;
  end;

begin
  if (Str=nil) or (Str[0]=#0) then exit;
  Assert(False, Format('trace:> [TGtkWidgetSet.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));

  Result := Longint(IsValidDC(DC));
  if Boolean(Result)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.DrawText] Uninitialized GC');
      Result := 0;
    end
    else begin
      Result := 0;
      Lines := nil;
      NumLines := 0;
      TempDC := -1;
      TempPen := -1;
      TempBrush := -1;

      Count := Min(StrLen(Str), Count);

      GetTextMetrics(DC, TM);

      DoCalcRect;

      If (Flags and DT_CalcRect) <> DT_CalcRect then begin
        TempDC := SaveDC(DC);

      If (Flags and DT_NOCLIP) <> DT_NOCLIP then begin
        If theRect.Right > Rect.Right then
          theRect.Right := Rect.Right;
        If theRect.Bottom > Rect.Bottom then
          theRect.Bottom := Rect.Bottom;
        IntersectClipRect(DC, theRect.Left, theRect.Top,
          theRect.Right, theRect.Bottom);
      end;

      If (Flags and DT_SingleLine) = DT_SingleLine then begin
        DrawLine(Str, Count, theRect.Top);
        Result := 1;
      end
      else
        If (Lines <> nil) and (NumLines <> 0) then begin
          For I := 0 to NumLines - 1 do begin
            If (((Flags and DT_EditControl) = DT_EditControl) and
              (tm.tmHeight > (theRect.Bottom - theRect.Top))) or
              (theRect.Top > theRect.Bottom)
            then
              break;

            If Lines[I] <> nil then
              DrawLine(Lines[I], StrLen(Lines[I]), theRect.Top);

            Inc(theRect.Top, TM.tmHeight);
          end;
          Result := 1;
        end;
      end
      else begin
        CopyRect(Rect, theRect);
        Result := 1;
      end;

      Reallocmem(Lines, 0);

      If TempBrush <> -1 then
        SelectObject(DC, TempBrush);

      If TempPen <> -1 then
        DeleteObject(SelectObject(DC, TempPen));

      If TempDC <> -1 then
        RestoreDC(DC, TempDC);
    end;
  end;
  Assert(False, Format('trace:> [TGtkWidgetSet.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));
end;

{------------------------------------------------------------------------------
  Function: EnableScrollBar
  Params:  Wnd, wSBflags, wArrows
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Assert(False, 'Trace:TODO: [TGtkWidgetSet.EnableScrollBar]');
  //TODO: Implement this;
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: EnableWindow
  Params: hWnd:
          bEnable:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Assert(False, Format('Trace:  [TGtkWidgetSet.EnableWindow] hWnd: 0x%x, Enable: %s', [hwnd, BOOL_TEXT[bEnable]]));
  
  if hWnd <> 0 then
    gtk_widget_set_sensitive(pgtkwidget(hWnd), bEnable);
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: EndPaint
  Params:  
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
{$IFDEF Gtk1}
var
  Widget: PGtkWidget;
  IsDoubleBuffer: Boolean;
  DCDrawable: PGdkDrawable;
  Width, Height: integer;
  DevContext: TDeviceContext;
  CaretWasVisible: Boolean;
  MainWidget: PGtkWidget;
  //LCLObject: TObject;
  //x, y: integer;
{$ENDIF}
begin
  Result:=1;
  if PS.HDC <> 0 then begin
    {$IFDEF Gtk1}
    Widget:=PGtkWidget(Handle);
    DevContext:=TDeviceContext(PS.HDC);
    if Widget<>PGtkWidget(DevContext.Wnd) then
      RaiseException('');
    DCDrawable:=DevContext.Drawable;
    IsDoubleBuffer:=dcfDoubleBuffer in DevContext.DCFlags;
    if IsDoubleBuffer then begin
      // copy
      gdk_window_get_size(DCDrawable,@Width,@Height);
      {$IFDEF VerboseDoubleBuffer}
      DebugLn('TGtkWidgetSet.EndPaint Copying from buffer to window: ',Width,' ',Height);
      {$ENDIF}
      gdk_gc_set_clip_region(DevContext.GC, nil);
      gdk_gc_set_clip_rectangle(DevContext.GC, nil);

      // hide caret
      HideCaretOfWidgetGroup(Widget,MainWidget,CaretWasVisible);
      // draw
      gdk_window_copy_area(Widget^.Window, DevContext.GC, 0,0,
        DCDrawable, 0, 0, Width, Height);
        
      {LCLObject:=GetParentLCLObject(Widget);
      if (LCLObject is TPanel)
      and (csDesigning in TPanel(LCLObject).ComponentState) then begin
        gdk_window_get_origin(Widget^.Window,@x,@y);
        DebugLn('TGtkWidgetSet.EndPaint ',TPanel(LCLObject).Name,':',TPanel(LCLObject).ClassName,
          ' Widget=',GetWidgetClassName(Widget),
          ' Origin=',x,',',y,
          ' ',Widget^.allocation.x,',',Widget^.allocation.y);
      end;}
        
      // restore caret
      if CaretWasVisible then
        GTKAPIWidget_ShowCaret(PGTKAPIWidget(MainWidget));
    end;
    {$ENDIF}
    ReleaseDC(Handle, PS.HDC);
  end;
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse.

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.Ellipse(DC: HDC;
  x1,y1,x2,y2: Integer): Boolean;
var
  x,y,width,height: integer;
  DCOrigin: TPoint;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.Ellipse] Uninitialized GC');
      Result := False;
    end
    else begin
      if x1<x2 then begin
        x:=x1;
        width:=x2-x1;
      end else begin
        x:=x2;
        width:=x1-x2;
      end;
      if y1<y2 then begin
        y:=y1;
        height:=y2-y1;
      end else begin
        y:=y2;
        height:=y1-y2;
      end;

      // first draw interior in brush color
      SelectGDKBrushProps(DC);
      DCOrigin:=GetDCOffset(TDeviceContext(DC));

      {$IFDEF DebugGDKTraps}
      BeginGDKErrorTrap;
      {$ENDIF}

      If not CurrentBrush^.IsNullBrush then
        gdk_draw_arc(Drawable, GC, 1, x+DCOrigin.X, y+DCOrigin.Y, Width, Height,
                   0, 360 shl 6);

      // Draw outline
      SelectGDKPenProps(DC);

      If (dcfPenSelected in DCFlags) then begin
        Result := True;
        if (CurrentPen^.IsNullPen) then exit;
        gdk_draw_arc(Drawable, GC, 0, x+DCOrigin.X, y+DCOrigin.Y, Width, Height,
                     0, 360 shl 6);
      end else
        Result := False;

      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: ExcludeClipRect
  Params:  dc: hdc; Left, Top, Right, Bottom : Integer
  Returns: integer

  Subtracts all intersecting points of the passed bounding rectangle
  (Left, Top, Right, Bottom) from the Current clipping region in the
  device context (dc).

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ExcludeClipRect(dc: hdc;
  Left, Top, Right, Bottom : Integer) : Integer;
begin
  Result := Inherited ExcludeClipRect(DC, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Function: ExtSelectClipRGN
  Params:  dc, RGN, Mode
  Returns: integer

  Combines the passed Region with the current clipping region in the device
  context (dc), using the specified mode.

  The Combine Mode can be one of the following:
      RGN_AND  : all points which are in both regions

      RGN_COPY : an exact copy of the source region, same as SelectClipRGN

      RGN_DIFF : all points which are in the Clipping Region but
                 not in the Source.(Clip - RGN)

      RGN_OR   : all points which are in either the Clip Region or
                 in the Source.(Clip + RGN)

      RGN_XOR  : all points which are in either the Clip Region
                 or in the Source, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ExtSelectClipRGN(dc: hdc; rgn : hrgn;
  Mode : Longint) : Integer;
var
  Clip,
  Tmp : hRGN;
  X, Y : Longint;
  DCOrigin: TPoint;
begin
  Result := SIMPLEREGION;
  If not IsValidDC(DC) then
    Result := ERROR
  else with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.ExtSelectClipRGN] Uninitialized GC');
      Result := ERROR;
    end
    else begin
      //DebugLn('TGtkWidgetSet.ExtSelectClipRGN A ClipRegValid=',dbgs(DCClipRegionValid(DC)),
      //  ' Mode=',dbgs(Mode),' RGN=',GDKRegionAsString(PGdiObject(RGN)^.GDIRegionObject));
      If ClipRegion=0 then begin
        // there is no clipping region in the DC
        Case Mode of
          RGN_COPY:
            begin
              Result := RegionType(PGdiObject(RGN)^.GDIRegionObject);
              If Result <> ERROR then
                Result := SelectClipRGN(DC, RGN);
            end;
          RGN_OR,
          RGN_XOR,
          RGN_AND,
          RGN_DIFF:
            begin
              // get existing clip
              GDK_Window_Get_Size(Drawable, @X, @Y);
              DCOrigin:=GetDCOffset(TDeviceContext(DC));
              Clip := CreateRectRGN(-DCOrigin.X,-DCOrigin.Y,X-DCOrigin.X,Y-DCOrigin.Y);
              // create target clip
              Tmp := CreateEmptyRegion;
              // combine
              Result := CombineRGN(Tmp, Clip, RGN, Mode);
              // commit
              //DebugLn('TGtkWidgetSet.ExtSelectClipRGN B ClipRegValid=',dbgs(ClipRegion),' TmpRGN=',GDKRegionAsString(PGdiObject(Tmp)^.GDIRegionObject));
              SelectClipRGN(DC, Tmp);
              // clean up
              DeleteObject(Clip);
              DeleteObject(Tmp);
            end;
          end;
        end
      else
        Result := Inherited ExtSelectClipRGN(dc, rgn, mode);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
{$Ifdef GTK2}
begin
  DebugLn('ToDo: TGtkWidgetSet.ExtTextOut');
  Result:=false;
end;
{$Else}
var
  LineStart, LineEnd, StrEnd: PChar;
  Left, Top, Width, Height: Integer;
  TopY, LineLen, LineHeight : Integer;
  TxtPt : TPoint;
  UseFont : PGDKFont;
  UnRef : Boolean;
  DCOrigin: TPoint;
  UnderLine: boolean;
  buffer: PGdkDrawable;
  buffered: Boolean;

  procedure DrawTextLine;
  var
    UnderLineLen, Y: integer;
    CurDistX: PInteger;
    CharsWritten, CurX, i: integer;
    LinePos: PChar;
    CharLen: LongInt;
  begin
    {$IFDEF DebugGDKTraps}
    BeginGDKErrorTrap;
    {$ENDIF}
    with TDeviceContext(DC) do begin
      if (Dx=nil) then begin
        // no dist array -> write as one block
        //debugln('TGtkWidgetSet.ExtTextOut.DrawTextLine Dx=nil ',dbgs(LineLen),' DCTextMetric.IsDoubleByteChar=',dbgs(DCTextMetric.IsDoubleByteChar));
        gdk_draw_text(Buffer, UseFont, GC, TxtPt.X, TxtPt.Y,
                      LineStart, LineLen);
      end else begin
        // dist array -> write each char separately
        CharsWritten:=integer(LineStart-Str);
        if DCTextMetric.IsDoubleByteChar then begin
          CharLen:=2;
          CharsWritten:=CharsWritten div 2;
        end else
          CharLen:=1;
        CurDistX:=Dx+CharsWritten*SizeOf(Integer);
        CurX:=TxtPt.X;
        LinePos:=LineStart;
        //debugln('TGtkWidgetSet.ExtTextOut.DrawTextLine ',dbgs(dx),' DCTextMetric.IsDoubleByteChar=',dbgs(DCTextMetric.IsDoubleByteChar));
        i:=1;
        while (i<=LineLen) do begin
          //debugln('TGtkWidgetSet.ExtTextOut.DrawTextLine ',dbgs(CharLen),' ',dbgs(ord(LinePos^)));
          gdk_draw_text(Buffer, UseFont, GC, CurX, TxtPt.Y, LinePos, CharLen);
          inc(LinePos,CharLen);
          inc(CurX,CurDistX^);
          inc(CurDistX);
          inc(i,CharLen);
        end;
      end;
      if UnderLine then begin
        if Rect<>nil then
          UnderLineLen := Rect^.Right-Rect^.Left
        else
          UnderLineLen := gdk_text_width(UseFont,LineStart, LineLen);
        Y := TxtPt.Y + 1;
        gdk_draw_line(Buffer, GC, TxtPt.X, Y, TxtPt.X+UnderLineLen, Y);
      end;
    end;
    {$IFDEF DebugGDKTraps}
    EndGDKErrorTrap;
    {$ENDIF}
  end;

begin
  Assert(False, Format('trace:> [TGtkWidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.ExtTextOut] Uninitialized GC');
      Result := False;
      exit;
    end;
    if ((Options and (ETO_OPAQUE+ETO_CLIPPED)) <> 0)
    and (Rect=nil) then begin
      DebugLn('WARNING: [TGtkWidgetSet.ExtTextOut] Rect=nil');
      Result := False;
      exit;
    end;
    // TODO: implement other parameters.

    // to reduce flickering calculate first and then paint
    DCOrigin:=GetDCOffset(TDeviceContext(DC));
    buffered := false;
    UseFont:=nil;
    buffer := Drawable;
    UnRef := false;
    UnderLine := false;

    if (Str<>nil) and (Count>0) then begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then begin
        UseFont := GetDefaultFont(false);
      end else begin
        UseFont := CurrentFont^.GDIFontObject;
        UnderLine := (CurrentFont^.LogFont.lfUnderline<>0);
      end;

      if UseFont <> nil then begin
        if (Options and ETO_CLIPPED) <> 0 then
        begin
          X := Rect^.Left;
          Y := Rect^.Top;
          IntersectClipRect(DC, Rect^.Left, Rect^.Top,
                            Rect^.Right, Rect^.Bottom);
        end;
      end else begin
        DebugLn('WARNING: [TGtkWidgetSet.ExtTextOut] Missing Font');
        Result := False;
      end;
    end;

    if ((Options and ETO_OPAQUE) <> 0) then
    begin
      Width := Rect^.Right - Rect^.Left;
      Height := Rect^.Bottom - Rect^.Top;
      SelectedColors := dcscCustom;
      EnsureGCColor(DC, dccCurrentBackColor, True, False);
      if buffered then begin
        Left:=0;
        Top:=0;
      end else begin
        Left:=Rect^.Left+DCOrigin.X;
        Top:=Rect^.Top+DCOrigin.Y;
      end;
      {$IFDEF DebugGDKTraps}
      BeginGDKErrorTrap;
      {$ENDIF}
      if IsBackgroundColor(TColor(CurrentBackColor.ColorRef)) then
        StyleFillRectangle(buffer, GC, CurrentBackColor.ColorRef,
                           Left, Top, Width, Height)
      else
        gdk_draw_rectangle(buffer, GC, 1, Left, Top, Width, Height);
      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}
    end;

    if UseFont<>nil then begin
      LineLen := FindChar(#10,Str,Count);
      UpdateDCTextMetric(TDeviceContext(DC));
      LineHeight:=GetTextHeight(DCTextMetric);
      if Buffered then begin
        TxtPt.X := 0;
        TxtPt.Y := LineHeight;
      end
      else begin
        TopY := Y;
        TxtPt.X := X + DCOrigin.X;
        TxtPt.Y := TopY + LineHeight + DCOrigin.Y;
      end;
      SelectGDKTextProps(DC);
      LineStart:=Str;
      if LineLen < 0 then begin
        LineLen:=Count;
        if Count> 0 then DrawTextLine;
      end else
      Begin  //write multiple lines
        StrEnd:=Str+Count;
        while LineStart < StrEnd do begin
          LineEnd:=LineStart+LineLen;
          if LineLen>0 then DrawTextLine;
          inc(TxtPt.Y,LineHeight);
          LineStart:=LineEnd+1; // skip #10
          if (LineStart<StrEnd) and (LineStart^=#13) then
            inc(LineStart); // skip #10
          Count:=StrEnd-LineStart;
          LineLen:=FindChar(#10,LineStart,Count);
          if LineLen<0 then
            LineLen:=Count;
        end;
      end;
      If UnRef then begin
        {$IFDEF DebugGDKTraps}
        BeginGDKErrorTrap;
        {$ENDIF}
        FontCache.Unreference(UseFont);
        {$IFDEF DebugGDKTraps}
        EndGDKErrorTrap;
        {$ENDIF}
      end;
    end;
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
end;
{$EndIf}
{------------------------------------------------------------------------------
  Function: FillRect
  Params: none
  Returns: Nothing

  The FillRect function fills a rectangle by using the specified brush.
  This function includes the left and top borders, but excludes the right and
  bottom borders of the rectangle.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  Width, Height: Integer;
  OldCurrentBrush: PGdiObject;
  DCOrigin: TPoint;
  BrushChanged: Boolean;
begin
  BrushChanged :=false;
  Result := IsValidDC(DC) and IsValidGDIObject(Brush);
  if not Result then exit;
  with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.FillRect] Uninitialized GC');
      Result := False;
      exit;
    end;
    if not PGdiObject(Brush)^.IsNullBrush then begin
      Width := Rect.Right - Rect.Left;
      Height := Rect.Bottom - Rect.Top;
      // Temporary hold the old brush to
      // replace it with the given brush
      OldCurrentBrush := CurrentBrush;
      if not CompareGDIBrushes(PGdiObject(Brush),OldCurrentBrush) then begin
        BrushChanged:=true;
        CurrentBrush := PGdiObject(Brush);
        SelectedColors:=dcscCustom;
      end;
      //DebugLn('TGtkWidgetSet.FillRect Color=',HexStr(Cardinal(CurrentBrush^.GDIBrushColor.ColorRef),8));

      SelectGDKBrushProps(DC);
      
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      if (CurrentBrush^.GDIBrushFill = GDK_SOLID)
      and (IsBackgroundColor(TColor(CurrentBrush^.GDIBrushColor.ColorRef)))
      then begin
        StyleFillRectangle(drawable, GC,
                           CurrentBrush^.GDIBrushColor.ColorRef,
                           Rect.Left+DCOrigin.X, Rect.Top+DCOrigin.Y,
                           Width, Height)
      end else begin
        gdk_draw_rectangle(Drawable, GC, 1,
                           Rect.Left+DCOrigin.X, Rect.Top+DCOrigin.Y,
                           Width, Height);
      end;
      
      // Restore current brush
      if BrushChanged then begin
        SelectedColors:=dcscCustom;
        CurrentBrush := OldCurrentBrush;
      end;
    end;

    Result := True;
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.FillRect] DC:0x%x; Rect: ((%d,%d)(%d,%d)); brush: %x', [Integer(DC), Rect.left, rect.top, rect.right, rect.bottom, brush]));
end;

{------------------------------------------------------------------------------
  function Frame(DC: HDC; const ARect: TRect): Integer; override;

  Draws the border of a rectangle.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
var
  DCOrigin: TPoint;
begin
  Result:=0;
  if IsValidDC(DC) and (TDeviceContext(DC).GC<>nil) then begin
    with TDeviceContext(DC) do
    begin
      // Draw outline
      SelectGDKPenProps(DC);
      If (dcfPenSelected in DCFlags) then begin
        Result := 1;
        if (not CurrentPen^.IsNullPen) then begin
          DCOrigin:=GetDCOffset(TDeviceContext(DC));
          gdk_draw_rectangle(Drawable, GC, 0,
                             ARect.Left+DCOrigin.X, ARect.Top+DCOrigin.Y,
                             ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
        end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: Frame3d
  Params: -
  Returns: Nothing

  Draws a 3d border in GTK native style.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.Frame3d(DC : HDC; var ARect : TRect;
  const FrameWidth : integer; const Style : TBevelCut) : boolean;

const GTKThinShadowType: array[TBevelCut] of integer =
  (GTK_SHADOW_NONE, GTK_SHADOW_IN, GTK_SHADOW_OUT, GTK_SHADOW_NONE);
const GTKStrongShadowType: array[TBevelCut] of integer =
  (GTK_SHADOW_NONE, GTK_SHADOW_ETCHED_IN, GTK_SHADOW_ETCHED_OUT, GTK_SHADOW_NONE);

var
  Widget, ClientWidget: PGtkWidget;
  i : integer;
  DCOrigin: TPoint;
  TheStyle: PGtkStyle;
  Area: TGdkRectangle;
  ShadowType: Integer;
  AWindow: PGdkWindow;
begin
  Result := IsValidDC(DC);
  if not Result then exit;
  if FrameWidth=0 then exit;
  TheStyle:=GetStyle(lgsButton);
  //DebugLn('TGtkWidgetSet.Frame3d A ',HexStr(Cardinal(TheStyle),8));
  if TheStyle=nil then exit;

  with TDeviceContext(DC) do
  begin
    if GC = nil then begin
      Result:= False;
      exit;
    end;
    Widget:=PGtkWidget(TDeviceContext(DC).Wnd);
    ClientWidget:=Widget;
    if Widget<>nil then begin
      ClientWidget:=GetFixedWidget(Widget);
      if ClientWidget=nil then
        ClientWidget:=Widget;
    end;
    AWindow:=Drawable;
    DCOrigin:=GetDCOffset(TDeviceContext(DC));
    Area.X:=ARect.Left+DCOrigin.X;
    Area.Y:=ARect.Top+DCOrigin.Y;
    Area.Width:=ARect.Right-ARect.Left;
    Area.Height:=ARect.Bottom-ARect.Top;
    if FrameWidth=1 then
      ShadowType:=GTKThinShadowType[Style]
    else
      ShadowType:=GTKStrongShadowType[Style];
    //DebugLn('ShadowType ',ShadowType,
    //' dark_gc=',HexStr(Cardinal(TheStyle^.dark_gc[GTK_STATE_NORMAL]),8),
    //' light_gc=',HexStr(Cardinal(TheStyle^.light_gc[GTK_STATE_NORMAL]),8),
    //'');

    for i:= 1 to FrameWidth do begin
      gtk_paint_shadow(TheStyle,
                       AWindow, GTK_STATE_NORMAL,
                       ShadowType,
                       @Area,
                       ClientWidget,
                       'button',
                       ARect.Left+DCOrigin.X, ARect.Top+DCOrigin.Y,
                       ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
      // inflate the rectangle (! ARect will be returned to the user with this)
      InflateRect(ARect, -1, -1);
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.FrameRect(DC: HDC; const ARect: TRect;
    hBr: HBRUSH): Integer;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH): Integer;
var
  DCOrigin: TPoint;
begin
  Result:=0;
  if IsValidDC(DC) and (TDeviceContext(DC).GC<>nil)
  and IsValidGDIObject(hBr) then begin
    // Draw outline
    Result := 1;
    if (not PGdiObject(hBr)^.IsNullBrush) then begin
      with TDeviceContext(DC) do
      begin
        SelectedColors:=dcscCustom;
        EnsureGCColor(DC, dccGDIBrushColor, True, False);//Brush Color
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        gdk_draw_rectangle(Drawable, GC, 0,
                           ARect.Left+DCOrigin.X, ARect.Top+DCOrigin.Y,
                           ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetActiveWindow
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetActiveWindow : HWND;
var
  TopList, List: PGList;
  Widget: PGTKWidget;
  Window: PGTKWindow;
begin
  // Default to 0
  Result := 0;

  TopList := gdk_window_get_toplevels;
  List := TopList;
  while List <> nil do
  begin
    if (List^.Data <> nil)
    then begin
      gdk_window_get_user_data(PGDKWindow(List^.Data), @Window);
      if gtk_is_window(Window)
      then begin
        Widget := Window^.focus_widget;

        if (Widget <> nil) and gtk_widget_has_focus(Widget)
        then begin
          Result := HWND(GetMainWidget(PGtkWidget(Window)));
          Break;
        end;
      end;
    end;
    list := g_list_next(list);
  end;
  if TopList <> nil
  then g_list_free(TopList);
end;

{------------------------------------------------------------------------------
  Function: GetDIBits
  Params:  
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
  Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Assert(False, 'trace:[TGtkWidgetSet.GetDIBits]');
  Result := 0;
  if IsValidGDIObject(Bitmap)
  then begin
    case PGDIObject(Bitmap)^.GDIType of
      gdiBitmap:
        Result := InternalGetDIBits(DC, Bitmap, StartScan, NumScans, -1, Bits,
                                      BitInfo, Usage, True);
      else
        DebugLn('WARNING: [TGtkWidgetSet.GetDIBits] not a Bitmap!');
    end;
  end
  else
    DebugLn('WARNING: [TGtkWidgetSet.GetDIBits] invalid Bitmap!');
end;

{------------------------------------------------------------------------------
  Function: GetBitmapBits
  Params:  
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
var
  BitInfo : tagBitmapInfo;
begin
  Assert(False, 'trace:[TGtkWidgetSet.GetBitmapBits]');
  Result := 0;
  if IsValidGDIObject(Bitmap)
  then begin
    case PGDIObject(Bitmap)^.GDIType of
      gdiBitmap:
        Result := InternalGetDIBits(0, Bitmap, 0, 0, Count, Bits, BitInfo, 0, False);
      else
        DebugLn('WARNING: [TGtkWidgetSet.GetBitmapBits] not a Bitmap!');
    end;
  end
  else
    DebugLn('WARNING: [TGtkWidgetSet.GetBitmapBits] invalid Bitmap!');
end;

{------------------------------------------------------------------------------
  Function: GetBitmapRawImageDescription
  Params: Bitmap: HBITMAP;
          Desc: PRawImageDescription
  Returns: boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): boolean;
var
  GDIObject: PGDIObject;
  GdkPixmap: PGdkPixmap;
begin
  Result:=false;
  if not IsValidGDIObject(Bitmap) then begin
    DebugLn('WARNING: [TGtkWidgetSet.GetBitmapRawImageDescription] invalid Bitmap!');
    exit;
  end;
  GDIObject:=PGDIObject(Bitmap);
  case GDIObject^.GDIBitmapType of
  gbBitmap: GdkPixmap:=PGdkPixmap(PGdiObject(Bitmap)^.GDIBitmapObject);
  gbPixmap: GdkPixmap:=PGdkPixmap(PGdiObject(Bitmap)^.GDIPixmapObject);
  else
    GdkPixmap:=nil;
    DebugLn('WARNING: [TGtkWidgetSet.GetBitmapRawImageDescription] GDI_RGBImage not implemented');
    exit;
  end;
  Result:=GetWindowRawImageDescription(PGdkWindow(GdkPixmap),Desc);
end;

{------------------------------------------------------------------------------
  Function: GetCapture
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetCapture: HWND;
var
  Widget: PGtkWidget;
  AWindow: PGtkWindow;
  IsModal: gboolean;
begin
  Widget:=gtk_grab_get_current;
  // for the LCL a modal window is not capturing
  if Widget<>nil then begin
    if GtkWidgetIsA(Widget,GTK_TYPE_WINDOW) then begin
      AWindow:=PGtkWindow(Widget);
      IsModal:=gtk_window_get_modal(AWindow);
      if IsModal then
        Widget:=nil;
    end;
  end;
  Result := HWnd(Widget);
end;

{------------------------------------------------------------------------------
  Function: GetCaretPos
  Params:  lpPoint: The caretposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
var
  //FocusObject: PGTKObject;
  modmask : TGDKModifierType;
begin
{  Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetCaretPos] finish');

  FocusObject := PGTKObject(GetFocus);
  Result := FocusObject <> nil;

  if Result
  then begin
//    Assert(False, Format('Trace:[TGtkWidgetSet.GetCaretPos] Got focusObject 0x%x --> %s', [Integer(FocusObject), gtk_type_name(FocusObject^.Klass^.theType)]));

    if gtk_type_is_a(gtk_object_type(FocusObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_GetCaretPos(PGTKAPIWidget(FocusObject), lpPoint.X, lpPoint.Y);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else DebugLn('[TGtkWidgetSet.GetCaretPos] got focusObject nil');
 }

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}
  gdk_window_get_pointer(nil,@lpPoint.X,@lpPoint.y,@modmask);
  {$IFDEF DebugGDKTraps}
  EndGDKErrorTrap;
  {$ENDIF}
  Result := True;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetCaretRespondToFocus(handle: HWND;
    var ShowHideOnFocus: boolean): Boolean;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  if handle<>0 then begin
    if gtk_type_is_a(gtk_object_type(PGTKObject(handle)), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_GetCaretRespondToFocus(PGTKAPIWidget(handle),
        ShowHideOnFocus);
      Result:=true;
    end
    else begin
      Result := False;
    end;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------------
  Function: GetCharABCWidths        pbd
  Params:  Don't care yet
  Returns: False so that the font cache in the newest mwEdit will use
           TextMetrics info which is working already


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetCharABCWidths(DC: HDC; p2, p3: UINT;
  const ABCStructs): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
var
  Widget, ClientWidget: PGtkWidget;
  MainOrigin, ClientOrigin: TPoint;
  ClientWindow, MainWindow: PGdkWindow;
begin
  Result := False;
  if Handle = 0 then Exit;
  Widget := pgtkwidget(Handle);
  ClientWidget := GetFixedWidget(Widget);
  if (ClientWidget <> Widget) then begin
    ClientWindow:=GetControlWindow(ClientWidget);
    MainWindow:=GetControlWindow(Widget);
    if MainWindow<>ClientWindow then begin
      if MainWindow<>nil then begin
        gdk_window_get_origin(MainWindow,@MainOrigin.X,@MainOrigin.Y);
      end else begin
        // widget not realized
        MainOrigin.X:=0;
        MainOrigin.Y:=0;
      end;
      // check if the main gdkwindow is the clientwindow of the parent
      if (Widget^.Parent<>nil)
      and (MainWindow=gtk_widget_get_parent_window(Widget)) then begin
        // the widget is using its parent window
        // -> adjust the coordinates
        inc(MainOrigin.X,Widget^.Allocation.X);
        inc(MainOrigin.Y,Widget^.Allocation.Y);
      end;
      if ClientWindow<>nil then begin
        {$Ifdef GTK2}
        if GTK_WIDGET_NO_WINDOW(ClientWidget)
        then begin
          ClientOrigin.X := ClientWidget^.Allocation.X;
          ClientOrigin.Y := ClientWidget^.Allocation.Y;
        end else
       {$EndIf}
        gdk_window_get_origin(ClientWindow,@ClientOrigin.X,@ClientOrigin.Y);
      end else begin
        // client widget not realized
        {$Ifdef GTK2}
        if GTK_WIDGET_NO_WINDOW(ClientWidget)
        then begin
          ClientOrigin.X := ClientWidget^.Allocation.X;
          ClientOrigin.Y := ClientWidget^.Allocation.Y;
        end else
       {$EndIf}
        ClientOrigin:=MainOrigin;
      end;
      ARect.Left:=ClientOrigin.X-MainOrigin.X;
      ARect.Top:=ClientOrigin.Y-MainOrigin.Y;
      ARect.Right:=ARect.Left+ClientWidget^.Allocation.Width;
      ARect.Bottom:=ARect.Top+ClientWidget^.Allocation.Height;

      Result:=true;
    end;
  end;
  if not Result then begin
    with Widget^.Allocation do
      ARect := Rect(0,0,Width,Height);
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client rectangle of a control. Left and Top are always 0.
  The client rectangle is the size of the inner area of a control, where the
  child controls are visible.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
var
  Widget, ClientWidget: PGtkWidget;
begin
  Result := false;
  if Handle = 0 then Exit;
  ARect.Left := 0;
  ARect.Top := 0;
  Widget := pgtkwidget(Handle);
  ClientWidget := GetFixedWidget(Widget);
  if (ClientWidget <> nil) then
    Widget := ClientWidget;
  if (Widget <> nil) then begin
    ARect.Right:=Widget^.Allocation.Width;
    ARect.Bottom:=Widget^.Allocation.Height;
  end else begin
    ARect.Right:=0;
    ARect.Bottom:=0;
  end;
  {$IfDef VerboseGetClientRect}
  if ClientWidget<>nil then begin
    DebugLn('GetClientRect  Widget=',HexStr(Cardinal(handle),8),
       ' Client=',HexStr(Cardinal(ClientWidget),8),
       ' WindowSize=',ARect.Right,',',ARect.Bottom,
       ' Allocation=',ClientWidget^.Allocation.Width,',',ClientWidget^.Allocation.Height
       );
  end else begin
    DebugLn('GetClientRect  Widget=',HexStr(Cardinal(handle),8),
       ' Client=',HexStr(Cardinal(ClientWidget),8),
       ' WindowSize=',ARect.Right,',',ARect.Bottom,
       ' Allocation=',Widget^.Allocation.Width,',',Widget^.Allocation.Height
       );
  end;
  {$EndIf}
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: GetClipBox
  Params: dc, lprect
  Returns: Integer

  Returns the smallest rectangle which includes the entire current
  Clipping Region, or if no Clipping Region is set, the current
  dimensions of the Drawable.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
var
  CRect : TGDKRectangle;
  X, Y : Longint;
  DCOrigin: Tpoint;
begin
  // set default values
  Result := SIMPLEREGION;
  If lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);

  If not IsValidDC(DC) then
    Result := ERROR;
  if Result <> ERROR
  then with TDeviceContext(DC) do
  begin
    DCOrigin:=GetDCOffset(TDeviceContext(DC));
    If Not IsValidGDIObject(ClipRegion) then begin
      {$IFDEF DebugGDKTraps}
      BeginGDKErrorTrap;
      {$ENDIF}
      gdk_window_get_size(Drawable, @X, @Y);
      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}
      lpRect^ := Rect(-DCOrigin.X, -DCOrigin.Y, X-DCOrigin.X, Y-DCOrigin.Y);
      Result := SIMPLEREGION;
    end
    else begin
      Result := RegionType(PGDIObject(ClipRegion)^.GDIRegionObject);
      gdk_region_get_clipbox(PGDIObject(ClipRegion)^.GDIRegionObject,
        @CRect);
      lpRect^.Left   := CRect.X-DCOrigin.X;
      lpRect^.Top    := CRect.Y-DCOrigin.Y;
      lpRect^.Right  := lpRect^.Left + CRect.Width;
      lpRect^.Bottom := lpRect^.Top + CRect.Height;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetRGNBox
  Params: rgn, lprect
  Returns: Integer

  Returns the smallest rectangle which includes the entire passed
  Region, if lprect is null then just returns RegionType.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
var
  CRect : TGDKRectangle;
begin
  Result := SIMPLEREGION;
  If lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);
  If Not IsValidGDIObject(RGN) then
    Result := ERROR
  else begin
    Result := RegionType(PGDIObject(RGN)^.GDIRegionObject);
    If lpRect <> nil then begin
      gdk_region_get_clipbox(PGDIObject(RGN)^.GDIRegionObject,
        @CRect);
      With lpRect^,CRect do begin
        Left   := X;
        Top    := Y;
        Right  := X + Width;
        Bottom := Y + Height;
      end;
    end;
  end;
end;

Function TGtkWidgetSet.GetROP2(DC: HDC): Integer;
var
  Values: TGdkGCValues;
begin
  if not IsValidDC(DC) then begin
    Assert(False, 'Trace:[TGtkWidgetSet.GetROP2] Invalid GC');
    result := 0
  end else
    with TDeviceContext(DC) do begin
      if GC = nil then begin
        Assert(False, 'Trace:[TGtkWidgetSet.GetROP2] Uninitialized GC');
        Result := 0;
      end else begin
        gdk_gc_get_values(GC, @Values);
        result := GdkFunctionToROP2Mode( Values.thefunction )
      end;
    end;
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer

  Returns a copy of the current Clipping Region.

  The result can be one of the following constants
     0 = no clipping set
     1 = ok
    -1 = error
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN) : longint;
var
  DCOrigin: TPoint;
  ClipRegionWithDCOffset: PGdkRegion;
  CurRegionObject: PGdkRegion;
  ARect: TRect;
begin
  Result := SIMPLEREGION;
  If (not IsValidDC(DC)) then
    Result := ERROR
  else If Not IsValidGDIObject(RGN) then begin
    Result := ERROR;
    DebugLn('WARNING: [TGtkWidgetSet.GetClipRGN] Invalid HRGN');
  end
  else if (TDeviceContext(DC).ClipRegion<>0)
  and (not IsValidGDIObject(TDeviceContext(DC).ClipRegion)) then
    Result := ERROR
  else with TDeviceContext(DC) do
  begin
    CurRegionObject:=nil;
    if ClipRegion<>0 then
      CurRegionObject:=PGdiObject(ClipRegion)^.GDIRegionObject;
    ARect:=Rect(0,0,0,0);
    if CurRegionObject<>nil then begin
      // create a copy of the current clipregion
      ClipRegionWithDCOffset:=gdk_region_copy(CurRegionObject);
      // move it to the DC offset
      // Example: if the ClipRegion is at 10,10 and the DCOrigin is at 10,10,
      //          then the ClipRegion must be moved to 0,0
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      //debugln('TGtkWidgetSet.GetClipRGN DCOrigin=',dbgs(DCOrigin),' CurRegionObject=',dbgs(CurRegionObject),' ',dbgs(ARect));
      gdk_region_offset(ClipRegionWithDCOffset,-DCOrigin.x,-DCOrigin.Y);
    end else begin
      // create a default clipregion
      GetClipBox(DC,@ARect);
      ClipRegionWithDCOffset:=CreateRectGDKRegion(ARect);
    end;
    // free the old region in RGN
    if PGdiObject(RGN)^.GDIRegionObject<>nil then
      gdk_region_destroy(PGdiObject(RGN)^.GDIRegionObject);
    // set the new region in RGN
    PGdiObject(RGN)^.GDIRegionObject := ClipRegionWithDCOffset;

    Result := RegionType(ClipRegionWithDCOffset);
    //DebugLn('TGtkWidgetSet.GetClipRGN B DC=',HexStr(Cardinal(DC),8),
    //  ' DCOrigin=',dbgs(DCOrigin),' RGN=',GDKRegionAsString(ClipRegionWithDCOffset),' Result=',dbgs(Result));
    If Result = NULLREGION then
      Result := 0
    else If Result <> ERROR then
      Result := 1;
  end;
  If Result = ERROR then
    Result := -1;
end;

{------------------------------------------------------------------------------
  Function: GetCmdLineParamDescForInterface
  Params: none
  Returns: ansistring

  Returns a description of the command line parameters, that are understood by
  the interface.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetCmdLineParamDescForInterface: string;
  function b(const s: string): string;
  begin
    Result:=BreakString(s,75,22)+LineEnding+LineEnding;
  end;

begin
  Result:=
     b(rsgtkOptionNoTransient)
    +b(rsgtkOptionModule)
    +b(rsgOptionFatalWarnings)
    +b(rsgtkOptionDebug)
    +b(rsgtkOptionNoDebug)
    +b(rsgdkOptionDebug)
    +b(rsgdkOptionNoDebug)
    +b(rsgtkOptionDisplay)
    +b(rsgtkOptionSync)
    +b(rsgtkOptionNoXshm)
    +b(rsgtkOptionName)
    +b(rsgtkOptionClass);
end;

{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetCursorPos(var lpPoint: TPoint ): Boolean;
{$IFDEF GTK2}
begin
  // TODO: GTK2 GetCursorPos
  DebugLn('TGtkWidgetSet.GetCursorPos ToDo');
  Result:=false;
end;
{$ELSE}
{$IFDEF UNIX}
var
  root, child: pointer;
  winx, winy: Integer;
  xmask: Cardinal;
  TopList, List: PGList;
begin
  Result := False;

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}
  try
    TopList := gdk_window_get_toplevels;
    List := TopList;
    while List <> nil do
    begin
      if (List^.Data <> nil)
      and gdk_window_is_visible(List^.Data)
      then begin
        XQueryPointer(gdk_window_xdisplay(List^.Data),
                      gdk_window_xwindow(List^.Data),
                      @root,@child,@lpPoint.X,@lpPoint.Y,@winx,@winy,@xmask);

        Result := True;
        Break;
      end;
      List := g_list_next(List);
    end;

    if TopList <> nil
    then g_list_free(TopList);

  finally
    {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
  end;
end;
{$ELSE}
begin
  // TODO: GTK1-win32 GetCursorPos
  Result := False;
end;
{$ENDIF unix}
{$ENDIF gkt2}

{------------------------------------------------------------------------------
  Function: GetDC
  Params:  none
  Returns: Nothing

  hWnd is any widget.
  The DC will be created for the client area and without the child areas
  (they are clipped away). Child areas are all child gdkwindows
  (e.g. not TControls).
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDC(hWnd: HWND): HDC;
begin
  Result:=CreateDCForWidget(PGtkWidget(hWnd),nil,false);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
var
  Visual: PGdkVisual;

  function GetVisual: boolean;
  begin
    Visual:=nil;
    with TDeviceContext(DC) do begin
      If Drawable <> nil then
        Visual:=gdk_window_get_visual(PGdkWindow(Drawable));
      if Visual = nil then
        Visual := GDK_Visual_Get_System;
    end;
    Result:=Visual<>nil;
  end;

begin
  Result := -1;
  If DC = 0 then begin
    DC := GetDC(0);
    If DC = 0 then
      exit;
    Result := GetDeviceCaps(DC, Index);
    ReleaseDC(0, DC);
  end;
  if not IsValidDC(DC) then exit;
  with TDeviceContext(DC) do
  Case Index of
  HORZRES : { Horizontal width in pixels }
    If Drawable = nil then
      Result := GetSystemMetrics(SM_CXSCREEN)
    else
      gdk_drawable_get_size(Drawable, @Result, nil);

  VERTRES : { Vertical height in pixels }
    If Drawable = nil then
      Result := GetSystemMetrics(SM_CYSCREEN)
    else
      gdk_drawable_get_size(Drawable, nil, @Result);

  BITSPIXEL : { Number of used bits per pixel = depth }
    If Drawable = nil then
      Result := GDK_Visual_Get_System^.Depth
    else
      //gdk_window_get_geometry(Drawable, nil, nil, nil, nil, @Result);
      Result := gdk_drawable_get_depth(Drawable);

  PLANES : { Number of planes }
    // ToDo
    Result := 1;

  //For Size in MM, MM = (Pixels*100)/(PPI*25.4)

  HORZSIZE : { Horizontal size in millimeters }
    Result := RoundToInt((GetDeviceCaps(DC, HORZRES) * 100) /
                    (GetDeviceCaps(DC, LOGPIXELSX) * 25.4));

  VERTSIZE : { Vertical size in millimeters }
    Result := RoundToInt((GetDeviceCaps(DC, VERTRES) * 100) /
                    (GetDeviceCaps(DC, LOGPIXELSY) * 25.4));

  //So long as gdk_screen_width_mm is acurate, these should be
  //acurate for Screen GDKDrawables. Once we get Metafiles
  //we will also have to add internal support for Papersizes etc..

  LOGPIXELSX : { Logical pixels per inch in X }
    Result := RoundToInt(gdk_screen_width / (gdk_screen_width_mm / 25.4));

  LOGPIXELSY : { Logical pixels per inch in Y }
    Result := RoundToInt(gdk_screen_height / (gdk_screen_height_mm / 25.4));

  SIZEPALETTE: { number of entries in color palette }
    if GetVisual then
      Result:=Visual^.colormap_size
    else
      Result:=0;

  NUMRESERVED: { number of reserverd colors in color palette }
    Result:=0;

  else
    DebugLn('TGtkWidgetSet.GetDeviceCaps not supported: Type=',dbgs(Index));
  end;
end;

{------------------------------------------------------------------------------
  function GetDeviceRawImageDescription(DC: HDC;
                                        Desc: PRawImageDescription): boolean;

  Retrieves the information about the structure of the supported image data.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDeviceRawImageDescription(DC: HDC;
  Desc: PRawImageDescription): boolean;
var
  GDKWindow: PGdkWindow;
begin
  GdkWindow:=nil;
  If IsValidDC(DC) then
    GDKWindow:=PGdkWindow(TDeviceContext(DC).Drawable);
  Result:=GetWindowRawImageDescription(GDKWindow,Desc);
end;

{------------------------------------------------------------------------------
  function GetDeviceSize(DC: HDC; var p: TPoint): boolean;

  Retrieves the width and height of the device context in pixels.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result := false;
  P := Point(0,0);
  If IsValidDC(DC) then
  with TDeviceContext(DC) do begin
    if Drawable<>nil then begin
      gdk_window_get_size(PGdkWindow(Drawable), @P.X, @P.Y);
      Result := true;
    end else begin
      {$IFDEF RaiseExceptionOnNilPointers}
      RaiseException('TGtkWidgetSet.GetDeviceSize Window=nil');
      {$ENDIF}
      DebugLn('TGtkWidgetSet.GetDeviceSize:',
        ' WARNING: DC ',HexStr(Cardinal(DC),8),' without gdkwindow.',
        ' Widget=',HexStr(Cardinal(wnd),8));
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
    WindowHandle: HWND; var OriginDiff: TPoint): boolean;
    
  Returns the origin of PaintDC relative to the window handle.
  Example:
    A PaintDC of a TButton at 20,10 with a DC Offset of 0,0 on a form and the
    WindowHandle is the form.
    Then OriginDiff will be the the difference between the Forms client origin
    and the PaintDC will be 20,10.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;

  procedure InvalidDrawable;
  begin
    {$IFDEF RaiseExceptionOnNilPointers}
    RaiseException('TGtkWidgetSet.GetDCOriginRelativeToWindow Window=nil');
    {$ENDIF}
    DebugLn('TGtkWidgetSet.GetDCOriginRelativeToWindow:',
      ' WARNING: PaintDC ',HexStr(Cardinal(PaintDC),8),' without gdkwindow.',
      ' Widget=',HexStr(Cardinal(TDeviceContext(PaintDC).wnd),8));
  end;

var
  DCOrigin: TPoint;
  DCScreenOrigin: TPoint;
  WindowScreenOrigin: TPoint;
  Widget: PGtkWidget;
  ScreenDrawable: PGdkDrawable;
begin
  Result := false;
  OriginDiff := Point(0,0);
  If not IsValidDC(PaintDC) then exit;
  with TDeviceContext(PaintDC) do begin
    DCOrigin:=GetDCOffset(TDeviceContext(PaintDC));
    ScreenDrawable:=Drawable;
    if (dcfDoubleBuffer in DCFlags) then
      ScreenDrawable:=OriginalDrawable;
    if ScreenDrawable=nil then
      InvalidDrawable;
    gdk_window_get_origin(PGdkWindow(Drawable),
                          @(DCScreenOrigin.X), @(DCScreenOrigin.Y));
    inc(DCScreenOrigin.X,DCOrigin.X);
    inc(DCScreenOrigin.Y,DCOrigin.Y);
    Widget:=GetFixedWidget(PGtkWidget(WindowHandle));
    if Widget=nil then Widget:=PGtkWidget(WindowHandle);
    gdk_window_get_origin(PGdkWindow(Widget^.window),
                          @(WindowScreenOrigin.X), @(WindowScreenOrigin.Y));
    OriginDiff.X:=DCScreenOrigin.X-WindowScreenOrigin.X;
    OriginDiff.Y:=DCScreenOrigin.Y-WindowScreenOrigin.Y;
    Result := true;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetDesignerDC
  Params:  none
  Returns: Nothing

  WindowHandle is any widget.
  The DC will be created for the client area including the child areas.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  //DebugLn('TGtkWidgetSet.GetDesignerDC A');
  Result:=CreateDCForWidget(PGtkWidget(WindowHandle),nil,true);
end;

{------------------------------------------------------------------------------
  Function: GetFocus
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetFocus: HWND;
var
  TopList, List: PGList;
  Widget: PGTKWidget;
  Window: PGTKWindow;
begin
  // Default to 0
  Result := 0;

  {$IFDEF DebugGDKTraps}
  BeginGDKErrorTrap;
  {$ENDIF}

  TopList := gdk_window_get_toplevels;
  List := TopList;
  while List <> nil do
  begin
    if (List^.Data <> nil)
    then begin
      gdk_window_get_user_data(PGDKWindow(List^.Data), @Window);
      if gtk_is_window(Window)
      then begin
        Widget := Window^.focus_widget;

        if (Widget <> nil) and gtk_widget_has_focus(Widget)
        then begin
          Result := HWND(GetMainWidget(Widget));
          Break;
        end;
      end;
    end;
    list := g_list_next(list);
  end;

  if TopList <> nil
  then g_list_free(TopList);

  {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
end;

{------------------------------------------------------------------------------
  function GetFontLanguageInfo(DC: HDC): DWord; override;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result := 0;
  If IsValidDC(DC) then
  with TDeviceContext(DC) do begin
    UpdateDCTextMetric(TDeviceContext(DC));
    if TDeviceContext(DC).DCTextMetric.IsDoubleByteChar then
      inc(Result,GCP_DBCS);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetKeyState
  Params:  nVirtKey: The requested key
  Returns: If the function succeeds, the return value specifies the status of
           the given virtual key. If the high-order bit is 1, the key is down;
           otherwise, it is up. If the low-order bit is 1, the key is toggled.

  The GetKeyState function retrieves the status of the specified virtual key.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
const
  KEYSTATE: array[Boolean] of Smallint = (0, -32768 { $8000});
  TOGGLESTATE: array[Boolean] of Smallint = (0, 1);
begin
  case nVirtKey of
    VK_LSHIFT:   nVirtKey := VK_SHIFT;
    VK_LCONTROL: nVirtKey := VK_CONTROL;
    VK_LMENU:    nVirtKey := VK_MENU;
  end;
  {$IFDEF Use_KeyStateList}
  Result := KEYSTATE[FKeyStateList_.IndexOf(Pointer(nVirtKey)) >=0];
  {$ELSE}
  Implement this
  {$ENDIF}

  // try extended keys
  if Result = 0
  then begin
    nVirtKey := nVirtKey or KEYMAP_EXTENDED;
    {$IFDEF Use_KeyStateList}
    Result := KEYSTATE[FKeyStateList_.IndexOf(Pointer(nVirtKey)) >=0];
    {$ELSE}
    Implement this
    {$ENDIF}
  end;

  {$IFDEF Use_KeyStateList}
  // add toggle
  if Result <> 0 then
    Result := Result or TOGGLESTATE[FKeyStateList_.IndexOf(Pointer(
                                             nVirtKey or KEYMAP_TOGGLE)) >=0];
  {$ENDIF}
  //Assert(False, Format('Trace:[TGtkWidgetSet.GetKeyState] %d -> 0x%x', [nVirtKey, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer;
  Buf: Pointer): Integer;
var
  NumColors : Longint;
  BitmapSection : TDIBSECTION;
begin
  Assert(False, 'trace:[TGtkWidgetSet.GetObject]');
  Result := 0;
  if IsValidGDIObject(GDIObj)
  then begin
    case PGDIObject(GDIObj)^.GDIType of
      gdiBitmap:
        begin
          Assert(False, 'Trace:FINISH: [TGtkWidgetSet.GetObject] gdiBitmap');
          if Buf = nil then
            Result := SizeOf(TDIBSECTION)
          else begin
            With PGDIObject(GDIObj)^, BitmapSection,
              BitmapSection.dsBm, BitmapSection.dsBmih
            do begin
              {dsBM - BITMAP}
              bmType := $4D42;
              bmWidth := 0 ;
              bmHeight := 0;
              {bmWidthBytes: Longint;}
              bmPlanes := 1;//Does Bitmap Format support more?
              bmBitsPixel := 1;
              bmBits := nil;

              {dsBmih - BITMAPINFOHEADER}
              biSize := 40;
              biWidth := 0;
              biHeight := 0;
              biPlanes := bmPlanes;
              biBitCount := 1;

              biCompression := 0;
              biSizeImage := 0;

              biXPelsPerMeter := 0;
              biYPelsPerMeter := 0;

              biClrUsed   := 0;
              biClrImportant := 0;

              {dsBitfields: array[0..2] of DWORD;
              dshSection: THandle;
              dsOffset: DWORD;}
              {$IFDEF DebugGDKTraps}
              BeginGDKErrorTrap;
              {$ENDIF}
              case GDIBitmapType of
                gbBitmap:
                  If GDIBitmapObject <> nil then begin
                    GDK_WINDOW_GET_SIZE(GDIBitmapObject, @biWidth, @biHeight);
                    NumColors := 2;
                    biBitCount := 1;
                  end;
                gbPixmap:
                  If GDIPixmapObject <> nil then begin
                     biBitCount := word(gdk_drawable_get_depth(GDIPixmapObject));
                     gdk_drawable_get_size(GDIPixmapObject,@biWidth, @biHeight);
                  end;
                {obsolete: gbImage :
                  If GDI_RGBImageObject <> nil then
                    With GDI_RGBImageObject^ do begin
                      biHeight := Height;
                      biWidth := Width;
                      biBitCount := Depth;
                    end;}
              end;

              If Visual = nil then begin
                Visual := gdk_visual_get_best_with_depth(biBitCount);
                If Visual = nil then  { Depth not supported }
                  Visual := gdk_visual_get_system;
                SystemVisual := True; { This visual should not be referenced }
                If Colormap <> nil then
                  gdk_colormap_unref(Colormap);
                ColorMap := gdk_colormap_new(Visual, GdkTrue);
              end else
                biBitCount := Visual^.Depth;

              {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
              If biBitCount < 16 then
                NumColors := Colormap^.Size;

              biSizeImage := (((biBitCount*biWidth+31) shr 5) shl 2)*biHeight;

              If GetSystemMetrics(SM_CXSCREEN) >= biWidth then
                biXPelsPerMeter := GetDeviceCaps(0, LOGPIXELSX)
              else
                biXPelsPerMeter :=
                  RoundToInt((single(biWidth) / GetSystemMetrics(SM_CXSCREEN)) *
                        GetDeviceCaps(0, LOGPIXELSX));

              If GetSystemMetrics(SM_CYSCREEN) >= biHeight then
                biYPelsPerMeter := GetDeviceCaps(0, LOGPIXELSY)
              else
                biYPelsPerMeter :=
                  RoundToInt((Single(biHeight) / GetSystemMetrics(SM_CYSCREEN))*
                        GetDeviceCaps(0, LOGPIXELSY));

              bmWidth := biWidth;
              bmHeight := biHeight;
              bmBitsPixel := biBitCount;

              //Need to retrieve actual Number of Colors if Indexed Image
              if (bmBitsPixel < 16) then begin
                biClrUsed   := NumColors;
                biClrImportant := biClrUsed;
              end;
            end;
            if BufSize >= SizeOf(BitmapSection)
            then begin
              PDIBSECTION(Buf)^ := BitmapSection;
              Result:= SizeOf(TDIBSECTION);
            end else
            if BufSize>0 then begin
              Move(BitmapSection,Buf^,BufSize);
              Result:=BufSize;
            end;
          end;
        end;

      gdiBrush:
        begin
          Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetObject] gdiBrush');
        end;
      gdiFont:
        begin
          {$IfDef GTK2}
          Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetObject] gdiFont(PANGO)');
          {$Else}
          if Buf = nil then
            Result := SizeOf(PGDIObject(GDIObj)^.LogFont)
          else begin
            if BufSize >= SizeOf(PGDIObject(GDIObj)^.LogFont)
            then begin
              PLogfont(Buf)^ := PGDIObject(GDIObj)^.LogFont;
              Result:= SizeOf(TLogFont);
            end else if BufSize>0 then begin
              Move(PGDIObject(GDIObj)^.LogFont,Buf^,BufSize);
              Result:=BufSize;
            end;
          end;
          {$EndIf}
        end;
      gdiPen:
        begin
          Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetObject] gdiPen');
        end;
      gdiRegion:
        begin
          Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetObject] gdiRegion');
        end;
    else
      DebugLn(Format('WARNING: [TGtkWidgetSet.GetObject] Unknown type %d', [Integer(PGDIObject(GDIObj)^.GDIType)]));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetParent
  Params: Handle:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetParent(Handle : HWND): HWND;
begin
  //DebugLn('TGtkWidgetSet.GetParent ',HexStr(Cardinal(Handle),8));
  Result:=0;
  if Handle<>0 then
    Result:=HWnd(PGtkWidget(Handle)^.Parent);
end;


{------------------------------------------------------------------------------
  Function: GetProp
  Params: Handle: Str
  Returns: Pointer

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetProp(Handle : hwnd; Str : PChar): Pointer;
Begin
  Result := gtk_object_get_data(pgtkobject(Handle),Str);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect;
    var NewRawImage: TRawImage): boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect;
  var NewRawImage: TRawImage): boolean;
var
  DCOrigin: TPoint;
  ARect: TRect;
  GDKWindow: PGdkWindow;
begin
  Result:=false;
  if not IsValidDC(SrcDC) then begin
    DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromDevice invalid SrcDC');
    exit;
  end;

  DCOrigin:=GetDCOffset(TDeviceContext(SrcDC));
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromDevice A DCOrigin=',dbgs(DCOrigin.X),',',dbgs(DCOrigin.Y),' SrcRect=',dbgs(SrcRect.Left),',',dbgs(SrcRect.Top),',',dbgs(SrcRect.Right),',',dbgs(SrcRect.Bottom));
  {$ENDIF}
  ARect:=SrcRect;
  OffSetRect(ARect,DCOrigin.x,DCOrigin.y);

  GDKWindow:=PGdkWindow(TDeviceContext(SrcDC).Drawable);
  Result:=GetRawImageFromGdkWindow(GDKWindow,nil,ARect,NewRawImage);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
    const SrcRect: TRect; var NewRawImage: TRawImage): boolean; override;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
var
  GDIImg: PGDIObject;
  GdkPixmap: PGdkPixmap;
  GDIMaskImg: PGDIObject;
  GdkMaskBitmap: PGdkBitmap;
begin
  Result:=false;
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromBitmap A');
  {$ENDIF}
  FillChar(NewRawImage,SizeOf(NewRawImage),0);

  if (not IsValidGDIObject(SrcBitmap)) then begin
    DebugLn('WARNING: [TGtkWidgetSet.GetRawImageFromBitmap] invalid SrcBitmap!');
    exit;
  end;
  if ((SrcMaskBitmap<>0) and not IsValidGDIObject(SrcMaskBitmap)) then begin
    DebugLn('WARNING: [TGtkWidgetSet.GetRawImageFromBitmap] invalid MaskBitmap!');
    exit;
  end;

  try
    // get rawimage for Bitmap
    GDIImg:=PGDIObject(SrcBitmap);
    GdkPixmap:=nil;
    case GDIImg^.GDIBitmapType of
    gbBitmap: GdkPixmap:=PGdkPixmap(GDIImg^.GDIBitmapObject);
    gbPixmap: GdkPixmap:=PGdkPixmap(GDIImg^.GDIPixmapObject);
    else
      DebugLn('WARNING: [TGtkWidgetSet.GetRawImageFromBitmap] GDI_RGBImage not implemented');
      exit;
    end;
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.GetRawImageFromBitmap A GdkPixmap=',HexStr(Cardinal(GdkPixmap),8),' SrcMaskBitmap=',HexStr(Cardinal(SrcMaskBitmap),8));
    {$ENDIF}

    GDIMaskImg:=nil;
    GdkMaskBitmap:=nil;
    if SrcMaskBitmap<>0 then begin
      // use special mask SrcMaskBitmap
      GDIMaskImg:=PGDIObject(SrcMaskBitmap);
      case GDIMaskImg^.GDIBitmapType of
      gbBitmap: GdkMaskBitmap:=GDIMaskImg^.GDIBitmapObject;
      else
        DebugLn('WARNING: [TGtkWidgetSet.GetRawImageFromBitmap] invalid MaskBitmap');
        exit;
      end;
    end else if GDIImg^.GDIBitmapMaskObject<>nil then begin
      // use mask in SrcBitmap
      GdkMaskBitmap:=GDIImg^.GDIBitmapMaskObject;
    end else begin
      // no mask available
    end;

    if not GetRawImageFromGdkWindow(PGdkWindow(GdkPixmap),GdkMaskBitmap,SrcRect,
      NewRawImage)
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.GetRawImageFromBitmap] unable to GetRawImageFromGdkWindow Image');
      exit;
    end;

  except
    FreeRawImageData(@NewRawImage);
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;

  Returns the current width of the scrollbar of the widget.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
var
  Widget, ScrollWidget, BarWidget: PGtkWidget;
begin
  Result:=0;
  Widget:=PGtkWidget(Handle);
  if GtkWidgetIsA(Widget,GTK_TYPE_SCROLLED_WINDOW) then begin
    ScrollWidget:=Widget;
  end else begin
    ScrollWidget:=PGtkWidget(gtk_object_get_data(
                                             PGtkObject(Widget),odnScrollArea));
  end;
  if ScrollWidget=nil then exit;
  if BarKind=SM_CYVSCROLL then begin
    BarWidget:=PGtkScrolledWindow(ScrollWidget)^.vscrollbar;
    if BarWidget<>nil then
      Result:=BarWidget^.Requisition.Width;
  end else begin
    BarWidget:=PGtkScrolledWindow(ScrollWidget)^.hscrollbar;
    if BarWidget<>nil then
      Result:=BarWidget^.Requisition.Height;
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetScrollbarVisible(Handle: HWND;
    SBStyle: Integer): boolean;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
var
  Widget, ScrollWidget, BarWidget: PGtkWidget;
begin
  Result:=false;
  if Handle=0 then exit;
  Widget:=PGtkWidget(Handle);
  if GtkWidgetIsA(Widget,GTK_TYPE_SCROLLED_WINDOW) then begin
    ScrollWidget:=Widget;
  end else begin
    ScrollWidget:=PGtkWidget(gtk_object_get_data(
                                             PGtkObject(Widget),odnScrollArea));
  end;
  if ScrollWidget=nil then exit;
  if SBStyle=SB_VERT then begin
    BarWidget:=PGtkScrolledWindow(ScrollWidget)^.vscrollbar;
  end else begin
    BarWidget:=PGtkScrolledWindow(ScrollWidget)^.hscrollbar;
  end;
  if BarWidget<>nil then
    Result:=GTK_WIDGET_VISIBLE(BarWidget);
end;

{------------------------------------------------------------------------------
  Function: GetScrollInfo
  Params:  Handle, BarFlag, ScrollInfo
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetScrollInfo(Handle: HWND; SBStyle: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
var
  Adjustment: PGtkAdjustment;
  Scroll : PGTKWidget;
begin
  Result := false;
  if (Handle = 0) then exit;

  Adjustment := nil;
  Scroll := GTK_Object_Get_Data(PGTKObject(Handle), odnScrollArea);
  If not GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
    Scroll := PGTKWidget(Handle);

  case SBStyle of
    SB_HORZ:
      If GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
        Adjustment := gtk_scrolled_window_get_hadjustment(
                         PGTKScrolledWindow(Scroll))
      else
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_hscrollbar_get_type) then
        Adjustment :=  PgtkhScrollBar(Scroll)^.Scrollbar.Range.Adjustment
      else  //clist
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_clist_get_type) then
        Adjustment := gtk_clist_get_hadjustment(PgtkCList(Scroll));

    SB_VERT:
      If GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
        Adjustment := gtk_scrolled_window_get_vadjustment(
                         PGTKScrolledWindow(Scroll))
      else
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_vscrollbar_get_type) then
        Adjustment :=  PgtkvScrollBar(Scroll)^.Scrollbar.Range.Adjustment
      else  //clist
      if GtkWidgetIsA(PGtkWidget(Scroll), gtk_clist_get_type) then
        Adjustment := gtk_clist_get_vadjustment(PgtkCList(Scroll));

    SB_CTL:
      if GtkWidgetIsA(PGtkWidget(Scroll), gtk_range_get_type) then
        Adjustment := gtk_range_get_adjustment(PGTKRange(Scroll));

  end;

  if Adjustment<>nil then begin
    with ScrollInfo, Adjustment^ do begin
      // POS
      if (fMask and SIF_POS) <> 0 then
        nPos := RoundToInt(Value);
      // RANGE
      if (fMask and SIF_RANGE) <> 0
      then begin
        nMin:= RoundToInt(Lower);
        nMax:= RoundToInt(Upper);
      end;
      // PAGE
      if (fMask and SIF_PAGE) <> 0 then
        nPage := RoundToCardinal(Page_Size);
      // TRACKPOS
      if (fMask and SIF_TRACKPOS)<>0 then
        nTrackPos := RoundToInt(Value);
    end;
    Result := true;
  end else begin
    with ScrollInfo, Adjustment^ do begin
      // POS
      if (fMask and SIF_POS) <> 0 then
        nPos := 0;
      // RANGE
      if (fMask and SIF_RANGE) <> 0
      then begin
        nMin:= 0;
        nMax:= 0;
      end;
      // PAGE
      if (fMask and SIF_PAGE) <> 0 then
        nPage := 0;
      // TRACKPOS
      if (fMask and SIF_TRACKPOS)<>0 then
        nTrackPos := 0;
    end;
    Result := false;
  end;
end;

{------------------------------------------------------------------------------
  Function TGtkWidgetSet.CreateSystemFont : hFont;
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.CreateSystemFont: hFont;
var
  GDIObj : PGDIObject;
begin
  GDIObj := NewGDIObject(gdiFont);
  {$IfDef GTK2}
  GDIObj^.GDIFontObject:= GetDefaultFontDesc(true);
  {$Else}
  GDIObj^.GDIFontObject:= GetDefaultFont(true);
  {$EndIf}
  Result := hFont(GDIObj);
end;

{------------------------------------------------------------------------------
  Function: GetStockObject
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetStockObject(Value: Integer): LongInt;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.GetStockObject] %d', [Value]));
  Result := 0;
  case Value of
    BLACK_BRUSH:         // Black brush.
      Result := FStockBlackBrush;
    DKGRAY_BRUSH:        // Dark gray brush.
      Result := FStockDKGrayBrush;
    GRAY_BRUSH:          // Gray brush.
      Result := FStockGrayBrush;
    LTGRAY_BRUSH:        // Light gray brush.
      Result := FStockLtGrayBrush;
    NULL_BRUSH:          // Null brush (equivalent to HOLLOW_BRUSH).
      Result := FStockNullBrush;
    WHITE_BRUSH:         // White brush.
      Result := FStockWhiteBrush;

    BLACK_PEN:           // Black pen.
      Result := FStockBlackPen;
    NULL_PEN:            // Null pen.
      Result := FStockNullPen;
    WHITE_PEN:           // White pen.
      Result := FStockWhitePen;

   (* ANSI_FIXED_FONT:     // Fixed-pitch (monospace) system font.
      begin
        {If FStockFixedFont = 0 then
          FStockFixedFont := GetStockFixedFont;
        Result := FStockFixedFont;}
      end;
    ANSI_VAR_FONT:       // Variable-pitch (proportional space) system font.
      begin
      end;
    DEVICE_DEFAULT_FONT: // Device-dependent font.
      begin
      end;  *)
(*    OEM_FIXED_FONT:      // Original equipment manufacturer (OEM) dependent fixed-pitch (monospace) font.
      begin
      end;
*)
    DEFAULT_GUI_FONT, SYSTEM_FONT:         // System font. By default, Windows uses the system font to draw menus, dialog box controls, and text. In Windows versions 3.0 and later, the system font is a proportionally spaced font; earlier versions of Windows used a monospace system font.
      begin
        If FStockSystemFont <> 0 then begin //This is a Temporary Hack!!! This
          DeleteObject(FStockSystemFont);   //should really only be done on
          FStockSystemFont := 0;            //theme change.
        end;

        If FStockSystemFont = 0 then
          FStockSystemFont := CreateSystemFont;
        Result := FStockSystemFont;
      end;
(*    SYSTEM_FIXED_FONT:   // Fixed-pitch (monospace) system font used in Windows versions earlier than 3.0. This stock object is provided for compatibility with earlier versions of Windows.
      begin
        Result := GetStockObject(ANSI_FIXED_FONT);
      end;
    DEFAULT_PALETTE:     // Default palette. This palette consists of the static colors in the system palette.
      begin
      end;
*)  else
    Assert(False, Format('Trace:TODO: [TGtkWidgetSet.GetStockObject] Implement value: %d', [Value]));
  end;
  Assert(False, Format('Trace:< [TGtkWidgetSet.GetStockObject] %d --> 0x%x', [Value, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS)
  then begin
    Result := 0;
    //RaiseException('');
    DebugLn(Format('ERROR: [TGtkWidgetSet.GetSysColor] Bad Value: %8x Valid Range between 0 and %d', [nIndex, MAX_SYS_COLORS]));
  end
  else Result := SysColorMap[nIndex];
  //Assert(False, Format('Trace:[TGtkWidgetSet.GetSysColor] Index %d --> %8x', [nIndex, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  P : Pointer;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.GetSystemMetrics] %d', [nIndex]));
  case nIndex of
    SM_ARRANGE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_ARRANGE          ');
      end;
    SM_CLEANBOOT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CLEANBOOT        ');
      end;
    SM_CMOUSEBUTTONS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXBORDER         ');
      end;
    SM_CYBORDER:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYBORDER         ');
      end;
    SM_CXCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXCURSOR         ');
      end;
    SM_CYCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYCURSOR         ');
      end;
    SM_CXDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
      end;
    SM_CXDRAG:
      begin
        Result := 2;
      end;
    SM_CYDRAG:
      begin
        Result := 2;
      end;
    SM_CXEDGE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXEDGE           ');
      end;
    SM_CYEDGE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYEDGE           ');
      end;
    SM_CXFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHSCROLL:
      begin
        P:=GetStyleWidget(lgsVerticalScrollbar);
        Result := GTK_Widget(P)^.requisition.Width;
      end;
    SM_CYHSCROLL:
      begin
        P:=GetStyleWidget(lgsHorizontalScrollbar);
        Result := GTK_Widget(P)^.requisition.Height;
      end;
    SM_CXHTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXICON           ');
      end;
    SM_CYICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYICON           ');
      end;
    SM_CXICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMENUSIZE       ');
      end;
    SM_CYMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENUSIZE       ');
      end;
    SM_CXMIN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
      end;
    SM_CXSCREEN:
      begin
        result := gdk_Screen_Width;
      end;
    SM_CYSCREEN:
      begin
        result := gdk_Screen_Height;
      end;
    SM_CXSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSIZEFRAME      ');
      end;
    SM_CYSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSIZEFRAME      ');
      end;
    SM_CXSMICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSMICON         ');
      end;
    SM_CYSMICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMICON         ');
      end;
    SM_CXSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVSCROLL:
      begin
        P:=GetStyleWidget(lgsVerticalScrollbar);
        Result := GTK_Widget(P)^.requisition.Width;
      end;
    SM_CYVSCROLL:
      begin
        P:=GetStyleWidget(lgsHorizontalScrollbar);
        Result := GTK_Widget(P)^.requisition.Height;
      end;
    SM_CYCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYCAPTION        ');
      end;
    SM_CYKANJIWINDOW:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
    else Result := 0;
  end;
  Assert(False, Format('Trace:< [TGtkWidgetSet.GetSystemMetrics] %d --> 0x%x (%d)', [nIndex, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetTextColor(DC: HDC) : TColorRef;
begin
  Result := 0;
  if IsValidDC(DC) then
    with TDeviceContext(DC) do
    begin
      Result := CurrentTextColor.ColorRef;
    end;
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
{$IfDef GTK2}
begin
  DebugLn('TGtkWidgetSet.GetTextExtentPoint ToDo');
  Result:=false;
end;
{$Else}
var
  lbearing, rbearing, width, ascent,descent: LongInt;
  UseFont : PGDKFont;
  UnRef : Boolean;
  IsDBCSFont: Boolean;
  NewCount: Integer;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      UseFont := GetDefaultFont(true);
      UnRef := True;
    end
    else begin
      UseFont := CurrentFont^.GDIFontObject;
      UnRef := False;
    end;
    If UseFont = nil then
      DebugLn('WARNING: [TGtkWidgetSet.GetTextExtentPoint] Missing font')
    else begin
      descent:=0;
      UpdateDCTextMetric(TDeviceContext(DC));
      IsDBCSFont:=TDeviceContext(DC).DCTextMetric.IsDoubleByteChar;
      if IsDBCSFont then begin
        NewCount:=Count*2;
        if FExtUTF8OutCacheSize<NewCount then begin
          ReAllocMem(FExtUTF8OutCache,NewCount);
          FExtUTF8OutCacheSize:=NewCount;
        end;
        NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
        gdk_text_extents(UseFont, FExtUTF8OutCache, NewCount,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end else begin
        gdk_text_extents(UseFont, Str, Count,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end;
      Size.cX := Width;
      // I THINK this is accurate...
      Size.cY := GDK_String_Height(UseFont, Str)
        {$IfNDef Win32} + descent div 2{$EndIf};
      If UnRef then
        FontCache.Unreference(UseFont);
    end;
  end;
  Assert(False, 'trace:< [TGtkWidgetSet.GetTextExtentPoint]');
end;
{$EndIf}

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Assert(False, Format('Trace:> TODO FINISH[TGtkWidgetSet.GetTextMetrics] DC: 0x%x', [DC]));

  Result := IsValidDC(DC);
  if Result then begin
    UpdateDCTextMetric(TDeviceContext(DC));
    TM:=TDeviceContext(DC).DCTextMetric.TextMetric;
  end;

  Assert(False, Format('Trace:< TODO FINISH[TGtkWidgetSet.GetTextMetrics] DC: 0x%x', [DC]));
end;

{------------------------------------------------------------------------------
  Function: GetWindowLong
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetWindowLong(Handle : hwnd; int : Integer): Longint;
var
  //Data : Tobject;
  P : Pointer;
begin
  //TODO:Started but not finished
  Assert(False, Format('Trace:> [TGtkWidgetSet.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d)', [Handle, int, int]));

  case int of
    GWL_WNDPROC :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'WNDPROC'));
      end;
    GWL_HINSTANCE :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'HINSTANCE'));
      end;
    GWL_HWNDPARENT :
      begin
        P := gtk_object_get_data(pgtkobject(Handle),'HWNDPARENT');
        if P = nil then Result := 0 else Result := LongInt(p);
      end;

{    GWL_WNDPROC :
      begin
        Data := GetLCLObject(Pointer(Handle));
        if Data is TControl
        then Result := Longint(@(TControl(Data).WindowProc));
        // TODO fix this, a method pointer (2 pointers) cant be casted to a longint
      end;
}
{    GWL_HWNDPARENT :
      begin
        Data := GetLCLObject(Pointer(Handle));
        if (Data is TWinControl)
        then Result := Longint(TWincontrol(Data).Handle)
        else Result := 0;
      end;
 }
    GWL_STYLE :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'Style'));
      end;
    GWL_EXSTYLE :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'ExStyle'));
      end;
    GWL_USERDATA :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'Userdata'));
      end;
    GWL_ID :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'ID'));
      end;
    else Result := 0;
  end; //case

  Assert(False, Format('Trace:< [TGtkWidgetSet.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d) --> 0x%x (%d)', [Handle, int, int, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetWindowOrgEx
  Params:  none
  Returns: Nothing

  Returns the current offset of the DC.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetWindowOrgEx(dc : hdc; P : PPoint): Integer;
begin
  Result := 0;
  if P=nil then exit;
  P^ := Point(0,0);
  If not IsValidDC(DC) then exit;
  with TDeviceContext(DC) do begin
    P^:=GetDCOffset(TDeviceContext(DC));
    Result:=1;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetWindowRect
  Params:  none
  Returns: 0

  After the call, ARect will be the control area in screen coordinates.
  That means, Left and Top will be the screen coordinate of the TopLeft pixel
  of the Handle object and Right and Bottom will be the screen coordinate of
  the BottomRight pixel.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect): Integer;
var
  X, Y, W, H: Integer;
  Widget: PGTKWidget;
  Window: PGdkWindow;
begin
  //DebugLn('GetWindowRect');
  Result := 0; //default
  if Handle <> 0 then
  begin
    Widget := pgtkwidget(Handle);
    Window:=GetControlWindow(Widget);
    if Window <> nil then Begin
      gdk_window_get_origin(Window, @X, @Y);
      gdk_window_get_size(Window, @W, @H);
    end
    else
    Begin
      X := 0;
      Y := 0;
      W := 100;
      Y := 200;
    end;

    ARect:=Rect(X,Y,X+W,Y+H);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the Left, Top, relative to the client origin of its parent
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetWindowRelativePosition(Handle : hwnd;
  var Left, Top: integer): boolean;
begin
  if GtkWidgetIsA(PGtkWidget(Handle),GTK_TYPE_WIDGET) then begin
    Result:=true;
    Left:=PGtkWidget(Handle)^.Allocation.X;
    Top:=PGtkWidget(Handle)^.Allocation.Y;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.GetWindowSize(Handle : hwnd;
  var Width, Height: integer): boolean;
begin
  if GtkWidgetIsA(PGtkWidget(Handle),GTK_TYPE_WIDGET) then begin
    Result:=true;
    Width:=PGtkWidget(Handle)^.Allocation.Width;
    Height:=PGtkWidget(Handle)^.Allocation.Height;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------------
  Function: GradientFill
  Params: DC - DeviceContext to perform on
          Vertices - array of Points W/Color & Alpha
          NumVertices - Number of Vertices
          Meshes - array of Triangle or Rectangle Meshes,
                   each mesh representing one Gradient Fill
          NumMeshes - Number of Meshes
          Mode - Gradient Type, either Triangle,
                 Vertical Rect, Horizontal Rect

  Returns: true on success

  Performs multiple Gradient Fills, either a Three way Triangle Gradient,
  or a two way Rectangle Gradient, each Vertex point also supports optional
  Alpha/Transparency for more advanced Gradients.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean;

  Function DoFillTriangle : Boolean;
  begin
    Result := (Mode and GRADIENT_FILL_TRIANGLE) = GRADIENT_FILL_TRIANGLE;
  end;

  Function DoFillVRect : Boolean;
  begin
    Result := (Mode and GRADIENT_FILL_RECT_V) = GRADIENT_FILL_RECT_V;
  end;

  Procedure GetGradientBrush(BeginColor, EndColor : TColorRef; Position,
    TotalSteps : Longint; var GradientBrush : hBrush);
  var
    R1, G1, B1 : Integer;
    R2, G2, B2 : Integer;
    NewBrush : TLogBrush;
  begin
    GetRGBIntValues(BeginColor,R1,G1,B1);
    GetRGBIntValues(EndColor,R2,G2,B2);

    R1 := R1 + (Position*(R2 - R1) div TotalSteps);
    G1 := G1 + (Position*(G2 - G1) div TotalSteps);
    B1 := B1 + (Position*(B2 - B1) div TotalSteps);

    With NewBrush do begin
      lbStyle := BS_SOLID;
      lbColor := RGB(R1,G1,B1);
    end;

    If GradientBrush <> 0 then
      LCLIntf.DeleteObject(GradientBrush);
    GradientBrush := LCLIntf.CreateBrushIndirect(NewBrush);
  end;

  Function FillTriMesh(Mesh : tagGradientTriangle) : Boolean;
  {var
    V1, V2, V3 : tagTRIVERTEX;
    C1, C2, C3 : TColorRef;
  begin
    With Mesh do begin
      Result := (Vertex1 < NumVertices) and (Vertex2 >= 0) and
        (Vertex2 < NumVertices) and (Vertex2 >= 0) and
        (Vertex3 < NumVertices) and (Vertex3 >= 0);

      If (Vertex1 = Vertex2) or (Vertex1 = Vertex3) or
        (Vertex2 = Vertex3) or not Result
      then
        exit;

      V1 := Vertices[Vertex1];
      V2 := Vertices[Vertex2];
      V3 := Vertices[Vertex3];

      //Check to make sure they are in reasonable positions..

      //then what??
    end;}
  begin
    Result := False;
  end;

  Function FillRectMesh(Mesh : tagGradientRect) : Boolean;
  var
    TL,BR : tagTRIVERTEX;
    StartColor, EndColor : TColorRef;
    I, Swap : Longint;
    SwapColors : Boolean;
    UseBrush : hBrush;
    Steps, MaxSteps : Longint;
  begin
    With Mesh do begin
      Result := (UpperLeft < NumVertices) and (UpperLeft >= 0) and
        (LowerRight < NumVertices) and (LowerRight >= 0);
      If (LowerRight = UpperLeft) or not Result then
        exit;
      TL := Vertices[UpperLeft];
      BR := Vertices[LowerRight];
      SwapColors := (BR.Y < TL.Y) and (BR.X < TL.X);
      If BR.X < TL.X then begin
        Swap := BR.X;
        BR.X := TL.X;
        TL.X := Swap;
      end;
      If BR.Y < TL.Y then begin
        Swap := BR.Y;
        BR.Y := TL.Y;
        TL.Y := Swap;
      end;
      StartColor := RGB(TL.Red, TL.Green, TL.Blue);
      EndColor := RGB(BR.Red, BR.Green, BR.Blue);
      If SwapColors then begin
        Swap := StartColor;
        StartColor := EndColor;
        EndColor := Swap;
      end;
      UseBrush := 0;
      MaxSteps := GetDeviceCaps(DC, BITSPIXEL);
      If MaxSteps >= 4 then
        MaxSteps := Floor(Power(2, MaxSteps))
      else
        MaxSteps := 256;
      If DoFillVRect then begin
        Steps := Min(BR.Y - TL.Y, MaxSteps);
        for I := 0 to Steps - 1 do begin
          GetGradientBrush(StartColor, EndColor, I, Steps, UseBrush);
            LCLIntf.FillRect(DC, Rect(TL.X, TL.Y + I, BR.X, TL.Y + I + 1),
              UseBrush)
        end
      end
      else begin
        Steps := Min(BR.X - TL.X, MaxSteps);
        for I := 0 to  Steps - 1 do begin
          GetGradientBrush(StartColor, EndColor, I, Steps, UseBrush);
          LCLIntf.FillRect(DC, Rect(TL.X + I, TL.Y, TL.X + I + 1, BR.Y),
            UseBrush);
        end;
      end;
      If UseBrush <> 0 then
        LCLIntf.DeleteObject(UseBrush);
    end;
  end;

const
  MeshSize : Array[Boolean] of Integer = (SizeOf(tagGradientRect), SizeOf(tagGradientTriangle));
var
  I : Integer;
begin
  //Currently Alpha blending is ignored... Ideas anyone?
  Result := (Meshes <> nil) and (NumMeshes >= 1) and (NumVertices >= 2) and (Vertices <> nil);
  If Result and DoFillTriangle then
    Result := NumVertices >= 3;
  If Result then begin
    Result := False;

    //Sanity Checks For Vertices Size vs. Count
    If MemSize(Vertices) < SizeOf(tagTRIVERTEX)*NumVertices then
      exit;

    //Sanity Checks For Meshes Size vs. Count
    If MemSize(Meshes) < MeshSize[DoFillTriangle]*NumMeshes then
      exit;

    For I := 0 to NumMeshes - 1 do begin
      If DoFillTriangle then begin
        If Not FillTriMesh(PGradientTriangle(Meshes)[I]) then
          exit;
      end
      else begin
        If not FillRectMesh(PGradientRect(Meshes)[I]) then
          exit;
      end;
    end;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: HideCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.HideCaret(hWnd: HWND): Boolean;
var
  GTKObject: PGTKObject;
  WasVisible: boolean;
begin
//DebugLn('[TGtkWidgetSet.HideCaret] A');
  Assert(False, Format('Trace:  [TGtkWidgetSet.HideCaret] HWND: 0x%x', [hWnd]));
//TODO: [TGtkWidgetSet.HideCaret] Finish (in gtkwinapi.inc)

  GTKObject := PGTKObject(HWND);
  Result := GTKObject <> nil;

  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_HideCaret(PGTKAPIWidget(GTKObject),WasVisible);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else DebugLn('WARNING: [TGtkWidgetSet.HideCaret] Got null HWND');

end;

{------------------------------------------------------------------------------
  Function: IntersectClipRect
  Params:  dc: hdc; Left, Top, Right, Bottom: Integer
  Returns: Integer

  Shrinks the clipping region in the device context dc to a region of all
  intersecting points between the boundary defined by Left, Top, Right,
  Bottom , and the Current clipping region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Inherited IntersectClipRect(DC, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.InvalidateRect(aHandle : HWND; Rect : pRect;
  bErase : Boolean) : Boolean;
var
  gdkRect : TGDKRectangle;
  Widget, PaintWidget: PGtkWidget;
  LCLObject: TObject;
begin
  //  DebugLn(format('Rect = %d,%d,%d,%d',[rect^.left,rect^.top,rect^.Right,rect^.Bottom]));
  Widget:=PGtkWidget(aHandle);
  LCLObject:=GetLCLObject(Widget);
  if (LCLObject<>nil) then begin
    if (LCLObject=CurrentSentPaintMessageTarget) then begin
      DebugLn('NOTE: TGtkWidgetSet.InvalidateRect during paint message: ',
        LCLObject.ClassName);
      //RaiseException('Double paint');
    end;
    {$IFDEF VerboseDsgnPaintMsg}
    if (LCLObject is TComponent)
    and (csDesigning in TComponent(LCLObject).ComponentState) then begin
      write('TGtkWidgetSet.InvalidateRect A ');
      write(TComponent(LCLObject).Name,':');
      write(LCLObject.ClassName);
      with Rect^ do
        write(' Rect=',Left,',',Top,',',Right,',',Bottom);
      DebugLn(' Erase=',bErase);
    end;
    {$ENDIF}
  end;
  Result := True;
  gdkRect.X := Rect^.Left;
  gdkRect.Y := Rect^.Top;
  gdkRect.Width := (Rect^.Right - Rect^.Left);
  gdkRect.Height := (Rect^.Bottom - Rect^.Top);

  PaintWidget:=GetFixedWidget(Widget);
  if PaintWidget=nil then PaintWidget:=Widget;

  {$IfDef GTK2}
    if (PaintWidget<>nil) and GTK_WIDGET_NO_WINDOW(PaintWidget)
      and (not GtkWidgetIsA(PGTKWidget(PaintWidget),GTKAPIWidget_GetType))
    then begin
      Inc(gdkRect.X, PaintWidget^.Allocation.x);
      Inc(gdkRect.Y, PaintWidget^.Allocation.y);
    end;
  {$EndIf}

  if bErase then
    gtk_widget_queue_clear_area(PaintWidget,
                              gdkRect.X,gdkRect.Y,gdkRect.Width,gdkRect.Height);

  gtk_widget_queue_draw_area(PaintWidget,
                             gdkRect.X,gdkRect.Y,gdkRect.Width,gdkRect.Height);

end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.IsWindowVisible(handle: HWND): boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  Result:=(handle<>0) and GTK_WIDGET_VISIBLE(PGtkWidget(handle));
end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TGtkWidgetSet.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC <> nil then begin
      SelectGDKPenProps(DC);

      If (dcfPenSelected in DCFlags) then begin
        Result := True;
        if (CurrentPen^.IsNullPen) then exit;

        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
        gdk_draw_line(Drawable, GC, PenPos.X+DCOrigin.X, PenPos.Y+DCOrigin.Y,
                      X+DCOrigin.X, Y+DCOrigin.Y);
        {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
        PenPos:= Point(X, Y);
      end else
        Result := False;
    end else begin
      DebugLn('WARNING: [TGtkWidgetSet.LineTo] Uninitialized GC');
      Result := False;
    end;
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

{------------------------------------------------------------------------------
  Function: MessageBox
  Params:  hWnd:                  The handle of parent window
  Returns: 0 if not successful (out of memory), otherwise one of the defined value :
      IDABORT, IDCANCEL, IDIGNORE, IDNO, IDOK, IDRETRY, IDYES

 The MessageBox function displays a modal dialog, with text and caption defined,
 and includes buttons.
 ------------------------------------------------------------------------------}

function MessageButtonClicked(Widget : PGtkWidget; data: gPointer) : GBoolean; cdecl;
begin
DebugLn('[MessageButtonClicked] ',dbgs(data),' ',dbgs(gtk_object_get_data(PGtkObject(Widget), 'modal_result')));
  if Integer(data^) = 0 then
    Integer(data^):= Integer(gtk_object_get_data(PGtkObject(Widget), 'modal_result'));
  Result:=false;
end;

function MessageBoxClosed(Widget : PGtkWidget; Event : PGdkEvent; data: gPointer) : GBoolean; cdecl;
var ModalResult : integer;
begin
  { We were requested by window manager to close }
  if Integer(data^) = 0 then begin
    ModalResult:= Integer(gtk_object_get_data(PGtkObject(Widget), 'modal_result'));
    { Don't allow to close if we don't have a default return value }
    Result:= (ModalResult = 0);
    if not Result then Integer(data^):= ModalResult
    else DebugLn('Do not close !!!');
  end else Result:= false;
end;

function TGtkWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType : Cardinal): integer;
var Dialog, ALabel : PGtkWidget;
    ButtonCount, DefButton, ADialogResult : Integer;
    DialogType : Cardinal;

    procedure CreateButton(const ALabel : PChar; const RetValue : integer);
    var AButton : PGtkWidget;
    begin
      AButton:= gtk_button_new_with_label(ALabel);
      Inc(ButtonCount);
      if ButtonCount = DefButton then begin
        gtk_window_set_focus(PGtkWindow(Dialog), AButton);
      end;
      { If there is the Cancel button, allow the dialog to close }
      if RetValue = IDCANCEL then begin
        gtk_object_set_data(PGtkObject(Dialog), 'modal_result', Pointer(IDCANCEL));
      end;
      gtk_object_set_data(PGtkObject(AButton), 'modal_result', Pointer(RetValue));
      g_signal_connect(PGtkObject(AButton), 'clicked', TGtkSignalFunc(@MessageButtonClicked), @ADialogResult);
      gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.action_area), AButton);
    end;

begin
  ButtonCount:= 0;
  { Determine which is the default button }
  DefButton:= ((uType and $00000300) shr 8) + 1;
  Assert(False, 'Trace:Default button is ' + IntToStr(DefButton));

  ADialogResult:= 0;
  Dialog:= gtk_dialog_new;
  g_signal_connect(PGtkObject(Dialog), 'delete-event', TGtkSignalFunc(@MessageBoxClosed), @ADialogResult);
  gtk_window_set_default_size(PGtkWindow(Dialog), 100, 100);
  ALabel:= gtk_label_new(lpText);
  gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.vbox), ALabel);
  DialogType:= (uType and $0000000F);
  if DialogType = MB_OKCANCEL
  then begin
    CreateButton(PChar(rsMbOK), IDOK);
    CreateButton(PChar(rsMbCancel), IDCANCEL);
  end
  else begin
    if DialogType = MB_ABORTRETRYIGNORE
    then begin
      CreateButton(PChar(rsMbAbort), IDABORT);
      CreateButton(PChar(rsMbRetry), IDRETRY);
      CreateButton(PChar(rsMbIgnore), IDIGNORE);
    end
    else begin
      if DialogType = MB_YESNOCANCEL
      then begin
        CreateButton(PChar(rsMbYes), IDYES);
        CreateButton(PChar(rsMbNo), IDNO);
        CreateButton(PChar(rsMbCancel), IDCANCEL);
      end
      else begin
        if DialogType = MB_YESNO
        then begin
          CreateButton(PChar(rsMbYes), IDYES);
          CreateButton(PChar(rsMbNo), IDNO);
        end
        else begin
          if DialogType = MB_RETRYCANCEL
          then begin
            CreateButton(PChar(rsMbRetry), IDRETRY);
            CreateButton(PChar(rsMbCancel), IDCANCEL);
          end
          else begin
            { We have no buttons to show. Create the default of OK button }
            CreateButton(PChar(rsMbOK), IDOK);
          end;
        end;
      end;
    end;
  end;
  gtk_window_set_title(PGtkWindow(Dialog), lpCaption);
  gtk_window_set_position(PGtkWindow(Dialog), GTK_WIN_POS_CENTER);
  gtk_window_set_modal(PGtkWindow(Dialog), true);
  gtk_widget_show_all(Dialog);
  while ADialogResult = 0 do begin
    Application.HandleMessage;
  end;
  DestroyConnectedWidget(Dialog,true);
  Result:= ADialogResult;
end;

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Assert(False, Format('trace:> [TGtkWidgetSet.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if OldPoint <> nil then OldPoint^ := PenPos;
    PenPos := Point(X, Y);
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

{------------------------------------------------------------------------------
  function MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean; override;

  Move the origin of all operations of a DeviceContext.
  For example:
  Moving the Origin to 10,20 and drawing a point to 50,50, results in
  drawing a point to 60,70.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
begin
  Result:=IsValidDC(DC);
  if Result then
  with TDeviceContext(DC) do begin
    //DebugLn('[TGtkWidgetSet.MoveWindowOrgEx] B DC=',HexStr(Cardinal(DC),8),
    //  ' Old=',Origin.X,',',Origin.Y,' d=',dX,',',dY,' ');
    inc(Origin.X,dX);
    inc(Origin.Y,dY);
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
    Side: integer): Boolean;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=false;
  if (SplitterHandle=0) or (SideHandle=0) or (Side<0) or (Side>1) then exit;
  if Side=0 then
    gtk_paned_add1(PGtkPaned(SplitterHandle),PGtkWidget(SideHandle))
  else
    gtk_paned_add2(PGtkPaned(SplitterHandle),PGtkWidget(SideHandle));
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.PairSplitterGetInterfaceInfo: Boolean;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PairSplitterGetInterfaceInfo: Boolean;
begin
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd;
    Side: integer): Boolean;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=false;
  DebugLn('WARNING: TGtkWidgetSet.PairSplitterRemoveSide not implemented');
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.PairSplitterSetPosition(SplitterHandle: hWnd;
    var NewPosition: integer): Boolean;

  Negative values for NewPosition will only read the value
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PairSplitterSetPosition(SplitterHandle: hWnd;
  var NewPosition: integer): Boolean;
begin
  Result:=false;
  if (SplitterHandle=0) then exit;
  if NewPosition>=0 then
    gtk_paned_set_position(PGtkPaned(SplitterHandle),NewPosition);
  NewPosition:=PGtkPaned(SplitterHandle)^.child1_size;
  Result:=true;
end;


{------------------------------------------------------------------------------
  Function: PeekMessage
  Params:  lpMsg        - Where it should put the message
           Handle       - Handle of the window (thread)
           wMsgFilterMin- Lowest MSG to grab
           wMsgFilterMax- Highest MSG to grab
           wRemoveMsg   - Should message be pulled out of the queue

  Returns: Boolean if an event was there
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PeekMessage(var lpMsg: TMsg; Handle : HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): Boolean;
var
  vlItem : TGtkMessageQueueItem;
begin
  //TODO Filtering
  DebugLn('Peek !!!' );
  vlItem := fMessageQueue.FirstMessageItem;
  Result := vlItem <> nil;

  if Result  then begin
    lpMsg := vlItem.Msg^;
    if (wRemoveMsg and PM_REMOVE) = PM_REMOVE then
      fMessageQueue.RemoveMessage(vlItem,FPMF_Internal,true);
  end;
end;

{------------------------------------------------------------------------------
  Method:  PolyBezier
  Params:  DC, Points, NumPts, Filled, Continous
  Returns: Boolean

  Use Polybezier to draw cubic Bézier curves. The first curve is drawn from the
  first point to the fourth point with the second and third points being the
  control points. If the Continuous flag is TRUE then each subsequent curve
  requires three more points, using the end-point of the previous Curve as its
  starting point, the first and second points being used as its control points,
  and the third point its end-point. If the continous flag is set to FALSE,
  then each subsequent Curve requires 4 additional points, which are used
  excatly as in the first curve. Any additonal points which do not add up to
  a full bezier(4 for Continuous, 3 otherwise) are ingored. There must be at
  least 4 points for an drawing to occur. If the Filled Flag is set to TRUE
  then the resulting Poly-Bézier will be drawn as a Polygon.

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
Begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.PolyBezier] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
  end;
End;

{------------------------------------------------------------------------------
  Method:   TGtkWidgetSet.Polygon
  Params:   DC: HDC; Points: ^TPoint; NumPts: integer; Winding: Boolean;
  Returns:  Nothing

  Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Pen. After drawing the complete shape, Polygon fills the shape using the
  value of Brush.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled. When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  NumPts indicates the number of points to use.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end.
}
function TGtkWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: Boolean): boolean;
var
  i: integer;
  PointArray: PGDKPoint;
  Tmp, RGN : hRGN;
  ClipRect : TRect;
  DCOrigin: TPoint;
  OldNumPts: integer;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if NumPts<=0 then exit;
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.Polygon] Uninitialized GC');
      Result := False;
    end
    else begin
      DCOrigin:=GetDCOffset(TDeviceContext(DC));

      // create the PointsArray, which is a copy of Points moved by the DCOrigin
      GetMem(PointArray,SizeOf(TGdkPoint)*(NumPts+1)); // +1 for return line
      for i:=0 to NumPts-1 do begin
        PointArray[i].x:=Points[i].x;
        PointArray[i].y:=Points[i].y;
        Inc(PointArray[i].x, DCOrigin.X);
        Inc(PointArray[i].y, DCOrigin.Y);
      end;

      OldNumPts:=NumPts;
      If (Points[NumPts-1].X <> Points[0].X) or
        (Points[NumPts-1].Y <> Points[0].Y)
      then begin
        // add last point to return to first
        PointArray[NumPts].x:=PointArray[0].x;
        PointArray[NumPts].y:=PointArray[0].y;
        Inc(NumPts);
      end;

      // first draw interior in brush color
      SelectGDKBrushProps(DC);

      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

      If not CurrentBrush^.IsNullBrush then
        if Winding then begin
          // store old clipping
          Tmp := CreateEmptyRegion;
          GetClipRGN(DC, Tmp);
          // apply new clipping
          RGN := CreatePolygonRgn(Points, OldNumPts, LCLType.Winding);
          ExtSelectClipRGN(DC, RGN, RGN_AND);
          DeleteObject(RGN);
          GetClipBox(DC, @ClipRect);
          // draw polygon area
          FillRect(DC, ClipRect, HBrush(CurrentBrush));
          // restore old clipping
          SelectClipRGN(DC, Tmp);
          DeleteObject(Tmp);
        end else
          gdk_draw_polygon(Drawable, GC, 1, PointArray, NumPts);

      // draw outline

      SelectGDKPenProps(DC);

      If (dcfPenSelected in DCFlags) then begin
        Result := True;
        if (not CurrentPen^.IsNullPen) then begin
          gdk_draw_polygon(Drawable, GC, 0, PointArray, NumPts);
        end;
      end else
        Result:=false;

      {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}

      FreeMem(PointArray);

      Result := True;
    end;
  end;
end;

function TGtkWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var i: integer;
  PointArray: PGDKPoint;
  DCOrigin: TPoint;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.Polyline] Uninitialized GC');
      Result := False;
    end
    else begin
      if NumPts<=0 then exit;
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      GetMem(PointArray,SizeOf(TGdkPoint)*NumPts);
      for i:=0 to NumPts-1 do begin
        PointArray[i].x:=Points[i].x+DCOrigin.X;
        PointArray[i].y:=Points[i].y+DCOrigin.Y;
      end;

      // draw outline
      SelectGDKPenProps(DC);

      If (dcfPenSelected in DCFlags) then begin
        Result := True;
        if (not CurrentPen^.IsNullPen) then begin
          {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
          gdk_draw_lines(Drawable, GC, PointArray, NumPts);
          {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
        end;
      end else
        Result:=false;

      FreeMem(PointArray);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: PostMessage
  Params: Handle:
          Msg:
          wParam:
          lParam:
  Returns: True if succesful

  The PostMessage function places (posts) a message in the message queue and
  then returns without waiting.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; wParam: WParam;
  lParam: LParam): Boolean;

  function ParentPaintMessageInQueue: boolean;
  var
    Target: TControl;
    Parent: TWinControl;
    ParentHandle: hWnd;
  begin
    Result:=false;
    Target:=TControl(GetLCLObject(Pointer(Handle)));
    if not (Target is TControl) then exit;
    Parent:=Target.Parent;
    if (Target is TControl) then begin
      Parent:=Target.Parent;
      while Parent<>nil do begin
        ParentHandle:=Parent.Handle;
        if fMessageQueue.FindPaintMessage(ParentHandle)<>nil then begin
          Result:=true;
        end;
        Parent:=Parent.Parent;
      end;
    end;
  end;

  procedure CombinePaintMessages(NewMsg:PMsg);
  // combine NewMsg and OldMsg paint message into NewMsg and free OldMsg
  var
    vlItem : TGtkMessageQueueItem;
    NewData: TLMGtkPaintData;
    OldData: TLMGtkPaintData;
    OldMsg : PMsg;
  begin
    vlItem := fMessageQueue.FindPaintMessage(NewMsg^.Hwnd);
    if vlItem = nil then exit;
    OldMsg := vlItem.Msg;
    if OldMsg=nil then exit;
    if (NewMsg^.Message=LM_PAINT) or (OldMsg^.Message=LM_PAINT) then begin
      // LM_PAINT means: repaint all
      // convert NewMsg into a LM_PAINT if not already done
      if NewMsg^.Message<>LM_PAINT then begin
        FinalizePaintTagMsg(NewMsg);
        NewMsg^.Message:=LM_PAINT;
      end;
    end else if (NewMsg^.Message<>LM_GtkPAINT) then begin
      RaiseException('CombinePaintMessages A unknown paint message');
    end else if (OldMsg^.Message<>LM_GtkPAINT) then begin
      RaiseException('CombinePaintMessages B unknown paint message');
    end else begin
      // combine the two LM_GtkPAINT messages
      NewData:=TLMGtkPaintData(NewMsg^.WParam);
      OldData:=TLMGtkPaintData(OldMsg^.WParam);
      NewData.RepaintAll:=NewData.RepaintAll or OldData.RepaintAll;
      if not NewData.RepaintAll then begin
        NewData.Rect.Left:=Min(NewData.Rect.Left,OldData.Rect.Left);
        NewData.Rect.Top:=Min(NewData.Rect.Top,OldData.Rect.Top);
        NewData.Rect.Right:=Max(NewData.Rect.Right,OldData.Rect.Right);
        NewData.Rect.Bottom:=Max(NewData.Rect.Bottom,OldData.Rect.Bottom);
      end;
    end;
    fMessageQueue.RemoveMessage(vlItem,FPMF_All,true);
  end;

var
  AMessage: PMsg;
begin
  Result := True;

  New(AMessage);
  AMessage^.HWnd := Handle; // this is normally the main gtk widget
  AMessage^.Message := Msg;
  AMessage^.WParam := WParam;
  AMessage^.LParam := LParam;
//  Message^.Time :=

  if (AMessage^.Message=LM_PAINT) or (AMessage^.Message=LM_GtkPAINT) then begin
    { Obsolete, because InvalidateRectangle now works.

    // paint messages are the most expensive messages in the LCL
    // A paint message to a control will also repaint all child controls.
    // -> check if there is already a paint message for one of its parents
    // if yes, then skip this message
    if ParentPaintMessageInQueue then begin
      FinalizePaintTagMsg(AMessage^);
      exit;
    end;}

    // delete old paint message to this widget,
    // so that the widget repaints only once

    CombinePaintMessages(AMessage);
  end ;

  FMessageQueue.AddMessage(AMessage);
end;

{------------------------------------------------------------------------------
  Method:   RadialArc
  Params:   DC,x,y,width,height,sx,sy,ex,ey
  Returns:  Nothing

  Use RadialArc to draw an elliptically curved line with the current Pen. The
  values sx,sy, and ex,ey represent the starting and ending radial-points
  between which the Arc is drawn.

------------------------------------------------------------------------------}
function TGtkWidgetSet.RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
Begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.RadialArc] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited RadialArc(DC, x, y, width, height, sx,sy,ex,ey);
  end;
End;

{------------------------------------------------------------------------------
  Method:   RadialChord
  Params:   DC,x,y,width,height,sx,sy,ex,ey
  Returns:  Nothing

  Use RadialChord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent the starting and ending radial-points between which
  the bounding-Arc is drawn.

------------------------------------------------------------------------------}
function TGtkWidgetSet.RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.RadialChord] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited RadialChord(DC, x, y, width, height, sx,sy,ex,ey);
  end;
End;

{------------------------------------------------------------------------------
  Method:   RadialPie
  Params:   DC,x,y,width,height,sx,sy,ex,ey
  Returns:  Nothing

  Use RadialPie to draw a filled Pie-shaped Wedge on the canvas. The values
  sx,sy, and ex,ey represent the starting and ending radial-points between which
  the bounding-Arc is drawn.

------------------------------------------------------------------------------}
function TGtkWidgetSet.RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.RadialPie] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited RadialPie(DC, x, y, width, height, sx,sy,ex,ey);
  end;
end;

{------------------------------------------------------------------------------
  Function: RadioMenuItemGroup
  Params:  hndMenu: HMENU; bRadio: Boolean
  Returns: Nothing

  Change the group of menuitems to 'radio' or to 'checked'.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean;
var
  LCLMenuItem: TMenuItem;
begin
  LCLMenuItem:=TMenuItem(GetLCLObject(Pointer(hndMenu)));
  if LCLMenuItem<>nil then begin
    LCLMenuItem.RecreateHandle;
    Result:=true;
  end else
    Result := false;
end;

{------------------------------------------------------------------------------
  Function: RealizePalette
  Params:  DC: HDC
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  Assert(False, 'Trace:FINISH: [TGtkWidgetSet.RealizePalette]');
  Result := 0;
  if IsValidDC(DC)
  then with TDeviceContext(DC) do
  begin

  end;
end;

{------------------------------------------------------------------------------
  Function: Rectangle
  Params:  DC: HDC; X1, Y1, X2, Y2: Integer
  Returns: Nothing

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  Left, Top, Width, Height: Integer;
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TGtkWidgetSet.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.Rectangle] Uninitialized GC');
      Result := False;
    end
    else begin
      CalculateLeftTopWidthHeight(X1,Y1,X2,Y2,Left,Top,Width,Height);
      // first draw interior in brush color
      SelectGDKBrushProps(DC);
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
      If not CurrentBrush^.IsNullBrush then
        if (CurrentBrush^.GDIBrushFill = GDK_SOLID)
        and (IsBackgroundColor(TColor(CurrentBrush^.GDIBrushColor.ColorRef))) then
          StyleFillRectangle(Drawable, GC, CurrentBrush^.GDIBrushColor.ColorRef, Left+DCOrigin.X, Top+DCOrigin.Y, Width, Height)
        else
          gdk_draw_rectangle(Drawable, GC, 1, Left+DCOrigin.X, Top+DCOrigin.Y,
                           Width, Height);

      // Draw outline
      SelectGDKPenProps(DC);

      If (dcfPenSelected in DCFlags) then begin
        Result := True;
        if (not CurrentPen^.IsNullPen) then
          gdk_draw_rectangle(Drawable, GC, 0, Left+DCOrigin.X, Top+DCOrigin.Y,
                             Width, Height);
      end else
        Result:=false;

      {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
    end;
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
end;

{------------------------------------------------------------------------------
  Function: RectVisible
  Params:  dc : hdc; ARect: TRect
  Returns: True if ARect is not completely clipped away.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
begin
  Result := inherited RectVisible(dc,ARect);
end;

{------------------------------------------------------------------------------
  Function: RegroupMenuItem
  Params:  hndMenu: HMENU; GroupIndex: integer
  Returns: Nothing

  Move a menuitem into its group
  This function is called by the LCL, after some menuitems were regrouped to
  GroupIndex. The hndMenu is one of them.
  Update all radio groups.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RegroupMenuItem(hndMenu: HMENU;
  GroupIndex: Integer): Boolean;

const
  GROUPIDX_DATANAME = 'GroupIndex';

  function GetGroup: PGSList;
  {$IfDef GTK1}
  var
    Item: PGList;
    Arg: TGTKArg;
  begin
    Result := nil;
    Arg.theType := GTK_TYPE_OBJECT;
    Arg.Name := 'parent';
    gtk_widget_get(Pointer(hndMenu), @Arg);
    if Arg.d.object_data = nil then Exit;

    Item := gtk_container_children(PGTKContainer(Arg.d.object_data));
    while Item <> nil do
    begin
      if (Item^.Data <> Pointer(hndMenu)) // exclude ourself
      and gtk_is_radio_menu_item(Item^.Data)
      and (GroupIndex = Integer(gtk_object_get_data(Item^.Data, GROUPIDX_DATANAME)))
      then begin
        Result := gtk_radio_menu_item_group(PGtkRadioMenuItem(Item^.Data));
        Exit;
      end;
      Item := Item^.Next;
    end;
  {$Else}
  var
    Item: PGList;
    parent : PGTKWidget;
  begin
    Result := nil;
    parent := gtk_widget_get_parent(Pointer(hndMenu));
    if parent = nil then Exit;

    Item := gtk_container_children(PGTKContainer(parent));
    while Item <> nil do
    begin
      if (Item^.Data <> Pointer(hndMenu)) // exclude ourself
      and gtk_is_radio_menu_item(Item^.Data)
      and (GroupIndex = Integer(gtk_object_get_data(Item^.Data, GROUPIDX_DATANAME)))
      then begin
        Result := gtk_radio_menu_item_get_group (PGtkRadioMenuItem(Item^.Data));
        Exit;
      end;
      Item := Item^.Next;
    end;
  {$EndIf}
  end;

var
  RadioGroup: PGSList;
  CurrentGroupIndex: Integer;
begin
  Result := False;

  if not gtk_is_radio_menu_item(Pointer(hndMenu))
  then begin
    DebugLn('WARNING: TGtkWidgetSet.RegroupMenuItem: handle is not a GTK_RADIO_MENU_ITEM');
    Exit;
  end;

  CurrentGroupIndex := Integer(gtk_object_get_data(Pointer(hndMenu), GROUPIDX_DATANAME));

  // Update needed ?
  if GroupIndex = CurrentGroupIndex
  then begin
    Result := True;
    Exit;
  end;

  // Remove current group
  gtk_radio_menu_item_set_group(PGtkRadioMenuItem(hndMenu), nil);
  gtk_object_set_data(Pointer(hndMenu), GROUPIDX_DATANAME, nil);

  // Check remove only
  if GroupIndex = 0
  then begin
    Result := True;
    Exit;
  end;

  // Try to find new group
  RadioGroup := GetGroup;

  // Set new group
  gtk_object_set_data(Pointer(hndMenu), GROUPIDX_DATANAME, Pointer(GroupIndex));
  if RadioGroup = nil
  then begin
    // We're the only member, get a group
    RadioGroup := gtk_radio_menu_item_group(PGtkRadioMenuItem(hndMenu))
  end
  else begin
    gtk_radio_menu_item_set_group(PGtkRadioMenuItem(hndMenu), RadioGroup);
  end;
                                                                    //radiogroup^.data
                                                                    //radiogroup^.next
  // Refetch newgroup list
  RadioGroup := gtk_radio_menu_item_group(PGtkRadioMenuItem(hndMenu));
  // Update checks
  UpdateRadioGroupChecks(RadioGroup);
  Result := True;
end;

// MWE: Reimplemented to get rid of unneeded group order constraint
//      (which doesn't work if the menu isn't created in order)
(*
  function GetGroup(ParentMenuItem: TMenuItem;
    GrpIndex, LastRadioItem: integer): PGSList;
  var
    i: Integer;
  begin
    for i:=LastRadioItem downto 0 do begin
      if ParentMenuItem[i].RadioItem
      and (ParentMenuItem[i].GroupIndex=GrpIndex)
      and ParentMenuItem[i].HandleAllocated
      and GtkWidgetIsA(Pointer(ParentMenuItem[i].Handle),
                       GTK_RADIO_MENU_ITEM_TYPE)
      then begin
        Result:=gtk_radio_menu_item_group(
                                 GTK_RADIO_MENU_ITEM(ParentMenuItem[i].Handle));
        //DebugLn('TGtkWidgetSet.RegroupMenuItem.GetGroup A i=',i,' ',ParentMenuItem[i].Name,' GrpIndex=',ParentMenuItem[i].GroupIndex,' LastRadioItem=',LastRadioItem,' Result=',HexStr(Cardinal(Result),8));
        exit;
      end;
    end;
    Result:=nil;
  end;

var
  RadioGroup: PGSList;
  AMenuItem: TMenuItem;
  ParentMenuItem: TMenuItem;
  LastRadioGroupStart: integer;
  i: Integer;
begin
  if GTK_IS_RADIO_MENU_ITEM(Pointer(hndMenu)) then begin
    AMenuItem:=TMenuItem(GetLCLObject(Pointer(hndMenu)));
    if AMenuItem=nil then exit;
    ParentMenuItem:=AMenuItem.Parent;
    if ParentMenuItem=nil then exit;
    //DebugLn('TGtkWidgetSet.RegroupMenuItem A ',AMenuItem.Name,' ',ParentMenuItem.Name,' GroupIndex=',AMenuItem.GroupIndex);
    LastRadioGroupStart:=-1;
    for i:=0 to ParentMenuItem.Count-1 do begin
      if ParentMenuItem[i].RadioItem
      and ParentMenuItem[i].HandleAllocated
      and GtkWidgetIsA(Pointer(ParentMenuItem[i].Handle),
                       GTK_RADIO_MENU_ITEM_TYPE)
      then begin
        //DebugLn('TGtkWidgetSet.RegroupMenuItem B i=',i,' ',ParentMenuItem[i].Name,
        //' GrpIndex=',ParentMenuItem[i].GroupIndex,
        //' LastRadioGroupStart=',LastRadioGroupStart,
        //' LastGroup=',HexStr(Cardinal(gtk_radio_menu_item_group(
        //                                 GTK_RADIO_MENU_ITEM(ParentMenuItem[i].Handle))),8)
        //);
        if (ParentMenuItem[i].GroupIndex<>0) then begin
          // item has a group -> bind to group
          RadioGroup:=GetGroup(ParentMenuItem,ParentMenuItem[i].GroupIndex,
                               LastRadioGroupStart);
          gtk_radio_menu_item_set_group(
            PGtkRadioMenuItem(ParentMenuItem[i].Handle),RadioGroup);
          if (LastRadioGroupStart<0)
          or (ParentMenuItem[LastRadioGroupStart].GroupIndex
              <>ParentMenuItem[i].GroupIndex)
          then
            LastRadioGroupStart:=i;
        end else begin
          // item has no group -> unbind
          if gtk_radio_menu_item_group(
                            GTK_RADIO_MENU_ITEM(ParentMenuItem[i].Handle))
            <>nil
          then
            gtk_radio_menu_item_set_group(
              PGtkRadioMenuItem(ParentMenuItem[i].Handle),nil);
        end;
      end;
    end;
    // update checks
    RadioGroup:=gtk_radio_menu_item_group(PGtkRadioMenuItem(hndMenu));
    UpdateRadioGroupChecks(RadioGroup);
    Result:=true;
  end else begin
    DebugLn('WARNING: TGtkWidgetSet.RegroupMenuItem: handle is not a GTK_RADIO_MENU_ITEM');
    Result:=false;
  end;
end;
*)

{------------------------------------------------------------------------------
  Function: ReleaseCapture
  Params:  none
  Returns: True if succesful

  The ReleaseCapture function releases the mouse capture from a window
  and restores normal mouse input processing.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ReleaseCapture: Boolean;
begin
  SetCapture(0);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: ReleaseDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
var
  aDC, pSavedDC: TDeviceContext;
begin
//DebugLn('[TGtkWidgetSet.ReleaseDC] ',HexStr(DC,8),'  ',FDeviceContexts.Count);
  Assert(False, Format('trace:> [TGtkWidgetSet.ReleaseDC] DC:0x%x', [DC]));
  Result := 0;

  if {(hWnd <> 0) and} (DC <> 0)
  then begin
    if FDeviceContexts.Contains(Pointer(DC))
    then begin
      aDC := TDeviceContext(DC);
      { Release all saved device contexts }
      pSavedDC:=aDC.SavedContext;
      if pSavedDC<>nil then begin
        if pSavedDC.CurrentBitmap = aDC.CurrentBitmap
        then
          aDC.CurrentBitmap := nil;
        if pSavedDC.CurrentFont = aDC.CurrentFont
        then
          aDC.CurrentFont := nil;
        if (pSavedDC.CurrentPen = aDC.CurrentPen)
        and (aDC.CurrentPen<>nil)
        then
          aDC.CurrentPen := nil;
        if pSavedDC.CurrentBrush = aDC.CurrentBrush
        then
          aDC.CurrentBrush := nil;
        {if pSavedDC.CurrentPalette = aDC.CurrentPalette
        then aDC.CurrentPalette := nil;}
        if pSavedDC.ClipRegion = aDC.ClipRegion
        then
          pSavedDC.ClipRegion := 0;
        ReleaseDC(0,HDC(pSavedDC));
        aDC.SavedContext:=nil;
      end;
      
      // Release all graphic objects
      DeleteObject(HGDIObj(aDC.CurrentBrush));
      DeleteObject(HGDIObj(aDC.CurrentPen));
      DeleteObject(HGDIObj(aDC.CurrentFont));
      // bitmaps are not auto created, they are set via SelectObject
      // -> user must free it
      // ... DeleteObject(HGDIObj(aDC.CurrentBitmap));
      //DeleteObject(HGDIObj(aDC.CurrentPalette));
      DeleteObject(HGDIObj(aDC.ClipRegion));
      {FreeGDIColor(aDC.CurrentTextColor);
      FreeGDIColor(aDC.CurrentBackColor);}

      try
        { On root window, we don't allocate a graphics context and so we dont free}
        if aDC.GC <> nil then begin
          gdk_gc_unref(aDC.GC);
          aDC.GC:=nil;
        end;
      except
        on E:Exception do begin
          //Nothing, just try to unref it
          //(it segfaults if the window doesnt exist anymore :-)
          DebugLn('TGtkWidgetSet.ReleaseDC: ',E.Message);
        end;
      end;

      DisposeDC(aDC);
      Result := 1;
    end;
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.ReleaseDC] FDeviceContexts DC:0x%x', [DC]));
end;

{------------------------------------------------------------------------------
  Function: RestoreDC
  Params:  none
  Returns: Nothing


-------------------------------------------------------------------------------}
function TGtkWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  aDC, pSavedDC: TDeviceContext;
  Count: Integer;
  ClipRegionChanged: Boolean;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.RestoreDC] DC:0x%x, SavedDC: %d', [DC, SavedDC]));
  Result := IsValidDC(DC) and (SavedDC <> 0);
  if Result
  then begin
    pSavedDC := TDeviceContext(DC);
    Count:=Abs(SavedDC);
    while (Count>0) and (pSavedDC<>nil) do begin
      aDC:=pSavedDC;
      pSavedDC:=aDC.SavedContext;
      dec(Count);
    end;

    // TODO copy bitmap also

    ClipRegionChanged:=false;
    if (aDC.ClipRegion<>0) and (pSavedDC.ClipRegion <> aDC.ClipRegion) then
    begin
      // clipping region has changed
      DeleteObject(aDC.ClipRegion);
      ClipRegionChanged:=true;
      aDC.ClipRegion := 0;
    end;

    if aDC.GC<>nil then begin
      gdk_gc_unref(aDC.GC);
      aDC.GC:=nil;
    end;

    Result := CopyDCData(aDC, pSavedDC);
    aDC.SavedContext := pSavedDC.SavedContext;
    pSavedDC.SavedContext := nil;
    if ClipRegionChanged then
      SelectGDIRegion(HDC(aDC));
    //DebugLn('TGtkWidgetSet.RestoreDC A ',GDKRegionAsString(PGdiObject(aDC.ClipRegion)^.GDIRegionObject));


    // free saved DC

    //prevent deleting of copied objects:
    if pSavedDC.CurrentBitmap = aDC.CurrentBitmap
    then
      pSavedDC.CurrentBitmap := nil;
    if pSavedDC.CurrentFont = aDC.CurrentFont
    then
      pSavedDC.CurrentFont := nil;
    if (pSavedDC.CurrentPen = aDC.CurrentPen)
    and (pSavedDC.CurrentPen<>nil) then
      pSavedDC.CurrentPen := nil;
    if pSavedDC.CurrentBrush = aDC.CurrentBrush
    then
      pSavedDC.CurrentBrush := nil;
    if pSavedDC.CurrentBrush = aDC.CurrentBrush
    then
      pSavedDC.CurrentBrush := nil;
    {if pSavedDC.CurrentPalette = aDC.CurrentPalette
    then pSavedDC.CurrentPalette := nil;}
    if pSavedDC.ClipRegion = aDC.ClipRegion
    then
      pSavedDC.ClipRegion := 0;

    DeleteDC(HGDIOBJ(pSavedDC));
  end;
  Assert(False, Format('Trace:< [TGtkWidgetSet.RestoreDC] DC:0x%x, Saved: %d --> %s', [Integer(DC), SavedDC, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Function: RightJustifyMenuItem
  Params:  HndMenu: HMenu; bRightJustify: boolean
  Returns: true on success

  Sets left or justification of a menuitem
-------------------------------------------------------------------------------}
function TGtkWidgetSet.RightJustifyMenuItem(HndMenu: HMenu;
  bRightJustify: boolean): Boolean;
var
  MenuItemWidget: PGtkMenuItem;
begin
  MenuItemWidget:=PGtkMenuItem(HndMenu);
  gtk_menu_item_set_right_justified(MenuItemWidget, bRightJustify);
  gtk_widget_queue_resize(GTK_WIDGET(MenuItemWidget));
  Result:=false;
end;

{------------------------------------------------------------------------------
  Method:   RoundRect
  Params:   X1, Y1, X2, Y2, RX, RY
  Returns:  If succesfull

  Draws a Rectangle with optional rounded corners. RY is the radial height
  of the corner arcs, RX is the radial width. If either is less than or equal to
  0, the routine simly calls to standard Rectangle.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Assert(False, Format('trace:> [TGtkWidgetSet.RoundRect] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d, RX:%d, RY:%d', [DC, X1, Y1, X2, Y2, RX, RY]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.RoundRect] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.RoundRect] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d, RX:%d, RY:%d', [DC, X1, Y1, X2, Y2, RX, RY]));
end;

{------------------------------------------------------------------------------
  Function: SaveDc
  Params:  DC: a DC to save
  Returns: 0 if the functions fails otherwise a positive integer identifing
           the saved DC

  The SaveDC function saves the current state of the specified device
  context (DC) by copying its elements to a context stack.
-------------------------------------------------------------------------------}
function TGtkWidgetSet.SaveDC(DC: HDC): Integer;
var
  aDC, aSavedDC: TDeviceContext;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.SaveDC]  0x%x', [Integer(DC)]));

  Result := 0;
  if IsValidDC(DC)
  then begin
    aDC := TDeviceContext(DC);
    aSavedDC := NewDC;
    CopyDCData(aSavedDC, aDC);
    aSavedDC.SavedContext:=aDC.SavedContext;
    aDC.SavedContext:= aSavedDC;
    Result:=1;
  end;

  Assert(False, Format('Trace:< [TGtkWidgetSet.SaveDC]  0x%x --> %d', [Integer(DC), Result]));
end;

{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params: Handle:
          P:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
var
  X, Y: Integer;
  Widget: PGTKWidget;
  Window: PgdkWindow;
Begin

  if Handle = 0
  then begin
    X := 0;
    Y := 0;
  end
  else
   begin
    Widget := GetFixedWidget(pgtkwidget(Handle));
    if Widget = nil then
      Widget := pgtkwidget(Handle);
    if Widget = nil then
    begin
      X := 0;
      Y := 0;
    end
    else begin
      Window:=GetControlWindow(Widget);
      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
      if Window<>nil then
        gdk_window_get_origin(Window, @X, @Y)
      else begin
        X:=0;
        Y:=0;
      end;
      {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
    end;
  end;

  //DebugLn('[TGtkWidgetSet.ScreenToClient] ',x,',',y,' P=',P.X,',',P.Y);
  dec(P.X, X);
  dec(P.Y, Y);
  Result := -1;
end;

{------------------------------------------------------------------------------
  Function: ScrollWindowEx
  Params:  hWnd:       handle of window to scroll
           dx:         horizontal amount to scroll
           dy:         vertical amount to scroll
           prcScroll:  pointer to scroll rectangle
           prcClip:    pointer to clip rectangle
           hrgnUpdate: handle of update region
           prcUpdate:  pointer to update rectangle
           flags:      scrolling flags

  Returns: True if succesfull;

  The ScrollWindowEx function scrolls the content of the specified window's
  client area
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: SelectClipRGN
  Params:  DC, RGN
  Returns: longint

  Sets the DeviceContext's ClipRegion. The Return value
  is the new clip regions type, or ERROR.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
var
  RegObj: PGdkRegion;
  DCOrigin: TPoint;
begin
  If not IsValidDC(DC) then begin
    Result := ERROR;
    exit;
  end;
  Result := SIMPLEREGION;
  with TDeviceContext(DC) do
  begin
    if (GC = nil) and (RGN <> 0)
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.SelectClipRGN] Uninitialized GC');
      Result := ERROR;
    end
    else begin
      // clear old clipregion
      if (ClipRegion<>0)
      and ((SavedContext=nil) or (SavedContext.ClipRegion<>ClipRegion)) then
        DeleteObject(ClipRegion);
      ClipRegion := 0;

      If (GC = nil) or (RGN = 0) then begin
        if GC<>nil then
          SelectGDIRegion(DC);
      end
      else
        If IsValidGDIObject(RGN) then begin
          ClipRegion := CreateRegionCopy(RGN);
          RegObj:=PGdiObject(ClipRegion)^.GDIRegionObject;
          DCOrigin:=GetDCOffset(TDeviceContext(DC));
          //DebugLn('TGtkWidgetSet.SelectClipRGN A RegObj=',GDKRegionAsString(RegObj),' DCOrigin=',dbgs(DCOrigin),' RGN=',GDKRegionAsString(PGdiObject(RGN)^.GDIRegionObject));
          gdk_region_offset(RegObj,DCOrigin.x,DCOrigin.Y);
          Result := RegionType(RegObj);
          //DebugLn('TGtkWidgetSet.SelectClipRGN B RegObj=',GDKRegionAsString(RegObj),' DCOrigin=',dbgs(DCOrigin));
          SelectGDIRegion(DC);
        end
        else begin
          Result := ERROR;
          DebugLn('WARNING: [TGtkWidgetSet.SelectClipRGN] Invalid RGN');
        end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;

  procedure RaiseInvalidGDIType;
  begin
    RaiseException('TGtkWidgetSet.SelectObject Invalid GDIType '+IntToStr(ord(PGdiObject(GDIObj)^.GDIType)));
  end;

begin
  Result := 0;

  {if not IsValidDC(DC) then begin
    DebugLn('TGtkWidgetSet.SelectObject invalid DC ',HexStr(Cardinal(DC),8));
  end;
  if not IsValidGDIObject(GDIObj) then begin
    DebugLn('TGtkWidgetSet.SelectObject invalid GDIObj ',HexStr(Cardinal(GDIObj),8));
  end;}

  if IsValidDC(DC) and IsValidGDIObject(GDIObj)
  then begin
    //DebugLn('TGtkWidgetSet.SelectObject DC=',HexStr(Cardinal(DC),8),' GDIObj=',HexStr(Cardinal(GDIObj),8),' GDIType=',ord(PGdiObject(GDIObj)^.GDIType),' ',ord(gdiBitmap),' ',ord(gdiRegion));
    case PGdiObject(GDIObj)^.GDIType of

      gdiBitmap:
        with TDeviceContext(DC) do
        begin
          Assert(False, Format('trace:  [TGtkWidgetSet.SelectObject] DC: 0x%x, Type: Bitmap', [DC]));
          Result := HBITMAP(CurrentBitmap);
          CurrentBitmap := PGDIObject(GDIObj);
          if GC <> nil then begin
            gdk_gc_unref(GC);
            GC:=nil;
          end;
          with CurrentBitmap^ do
            case GDIBitmapType of
              gbPixmap: Drawable := GDIPixmapObject;
              gbBitmap: Drawable := GDIBitmapObject;
              {obsolete: gbImage:  Drawable := nil;//GDI_RGBImageObject;}
            else
              Drawable := nil;
            end;
          //DebugLn('TGtkWidgetSet.SelectObject DC=',HexStr(Cardinal(DC),8),' GDIBitmap=',HexStr(Cardinal(CurrentBitmap),8),
          //' GDIBitmapType=',ord(CurrentBitmap^.GDIBitmapType),' Drawable=',HexStr(Cardinal(Drawable),8));

          GC := gdk_gc_new(Drawable);

          gdk_gc_set_function(GC, GDK_COPY);
          SelectedColors := dcscCustom;
        end;

      gdiBrush:
        with TDeviceContext(DC), PGdiObject(GDIObj)^ do
        begin
          Assert(False, Format('trace:  [TGtkWidgetSet.SelectObject] DC: 0x%x, Type: Brush', [DC]));
          Result := HBRUSH(CurrentBrush);
          CurrentBrush := PGDIObject(GDIObj);
          if GC <> nil
          then begin
            gdk_gc_set_fill(GC, GDIBrushFill);
            case GDIBrushFill of
              GDK_STIPPLED: gdk_gc_set_stipple(GC, GDIBrushPixMap);
              GDK_TILED:    gdk_gc_set_tile(GC, GDIBrushPixMap);
            end;
          end;
          SelectedColors := dcscCustom;
        end;

      gdiFont:
        with TDeviceContext(DC) do
        begin
          Assert(False, Format('trace:  [TGtkWidgetSet.SelectObject] DC: 0x%x, Type: Font', [DC]));
          Result := HFONT(CurrentFont);
          CurrentFont := PGDIObject(GDIObj);
          {$IfDef GTK1}
          if GC <> nil then begin
            gdk_gc_set_font(GC, PGdiObject(GDIObj)^.GDIFontObject);
          end;
          {$ENDIF}
          Exclude(DCFlags,dcfTextMetricsValid);
          SelectedColors := dcscCustom;
        end;

      gdiPen:
        with TDeviceContext(DC) do
        begin
          Result := HPEN(CurrentPen);
          CurrentPen := PGDIObject(GDIObj);
          DCFlags:=DCFlags-[dcfPenSelected];
          if GC <> nil then SelectGDKPenProps(DC);
          SelectedColors := dcscCustom;
        end;

      gdiRegion:
      begin
        with TDeviceContext(DC) do
        begin
          Result := ClipRegion;
          if GC <> nil then
            SelectClipRGN(DC, GDIObj)
          else
            ClipRegion:=0;
        end;
      end;
      
    else
      RaiseInvalidGDIType;
    end;
  end;
//DebugLn('[TGtkWidgetSet.SelectObject] GDI=',HexStr(Cardinal(GDIObj),8)
// ,'  Old=',Hexstr(Cardinal(Result),8));
end;

{------------------------------------------------------------------------------
  Function: SelectPalette
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Assert(False, 'Trace:TODO: [TGtkWidgetSet.SelectPalette]');
  //TODO: Implement this;
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: SendMessage
  Params: hWnd:
          Msg:
          wParam:
          lParam:
  Returns:

  The SendMessage function sends the specified message to a window or windows.
  The function calls the window procedure for the specified window and does
  not return until the window procedure has processed the message.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: WParam;
  lParam: LParam): LResult;
var
  OldMsg: Cardinal;
  
  procedure PreparePaintMessage(TargetObject: TObject; var AMessage: TLMessage);
  var
    PaintDC: HDC;
    GtkPaintData: TLMGtkPaintData;
    DCOrigin: TPoint;
    OldGtkPaintMsg: TLMGtkPaint;
  begin
    (* MG: old trick. Not used anymore, but it might be, that someday there
           will be component, that works better with this, so it is kept.
    { The LCL repaints controls in a top-down hierachy. But the gtk sends
      gtkdraw events bottom-up. So, controls at the bottom are repainted
      many times. To avoid this the queue is checked for LM_PAINT messages
      for the parent control. If there is a parent LM_PAINT, this message
      is ignored.}
    if (Target is TControl) then begin
      ParentControl:=TControl(Target).Parent;
      while ParentControl<>nil do begin
        ParentHandle:=TWinControl(ParentControl).Handle;
        if FindPaintMessage(ParentHandle)<>nil then begin
          {$IFDEF VerboseDsgnPaintMsg}
          if (csDesigning in TComponent(Target).ComponentState) then begin
            DebugLn('TGtkWidgetSet.SendMessage A ',
              TComponent(Target).Name,':',Target.ClassName,
              ' Parent Message found: ',ParentControl.Name,':',ParentControl.ClassName
              );
          end;
          {$ENDIF}
          if Msg=LM_PAINT then
            ReleaseDC(0,AMessage.WParam);
          //exit;
        end;
        ParentControl:=ParentControl.Parent;
      end;
    end; *)
    {$IFDEF VerboseDsgnPaintMsg}
    if (csDesigning in TComponent(TargetObject).ComponentState) then begin
      write('TGtkWidgetSet.SendMessage B ',
        TComponent(TargetObject).Name,':',TargetObject.ClassName,
        ' GtkPaint=',AMessage.Msg=LM_GtkPAINT);
      if AMessage.Msg=LM_GtkPAINT then begin
        if AMessage.wParam<>0 then begin
          with TLMGtkPaintData(AMessage.wParam) do begin
            write(' GtkPaintData(',
              ' Widget=',HexStr(Cardinal(Widget),8),'=',GetWidgetClassName(Widget),
              ' State=',State,
              ' Rect=',Rect.Left,',',Rect.Top,',',Rect.Right,',',Rect.Bottom,
              ' RepaintAll=',RepaintAll,
              ')');
          end;
        end else begin
          write(' GtkPaintData=nil');
        end;
      end;
      DebugLn('');
    end;
    {$ENDIF}
    if AMessage.Msg=LM_GtkPAINT then begin
      OldGtkPaintMsg:=TLMGtkPaint(AMessage);
      GtkPaintData:=OldGtkPaintMsg.Data;
      // convert LM_GtkPAINT to LM_PAINT
      AMessage := TLMessage(GtkPaintMessageToPaintMessage(
                                                TLMGtkPaint(AMessage), False));
      {$IfNDef GTK2}
      if (GtkPaintData<>nil) and (not GtkPaintData.RepaintAll) then begin
        PaintDC:=TLMPaint(AMessage).DC;
        DCOrigin:=GetDCOffset(TDeviceContext(PaintDC));
        with GtkPaintData.Rect do
          IntersectClipRect(PaintDC,Left-DCOrigin.X,Top-DCOrigin.Y,
                            Right-DCOrigin.X,Bottom-DCOrigin.Y);
      end;
      {$EndIf}
      GtkPaintData.Free;
    end;
  end;
  
  procedure DisposePaintMessage(TargetObject: TObject; var AMessage: TLMessage);
  begin
    if OldMsg=LM_GtkPAINT then begin
      FinalizePaintMessage(@AMessage);
      
      //if (csDesigning in TComponent(TargetObject).ComponentState)
      //and (TargetObject is TWinControl) then
      //  SendPaintMessagesForInternalWidgets(TWinControl(TargetObject));
    end else
    if ((AMessage.Msg=LM_PAINT) or (AMessage.Msg=LM_INTERNALPAINT))
    and (AMessage.WParam<>0) then begin
      // free DC
      ReleaseDC(0,AMessage.WParam);
      AMessage.WParam:=0;

      //if (csDesigning in TComponent(TargetObject).ComponentState)
      //and (TargetObject is TWinControl) then
      //  SendPaintMessagesForInternalWidgets(TWinControl(TargetObject));
    end;
  end;
  
var
  AMessage: TLMessage;
  Target: TObject;
begin
  OldMsg:=Msg;

  AMessage.Msg := Msg;
  AMessage.WParam := WParam;
  AMessage.LParam := LParam;
  AMessage.Result := 0;

  Target := GetLCLObject(Pointer(HandleWnd));

  if Target<>nil then begin
    if (Msg=LM_PAINT) or (Msg=LM_GtkPaint) then begin
      PreparePaintMessage(Target,AMessage);
    end;

    // deliver it
    Result := DeliverMessage(Target, AMessage);
    
    if (Msg=LM_PAINT) or (Msg=LM_INTERNALPAINT) or (Msg=LM_GtkPaint) then begin
      DisposePaintMessage(Target,AMessage);
    end;
  end;
end;

{------------------------------------------------------------------------------
  function SetActiveWindow(Handle: HWND): HWND;


------------------------------------------------------------------------------}
function TGtkWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  // ToDo
  Result:=GetActiveWindow;
end;

{------------------------------------------------------------------------------
  Function: SetBkColor        pbd
  Params:  DC:    Device context to change the text background color
           Color: RGB Tuple
  Returns: Old Background color


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Assert(False, Format('trace:> [TGtkWidgetSet.SetBKColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := CLR_INVALID;
  if IsValidDC(DC)
  then begin
    with TDeviceContext(DC) do
    begin
      Result := CurrentBackColor.ColorRef;
      SetGDIColorRef(CurrentBackColor,Color);
    end;

  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.SetBKColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Function: SetBkMode
  Params: DC:
          bkMode:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function TGtkWidgetSet.SetComboMinDropDownSize(Handle: HWND;
    MinItemsWidth, MinItemsHeight: integer): boolean;

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
var
  ComboWidget: PGtkCombo;
  DropDownWidget, ListWidget, FirstChildWidget: PGtkWidget;
  FirstChild: PGList;
  CurX, CurY, CurWidth, CurHeight, CurItemHeight, BorderX, BorderY,
  NewWidth, NewHeight: integer;
  ComboPopup: PGtkScrolledWindow;
  item_requisition: TGtkRequisition;
begin
  Result:=true;
  if not (GtkWidgetIsA(PgtkWidget(Handle),GTK_TYPE_COMBO)) then
    RaiseException('TGtkWidgetSet.SetComboMinDropDownSize invalid handle');

  // get current items width and height
  ComboWidget:=PGtkCombo(Handle);
  ListWidget:=ComboWidget^.List;
  if ListWidget=nil then exit;
  CurWidth:=ListWidget^.Allocation.Width;
  CurHeight:=ListWidget^.Allocation.Height;
  if MinItemCount>0 then begin
    FirstChild:=PGTkList(ListWidget)^.children;
    if FirstChild<>nil then begin
      FirstChildWidget:=PGtkWidget(FirstChild^.Data);
      gtk_widget_size_request(FirstChildWidget,@item_requisition);
      CurItemHeight:=Max(FirstChildWidget^.Allocation.Height,
                         item_requisition.Height);
      if MinItemsHeight<CurItemHeight*MinItemCount then
        MinItemsHeight:=CurItemHeight*MinItemCount;
    end;
  end;

  // calculate new width and height
  DropDownWidget:=ComboWidget^.popwin;
  if DropDownWidget=nil then exit;
  CurX:=DropDownWidget^.Allocation.x;
  CurY:=DropDownWidget^.Allocation.y;
  ComboPopup:=PGtkScrolledWindow(ComboWidget^.popup);
  if ComboPopup=nil then exit;
  // ToDo: add scrollbars only if needed
  BorderX:=DropDownWidget^.Allocation.Width-CurWidth;
  if BorderX<0 then BorderX:=0;
  inc(BorderX,
    ComboPopup^.hscrollbar^.requisition.height
    {+GTK_SCROLLED_WINDOW_GET_CLASS(ComboWidget^.popup)^.scrollbar_spacing});
  BorderY:=DropDownWidget^.Allocation.Height-CurHeight;
  if BorderY<0 then BorderY:=0;
  inc(BorderX,
    ComboPopup^.vscrollbar^.requisition.width
    {+GTK_SCROLLED_WINDOW_GET_CLASS(ComboWidget^.popup)^.scrollbar_spacing});
  NewWidth := MinItemsWidth+BorderX;
  NewHeight := MinItemsHeight+BorderY;

  if NewWidth<CurWidth then NewWidth:=CurWidth;
  if NewHeight<CurHeight then NewHeight:=CurHeight;
  //DebugLn('NewWidth=',NewWidth,' NewHeight=',NewHeight,' CurWidth=',CurWidth,' CurHeight=',CurHeight);
  if (NewWidth=CurWidth) and (NewHeight=CurHeight) then exit;

  //gtk_widget_set_uposition(DropDownWidget,NewX,NewY);
  NewWidth:=Min(NewWidth, Screen.Width - CurX);
  NewHeight:=Min(NewHeight, Screen.Height - CurY);
  gtk_widget_set_usize(DropDownWidget,NewWidth,NewHeight);
end;

{------------------------------------------------------------------------------
  Function: SetCapture
  Params:  Value: Handle of window to capture
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetCapture(Value: Longint): Longint;
var
  Widget: PGtkWidget;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.SetCapture] 0x%x', [Value]));
  Widget:=PGtkWidget(Value);
  {$IfDef VerboseMouseCapture}
  DebugLn('TGtkWidgetSet.SetCapture NewValue=[',GetWidgetDebugReport(Widget),']');
  {$EndIf}

  // return old capture handle
  Result := GetCapture;
  
  // capture
  CaptureMouseForWidget(Widget,mctLCL);
end;

{------------------------------------------------------------------------------
  Function: SetCaretPos
  Params:  new position x, y
  Returns: true on success

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
var
  FocusObject: PGTKObject;
begin
  FocusObject := PGTKObject(GetFocus);
  Result:=SetCaretPosEx(LongInt(FocusObject),X,Y);
end;

{------------------------------------------------------------------------------
  Function: SetCaretPos
  Params:  new position x, y
  Returns: true on success

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetCaretPosEx(Handle: HWNd; X, Y: Integer): Boolean;
var
  GtkObject: PGTKObject;
begin
  GtkObject := PGTKObject(Handle);
  Result := GtkObject <> nil;

  if Result then begin
    if gtk_type_is_a(gtk_object_type(GtkObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_SetCaretPos(PGTKAPIWidget(GtkObject), X, Y);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: SetCaretRespondToFocus
  Params:  handle : Handle of a TWinControl
           ShowHideOnFocus: true = caret is hidden on focus lost
  Returns: true on success

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  if handle<>0 then begin
    if gtk_type_is_a(gtk_object_type(PGTKObject(handle)), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_SetCaretRespondToFocus(PGTKAPIWidget(handle),
        ShowHideOnFocus);
      Result:=true;
    end
    else begin
      Result := False;
    end;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------------
  Function: SetFocus
  Params:  hWnd: Handle of new focus window
  Returns: The old focus window

  The SetFocus function sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetFocus(hWnd: HWND): HWND;
var
  Widget, TopLevel, ImplWidget, NewFocusWidget: PGtkWidget;
  WinWidgetInfo: PWinWidgetInfo;
  {$IfDef VerboseFocus}
  LCLObject, AWinControl: TWinControl;
  {$EndIf}
begin
  if hWnd=0 then exit;
  Widget:=PGtkWidget(hWnd);
  {$IfDef VerboseFocus}
  DebugLn('');
  write('[TGtkWidgetSet.SetFocus] A hWnd=',GetWidgetDebugReport(Widget));
  LCLObject:=TWinControl(GetLCLObject(Widget));
  {$EndIf}
  if hwnd = 0 then begin
    Result:=0;
    exit;
  end;

  // return the old focus handle
  Result := GetFocus;
  NewFocusWidget:=nil;

  TopLevel := gtk_widget_get_toplevel(Widget);
  {$IfDef VerboseFocus}
  Debugln('[TGtkWidgetSet.SetFocus] B');
  DbgOut('   TopLevel=',HexStr(Cardinal(TopLevel),8));
  DbgOut(' OldFocus=',GetWidgetDebugReport(PGtkWidget(Result)));
  DebugLn('');
  if not GTK_WIDGET_VISIBLE(Widget) then
    RaiseException('TGtkWidgetSet.SetFocus: Widget is not visible');
  {$EndIf}
  
  if Result=hWnd then exit;

  if GtkWidgetIsA(TopLevel, gtk_window_get_type)
  then begin
    // TopLevel is a gtkwindow
    {$IfDef VerboseFocus}
    AWinControl:=TWinControl(GetNearestLCLObject(PGtkWindow(TopLevel)^.focus_widget));
    write('  C  TopLevel is a gtkwindow ');
    write(' focus_widget=',HexStr(Cardinal(PGtkWindow(TopLevel)^.focus_widget),8));
    if AWinControl<>nil then
      write(' LCLParent=',AWinControl.Name,':',AWinControl.ClassName)
    else
      write(' LCLParent=nil');
    DebugLn('');
    {$EndIf}
    if (NewFocusWidget=nil)
    and GtkWidgetIsA(Widget, gtk_combo_get_type) then begin
      // handle is a gtk combo
      {$IfDef VerboseFocus}
      DebugLn('  D taking gtkcombo entry');
      {$EndIf}
      NewFocusWidget:=PgtkWidget(PGtkCombo(Widget)^.entry);
    end;
    if NewFocusWidget=nil then begin
      // check if widget has a WinWidgetInfo record
      WinWidgetInfo:=GetWidgetInfo(Widget, false);
      if (WinWidgetInfo<>nil) then begin
        ImplWidget:= WinWidgetInfo^.CoreWidget;
        if ImplWidget <> nil then begin
          // handle has a ImplementationWidget
          {$IfDef VerboseFocus}
          DebugLn('  E taking ImplementationWidget');
          {$EndIf}
          NewFocusWidget:=ImplWidget;
        end;
      end;
    end;
    if (NewFocusWidget=nil) then begin
      NewFocusWidget:=Widget;
      {$IfDef VerboseFocus}
      DebugLn('  F taking default ');
      {$EndIf}
    end;
    {$IfDef VerboseFocus}
    write('  G NewFocusWidget=',HexStr(Cardinal(NewFocusWidget),8));
    write(' WidVisible=',GTK_WIDGET_VISIBLE(PGtkWidget(NewFocusWidget)));
    write(' WidRealized=',GTK_WIDGET_REALIZED(PGtkWidget(NewFocusWidget)));
    write(' WidMapped=',GTK_WIDGET_MAPPED(PGtkWidget(NewFocusWidget)));
    write(' WidCanfocus=',GTK_WIDGET_CAN_FOCUS(PGtkWidget(NewFocusWidget)));
    write(' TopLvlVisible=',GTK_WIDGET_VISIBLE(PGtkWidget(TopLevel)));
    DebugLn('');
    {$EndIf}
    if (NewFocusWidget<>nil) and GTK_WIDGET_CAN_FOCUS(NewFocusWidget) then begin
      if (PGtkWindow(TopLevel)^.Focus_Widget<>NewFocusWidget)
      then begin
        {$IfDef VerboseFocus}
        DebugLn('  H SETTING NewFocusWidget=',HexStr(Cardinal(NewFocusWidget),8));
        {$EndIf}
        gtk_window_set_focus(PGtkWindow(TopLevel),NewFocusWidget);
        {$IfDef VerboseFocus}
        DebugLn('  I NewTopLevel FocusWidget=',HexStr(Cardinal(PGtkWindow(TopLevel)^.Focus_Widget),8),' Success=',dbgs(PGtkWindow(TopLevel)^.Focus_Widget=NewFocusWidget));
        {$EndIf}
      end;
    end;
  end
  else begin
    NewFocusWidget:=Widget;
  end;

  if (NewFocusWidget <> nil) and not gtk_widget_has_focus(NewFocusWidget) then begin
    // grab the focus to the parent window
    if (Screen<>nil)
    and (Screen.FocusedForm<>nil)
    and (fsModal in Screen.FocusedForm.FormState)
    and (GetNearestLCLObject(TopLevel)<>Screen.FocusedForm) then begin
      {$IFDEF VerboseFocus}
      DebugLn('[TGtkWidgetSet.SetFocus] there is a modal form -> not grabbing');
      {$ENDIF}
    end else begin
      {$IfDef VerboseFocus}
      DebugLn('  J Grabbing focus');
      {$EndIf}
      gtk_widget_grab_focus(NewFocusWidget);
    end;
  end;

  {$IfDef VerboseFocus}
  write('[TGtkWidgetSet.SetFocus] END hWnd=',HexStr(Cardinal(hWnd),8));
  NewFocusWidget:=PGtkWidget(GetFocus);
  write(' NewFocus=',HexStr(Cardinal(NewFocusWidget),8));
  AWinControl:=TWinControl(GetNearestLCLObject(NewFocusWidget));
  if AWinControl<>nil then
    write(' NewLCLParent=',AWinControl.Name,':',AWinControl.ClassName)
  else
    write(' NewLCLParent=nil');
  DebugLn('');
  {$EndIf}
end;

{------------------------------------------------------------------------------
  Function TGtkWidgetSet.SetProp(Handle: hwnd; Str : PChar;
                              Data : Pointer) : Boolean;
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
Begin
  gtk_object_set_data(pGTKObject(handle),Str,data);
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function TGtkWidgetSet.SetROPMode(Handle: hwnd; Str : PChar;
                              Data : Pointer) : Boolean;
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SetROP2(DC: HDC; Mode: Integer) : Integer;
Begin
  if IsValidDC(DC) then with TDeviceContext(DC) do begin
    if GC=nil then begin
      Assert(False, 'Trace:[TGtkWidgetSet.SetROP2] Uninitialized GC');
      result := 0
    end else begin
      Result := GetROP2(DC);
      gdk_gc_set_function(GC, ROP2ModeToGdkFunction(Mode));
    end;
  end else begin
    Assert(False, 'Trace:[TGtkWidgetSet.SetROP2] Invalid GC');
    Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: SetScrollInfo
  Params:  none
  Returns: The old position value


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;

  procedure SetRangeUpdatePolicy(Range: PGtkRange);
  var
    UpdPolicy: TGTKUpdateType;
  begin
    case ScrollInfo.nTrackPos of
      SB_POLICY_DISCONTINUOUS:	UpdPolicy := GTK_UPDATE_DISCONTINUOUS;
      SB_POLICY_DELAYED:	UpdPolicy := GTK_UPDATE_DELAYED;
      else 			UpdPolicy := GTK_UPDATE_CONTINUOUS;
    end;
    gtk_range_set_update_policy(Range, UpdPolicy);
  end;
  procedure SetScrolledWindowUpdatePolicy(ScrolledWindow:PGTKScrolledWindow);
  var
    Range: PGtkRange;
  begin
    case SBStyle of
      SB_VERT: Range := PGtkRange(ScrolledWindow^.vscrollbar);
      SB_HORZ: Range := PGtkRange(ScrolledWindow^.hscrollbar);
      else exit;
    end;
    SetRangeUpdatePolicy(Range);
  end;
  
const
  POLICY: array[BOOLEAN] of TGTKPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
var
  Adjustment: PGtkAdjustment;
  Scroll: PGTKWidget;
  NewPolicy: Integer;
  i: Integer;
begin
  Result := 0;
  if (Handle = 0) then exit;

  //DebugLn('TGtkWidgetSet.SetScrollInfo A Widget=',GetWidgetClassName(PGtkWidget(Handle)));

  Adjustment := nil;
  Scroll := GTK_Object_Get_Data(PGTKObject(Handle), odnScrollArea);
  If not GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
    Scroll := PGTKWidget(Handle);

  // scrollbar update policy
  if (Scrollinfo.fmask and SIF_UPDATEPOLICY <> 0) then begin
    if GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
      SetScrolledWindowUpdatePolicy(PGTKScrolledWindow(Scroll))
    else if GtkWidgetIsA(PgtkWidget(Scroll), gtk_clist_get_type) then
      SetScrolledWindowUpdatePolicy(PGTKScrolledWindow(@PgtkCList(Scroll)^.container))
    else if GtkWidgetIsA(PGtkWidget(Scroll),gtk_hscrollbar_get_type) then
      SetRangeUpdatePolicy(PgtkRange(Scroll))
    else if GtkWidgetIsA(PGtkWidget(Scroll),gtk_vscrollbar_get_type) then
      SetRangeUpdatePolicy(PgtkRange(Scroll))
    else if GtkWidgetIsA(PGtkWidget(Scroll), gtk_range_get_type) then
      SetRangeUpdatePolicy(PGTKRange(Scroll));
  end;


  Adjustment:=nil;
  case SBStyle of
    SB_HORZ:
      If GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
        Adjustment := gtk_scrolled_window_get_hadjustment(
                         PGTKScrolledWindow(Scroll))
      else
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_hscrollbar_get_type) then
        Adjustment :=  PgtkhScrollBar(Scroll)^.Scrollbar.Range.Adjustment
      else  //clist
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_clist_get_type) then
        Adjustment := gtk_clist_get_hadjustment(PgtkCList(Scroll));

    SB_VERT:
      If GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
        Adjustment := gtk_scrolled_window_get_vadjustment(
                         PGTKScrolledWindow(Scroll))
      else
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_vscrollbar_get_type) then
        Adjustment :=  PgtkvScrollBar(Scroll)^.Scrollbar.Range.Adjustment
      else  //clist
      if GtkWidgetIsA(PGtkWidget(Scroll), gtk_clist_get_type) then
        Adjustment := gtk_clist_get_vadjustment(PgtkCList(Scroll));

    SB_CTL:
      if GtkWidgetIsA(PGtkWidget(Scroll), gtk_range_get_type) then
        Adjustment := gtk_range_get_adjustment(PGTKRange(Scroll));

  end;

  if Adjustment = nil then exit;

  with ScrollInfo, Adjustment^ do begin
    //DebugLn('SetScrollInfo Value=',Value);
    // workaround for strange floating point bug
    for i:=0 to 2 do begin
      try
        Result := RoundToInt(Value);
        break;
      except
        on e: Exception do begin
          DebugLn('TGtkWidgetSet.SetScrollInfo Workaround for ',E.Message,' try: ',dbgs(i));
          Result:=0;
        end;
      end;
    end;
    //DebugLn('SetScrollInfo Result=',Result);
    if (fMask and SIF_POS) <> 0
    then Value := nPos;
    if (fMask and SIF_RANGE) <> 0
    then begin
      Lower := nMin;
      Upper := nMax;
    end;
    if (fMask and SIF_PAGE) <> 0
    then begin
      Page_Size := nPage;
      Page_Increment := nPage;
    end;

    {DebugLn('');
    DebugLn('[TGtkWidgetSet.SetScrollInfo] Result=',Result,
    ' Lower=',RoundToInt(Lower),
    ' Upper=',RoundToInt(Upper),
    ' Page_Size=',RoundToInt(Page_Size),
    ' Page_Increment=',RoundToInt(Page_Increment),
    ' bRedraw=',bRedraw,
    ' Handle=',HexStr(Cardinal(Handle),8));}

    // do we have to set this always ?
    if bRedraw then
    begin
      if GtkWidgetIsA(PGtkWidget(Scroll),gtk_scrolled_window_get_type) then
      begin
        if SBStyle in [SB_BOTH, SB_HORZ] then begin
          NewPolicy:=POLICY[bRedraw];
          gtk_object_set(PGTKObject(Scroll),'hscrollbar_policy',[NewPolicy,nil]);
        end;
        if SBStyle in [SB_BOTH, SB_VERT] then begin
          NewPolicy:=POLICY[bRedraw];
          gtk_object_set(PGTKObject(Scroll),'vscrollbar_policy',[NewPolicy,nil]);
        end;
      end
      else
      begin
        if (SBSTYLE = SB_CTL)
        and GtkWidgetIsA(PGtkWidget(Scroll),gtk_widget_get_type) then
          gtk_widget_show(PGTKWidget(Scroll))
        else
          gtk_widget_hide(PGTKWidget(Scroll))
      end;
      {DebugLn('');
      DebugLn('TGtkWidgetSet.SetScrollInfo: ',
      ' lower=',RoundToInt(lower),'/',nMin,
      ' upper=',RoundToInt(upper),'/',nMax,
      ' value=',RoundToInt(value),'/',nPos,
      ' step_increment=',RoundToInt(step_increment),'/',1,
      ' page_increment=',RoundToInt(page_increment),'/',nPage,
      ' page_size=',RoundToInt(page_size),'/',nPage,
      '');}

      gtk_adjustment_changed(Adjustment);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: SetSysColors
  Params:  cElements: the number of elements
           lpaElements: array with element numbers
           lpaRgbValues: array with colors
  Returns: 0 if unsuccesful

  The SetSysColors function sets the colors for one or more display elements.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetSysColors(cElements: Integer; const lpaElements;
  const lpaRgbValues): Boolean;
type
  TLongArray = array[0..0] of Longint;
  PLongArray = ^TLongArray;
var
  n: Integer;
  Element: LongInt;
begin
  Result := False;
  if cElements > MAX_SYS_COLORS then Exit;

  for n := 0 to cElements - 1 do
  begin
    Element := PLongArray(lpaElements)^[n];
    if (Element > MAX_SYS_COLORS)
    or (Element < 0)
    then Exit;
    SysColorMap[PLongArray(lpaElements)^[n]] := PLongArray(lpaRgbValues)^[n];
    //Assert(False, Format('Trace:[TGtkWidgetSet.SetSysColor] Index %d (%8x) --> %8x', [PLongArray(lpaElements)^[n], SysColorMap[PLongArray(lpaElements)^[n]], PLongArray(lpaRgbValues)^[n]]));
  end;

  //TODO send WM_SYSCOLORCHANGE
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SetTextCharacterExtra
  Params: _hdc:
          nCharExtra:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SetTextColor
  Params:  hdc: Identifies the device context.
           Color: Specifies the color of the text.
  Returns: The previous color if succesful, CLR_INVALID otherwise

  The SetTextColor function sets the text color for the specified device
  context to the specified color.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Assert(False, Format('trace:> [TGtkWidgetSet.SetTextColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := CLR_INVALID;
  if IsValidDC(DC)
  then begin
    with TDeviceContext(DC) do
    begin
      Result := CurrentTextColor.ColorRef;
      SetGDIColorRef(CurrentTextColor,Color);
    end;
  end;
  Assert(False, Format('trace:< [TGtkWidgetSet.SetTextColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Procedure: SetWindowLong
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetWindowLong(Handle: HWND; Idx: Integer;
  NewLong: Longint): LongInt;
begin
  //TODO: Finish this;
  Assert(False, Format('Trace:> [TGtkWidgetSet.SETWINDOWLONG] HWND: 0x%x, idx: 0x%x(%d), Value: 0x%x(%d)', [Handle, idx, idx, newlong, newlong]));
  Result:=0;

  case idx of
    GWL_WNDPROC :
      begin
        gtk_object_set_data(pgtkobject(Handle),'WNDPROC',pointer(NewLong));
      end;
    GWL_HINSTANCE :
      begin
        gtk_object_set_data(pgtkobject(Handle),'HINSTANCE',pointer(NewLong));
      end;
    GWL_HWNDPARENT :
      begin
        gtk_object_set_data(pgtkobject(Handle),'HWNDPARENT',pointer(NewLong));
      end;
    GWL_STYLE :
      begin
        gtk_object_set_data(pgtkobject(Handle),'Style',pointer(NewLong));
      end;
    GWL_EXSTYLE :
      begin
        gtk_object_set_data(pgtkobject(Handle),'ExStyle',pointer(NewLong));
      end;
    GWL_USERDATA :
      begin
        gtk_object_set_data(pgtkobject(Handle),'Userdata',pointer(NewLong));
      end;
    GWL_ID :
      begin
        gtk_object_set_data(pgtkobject(Handle),'ID',pointer(NewLong));
      end;
  end; //case
  Assert(False, Format('Trace:< [TGtkWidgetSet.SETWINDOWLONG] HWND: 0x%x, idx: 0x%x(%d), Value: 0x%x(%d) --> 0x%x(%d)', [Handle, idx, idx, newlong, newlong, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function TGtkWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer;
    OldPoint: PPoint) : Boolean;

  Sets the DC offset for the specified device context.
 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer;
  OldPoint: PPoint) : Boolean;
var
  OldP: TPoint;
begin
  //DebugLn('[TGtkWidgetSet.SetWindowOrgEx] ',NewX,' ',NewY);
  GetWindowOrgEx(DC,@OldP);
  Result := MoveWindowOrgEx(DC,NewX-OldP.X,NewY-OldP.Y);
  if OldPoint<>nil then
    OldPoint^:=OldP;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
    X, Y, cx, cy: Integer; uFlags: UINT): Boolean;

  hWnd: Widget to move
  hWndInsertAfter:
    HWND_BOTTOM to move bottommost
    HWND_TOP to move topmost
    the Widget, that should lie just on top of hWnd
  uFlags:
    SWP_NOMOVE: ignore X, Y
    SWP_NOSIZE: ignore cx, cy
    SWP_NOZORDER: ignore hWndInsertAfter
    SWP_NOREDRAW: skip instant redraw
    SWP_NOACTIVATE: skip switching focus

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
  X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
  
  procedure SetZOrderOnFixedWidget(Widget, FixedWidget: PGtkWidget);
  var
    OldListItem: PGList;
    AfterWidget: PGtkWidget;
    AfterListItem: PGList;
  begin
    OldListItem:=FindFixedChildListItem(PGtkFixed(FixedWidget),Widget);
    if OldListItem=nil then begin
      DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnFixedWidget WARNING: Widget not on parents fixed widget');
      exit;
    end;
    AfterWidget:=nil;
    AfterListItem:=nil;
    if hWndInsertAfter=HWND_BOTTOM then begin
      //debugln('HWND_BOTTOM');
      // HWND_BOTTOM
    end else if hWndInsertAfter=HWND_TOP then begin
      //debugln('HWND_TOP');
      // HWND_TOP
      AfterListItem:=FindFixedLastChildListItem(PGtkFixed(FixedWidget));
    end else if hWndInsertAfter=0 then begin
      DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnFixedWidget WARNING: hWndInsertAfter=0');
      exit;
    end else begin
      // hWndInsertAfter
      AfterWidget:=PGtkWidget(hWndInsertAfter);
      AfterListItem:=FindFixedChildListItem(PGtkFixed(FixedWidget),AfterWidget);
      //debugln('AfterWidget=',GetWidgetDebugReport(AfterWidget));
    end;
    if (AfterListItem=nil) and (AfterWidget<>nil) then begin
      DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnFixedWidget WARNING: AfterWidget not on parents fixed widget');
      exit;
    end;
    if (OldListItem=AfterListItem) or (OldListItem^.next=AfterListItem) then
    begin
      {$IFDEF EnableGtkZReordering}
      DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnFixedWidget Hint: Already there');
      {$ENDIF}
      exit;
    end;
    //DebugLn('TGtkWidgetSet.SetWindowPos Moving GList entry');

    // reorder
    {$IFDEF EnableGtkZReordering}
    // MG: This trick does not work properly
    debugln('SetZOrderOnFixedWidget FixedWidget=['+GetWidgetDebugReport(FixedWidget)+']',
      ' Widget=['+GetWidgetDebugReport(Widget)+']',
      ' AfterWidget=['+GetWidgetDebugReport(AfterWidget)+']');
    MoveGListLinkBehind(PGtkFixed(FixedWidget)^.children,
                        OldListItem,AfterListItem);
    if GTK_WIDGET_VISIBLE(FixedWidget) and GTK_WIDGET_VISIBLE(Widget)
    and GTK_WIDGET_MAPPED(Widget) then begin
      DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnFixedWidget resize ..');
      gtk_widget_queue_resize(FixedWidget);
      AfterListItem:=PGtkFixed(FixedWidget)^.children;
      while AfterListItem<>nil do begin
        AfterWidget:=GetFixedChildListWidget(AfterListItem);
        DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnFixedWidget A ',GetWidgetDebugReport(AfterWidget));
        AfterListItem:=AfterListItem^.next;
      end;
    end;
    {$ENDIF}
  end;
  
  procedure SetZOrderOnLayoutWidget(Widget, LayoutWidget: PGtkWidget);
  begin
    //DebugLn('TGtkWidgetSet.SetWindowPos.SetZOrderOnLayoutWidget Not implemented: ZOrdering .. on ',GetWidgetDebugReport(LayoutWidget));
  end;

var
  Widget: PGTKWidget;
  FixedWidget: PGtkWidget;
begin
  Result:=false;
  Widget:=PGtkWidget(hWnd);
  {DebugLn('[TGtkWidgetSet.SetWindowPos] ',GetWidgetDebugReport(Widget),
    ' Top=',hWndInsertAfter=HWND_TOP,
    ' SWP_NOZORDER=',(SWP_NOZORDER and uFlags)<>0,
    ' SWP_NOSIZE=',(SWP_NOSIZE and uFlags)<>0,
    ' SWP_NOMOVE=',(SWP_NOMOVE and uFlags)<>0,
    '');}
  if GtkWidgetIsA(Widget,GTK_TYPE_WINDOW) then begin
    { case hWndInsertAfter of
        HWND_BOTTOM: ;  //gdk_window_lower(Widget^.Window);
        HWND_TOP:    gtk_window_set_position(PGtkWindow(hWnd),GTK_WIN_POS_CENTER);
                    //gdk_window_raise(Widget^.Window);
      end;
    }
  end else if (SWP_NOZORDER and uFlags)=0 then begin
    FixedWidget:=Widget^.Parent;
    if FixedWidget=nil then exit;
    
    //DebugLn('TGtkWidgetSet.SetWindowPos ZOrdering .. on ',GetWidgetDebugReport(FixedWidget));
    if GtkWidgetIsA(FixedWidget,GTK_Fixed_Get_Type) then begin
      // parent's client area is a gtk_fixed widget
      SetZOrderOnFixedWidget(Widget,FixedWidget);
    end else if GtkWidgetIsA(FixedWidget,GTK_Layout_Get_Type) then begin
      // parent's client area is a gtk_layout widget
      SetZOrderOnLayoutWidget(Widget,FixedWidget);
    end else begin
      //DebugLn('TGtkWidgetSet.SetWindowPos Not implemented: ZOrdering .. on ',GetWidgetDebugReport(FixedWidget));
      exit;
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: ShowCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ShowCaret(hWnd: HWND): Boolean;
var
  GTKObject: PGTKObject;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.ShowCaret] HWND: 0x%x', [hWnd]));

  GTKObject := PGTKObject(HWND);
  Result := GTKObject <> nil;

  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_ShowCaret(PGTKAPIWidget(GTKObject));
    end
    else begin
      Result := False;
    end;
  end
  else DebugLn('WARNING: [TGtkWidgetSet.ShowCaret] Got null HWND');

  Assert(False, Format('Trace:< [TGtkWidgetSet.ShowCaret] HWND: 0x%x --> %s', [hWnd, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Function:  ShowScrollBar
  Params:  Wnd, wBar, bShow
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer;
  bShow: Boolean): Boolean;
const
  POLICY: array[BOOLEAN] of TGTKPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
var
  Widget: PGtkWidget;
  NewPolicy: Integer;
begin
  Assert(False, 'trace:[TGtkWidgetSet.ShowScrollBar]');
  Result:=false;
  Result := (Handle <> 0);
  if Result then begin
    Widget:=PGtkWidget(Handle);
    if GtkWidgetIsA(Widget,gtk_scrolled_window_get_type) then begin
      if wBar in [SB_BOTH, SB_HORZ] then begin
        if bShow then
          NewPolicy:=POLICY[bShow]
        else
          NewPolicy:=GTK_POLICY_NEVER;
        gtk_object_set(PGTKObject(Widget), 'hscrollbar_policy', [NewPolicy,nil]);
      end;
      if wBar in [SB_BOTH, SB_VERT] then begin
        if bShow then
          NewPolicy:=POLICY[bShow]
        else
          NewPolicy:=GTK_POLICY_NEVER;
        gtk_object_set(PGTKObject(Widget), 'vscrollbar_policy', [NewPolicy,nil]);
      end;
    end
    else begin
      if (wBar = SB_CTL)
      and gtk_type_is_a(gtk_object_type(PGTKObject(Handle)),gtk_widget_get_type)
      then begin
        if bShow
        then gtk_widget_show(Widget)
        else gtk_widget_hide(Widget);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TGtkWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  GtkWindow: PGtkWindow;
begin
  Result:=false;
  GtkWindow:=PGtkWindow(hWnd);
  if GtkWindow=nil then
    RaiseException('TGtkWidgetSet.ShowWindow  hWnd is nil');

  {$IFDEF Gtk2}

  case nCmdShow of

  SW_SHOWNORMAL:
    begin
      gtk_window_deiconify(GtkWindow);
      gtk_window_unmaximize(GtkWindow);
    end;

  SW_MINIMIZE:
    gtk_window_iconify(GtkWindow);

  SW_SHOWMAXIMIZED:
    gtk_window_maximize(GtkWindow);

  end;

  {$ELSE}

  case nCmdShow of

  SW_SHOWNORMAL:
    begin
      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
      gdk_window_show(PgtkWidget(GtkWindow)^.Window);
      {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
    end;

  SW_MINIMIZE, SW_SHOWMAXIMIZED:
    DebugLn('TGtkWidgetSet.ShowWindow:  not implemented yet');

  end;

  Result:=true;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Function: StretchBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified raster operation. If needed it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
  If SrcDC contains a mask the pixmap will be copied with this transparency.

  ToDo: Mirroring, extended NonDrawable support (Image, Bitmap, etc)
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean;
begin
  Result:=StretchCopyArea(DestDC,X,Y,Width,Height,
                          SrcDC,XSrc,YSrc,SrcWidth,SrcHeight,
                          0,0,0,
                          ROp);
end;

{------------------------------------------------------------------------------
  Function: StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified mask and raster operation. If needed
  it resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result:=StretchCopyArea(DestDC,X,Y,Width,Height,
                          SrcDC,XSrc,YSrc,SrcWidth,SrcHeight,
                          Mask,XMask,YMask,
                          ROp);
end;

{------------------------------------------------------------------------------
  Function: TextOut
  Params: DC:
          X:
          Y:
          Str:
          Count:
  Returns:

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar;
  Count: Integer) : Boolean;
{$IfDef GTK2}
begin
  DebugLn('TGtkWidgetSet.TextOut ToDo');
  Result:=false;
end;
{$ELSE}
var
  aRect : TRect;
  txtpt : TPoint;
  sz : TSize;
  UseFont : PGDKFont;
  UnRef,
  Underline,
  StrikeOut : Boolean;
  DCOrigin: TPoint;

  TempPen : hPen;
  LogP : TLogPen;
  Points : array[0..1] of TSize;
begin
  Result := IsValidDC(DC);
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.TextOut] Uninitialized GC');
    end
    else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFont := GetDefaultFont(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFont := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := LongBool(CurrentFont^.LogFont.lfUnderline);
        StrikeOut := LongBool(CurrentFont^.LogFont.lfStrikeOut);
      end;
      If UseFont = nil then
        DebugLn('WARNING: [TGtkWidgetSet.TextOut] Missing Font')
      else begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        GetTextExtentPoint(DC, Str, Count, Sz);
        aRect := Rect(X+DCOrigin.X,Y+DCOrigin.Y,X + Sz.CX, Sz.CY);
        //DebugLn('TGtkWidgetSet.TextOut ',ARect.Left,',',ARect.Top,',',ARect.RIght,',',ARect.Bottom);
        FillRect(DC,aRect,hBrush(CurrentBrush));
        UpdateDCTextMetric(TDeviceContext(DC));
        TxtPt.X := X;
        {$IfDef Win32}
          TxtPt.Y := Y + DCTextMetric.TextMetric.tmHeight div 2;
        {$Else}
          TxtPt.Y := Y + DCTextMetric.TextMetric.tmAscent;
        {$EndIf}
        SelectGDKTextProps(DC);
        {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
        gdk_draw_text(Drawable, UseFont,
          GC, TxtPt.X+DCOrigin.X, TxtPt.Y+DCOrigin.Y, Str, Count);
        {$IFDEF DebugGDKTraps}EndGDKErrorTrap;{$ENDIF}
        If Underline or StrikeOut then begin
          {Create & select pen of font color}
          LogP.lopnStyle := PS_SOLID;
          LogP.lopnWidth.X := 1;
          LogP.lopnColor := GetTextColor(DC);
          TempPen := SelectObject(DC, CreatePenIndirect(LogP));

          {Get line(s) horizontal position(s)}
          Points[0].cX := X;
          Points[1].cX := X + sz.cX;

          {Draw line(s)}
          If Underline then begin
            Points[0].cY := Y + 2 + DCTextMetric.TextMetric.tmHeight -
                            DCTextMetric.TextMetric.tmDescent;
            Points[1].cY := Points[0].cY;
            Polyline(DC, @Points[0], 2);
          end;

          If StrikeOut then begin
            Points[0].cY := Y + 2 + (TxtPt.Y - Y) div 2;
            Points[1].cY := Points[0].cY;
            Polyline(DC, @Points[0], 2);
          end;

          DeleteObject(SelectObject(DC, TempPen));
        end;
        Result := True;
        If UnRef then
          FontCache.Unreference(UseFont);
      end;
    end;
  end;
end;
{$EndIf}
{------------------------------------------------------------------------------
  Function: VkKeyScan
  Params: AChar: Character to translate
  Returns: LoByte: VK-code
           HiByte: ALT | CTRL | SHIFT pressed -> bit2 | bit1 | bit0

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.VkKeyScan(AChar: Char): Short;
begin
  Result := CharToVkAndFlags(AChar);
end;

{------------------------------------------------------------------------------
  Function: WindowFromPoint
  Params: Point: Specifies the x and y Coords
  Returns: The handle of the gtkwidget.  If none exist, then NULL is returned.

 ------------------------------------------------------------------------------}
Function TGtkWidgetSet.WindowFromPoint(Point : TPoint) : HWND;
var
  ev : TgdkEvent;
  Window : PgdkWindow;
  Widget : PgtkWidget;
  p: TPoint;
begin
  Result := 0;

  // !!!gdk_window_at_pointer changes the coordinates!!!
  // -> using local variable p
  p:=Point;
  Window := gdk_window_at_pointer(@p.x,@p.Y);
  if window <> nil then
  begin
    FillChar(ev,SizeOf(ev),0);
    ev.any.window := Window;
    Widget := gtk_get_event_widget(@ev);
    Result := Longint(widget);
  end;
end;

//##apiwiz##eps##   // Do not remove

// Placed CriticalSectionSupport outside the API wizard bounds
// so it won't affect sorting etc.

{$IfDef Critical_Sections_Support}

  {$IfNDef Win32}

    {$Define pthread}

    Type
      _pthread_fastlock = packed record
        __status: Longint;
        __spinlock: Integer;
      end;

      pthread_mutex_t = packed record
        __m_reserved: Integer;
        __m_count: Integer;
        __m_owner: Pointer;
        __m_kind: Integer;
        __m_lock: _pthread_fastlock;
      end;
      ppthread_mutex_t = ^pthread_mutex_t;

      pthread_mutexattr_t = packed record
        __mutexkind: Integer;
      end;

    {$linklib pthread}

    function pthread_mutex_init(var Mutex: pthread_mutex_t;
      var Attr: pthread_mutexattr_t): Integer; cdecl;external;
    function pthread_mutexattr_settype(var Attr: pthread_mutexattr_t;
      Kind: Integer): Integer; cdecl;external;
    function pthread_mutex_lock(var Mutex: pthread_mutex_t):
      Integer; cdecl; external;
    function pthread_mutex_unlock(var Mutex: pthread_mutex_t):
      Integer; cdecl; external;
    function pthread_mutex_destroy(var Mutex: pthread_mutex_t):
      Integer; cdecl; external;
  {$EndIf}

{$EndIf}

Procedure TGtkWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
{$IfDef pthread}
var
  Crit : ppthread_mutex_t;
  Attribute: pthread_mutexattr_t;
begin
  if pthread_mutexattr_settype(Attribute, 1) <> 0 then
    Exit;
  If CritSection <> 0 then
    Try
      Crit := ppthread_mutex_t(CritSection);
      Dispose(Crit);
    except
      CritSection := 0;
    end;
  New(Crit);
  pthread_mutex_init(Crit^, Attribute);
  CritSection := Longint(Crit);
end;
{$Else}
begin
end;
{$EndIf}

Procedure TGtkWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
{$IfDef pthread}
var
  Crit,
  tmp : ppthread_mutex_t;
begin
  New(Crit);
  If CritSection <> 0 then
    Try
      Crit^ := ppthread_mutex_t(CritSection)^;
    except
      begin
        CritSection := Longint(Crit);
        exit;
      end;
    end;
  pthread_mutex_lock(Crit^);
  tmp := ppthread_mutex_t(CritSection);
  CritSection := Longint(Crit);
  Dispose(Tmp);
end;
{$Else}
begin
end;
{$EndIf}

Procedure TGtkWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
{$IfDef pthread}
var
  Crit,
  tmp : ppthread_mutex_t;
begin
  New(Crit);
  If CritSection <> 0 then
    Try
      Crit^ := ppthread_mutex_t(CritSection)^;
    except
      begin
        CritSection := Longint(Crit);
        exit;
      end;
    end;
  pthread_mutex_unlock(Crit^);
  tmp := ppthread_mutex_t(CritSection);
  CritSection := Longint(Crit);
  Dispose(Tmp);
end;
{$Else}
begin
end;
{$EndIf}

Procedure TGtkWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
{$IfDef pthread}
var
  Crit,
  tmp : ppthread_mutex_t;
begin
  New(Crit);
  If CritSection <> 0 then
    Try
      Crit^ := ppthread_mutex_t(CritSection)^;
    except
      begin
        CritSection := Longint(Crit);
        exit;
      end;
    end;
  pthread_mutex_destroy(Crit^);
  Dispose(Crit);
  tmp := ppthread_mutex_t(CritSection);
  CritSection := 0;
  Dispose(Tmp);
end;
{$Else}
begin
end;
{$EndIf}

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}

{ =============================================================================

  $Log$
  Revision 1.385  2005/01/16 11:40:11  mattias
  fixed TGtkWidgetSet.ExtSelectClipRGN for DCOrigin

  Revision 1.384  2005/01/08 11:03:18  mattias
  implemented TPen.Mode=pmXor  from Jesus

  Revision 1.383  2005/01/07 18:40:10  mattias
  clean up, added GetRGBValues

  Revision 1.382  2005/01/01 20:17:32  mattias
  implemented gtk GetTextExtentPoint for UTF8

  Revision 1.381  2005/01/01 16:04:13  mattias
  implemented CodeExplorer auto update on switching source editor page

  Revision 1.380  2004/12/22 19:56:44  mattias
  started TFont mirgration to fpCanvas font

  Revision 1.379  2004/12/21 22:49:29  mattias
  implemented scrollbar codes for gtk intf  from Jesus

  Revision 1.378  2004/12/16 19:03:57  mattias
  applied patch for smooth scrolling parameters  from Jesus

  Revision 1.377  2004/12/11 01:28:58  mattias
  implemented bvSpace of TBevelCut

  Revision 1.376  2004/12/01 16:17:18  mattias
  updated fpdoc sceletons for lcl and gtk intf

  Revision 1.375  2004/11/27 13:57:49  mattias
  added more gtk ISO character sets

  Revision 1.374  2004/11/20 11:49:15  mattias
  implemented stopping project on close project

  Revision 1.373  2004/11/20 11:20:06  mattias
  implemented creating classes at run time from any TComponent descendant

  Revision 1.372  2004/11/10 18:23:56  mattias
  impementing changing a TLabel.Font properties Size, Height, Name, Style - set only at Handle creation time

  Revision 1.371  2004/11/08 19:11:55  mattias
  disabled hardly used gtk FillScreenFont, this should be only done on demand, improved getting default font family for gtk

  Revision 1.370  2004/10/15 13:28:22  mattias
  codeexplorer: using lower recursive depth

  Revision 1.369  2004/10/01 13:16:44  mattias
  fixed unselecting TCanvas objects

  Revision 1.368  2004/09/29 15:18:27  mattias
  fixed TBitmap.Canvas.Frame3d

  Revision 1.367  2004/09/17 20:30:13  vincents
  replaced write by DbgOut

  Revision 1.366  2004/09/10 16:28:51  mattias
  implemented very rudimentary TTabControl

  Revision 1.365  2004/09/06 22:24:52  mattias
  started the carbon LCL interface

  Revision 1.364  2004/09/02 09:17:00  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.363  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.362  2004/08/19 18:50:53  mattias
  splitted IDE component owner hierachy to reduce notification time

  Revision 1.361  2004/08/18 20:49:03  mattias
  simple forms can now be child controls

  Revision 1.360  2004/08/13 20:40:27  mattias
  fixed DebugLn for VerboseRawImage

  Revision 1.359  2004/08/11 12:57:03  mattias
  improved gtk1 FontCache to handle several descriptors per gdkfont

  Revision 1.358  2004/08/10 17:34:13  mattias
  implemented font cache for gtk, which accelerates switching fonts

  Revision 1.357  2004/07/01 10:23:27  mattias
  fixed uninitialsed vars   from Jeroen

  Revision 1.356  2004/06/28 23:16:24  mattias
  added TListView.AddItems  from Andrew Haines

  Revision 1.355  2004/06/28 20:03:33  mattias
  fixed TGtkWidgetSet.DrawFrameControl

  Revision 1.354  2004/06/28 17:03:37  mattias
  clean up

  Revision 1.353  2004/06/28 15:45:48  mattias
  fixed a mem violation in gtk intf paint msg conversion

  Revision 1.352  2004/06/09 20:51:45  vincents
  implemented basic clipboard support for win32

  Revision 1.351  2004/05/22 14:35:33  mattias
  fixed button return key

  Revision 1.350  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.349  2004/05/07 08:07:57  mattias
  ifdefd UseSimpleJpeg

  Revision 1.348  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.347  2004/04/15 21:27:40  marc
  * Applied patch from Ladislav Michl

  Revision 1.346  2004/04/12 22:36:29  mattias
  made TIcon more independent of TBitmap  from Colin

  Revision 1.345  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.344  2004/04/02 14:28:44  vincents
  Fixed compilation with -dVerboseFocus

  Revision 1.343  2004/03/30 20:38:14  mattias
  fixed interface constraints, fixed syncompletion colors

  Revision 1.342  2004/03/28 12:49:23  mattias
  implemented mask merge and extraction for raw images

  Revision 1.341  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.340  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.339  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.338  2004/03/06 17:12:19  mattias
  fixed CreateBrushIndirect

  Revision 1.337  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.336  2004/03/05 00:31:52  marc
  * Renamed TGtkObject to TGtkWidgetSet

  Revision 1.335  2004/02/28 00:34:36  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.334  2004/02/23 23:15:14  mattias
  improved FindDragTarget

  Revision 1.333  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.332  2004/02/21 01:01:03  mattias
  added uninstall popupmenuitem to package graph explorer

  Revision 1.331  2004/02/19 05:07:17  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.330  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.329  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.328  2004/02/10 00:05:03  mattias
  TSpeedButton now uses MaskBlt

  Revision 1.327  2004/02/04 22:17:09  mattias
  removed workaround VirtualCreate

  Revision 1.326  2004/02/04 12:48:17  mattias
  added CLX colors

  Revision 1.325  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.324  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.323  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.322  2004/01/26 11:55:35  mattias
  fixed resizing synedit

  Revision 1.321  2004/01/23 13:55:30  mattias
  style widgets are now realized, so all values are initialized

  Revision 1.320  2004/01/22 11:23:36  mattias
  started MaskBlt for gtkIF and applied patch for dir dlg in env opts from Vincent

  Revision 1.319  2004/01/18 11:03:01  mattias
  added finnish translation

  Revision 1.318  2004/01/17 13:29:04  mattias
  using now fpc constant LineEnding   from Vincent

  Revision 1.317  2004/01/15 22:36:24  mattias
  workaround for fpc fpu bug and added calendar debugging msg

  Revision 1.316  2004/01/13 10:41:40  mattias
  fixed statusbar updating all panels

  Revision 1.315  2004/01/12 23:56:10  mattias
  improved double buffering, only one issue left: parent gdkwindow paint messages

  Revision 1.314  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.313  2004/01/10 18:00:42  mattias
  fixed GetWindowOrgEx, added GetDCOriginRelativeToWindow

  Revision 1.312  2004/01/10 00:46:46  mattias
  fixed DestroyComponent

  Revision 1.311  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.310  2004/01/05 01:18:16  mattias
  implemented Double Buffering for synedit and deactivated multi buffering in TGTKObject.ExtTextOut

  Revision 1.309  2004/01/03 23:15:00  mattias
  default font can now change height and fixed gtk crash

  Revision 1.308  2004/01/03 20:31:02  mattias
  fixed CreateRectRgn for negative widths/heights

  Revision 1.307  2003/12/30 21:05:13  micha
  fix gtk interface due to lcl interface change (from vincent

  Revision 1.306  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.305  2003/12/23 11:16:41  mattias
  started key combinations, fixed some range check errors

  Revision 1.304  2003/11/29 15:23:23  mattias
  ct parser now understands interconst:const

  Revision 1.303  2003/11/29 13:17:38  mattias
  made gtklayout using window theme at start

  Revision 1.302  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.301  2003/11/23 13:13:35  mattias
  added clWindow for gtklistitem

  Revision 1.300  2003/11/23 10:58:47  mattias
  fixed de-associating TUpDown

  Revision 1.299  2003/11/10 16:15:32  micha
  cleanups; win32 fpimage support

  Revision 1.298  2003/11/08 22:53:11  mattias
  workaround for gtk1 invalidate bug

  Revision 1.297  2003/11/03 22:37:41  mattias
  fixed vert scrollbar, implemented GetDesignerDC

  Revision 1.296  2003/11/01 10:27:41  mattias
  fpc 1.1 fixes, started scrollbar hiding, started polymorphing client areas

  Revision 1.295  2003/10/31 14:54:10  mattias
  added the possibility to disbale double buffering

  Revision 1.294  2003/10/30 21:26:23  mattias
  removed some hints

  Revision 1.293  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.292  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.291  2003/10/15 20:33:37  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.290  2003/10/06 16:13:52  ajgenius
  partly fixed gtk2 mouse offsets;
  added new includes to gtk2 lpk

  Revision 1.289  2003/10/02 18:18:32  ajgenius
  buffer cs_opaque ExtTextOut blocks to help prevent extensive flickering

  Revision 1.288  2003/09/25 16:02:16  ajgenius
  try to catch GDK/X drawable errors and raise an AV to stop killing App

  Revision 1.287  2003/09/19 00:41:52  ajgenius
  remove USE_PANGO define since pango now apears to work properly.

  Revision 1.286  2003/09/18 14:06:30  ajgenius
  fixed Tgtkobject.drawtext for Pango till the native pango one works better

  Revision 1.285  2003/09/18 12:15:01  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.284  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.283  2003/09/17 19:40:46  ajgenius
  Initial DoubleBuffering Support for GTK2

  Revision 1.282  2003/09/16 11:35:14  mattias
  started TDBCheckBox

  Revision 1.281  2003/09/15 15:43:04  mattias
  fixed gtk2interface package

  Revision 1.280  2003/09/11 21:33:11  ajgenius
  partly fixed TWinControl(csFixed)

  Revision 1.279  2003/09/10 18:03:46  ajgenius
  more changes for pango -
  partly fixed ref counting,
  added Pango versions of TextOut, CreateFontIndirectEx, and GetTextExtentPoint to the GTK2 interface

  Revision 1.278  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.277  2003/09/09 17:16:24  ajgenius
  start implementing pango routines for GTK2

  Revision 1.276  2003/09/09 04:15:08  ajgenius
  more updates for GTK2, more GTK1 wrappers, removal of more ifdef's, partly fixed signals

  Revision 1.275  2003/09/06 20:23:53  ajgenius
  fixes for gtk2
  added more wrappers for gtk1/gtk2 converstion and sanity
  removed pointless version $Ifdef GTK2 etc
  IDE now "runs" Tcontrol drawing/using problems
  renders it unuseable however

  Revision 1.274  2003/09/06 17:24:52  ajgenius
  gtk2 changes for pixmap, getcursorpos, mouse events workaround

  Revision 1.273  2003/09/05 19:29:38  mattias
  Success: The first gtk2 application ran without error

  Revision 1.272  2003/09/05 18:19:54  ajgenius
  Make GTK2 "compile". linking fails still
  (Makefile.fpc needs pkgconfig libs/GTK2 linking rules,
  but not sure how not sure how) and when linked via a make script
  (like gtk2 examples do) apps still won't work(yet).  I think we
  need to do a lot of work to make sure incompatible(also to get rid
  of deprecated) things are done in GTK2 interface itself, and just
  use more $Ifdef GTK1 in the gtk interface itself.

  Revision 1.271  2003/08/27 08:14:37  mattias
  fixed system fonts for win32 intf

  Revision 1.270  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.269  2003/08/18 19:24:18  mattias
  fixed TCanvas.Pie

  Revision 1.268  2003/08/18 13:21:23  mattias
  renamed lazqueue to lazlinkedlist, patch from Jeroen

  Revision 1.267  2003/08/16 15:29:56  mattias
  fixed TBitmap.GetHandle

  Revision 1.266  2003/08/15 14:01:20  mattias
  combined lazconf things for unix

  Revision 1.265  2003/07/29 00:28:43  marc
  + Implemented GetCursorPos

  Revision 1.264  2003/07/21 23:43:32  marc
  * Fixed radiogroup menuitems

  Revision 1.263  2003/07/20 06:39:03  mattias
  added comments

  Revision 1.262  2003/07/08 20:09:40  mattias
  updated build fpc rpm script

  Revision 1.261  2003/07/07 07:59:34  mattias
  made Size_SourceIsInterface a flag

  Revision 1.260  2003/07/06 20:40:34  mattias
  TWinControl.WmSize/Move now updates interface messages smarter

  Revision 1.259  2003/07/04 22:06:49  mattias
  implemented interface graphics

  Revision 1.258  2003/07/04 08:54:53  mattias
  implemented 16bit rawimages for gtk

  Revision 1.257  2003/07/03 18:10:55  mattias
  added fontdialog options to win32 intf from Wojciech Malinowski

  Revision 1.256  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.255  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.254  2003/07/01 13:49:36  mattias
  clean up

  Revision 1.253  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.252  2003/06/30 10:09:46  mattias
  fixed Get/SetPixel for DC without widget

  Revision 1.251  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.250  2002/08/19 15:15:24  mattias
  implemented TPairSplitter

  Revision 1.249  2002/08/18 16:50:09  mattias
  fixes for debugging

  Revision 1.248  2002/08/18 04:57:01  mattias
  fixed csDashDot

  Revision 1.247  2002/08/17 23:41:35  mattias
  many clipping fixes

  Revision 1.246  2003/06/20 12:56:53  mattias
  reduced paint messages on destroy

  Revision 1.245  2003/06/19 09:26:58  mattias
  fixed changing unitname during update

  Revision 1.244  2003/06/18 11:21:07  mattias
  fixed taborder=0, implemented TabOrder Editor

  Revision 1.243  2003/06/13 21:08:53  mattias
  moved TColorButton to dialogs.pp

  Revision 1.242  2003/06/13 10:37:20  mattias
  fixed AV on StretchDraw 0x0

  Revision 1.241  2003/06/07 13:04:03  mattias
  ComboBoxDropDown from Yoyong

  Revision 1.240  2003/06/07 09:34:21  mattias
  added ambigius compiled unit test for packages

  Revision 1.239  2003/06/03 08:02:33  mattias
  implemented showing source lines in breakpoints dialog

  Revision 1.238  2003/05/20 21:41:07  mattias
  started loading/saving breakpoints

  Revision 1.237  2003/05/19 08:16:33  mattias
  fixed allocation of dc backcolor

  Revision 1.236  2003/04/26 10:45:34  mattias
  fixed right control release

  Revision 1.235  2003/04/16 22:11:35  mattias
  fixed codetools Makefile, fixed default prop not found error

  Revision 1.234  2003/04/16 17:20:24  mattias
  implemented package check broken dependency on compile

  Revision 1.233  2003/04/11 21:21:34  mattias
  implemented closing unneeded package

  Revision 1.232  2003/04/11 17:10:20  mattias
  added but not implemented ComboBoxDropDown

  Revision 1.231  2003/04/11 09:05:41  mattias
  fixed adding items on TComboBox.DropDown

  Revision 1.230  2003/04/03 17:42:13  mattias
  added exception handling for createpixmapindirect

  Revision 1.229  2003/04/02 13:23:24  mattias
  fixed default font

  Revision 1.228  2003/03/31 20:25:19  mattias
  fixed scrollbars of TIpHtmlPanel

  Revision 1.227  2003/03/29 23:52:25  mattias
  IpHtmlPanel can show simple HTML pages, but there are mem bugs

  Revision 1.226  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.225  2003/03/28 19:39:54  mattias
  started typeinfo for double extended

  Revision 1.224  2003/03/26 19:25:27  mattias
  added transient deactivation option and updated localization

  Revision 1.223  2003/03/26 00:21:25  mattias
  implemented build lazarus extra options -d

  Revision 1.222  2003/03/25 10:45:41  mattias
  reduced focus handling and improved focus setting

  Revision 1.221  2003/03/18 13:04:25  mattias
  improved focus debugging output

  Revision 1.220  2003/03/17 20:53:16  mattias
  removed SetRadioButtonGroupMode

  Revision 1.219  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.218  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.217  2003/03/16 09:41:06  mattias
  fixed checking menuitems

  Revision 1.216  2003/03/12 14:39:29  mattias
  fixed clipping origin in stretchblt

  Revision 1.215  2003/03/11 08:14:22  mattias
  implemented ShowWindow for gtk2

  Revision 1.214  2003/03/10 20:10:28  ajgenius
  initial changes to fix mask vs. region clipping

  Revision 1.213  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.212  2003/02/28 19:54:05  mattias
  added ShowWindow

  Revision 1.211  2003/02/23 10:42:06  mattias
  implemented changing TMenuItem.GroupIndex at runtime

  Revision 1.210  2003/02/16 01:40:43  mattias
  fixed uninitialized style

  Revision 1.209  2003/02/04 14:36:19  mattias
  fixed set method in OI

  Revision 1.208  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.207  2003/01/24 11:58:01  mattias
  fixed clipboard waiting and kwrite targets

  Revision 1.206  2003/01/06 14:41:24  mattias
  fixed synedit mouse pos to logical column

  Revision 1.205  2003/01/06 13:59:45  mattias
  fixed synedit ensure cursor pos visible with tab chars

  Revision 1.204  2003/01/01 12:38:53  mattias
  clean ups

  Revision 1.203  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.202  2002/12/30 17:24:08  mattias
  added history to identifier completion

  Revision 1.201  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.200  2002/12/28 11:29:47  mattias
  xmlcfg deletion, focus fixes

  Revision 1.199  2002/12/27 17:58:47  mattias
  cleanup

  Revision 1.198  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.197  2002/12/27 08:46:32  mattias
  changes for fpc 1.1

  Revision 1.196  2002/12/26 11:00:15  mattias
  added included by to unitinfo and a few win32 functions

  Revision 1.195  2002/12/25 13:30:37  mattias
  added more windows funcs and fixed jump to compiler error end of file

  Revision 1.194  2002/12/22 22:42:55  mattias
  custom controls now support child wincontrols

  Revision 1.193  2002/12/07 08:42:09  mattias
  improved ExtTxtOut: support for char dist array

  Revision 1.192  2002/12/05 22:16:33  mattias
  double byte char font started

  Revision 1.191  2002/12/05 17:26:02  mattias
  implemented fsUnderLine for ExtTextOut for gtk

  Revision 1.190  2002/11/23 13:48:46  mattias
  added Timer patch from Vincent Snijders

  Revision 1.189  2002/11/12 10:16:20  lazarus
  MG: fixed TMainMenu creation

  Revision 1.188  2002/11/09 18:13:36  lazarus
  MG: fixed gdkwindow checks

  Revision 1.187  2002/11/09 15:02:08  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.186  2002/11/03 22:14:44  lazarus
  MG: fixed Polygon and not winding

  Revision 1.185  2002/11/01 17:55:35  lazarus
  AJ: ignore offset in Polygon Winding, Region/FillRect should take care of it

  Revision 1.184  2002/11/01 17:26:45  lazarus
  MG: fixed GetClipBox

  Revision 1.183  2002/11/01 14:40:31  lazarus
  MG: fixed mouse coords on scrolling wincontrols

  Revision 1.182  2002/10/31 22:14:16  lazarus
  MG: fixed GetClipBox when clipping region invalid

  Revision 1.181  2002/10/31 21:29:47  lazarus
  MG: implemented TControlScrollBar.Size

  Revision 1.180  2002/10/31 18:37:30  lazarus
  MG: fixed GetClipBox

  Revision 1.179  2002/10/31 17:31:11  lazarus
  MG: fixed return polygon point

  Revision 1.178  2002/10/31 04:27:59  lazarus
  AJ: added TShape

  Revision 1.177  2002/10/30 17:43:37  lazarus
  AJ: added IsNullBrush checks to reduce pointless color allocations & GDK function calls

  Revision 1.176  2002/10/29 23:14:28  lazarus
  MG: removed interfaces

  Revision 1.175  2002/10/29 19:33:42  lazarus
  MG: removed interfaces

  Revision 1.174  2002/10/29 12:30:45  lazarus
  AJ: fixed initial result in clipping/region routines

  Revision 1.173  2002/10/28 23:25:36  lazarus
  AJ: initialize SelectClipRgn Result

  Revision 1.172  2002/10/28 18:17:04  lazarus
  MG: impoved focussing, unfocussing on destroy and fixed unit search

  Revision 1.171  2002/10/26 12:32:29  lazarus
  AJ:Minor fixes for Win32 GTK compiling

  Revision 1.170  2002/10/24 20:59:35  lazarus
  AJ: fixed typo causing gdk cmap error

  Revision 1.169  2002/10/23 20:47:27  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.168  2002/10/21 22:12:49  lazarus
  MG: fixed frmactivate

  Revision 1.167  2002/10/21 18:21:39  lazarus
  AJ:minor styles improvement; fixed drawing checks under all(?) themes

  Revision 1.166  2002/10/21 14:40:53  lazarus
  MG: fixes for 1.1

  Revision 1.165  2002/10/20 21:54:04  lazarus
  MG: fixes for 1.1

  Revision 1.164  2002/10/20 21:49:11  lazarus
  MG: fixes for fpc1.1

  Revision 1.163  2002/10/20 19:03:57  lazarus
  AJ: minor fixes for FPC 1.1

  Revision 1.162  2002/10/18 16:08:10  lazarus
  AJ: Partial HintWindow Fix; Added Screen.Font & Font.Name PropEditor; Started to fix ComboBox DropDown size/pos

  Revision 1.161  2002/10/17 21:00:18  lazarus
  MG: fixed uncapturing of mouse

  Revision 1.160  2002/10/17 15:09:33  lazarus
  MG: made mouse capturing more strict

  Revision 1.159  2002/10/15 22:28:06  lazarus
  AJ: added forcelinebreaks

  Revision 1.158  2002/10/15 17:09:54  lazarus
  AJ: fixed GTK DrawText to use WordWrap, and add DT_EditControl

  Revision 1.157  2002/10/15 16:01:38  lazarus
  MG: fixed timers

  Revision 1.156  2002/10/15 07:01:31  lazarus
  MG: fixed timer checking

  Revision 1.155  2002/10/14 19:00:50  lazarus
  MG: fixed zombie timers

  Revision 1.154  2002/10/10 19:43:17  lazarus
  MG: accelerated GetTextMetrics

  Revision 1.153  2002/10/10 08:51:15  lazarus
  MG: added paint messages for some gtk internal widgets

  Revision 1.152  2002/10/09 20:08:41  lazarus
  Cleanups

  Revision 1.151  2002/10/09 10:22:55  lazarus
  MG: fixed client origin coordinates

  Revision 1.150  2002/10/08 21:51:12  lazarus
  MG: fixed Ellipse

  Revision 1.149  2002/10/08 14:28:14  lazarus
  MG: accelerated FillRect

  Revision 1.148  2002/10/08 14:10:03  lazarus
  MG: added TDeviceContext.SelectedColors

  Revision 1.147  2002/10/08 13:42:26  lazarus
  MG: added TDevContextColorType

  Revision 1.146  2002/10/08 10:08:47  lazarus
  MG: accelerated GDIColor allocating

  Revision 1.145  2002/10/07 20:50:59  lazarus
  MG: accelerated SelectGDKPenProps

  Revision 1.144  2002/10/07 10:55:18  lazarus
  MG: accelerated TDynHashArray

  Revision 1.143  2002/10/04 22:59:14  lazarus
  MG: added OnDrawItem to OI

  Revision 1.142  2002/10/04 14:24:17  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.141  2002/10/03 14:47:32  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.140  2002/10/01 10:05:50  lazarus
  MG: changed PDeviceContext into class TDeviceContext

  Revision 1.139  2002/09/30 20:19:14  lazarus
  MG: fixed flickering of modal forms

  Revision 1.138  2002/09/27 20:52:25  lazarus
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

  Revision 1.137  2002/09/20 13:11:13  lazarus
  MG: fixed TPanel and Frame3D

  Revision 1.136  2002/09/19 19:56:17  lazarus
  MG: accelerated designer drawings

  Revision 1.135  2002/09/19 16:45:54  lazarus
  MG: fixed Menu.Free and gdkwindow=nil bug

  Revision 1.134  2002/09/18 17:07:29  lazarus
  MG: added patch from Andrew

  Revision 1.133  2002/09/13 16:58:28  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.132  2002/09/13 11:49:48  lazarus
  Cleanups, extended TStatusBar, graphic control cleanups.

  Revision 1.131  2002/09/12 15:35:57  lazarus
  MG: small bugfixes

  Revision 1.130  2002/09/12 05:56:17  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.129  2002/09/12 05:32:14  lazarus
  MG: fixed DeleteObject

  Revision 1.128  2002/09/10 15:23:22  lazarus
  MG: fixed calculation of bitmap size

  Revision 1.127  2002/09/10 06:49:22  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.126  2002/09/09 14:01:06  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.125  2002/09/06 19:45:11  lazarus
  Cleanups plus a fix to TPanel parent/drawing problem.

  Revision 1.124  2002/09/06 19:11:48  lazarus
  MG: fixed scrollbars of TTreeView

  Revision 1.123  2002/09/06 16:41:31  lazarus
  MG: set SpecialOrigin

  Revision 1.122  2002/09/06 16:38:25  lazarus
  MG: added GetDCOffset

  Revision 1.121  2002/09/06 15:57:36  lazarus
  MG: fixed notebook client area, send messages and minor bugs

  Revision 1.120  2002/09/06 11:33:36  lazarus
  MG: added jitform error messagedlg

  Revision 1.119  2002/09/03 08:07:22  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.118  2002/09/02 08:13:17  lazarus
  MG: fixed GraphicClass.Create

  Revision 1.117  2002/08/30 13:43:38  lazarus
  MG: fixed drawing of non visual components in designer

  Revision 1.116  2002/08/30 12:32:24  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.115  2002/08/29 00:07:03  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.114  2002/08/28 09:40:50  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.113  2002/08/27 18:45:15  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.112  2002/08/27 06:40:51  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.111  2002/08/24 12:55:00  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.110  2002/08/24 06:51:24  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.109  2002/08/22 16:43:36  lazarus
  MG: improved theme support from Andrew

  Revision 1.108  2002/08/22 16:22:39  lazarus
  MG: started debugging of mouse capturing

  Revision 1.107  2002/08/22 13:45:58  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.106  2002/08/22 12:25:00  lazarus
  MG: fixed mouse events

  Revision 1.105  2002/08/22 07:30:16  lazarus
  MG: freeing more unused GCs

  Revision 1.104  2002/08/21 15:46:08  lazarus
  MG: fixed a mem leak in RestoreDC

  Revision 1.103  2002/08/21 14:44:18  lazarus
  MG: accelerated synedit

  Revision 1.102  2002/08/21 14:06:41  lazarus
  MG: added TDeviceContextMemManager

  Revision 1.101  2002/08/21 13:51:31  lazarus
  MG: removed SaveDC and RestoreDC in ExtTextOut

  Revision 1.100  2002/08/21 13:35:25  lazarus
  MG: accelerations for synedit

  Revision 1.99  2002/08/21 11:29:36  lazarus
  MG: fixed mem some leaks in ide and gtk

  Revision 1.98  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.97  2002/08/21 08:13:38  lazarus
  MG: accelerated new/dispose of gdiobjects

  Revision 1.96  2002/08/21 07:16:59  lazarus
  MG: reduced mem leak of clipping stuff, still not fixed

  Revision 1.95  2002/08/19 20:34:48  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.94  2002/08/17 15:45:34  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.93  2002/08/15 15:46:50  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.92  2002/08/15 13:37:58  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.91  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.90  2002/08/08 18:05:47  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.89  2002/08/08 17:26:39  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.88  2002/08/08 09:07:07  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.87  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.86  2002/08/05 10:45:06  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.85  2002/08/05 08:56:57  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.84  2002/08/05 07:43:29  lazarus
  MG: fixed BadCursor bug and Circle Reference of FixedWidget of csPanel

  Revision 1.83  2002/07/23 07:40:52  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.82  2002/07/20 13:47:04  lazarus
  MG: fixed eventmask for realized windows

  Revision 1.81  2002/07/09 17:18:23  lazarus
  MG: fixed parser for external vars

  Revision 1.80  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.79  2002/06/19 19:46:10  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.78  2002/06/12 12:35:44  lazarus
  MG: fixed apiwidget warnings/criticals

  Revision 1.77  2002/06/11 13:41:11  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.76  2002/06/05 12:33:58  lazarus
  MG: fixed fonts in XLFD format and styles

  Revision 1.75  2002/06/04 19:28:17  lazarus
  MG: cursor is now inverted and can be used with twilight color scheme

  Revision 1.74  2002/06/04 15:17:24  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.73  2002/06/01 08:41:28  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.72  2002/05/27 17:58:42  lazarus
  MG: added command line help

  Revision 1.71  2002/05/24 07:16:34  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.70  2002/05/17 10:45:23  lazarus
  MG: finddeclaration for stupid things like var a:a;

  Revision 1.69  2002/05/16 18:26:08  lazarus
  MG: fixed selection painting of non highlighter

  Revision 1.68  2002/05/10 06:05:57  lazarus
  MG: changed license to LGPL

  Revision 1.67  2002/05/09 12:41:30  lazarus
  MG: further clientrect bugfixes

  Revision 1.66  2002/05/06 08:50:37  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.65  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.64  2002/04/04 12:25:02  lazarus
  MG: changed except statements to more verbosity

  Revision 1.63  2002/03/31 22:01:38  lazarus
  MG: fixed unreleased/unpressed Ctrl/Alt/Shift

  Revision 1.62  2002/03/14 20:28:49  lazarus
  Bug fix for Mattias.
  Fixed spinedit so you can now get the value and set the value.
  Shane

  Revision 1.61  2002/02/25 16:48:13  lazarus
  MG: new IDE window layout system

  Revision 1.60  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLIntf and Graphics was split, so a bunch of files had to be modified.

  Revision 1.59  2002/01/24 15:40:59  lazarus
  MG: deactivated clipboard setting target list for win32

  Revision 1.58  2002/01/21 14:17:47  lazarus
  MG: added find-block-start and renamed find-block-other-end

  Revision 1.57  2002/01/08 16:02:45  lazarus
  Minor changes to TListView.
  Added TImageList to the IDE
  Shane

  Revision 1.56  2002/01/04 21:07:49  lazarus
  MG: added TTreeView

  Revision 1.55  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.54  2001/12/28 11:41:51  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.53  2001/12/27 16:31:28  lazarus
  MG: implemented TCanvas.Arc

  Revision 1.52  2001/12/20 14:41:20  lazarus
  Fixed setfocus for TComboBox and TMemo
  Shane

  Revision 1.51  2001/12/12 14:23:18  lazarus
  MG: implemented DestroyCaret

  Revision 1.50  2001/12/11 16:51:37  lazarus
  Modified the Watches dialog
  Shane

  Revision 1.49  2001/11/14 17:46:59  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.48  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.47  2001/11/09 19:14:25  lazarus
  HintWindow changes
  Shane

  Revision 1.46  2001/10/31 16:29:23  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.45  2001/10/24 00:35:55  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.44  2001/10/16 14:19:13  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.41  2001/09/30 08:34:52  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.40  2001/07/01 23:33:13  lazarus
  MG: added WaitMessage and HandleEvents is now non blocking

  Revision 1.39  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.37  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.36  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.33  2001/04/13 13:22:23  lazarus

       Made fix to buttonglyph to use the correct size of single glyph
       Made fix to StretchBlt to use the correct height and width
       Both of these corrected the Win32 Speedbutton problem        MAH

  Revision 1.32  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.31  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.26  2001/03/19 18:51:57  lazarus
  MG: added dynhasharray and renamed tsynautocompletion

  Revision 1.25  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.22  2001/03/12 12:17:02  lazarus
  MG: fixed random function results

  Revision 1.21  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.20  2001/02/16 19:13:31  lazarus
  Added some functions
  Shane

  Revision 1.19  2001/02/06 18:19:38  lazarus
  Shane

  Revision 1.18  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.17  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.16  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.15  2001/01/23 19:01:10  lazarus
  Fixxed bug in RestoreDC
  Shane

  Revision 1.12  2001/01/12 18:46:50  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.11  2001/01/04 16:12:54  lazarus
  Removed some writelns and changed the property editor for TStrings a bit.
  Shane

  Revision 1.10  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.9  2000/10/09 22:50:33  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.8  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.7  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.6  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.5  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.4  2000/08/07 17:06:39  lazarus
  Slight modification to CreateFontIndirect.
  I check to see if the GdiObject^.GDIFontObject is nil.  If so After the code to retry the weight and slant I added code to retry the Family and Foundry.
  Shane

  Revision 1.3  2000/07/30 21:48:34  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.2  2000/07/23 10:53:41  lazarus
  workaround for possible compiler bug (KEYSTATE), stoppok

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import

  Revision 1.17  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.16  2000/06/04 10:00:33  lazarus
  MWE:
    * Fixed bug #6.

  Revision 1.15  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment

  Revision 1.14  2000/05/14 21:56:12  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.13  2000/05/11 22:04:16  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.12  2000/05/10 22:52:59  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.11  2000/05/10 02:32:34  lazarus
  Put ERRORs and WARNINGs back to writelns.                CAW

  Revision 1.10  2000/05/10 01:45:12  lazarus
  Replaced writelns with Asserts.
  Put ERROR and WARNING messages back to writelns.            CAW

  Revision 1.9  2000/05/09 18:37:02  lazarus
  *** empty log message ***

  Revision 1.8  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane



  Revision 1.7  2000/05/08 15:56:59  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.6  2000/05/08 12:54:20  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane


  Revision 1.5  2000/05/03 00:27:05  lazarus
  MWE:
    + First rollout of the API wizzard.

  Revision 1.4  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.3  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.2  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.1  2000/03/30 22:51:43  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.62  2000/03/30 21:57:44  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.61  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.60  2000/03/28 22:47:49  lazarus
  MWE:
    Started with the blt function family

  Revision 1.59  2000/03/22 18:49:51  lazarus
  Initial work for getting transparent speedbutton glyphs
  Shane

  Revision 1.58  2000/03/22 17:09:30  lazarus
  *** empty log message ***

  Revision 1.57  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.56  2000/03/17 19:19:58  lazarus
  Added Hans Ott's code for TMemo
  Shane

  Revision 1.55  2000/03/17 17:07:00  lazarus
  Added images to speedbuttons
  Shane

  Revision 1.54  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.53  2000/03/15 20:15:32  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.52  2000/03/15 01:09:59  lazarus
  MWE:
    + Removed comment on LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       it does compile (compiler hickup ?)

  Revision 1.51  2000/03/15 00:51:58  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.50  2000/03/14 21:18:23  lazarus
  Added the ability to click on the speedbuttons
  Shane

  Revision 1.48  2000/03/10 18:31:10  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.47  2000/03/09 23:47:58  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.46  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.45  2000/03/07 16:52:58  lazarus
  Fixxed a problem with the main.pp unit determining a new files FORM name.
  Shane

  Revision 1.44  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.43  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.42  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.41  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.40  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.39  2000/02/18 19:38:53  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.38  2000/01/31 20:00:21  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.37  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.36  2000/01/25 23:51:14  lazarus
  MWE:
    Added more Caret functionality.
    Removed old ifdef stuff from the editor

  Revision 1.35  2000/01/25 22:04:27  lazarus
  MWE:
    The first primitive Caret functions are getting visible

  Revision 1.34  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.33  2000/01/22 20:07:47  lazarus
  Some cleanups. It needs much more cleanup than this.
  Worked around a compiler bug (?) in mwCustomEdit.
  Reverted some changes to font generation and increased font size.

  Revision 1.32  2000/01/18 22:18:35  lazarus

  Moved bitmap creation into appropriate place. Cleaned up a bit.
  Finished DeleteObject procedure.

  Revision 1.31  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.30  2000/01/17 23:33:08  lazarus
  MWE:
    fixed: nil pointer reference in DeleteObject
    fixed: some trace info didn't start with 'trace:'

  Revision 1.29  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.28  2000/01/16 23:23:07  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.27  2000/01/14 21:47:04  lazarus
  Commented out SHOWCARET.  Not sure how to implement yet.  Seems like I may need to draw it myself and therefore will need to create a timer and draw a line, then copy the pixmap over the line to erase it.......not sure yet.
  Shane

  Revision 1.26  2000/01/13 22:44:05  lazarus
  MWE:
    Created/updated net gtkwidget for TWinControl decendants
      also improved foccusing on such a control

  Revision 1.25  2000/01/12 22:13:07  lazarus
  Modified ShowCaret.  Still not working.
  Shane

  Revision 1.24  2000/01/11 20:50:32  lazarus
  Added some code for SETCURSOR.  Doesn't work perfect yet but getting there.
  Shane

  Revision 1.22  2000/01/10 21:24:12  lazarus
  Minor cleanup and changes.

  Revision 1.21  2000/01/07 21:14:13  lazarus
  Added code for getwindowlong and setwindowlong.
  Shane

  Revision 1.20  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.19  1999/12/21 00:37:19  lazarus
  MWE:
    Fixed SetTextColor

  Revision 1.18  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.17  1999/12/20 21:01:13  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.16  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.15  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.14  1999/12/14 01:08:56  lazarus
  MWE:
    Started GetTextMetrics

  Revision 1.13  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.12  1999/12/06 20:41:14  lazarus
  Miinor debugging changes.
  Shane

  Revision 1.11  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.10  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.9  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.8  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.7  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.6  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.5  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.4  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

}

