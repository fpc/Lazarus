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
  SYes = 'Yes';
  SNo = 'No';
  SOK = 'OK';
  SCancel = 'Cancel';
  SAbort = 'Abort';
  SRetry = 'Retry';
  SIgnore = 'Ignore';
  
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
function TgtkObject.Arc(DC: HDC; 
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
      WriteLn('WARNING: [TgtkObject.Arc] Uninitialized GC');
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
        gdk_draw_arc(Drawable, GC, 0, X, Y, Width, Height,
                     Angle1 shl 2, Angle2 shl 2);
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
function TgtkObject.AngleChord(DC: HDC;
  x,y,width,height,angle1,angle2 : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.AngleChord] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited AngleChord(DC, x, y, width, height, angle1, angle2);
  end;
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
function TgtkObject.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; 
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
Function TGTKObject.BringWindowToTop(hWnd : HWND): Boolean;
{$IFDEF VerboseFocus}
var
  LCLObject: TControl;
{$ENDIF}
begin
  //hwnd should be a PgtkWidget.
  {$IFDEF VerboseFocus}
  write('TGTKObject.BringWindowToTop hWnd=',HexStr(Cardinal(hWnd),8));
  LCLObject:=TControl(GetLCLObject(Pointer(hWnd)));
  if LCLObject<>nil then
    writeln(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    writeln(' LCLObject=nil');
  {$ENDIF}
  Result := (hWnd<>0);
  if Result then
    gdk_window_raise(GetControlWindow(PgtkWidget(hwnd)));
end;

{------------------------------------------------------------------------------
  Function: CallNextHookEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer;
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
Function TGTKObject.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND;
  Msg : UINT; wParam ,lParam : LongInt) : Integer;
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
  Function: CheckMenuItem
  Params:  hndMenu: HMENU; uIDEnableItem: Integer; bChecked: Boolean
  Returns: Nothing
  

 ------------------------------------------------------------------------------}
function TgtkObject.CheckMenuItem(hndMenu: HMENU; uIDEnableItem: Integer;
  bChecked: Boolean): Boolean;
var
  LCLMenuItem: TMenuItem;
begin
  if GTK_IS_CHECK_MENU_ITEM(Pointer(hndMenu)) then begin
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(hndMenu),bChecked);
    Result:=true;
  end else begin
    LCLMenuItem:=TMenuItem(GetLCLObject(Pointer(hndMenu)));
    if LCLMenuItem<>nil then begin
      LCLMenuItem.RecreateHandle;
      Result := true;
    end else
      Result := false;
  end;
end;

{------------------------------------------------------------------------------
  Function: ClientToScreen
  Params:  Handle : HWND; var P : TPoint
  Returns: Nothing
 ------------------------------------------------------------------------------}
Function TgtkObject.ClientToScreen(Handle : HWND; var P : TPoint) : Boolean;
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
function TgtkObject.ClipboardFormatToMimeType(
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
function TgtkObject.ClipboardGetData(ClipboardType: TClipboardType;
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
      AllID:=gdk_atom_intern('TARGETS',0);
      SelData:=RequestSelectionData(ClipboardWidget,ClipboardType,AllID);
      {writeln('BBB2.2 ',HexStr(Cardinal(SelData.Selection),8),
      ' ',HexStr(Cardinal(ClipboardTypeAtoms[ClipboardType]),8),
      ' SelData.Target=',SelData.Target,' AllID=',AllID,
      ' SelData.TheType=',SelData.TheType,' ',gdk_atom_intern('ATOM',0),
      ' SelData.Length=',SelData.Length,
      ' SelData.Format=',SelData.Format
      );}
      if (SelData.Selection<>ClipboardTypeAtoms[ClipboardType])
      or (SelData.Target<>AllID)
      or (SelData.TheType<>gdk_atom_intern('ATOM',0)) then begin
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
  writeln('[TgtkObject.ClipboardGetData] A ClipboardWidget=',HexStr(Cardinal(ClipboardWidget),8),' Format=',ClipboardFormatToMimeType(FormatID));
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
    if (FormatAtom=gdk_atom_intern('text/plain',1)) then begin
      // text/plain is supported in various formats in gtk
      // The COMPOUND_TEXT format supports internationalization and is therefore
      // preferred even before 'text/plain'
      FormatAtom:=0;
      FormatTry:=gdk_atom_intern('COMPOUND_TEXT',1);
      if IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      if (SupportedCnt=0) then
        FormatAtom:=gdk_atom_intern('COMPOUND_TEXT',1);
      // then check for simple text format 'text/plain'
      FormatTry:=gdk_atom_intern('text/plain',1);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      // then check for simple text format STRING
      FormatTry:=gdk_atom_intern('STRING',1);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      // check for some other formats that can be interpreted as text
      FormatTry:=gdk_atom_intern('FILE_NAME',1);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      FormatTry:=gdk_atom_intern('HOST_NAME',1);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      FormatTry:=gdk_atom_intern('USER',1);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
      // the TEXT format is not reliable, but it should be supported
      FormatTry:=gdk_atom_intern('TEXT',1);
      if (FormatAtom=0) and IsFormatSupported(FormatTry) then
        FormatAtom:=FormatTry;
    end;

    {$IfDef DEBUG_CLIPBOARD}
    writeln('[TgtkObject.ClipboardGetData] B  Format=',ClipboardFormatToMimeType(FormatAtom));
    {$EndIf}
    if FormatAtom=0 then exit;
   
    // request data from owner
    SelData:=RequestSelectionData(ClipboardWidget,ClipboardType,FormatAtom);
    try
      {$IfDef DEBUG_CLIPBOARD}
      writeln('[TgtkObject.ClipboardGetData] C  Length=',SelData.Length);
      {$EndIf}
      if (SelData.Selection<>ClipboardTypeAtoms[ClipboardType])
      or (SelData.Target<>FormatAtom) then
        exit;
    
      // write data to stream
      if (SelData.Data<>nil) and (SelData.Length>0) then begin
        if (FormatID=gdk_atom_intern('text/plain',1)) then begin
          // the lcl expects the return format as simple text
          // transform if necessary
          if FormatAtom=gdk_atom_intern('COMPOUND_TEXT',1) then begin
            CompoundTextCount:=gdk_text_property_to_text_list(SelData.theType,
              SelData.Format,SelData.Data,SelData.Length,@CompoundTextList);
            {$IfDef DEBUG_CLIPBOARD}
            writeln('[TgtkObject.ClipboardGetData] D  CompoundTextCount=',CompoundTextCount);
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
      writeln('[TgtkObject.ClipboardGetData] END');
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
function TGtkObject.ClipboardGetFormats(ClipboardType: TClipboardType;
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
writeln('  IsFormatSupported ',Format,'  ',FormatAtoms[a]);
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
                gdk_atom_intern(PGChar(GtkClipboardFormatName[Format]),1)))
      then begin
        Result:=true;
        exit;
      end;
    Result:=false;
  end;
  
  
begin
{$IfDef DEBUG_CLIPBOARD}
writeln('[TgtkObject.ClipboardGetFormats] A ClipboardWidget=',HexStr(Cardinal(ClipboardWidget),8));
{$EndIf}
  Result:=false;
  Count:=0;
  List:=nil;
  if not (ClipboardType in [ctPrimarySelection,ctSecondarySelection,ctClipboard])
  then exit;
  // request the list of supported formats from the selection owner
  AllID:=gdk_atom_intern('TARGETS',0);
  SelData:=RequestSelectionData(ClipboardWidget,ClipboardType,AllID);
  
  try
{$IfDef DEBUG_CLIPBOARD}
writeln('[TgtkObject.ClipboardGetFormats] A2 ',AllID);
{$EndIf}
    if (SelData.Selection<>ClipboardTypeAtoms[ClipboardType])
    or (SelData.Target<>AllID)
    or (SelData.TheType<>gdk_atom_intern('ATOM',0)) then
      exit;
  
    Cnt:=SelData.Length div (SelData.Format shr 3);
    if (SelData.Data<>nil) and (Cnt>0) then begin
      Count:=Cnt;
      FormatAtoms:=PGdkAtom(SelData.Data);
      // add transformable lcl formats
      // for example: the lcl expects text as 'text/plain', but gtk applications
      // also knows 'TEXT' and 'STRING'. These formats can automagically
      // transformed into the lcl format, so the lcl format is also supported
      // and will be added to the list
    
      AddTextPlain:=false;
      if (not IsFormatSupported(gdk_atom_intern('text/plain',1)))
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
writeln('  ',i,':  ',FormatAtoms[i]);
writeln('         "',ClipboardFormatToMimeType(FormatAtoms[i]),'"');
{$EndIf}
        List[i]:=FormatAtoms[i];
        inc(i);
      end;
    
      // add all lcl formats that the gtk-interface can transform from the
      // supported formats
      if AddTextPlain then begin
        List[i]:=gdk_atom_intern('text/plain',0);
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
           OnRequestProc - TClipboardRequestEvent is defined in LCLLinux.pp
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
function TgtkObject.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
var TargetEntries: PGtkTargetEntry;
  
  function IsFormatSupported(FormatID: integer): boolean;
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
writeln('  AddTargetEntry ',FormatName);
{$EndIf}
    TargetEntries[Index].Target := StrAlloc(Length(FormatName) + 1);
    StrPCopy(TargetEntries[Index].Target, FormatName);
    TargetEntries[Index].Info:=Index;
    inc(Index);
  end;

type
  TAddedFormats = array[TGtkClipboardFormat] of boolean;

{function TgtkObject.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
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
writeln('[TgtkObject.ClipboardGetOwnerShip] A');
{$EndIf}
    ClipboardHandler[ClipboardType]:=nil;
    Result:=false;
    if (ClipboardWidget=nil) or (FormatCount=0) or (Formats=nil) then
    begin
      // end ownership
      if (ClipBoardWidget <> nil) and
         (gdk_selection_owner_get(ClipboardTypeAtoms[ClipboardType]) = GetControlWindow(ClipboardWidget)) then
      begin
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
writeln('[TgtkObject.ClipboardGetOwnerShip] B');
{$EndIf}
    if IsFormatSupported(gdk_atom_intern('text/plain',1)) then
    begin
      // lcl provides 'text/plain' and the gtk-interface will automatically
      // provide some more text formats
      ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]:=
          not IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[gfCOMPOUND_TEXT]),0));
      ClipboardExtraGtkFormats[ClipboardType][gfSTRING]:=not IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[gfSTRING]),0));
      ClipboardExtraGtkFormats[ClipboardType][gfTEXT]:=not IsFormatSupported(
                gdk_atom_intern(PGChar(GtkClipboardFormatName[gfTEXT]),0));
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
writeln('[TgtkObject.ClipboardGetOwnerShip] C');
{$EndIf}
    if gtk_selection_owner_set(ClipboardWidget, ClipboardTypeAtoms[ClipboardType],0)=0 then
      begin
{$IfDef DEBUG_CLIPBOARD}
writeln('[TgtkObject.ClipboardGetOwnerShip] D FAILED');
{$EndIf}
      exit;
    end;
    
{$IfDef DEBUG_CLIPBOARD}
writeln('[TgtkObject.ClipboardGetOwnerShip] YEAH, got it!');
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
function TgtkObject.ClipboardRegisterFormat(
  const AMimeType:String): TClipboardFormat;
var AtomName: PChar;
begin
  if Assigned(Application) then begin
    AtomName:=PChar(AMimeType);
    Result:=gdk_atom_intern(AtomName,0);
  end else
    raise Exception.Create(
      'ERROR: TgtkObject.ClipboardRegisterFormat gdk not initialized');
end;


{------------------------------------------------------------------------------
  Function: CreateBitmap
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  GdiObject: PGdiObject;
  //RawImage: PGDIRawImage;
begin
  Assert(False, Format('Trace:> [TgtkObject.CreateBitmap] Width: %d, Height: %d, Planes: %d, BitCount: %d, BitmapBits: 0x%x', [Width, Height, Planes, BitCount, Longint(BitmapBits)]));

  if (BitCount < 1) or (Bitcount > 32) 
  then begin
    Result := 0;
    WriteLn(Format('ERROR: [TgtkObject.CreateBitmap] Illegal depth %d', [BitCount]));
    Exit;
  end;
  
//write('TgtkObject.CreateBitmap->');
  GdiObject := NewGDIObject(gdiBitmap);
  
  // if the bitcount is the system depth create a Pixmap
  // if depth is 1 then a Bitmap
  // else an image
  
  {if BitCount > 1
  then begin
    Assert(False, Format('Trace:  [TgtkObject.CreateBitmap] gbPixmap', [])); }
    GdiObject^.GDIBitmapType := gbPixmap;
    If BitCount = 1 then
      GdiObject^.GDIBitmapType := gbBitmap;


    If BitCount = 1 then begin
      GdiObject^.GDIBitmapObject := gdk_pixmap_new(nil, Width, Height, BitCount);
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);
    end
    else begin
      GdiObject^.GDIPixmapObject := gdk_pixmap_new(nil, Width, Height, BitCount);
      GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIBitmapObject);
    end;
    If GdiObject^.Visual = nil then
      GdiObject^.Visual := gdk_visual_get_best_with_depth(BitCount)
    else
      gdk_visual_ref(GdiObject^.Visual);

    GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, 1);

    If BitmapBits <> nil then
      LoadFromPixbufData(hBitmap(GdiObject), BitmapBits);

  {end
  else if Bitcount = 1
  then begin
    Assert(False, Format('Trace:  [TgtkObject.CreateBitmap] gbBitmap', []));
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
    Assert(False, Format('Trace:  [TgtkObject.CreateBitmap] gbImage', []));
    GdiObject^.GDIBitmapType := gbImage;
    GdiObject^.GDIRawImageObject := NewGDIRawImage(Width, Height, BitCount);
    GdiObject^.Visual := gdk_visual_get_best_with_depth(BitCount);
    GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, 1);
  end;}

  Result := HBITMAP(GdiObject);
//writeln('[TgtkObject.CreateBitmap] ',HexStr(Result,8));
  Assert(False, Format('Trace:< [TgtkObject.CreateBitmap] --> 0x%x', [Integer(Result)]));
end;


{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
const
  HATCH_NULL      : array[0..7] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00);
  HATCH_BDIAGONAL : array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  HATCH_CROSS     : array[0..7] of Byte = ($08, $08, $08, $FF, $08, $08, $08, $08);
  {This is too fine for a Cross Hatch ($22, $22, $FF, $22, $22, $22, $FF, $22);}
  HATCH_DIAGCROSS : array[0..7] of Byte = ($81, $42, $24, $18, $18, $24, $42, $81);
  HATCH_FDIAGONAL : array[0..7] of Byte = ($01, $02, $04, $08, $10, $20, $40, $80);
  HATCH_HORIZONTAL: array[0..7] of Byte = ($00, $00, $00, $FF, $00, $00, $00, $00);
  HATCH_VERTICAL  : array[0..7] of Byte = ($08, $08, $08, $08, $08, $08, $08, $08);
var
  GObject: PGdiObject;
  sError: String;
begin
  Assert(False, Format('Trace:> [TgtkObject.CreateBrushIndirect]  Style: %d, Color: %8x', [LogBrush.lbStyle, LogBrush.lbColor]));

  sError := '';

//write('CreateBrushIndirect->');
  GObject := NewGDIObject(gdiBrush);
//writeln('[TgtkObject.CreateBrushIndirect] ',HexStr(Cardinal(GObject),8));

  with LogBrush do
  begin
    case lbStyle of
 //     BS_HOLLOW,         // Hollow brush.
       BS_NULL:           // Same as BS_HOLLOW.
        begin
          GObject^.GDIBrushFill := GDK_STIPPLED;
          GObject^.GDIBrushPixmap :=
            gdk_bitmap_create_from_data(nil, @HATCH_NULL, 8, 8);
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
              sError := Format('WARNING: [TgtkObject.CreateBrushIndirect] Got unsupported Hatch %d', [lbHatch]);
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
          if IsValidGDIObject(lbHatch) and (PGdiObject(lbHatch)^.GDIType = gdiBitmap)
          then GObject^.GDIBrushPixmap := PGdiObject(lbHatch)^.GDIBitmapObject
          else sError := 'WARNING: [TgtkObject.CreateBrushIndirect] Got unsupported bitmap';
        end;
        
      else
        sError := Format('WARNING: [TgtkObject.CreateBrushIndirect] Got unsupported Style %d'
                        , [lbStyle]);
    end;

    SetGDIColorRef(GObject^.GDIBrushColor,lbColor);
  end;
  if sError = '' then
    Result := HBRUSH(GObject)
  else begin
    Assert(False, 'Trace:' + sError);
    Result := 0;
    DisposeGDIObject(GObject)
  end;
  Assert(False, Format('Trace:< [TgtkObject.CreateBrushIndirect] Got --> %x', [Result]));
end;

{------------------------------------------------------------------------------
  Function: CreateCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateCaret(Handle: HWND; Bitmap: hBitmap;
  Width, Height: Integer): Boolean;
var
  GTKObject: PGTKObject;
  BMP: PGDKPixmap;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.CreateCaret] Finish');

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
  else Assert(False, 'Trace:WARNING: [TgtkObject.CreateCaret] Got null HWND');
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleBitmap
  Params: DC:
          Width:
          Height:
  Returns:

  Creates a bitmap compatible with the specified device context.
 ------------------------------------------------------------------------------}
function TGTKObject.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
var
  Depth : Longint;
begin
  Assert(False, Format('Trace:> [TgtkObject.CreateCompatibleBitmap] DC: 0x%x, W: %d, H: %d', [DC, Width, Height]));

  Depth := -1;

  if (IsValidDC(DC) and (TDeviceContext(DC).Drawable <> nil))
  then begin
    gdk_window_get_geometry(TDeviceContext(DC).Drawable, nil, nil, nil,
      nil, @Depth);
    If Depth = -1 then
      Depth := gdk_visual_get_system^.Depth;
  end
  else Depth := gdk_visual_get_system^.Depth;

  if Depth <> -1
  then Result := CreateBitmap(Width, Height, 1, Depth, nil)
  else Result := 0;

  Assert(False, Format('Trace:< [TgtkObject.CreateCompatibleBitmap] DC: 0x%x --> 0x%x', [DC, Result]));
end;

function Tgtkobject.InternalGetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
  BitSize : Longint; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;
const
  PadLine : array[0..12] of Byte = (0,0,0,0,0,0,0,0,0,0,1,0,0);
  TempBuffer : array[0..2] of Byte = (0,0,0);
var
  {$IfNDef NoGDKPixbuflib}
  Source : PGDKPixbuf;
  rowstride, PixelPos : Longint;
  Pixels : PByte;
  {$Else}
  Source : PGDKImage;//The MONDO slow way...
  {$EndIf}
  FDIB : TDIBSection;
  X, Y : Longint;
  PadSize, Pos : Longint;
  
  Procedure DataSourceInitialize(Bitmap : PGDIObject; Width : Longint);
  begin
    Source := nil;
    
    case Bitmap^.GDIBitmapType of
      gbBitmap:
        If Bitmap^.GDIBitmapObject <> nil then begin
         {$IfNDef NoGDKPixbuflib}
           Source := gdk_pixbuf_get_from_drawable(nil, Bitmap^.GDIBitmapObject,
             Bitmap^.Colormap,0,StartScan,0,0,Width,StartScan + NumScans);
           rowstride := gdk_pixbuf_get_rowstride(Source);
           Pixels := PByte(gdk_pixbuf_get_pixels(Source));
         {$else}
           gdk_error_trap_push; //try to prevent GDK from killing us...
           Source := gdk_image_get(Bitmap^.GDIBitmapObject, 0, StartScan, Width,
             StartScan + NumScans);
         {$EndIf}
        end;
      gbPixmap:
        If Bitmap^.GDIPixmapObject <> nil then begin
         {$IfNDef NoGDKPixbuflib}
           Source := gdk_pixbuf_get_from_drawable(nil, Bitmap^.GDIPixmapObject,
             Bitmap^.Colormap,0,StartScan,0,0,Width,StartScan + NumScans);
           rowstride := gdk_pixbuf_get_rowstride(Source);
           Pixels := PByte(gdk_pixbuf_get_pixels(Source));
         {$else}
           gdk_error_trap_push; //try to prevent GDK from killing us...
           Source := gdk_image_get(Bitmap^.GDIPixmapObject, StartScan, 0, Width,
             StartScan + NumScans);
         {$EndIf}
        end;
      gbImage :
        If Bitmap^.GDIRawImageObject <> nil then begin
          Writeln('WARNING : [TgtkObject.GetDIBits] support for gdiImage unimplimented!.');
        end;
    end;
  end;

  Function DataSourceGetGDIRGB(Bitmap : PGDIObject; X, Y : Longint) : TGDIRGB;
  {$IfNDef NoGDKPixbuflib}
  begin
     PixelPos := rowstride*Y + X*3;

     If Bitmap <> nil then
       While Bitmap = nil do; //Keep compiler happy..

     With Result do begin
       Red := Pixels[PixelPos + 0];
       Green := Pixels[PixelPos + 1];
       Blue := Pixels[PixelPos + 2];
     end;

  {$else}
  var
    Pixel : Longint;
  begin
    Pixel := 0;

    gdk_error_trap_push;//try to prevent GDK from killing us...

    Pixel := gdk_image_get_pixel(Source, X, Y);

    Result := GDKPixel2GDIRGB(Pixel, Bitmap^.Visual, Bitmap^.Colormap);
  {$EndIf}
  end;

  Procedure DataSourceFinalize;
  begin
  {$IfNDef NoGDKPixbuflib}
    GDK_Pixbuf_Unref(Source);
  {$else}
    gdk_error_trap_push; //try to prevent GDK from killing us...
    gdk_image_destroy(Source);
  {$EndIf}
  end;

  Procedure WriteData(Value : PByte; Size : Longint);
  var
    I : Longint;
  begin
    For I := 0 to Size - 1 do
      PByte(Bits)[Pos + I] := Value[I];
    Inc(Pos, Size);
  end;

begin
  Assert(False, 'trace:[TgtkObject.InternalGetDIBits]');
  Result := 0;
  if IsValidGDIObject(Bitmap)
  then begin
    case PGDIObject(Bitmap)^.GDIType of
      gdiBitmap:
        begin
          FillChar(FDIB, sizeof(FDIB), 0);
          GetObject(Bitmap, SizeOf(FDIB), @FDIB);
          BitInfo.bmiHeader := FDIB.dsBmih;

          With PGDIObject(Bitmap)^, BitInfo.bmiHeader do begin
            If not DIB then begin
              NumScans := biHeight;
              StartScan := 0;
            end;

            If BitSize <= 0 then
              BitSize := SizeOf(Byte)*(Longint(biSizeImage) div biHeight)
                                     *(NumScans + StartScan);
            If MemSize(Bits) <> BitSize then begin
              writeln('WARNING: [TgtkObject.InternalGetDIBits] not enough memory allocated for Bits!');
              exit;
            end;
            Pos := 0;
            PadSize := (Longint(biSizeImage) div biHeight) - biWidth*3;
            DataSourceInitialize(PGDIObject(Bitmap), biWidth);
            If DIB then begin
              for Y := NumScans - 1 downto 0 do begin
                for X := 0 to biwidth - 1 do begin
                  With DataSourceGetGDIRGB(PGDIObject(Bitmap), X, Y) do begin
                    TempBuffer[0]  := Blue;
                    TempBuffer[1] := Green;
                    TempBuffer[2]   := Red;
                  end;
                  WriteData(TempBuffer, 3);
                end;
                WriteData(PadLine, PadSize);
              end;
            end
            else
              for Y := 0 to NumScans - 1 do begin
                for X := 0 to biwidth - 1 do begin
                  With DataSourceGetGDIRGB(PGDIObject(Bitmap), X, Y) do begin
                    TempBuffer[0]  := Blue;
                    TempBuffer[1] := Green;
                    TempBuffer[2]   := Red;
                  end;
                  WriteData(TempBuffer, 3);
                end;
                WriteData(PadLine, PadSize);
              end;
            end;
          DataSourceFinalize;
        end;
      else
        writeln('WARNING: [TgtkObject.InternalGetDIBits] not a Bitmap!');
    end;
  end
  else
    writeln('WARNING: [TgtkObject.InternalGetDIBits] invalid Bitmap!');
  gdk_error_trap_pop;
end;

function Tgtkobject.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT;
  Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Assert(False, 'trace:[TgtkObject.GetDIBits]');
  Result := 0;
  if IsValidGDIObject(Bitmap)
  then begin
    case PGDIObject(Bitmap)^.GDIType of
      gdiBitmap:
        Result := InternalGetDIBits(DC, Bitmap, StartScan, NumScans, -1, Bits,
                                      BitInfo, Usage, True);
      else
        writeln('WARNING: [TgtkObject.GetDIBits] not a Bitmap!');
    end;
  end
  else
    writeln('WARNING: [TgtkObject.GetDIBits] invalid Bitmap!');
end;

function Tgtkobject.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
var
  BitInfo : tagBitmapInfo;
begin
  Assert(False, 'trace:[TgtkObject.GetBitmapBits]');
  Result := 0;
  if IsValidGDIObject(Bitmap)
  then begin
    case PGDIObject(Bitmap)^.GDIType of
      gdiBitmap:
        Result := InternalGetDIBits(0, Bitmap, 0, 0, Count, Bits, BitInfo, 0, False);
      else
        writeln('WARNING: [TgtkObject.GetBitmapBits] not a Bitmap!');
    end;
  end
  else
    writeln('WARNING: [TgtkObject.GetBitmapBits] invalid Bitmap!');
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateCompatibleDC(DC: HDC): HDC;
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

  Assert(False,Format('trace:  [TgtkObject.CreateCompatibleDC] DC: 0x%x --> 0x%x', [Integer(DC), Integer(Result)]));
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TgtkObject.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result:=CreateFontIndirectEx(LogFont,'');
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirectEx
  Params:  const LogFont: TLogFont; const LongFontName: string
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TgtkObject.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
var
  GdiObject: PGdiObject;
  S: String;
  FontNameRegistry, Foundry, FamilyName, WeightName,
  Slant, SetwidthName, AddStyleName, PixelSize,
  PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
  CharSetRegistry, CharSetCoding: string;
  n: Integer;

  procedure LoadFont;
  begin
    S := Format('%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s',
      [FontNameRegistry, Foundry, FamilyName, WeightName,
       Slant, SetwidthName, AddStyleName, PixelSize,
       PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
       CharSetRegistry, CharSetCoding
      ]);

    GdiObject^.GDIFontObject := gdk_font_load(PChar(s));
    //writeln('  Trying "',S,'" Success=',GdiObject^.GDIFontObject<>nil);
  end;

  procedure LoadDefaultFont;
  begin
    DisposeGDIObject(GdiObject);
    GdiObject:=CreateDefaultFont;
  end;

begin
  // For info about xlfd see:
  //    http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html
  // Lets fill in all the xlfd parts. Assume we have scalable fonts

  Result := 0;
  GDIObject := NewGDIObject(gdiFont);

  try
    GdiObject^.LogFont := LogFont;
    
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
    
    //writeln('TgtkObject.CreateFontIndirectEx Name="',LogFont.lfFaceName,'"',
    //' Long="',LongFontName,'" ',IsFontNameXLogicalFontDesc(LongFontName)
    //,' ',ord(LogFont.lfFaceName[0]));
    S:=LongFontName;
    if IsFontNameXLogicalFontDesc(LongFontName) then begin
      FontNameRegistry := ExtractXLFDItem(LongFontName,0);
      Foundry          := ExtractXLFDItem(LongFontName,1);
      FamilyName       := ExtractXLFDItem(LongFontName,2);
      WeightName       := ExtractXLFDItem(LongFontName,3);
      Slant            := ExtractXLFDItem(LongFontName,4);
      SetwidthName     := ExtractXLFDItem(LongFontName,5);
      AddStyleName     := ExtractXLFDItem(LongFontName,6);
      PixelSize        := ExtractXLFDItem(LongFontName,7);
      PointSize        := ExtractXLFDItem(LongFontName,8);
      ResolutionX      := ExtractXLFDItem(LongFontName,9);
      ResolutionY      := ExtractXLFDItem(LongFontName,10);
      Spacing          := ExtractXLFDItem(LongFontName,11);
      AverageWidth     := ExtractXLFDItem(LongFontName,12);
      CharSetRegistry  := ExtractXLFDItem(LongFontName,13);
      CharSetCoding    := ExtractXLFDItem(LongFontName,14);
    end;
    
    with LogFont do
    begin

      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [TgtkObject.CreateFontIndirectEx] No fontname');
        Exit;
      end;

      FamilyName := StrPas(lfFaceName); //StringReplace(FaceName, ' ', '*');
      if AnsiCompareText(FamilyName,'default')=0 then begin
        LoadDefaultFont;
        exit;
      end;

      Assert(False, Format('trace:  [TgtkObject.CreateFontIndirectEx] Name: %s, Height: %d', [FamilyName, lfHeight]));

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

      // SetwidthName := '*';

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
        //       positive and negative heigtht values.
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

      // CharSetRegistry := '*';

      // TODO: Match charset.
      // CharSetCoding := '*';
    end;

  //write('CreateFontIndirect->');
    LoadFont;
    
    if GdiObject^.GDIFontObject = nil
    then begin
      if (WeightName='normal') then begin
        WeightName:='medium';
        LoadFont;
      end else if (WeightName='bold') then begin
        WeightName:='black';
        LoadFont;
      end;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      if (WeightName='medium') then begin
        WeightName:='regular';
        LoadFont;
      end else if (WeightName='black') then begin
        WeightName:='demi bold';
        LoadFont;
      end;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try instead of mono spaced, character cell spaced
      if (Spacing='m') then begin
        Spacing:='c';
        LoadFont;
      end;
    end;
    
    if GdiObject^.GDIFontObject = nil
    then begin
      // try instead of italic  oblique
      if (Slant='i') then begin
        Slant := 'o';
        LoadFont;
      end;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try all weights
      WeightName := '*';
      LoadFont;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try all slants
      Slant := '*';
      LoadFont;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try all spacings
      Spacing := '*';
      LoadFont;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try one height lower
      PixelSize := IntToStr(Abs(LogFont.lfHeight)-1);
      LoadFont;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try one height higher
      PixelSize := IntToStr(Abs(LogFont.lfHeight)+1);
      LoadFont;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try all Familys
      PixelSize := IntToStr(Abs(LogFont.lfHeight));
      FamilyName := '*';
      LoadFont;
    end;

    if GdiObject^.GDIFontObject = nil
    then begin
      // try all Foundrys
      Foundry := '*';
      LoadFont;
    end;

  finally
    if GdiObject^.GDIFontObject = nil
    then begin
  //writeln('[TgtkObject.CreateFontIndirect] ',HexStr(Cardinal(GdiObject),8),'  ',FGDIObjects.Count);
      DisposeGDIObject(GdiObject);
      Result := 0;
    end
    else begin
      Result := HFONT(GdiObject);
    end;

    if Result = 0
    then WriteLn(Format('WARNING: [TgtkObject.CreateFontIndirectEx] NOT found XLFD: <%s>', [S]))
    else Assert(False, Format('Trace:  [TgtkObject.CreateFontIndirectEx] found XLFD: <%s>', [S]));
  end;
end;

{------------------------------------------------------------------------------
  Function: CreatePalette
  Params:  LogPalette
  Returns: a handle to the Palette created


 ------------------------------------------------------------------------------}
function TgtkObject.CreatePalette(const LogPalette: TLogPalette): HPALETTE;
var
  GObject: PGdiObject;
begin
  Assert(False, 'trace:[TgtkObject.CreatePalette]');
  GObject := NewGDIObject(gdiPalette);

  with LogPalette, GObject^ do
  begin
    SystemPalette := False;

    PaletteRealized := False;

    VisualType := GDK_VISUAL_PSEUDO_COLOR;

    PaletteVisual := nil;

    PaletteVisual := gdk_visual_get_best_with_type(VisualType);
    If PaletteVisual = nil then begin
      PaletteVisual := GDK_Visual_Get_System;
      GDK_Visual_Ref(PaletteVisual);
    end;
    PaletteColormap := GDK_Colormap_new(PaletteVisual, 1);
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
function TgtkObject.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  GObject: PGdiObject;
begin
  Assert(False, 'trace:[TgtkObject.CreatePenIndirect]');
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
  Params:  Data: Raw pixmap data (PPGChar fo xpm file)
  Returns: Handle to LCL bitmap

  Creates a bitmap from raw pixmap data.
  If TransColor < 0 the transparency mask will be automatically gnerated.
 ------------------------------------------------------------------------------}
function TgtkObject.CreatePixmapIndirect(const Data: Pointer;
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
  if Window<>nil then
    ColorMap:=gdk_window_get_colormap(Window)
  else
    ColorMap:=gdk_colormap_get_system;
  GdiObject^.GDIPixmapObject :=
    gdk_pixmap_colormap_create_from_xpm_d(Window,Colormap,
           @(GdiObject^.GDIBitmapMaskObject), p, Data);

  gdk_window_get_geometry(GdiObject^.GDIPixmapObject, nil, nil, nil, nil, @Depth);

  If GdiObject^.Visual <> nil then
    GDK_Visual_UnRef(GdiObject^.Visual);

  GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIPixmapObject);

  If GdiObject^.Visual = nil then
    GdiObject^.Visual := gdk_visual_get_best_with_depth(Depth)
  else
    gdk_visual_ref(GdiObject^.Visual);

  If GdiObject^.Colormap <> nil then
    GDK_Colormap_UnRef(GdiObject^.Colormap);

  GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, 1);
  
  GdiObject^.GDIBitmapType:=gbPixmap;
  Result := HBITMAP(GdiObject);
end;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points, NumPts, Winding
  Returns: the handle to the region

  Creates a Polygon, a closed many-sided shaped region. The Points parameter is
  an array of points that give the vertices of the polygon. Winding determines
  what points are going to be included in the region. When Winding is True,
  points are selected by using the Winding fill algorithm. When Winding is
  False, points are selected by using using the even-odd (alternative) fill
  algorithm. NumPts indicates the number of points to use.
  The first point is always connected to the last point.
 ------------------------------------------------------------------------------}
Function TgtkObject.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  Winding : Boolean): HRGN;
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

  If Winding then
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
function TgtkObject.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
var
  R : TGDKRectangle;
  RRGN : PGDKRegion;
  GObject: PGdiObject;
begin
  GObject := NewGDIObject(gdiRegion);
  R.X := X1;
  R.Y := Y1;
  R.Width := X2 - X1;
  R.Height := Y2 - Y1;
  RRGN := GDK_Region_New;
  GObject^.GDIRegionObject := gdk_region_union_with_rect(RRGN,@R);
  gdk_region_destroy(RRGN);
  Result := HRGN(GObject);
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
Function TgtkObject.CombineRgn(Dest, Src1, Src2 : HRGN;
  fnCombineMode : Longint) : Longint;
var
  Continue : Boolean;
  D, S1, S2 : PGDKRegion;
  Tmp1 : PGDKRegion;
  DObj, S1Obj, S2Obj : PGDIObject;
begin
  Result := SIMPLEREGION;
  DObj := PGdiObject(Dest);
  S1Obj := PGdiObject(Src1);
  S2Obj := PGdiObject(Src2);
  Continue := IsValidGDIObject(Dest) and IsValidGDIObject(Src1)
              and IsValidGDIObject(Src2);
  If Not Continue then begin
    WriteLn('WARNING: [TgtkObject.CombineRgn] Invalid HRGN');
    Result := Error;
  end
  else begin
    If DObj^.GDIRegionObject <> nil then begin
      GDK_Region_Destroy(DObj^.GDIRegionObject);
      DObj^.GDIRegionObject:=nil;
    end;
    S1 := S1Obj^.GDIRegionObject;
    S2 := S2Obj^.GDIRegionObject;
    Case fnCombineMode of
      RGN_AND :
        D := gdk_regions_intersect(S1, S2);
      RGN_COPY :
        begin
          Tmp1 := gdk_region_new;
          D := gdk_regions_union(S1, Tmp1);
          gdk_region_destroy(Tmp1);
        end;
      RGN_DIFF :
        D := gdk_regions_subtract(S1, S2);
      RGN_OR :
        D := gdk_regions_union(S1, S2);
      RGN_XOR :
        D := gdk_regions_xor(S1, S2);
      else begin
        Result:= ERROR;
        D := nil;
      end;
    end;
    DObj^.GDIRegionObject := D;
    Result := RegionType(D);
  end;
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
                 but not in the Source.(Clip - RGN)

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
function TgtkObject.ExtSelectClipRGN(dc: hdc; rgn : hrgn;
  Mode : Longint) : Integer;
var
  OldC, Clip,
  Tmp : hRGN;
  X, Y : Longint;
begin
  If not IsValidDC(DC) then
    Result := ERROR;
  if Result <> ERROR
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.ExtSelectClipRGN] Uninitialized GC');
      Result := ERROR;
    end
    else begin
      OldC := CreateRectRGN(0,0,1,1);
      If GetClipRGN(DC, OldC) <= 0 then begin
        Case Mode of
          RGN_COPY:
            begin
              Clip := CreateRectRGN(0,0,1,1);
              Result := CombineRGN(Clip, RGN, RGN, Mode);
              If Result <> ERROR then
                Result := SelectClipRGN(DC, Clip);
              DeleteObject(Clip);
            end;
          RGN_OR,
          RGN_XOR,
          RGN_AND,
          RGN_DIFF:
            begin
              GDK_Window_Get_Size(Drawable, @X, @Y);
              Clip := CreateRectRGN(0,0,X,Y);
              Tmp := CreateRectRGN(0,0,1,1);
              Result := CombineRGN(Tmp, Clip, RGN, mode);
              DeleteObject(Clip);
              SelectClipRGN(DC, Tmp);
              DeleteObject(Tmp);
            end;
          end;
        end
      else
        Result := Inherited ExtSelectClipRGN(dc, rgn, mode);
      DeleteObject(OldC);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: DeleteDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.DeleteDC(hDC: HDC): Boolean;
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
function TgtkObject.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  GDIObjectExists: boolean;
begin
  // Find out if we want to release internal GDI object
  GDIObjectExists:=FGDIObjects.Contains(PGDIObject(GDIObject));
  Result:=GDIObjectExists;
  if GDIObjectExists then begin
    with PGdiObject(GDIObject)^ do
    begin
      case GDIType of
        gdiFont:
          begin
            if GDIFontObject<>nil then gdk_font_unref(GDIFontObject);
          end;
        gdiBrush:
          begin
            if (GDIBrushPixmap <> nil)
            then gdk_bitmap_unref(GDIBrushPixmap);
            
            FreeGDIColor(GDIBrushColor);
          end;
        gdiBitmap:
          begin
            if (GDIBitmapObject <> nil)
            then gdk_bitmap_unref(GDIBitmapObject);
            If Visual <> nil then
              gdk_visual_unref(Visual);
            If Colormap <> nil then
              gdk_colormap_unref(Colormap);
          end;
        gdiPen:
          begin
            FreeGDIColor(GDIPenColor);
          end;
        gdiRegion:
          begin
            if (GDIRegionObject <> nil) then
              gdk_region_destroy(GDIRegionObject);
          end;
        gdiPalette:
          begin
            If PaletteVisual <> nil then
              gdk_visual_unref(PaletteVisual);
            If PaletteColormap <> nil then
              gdk_colormap_unref(PaletteColormap);
            RGBTable.Free;
            IndexTable.Free;
          end;
        else begin
          Result:= false;
          writeln('[TgtkObject.DeleteObject] TODO : Unimplemented GDI type');
          Assert(False, 'Trace:TODO : Unimplemented GDI object in delete object');
        end;
      end;
    end;

    { Dispose of the GDI object }
    //writeln('[TgtkObject.DeleteObject] ',Result,'  ',HexStr(GDIObject,8),'  ',FGDIObjects.Count);
    DisposeGDIObject(PGDIObject(GDIObject));
  end;
end;

{------------------------------------------------------------------------------
  Function: DestroyCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.DestroyCaret(Handle: HWND): Boolean;
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
  else Assert(False, 'Trace:WARNING: [TgtkObject.DestroyCaret] Got null HWND');
end;

{------------------------------------------------------------------------------
  Function: DrawFrameControl
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TgtkObject.DrawFrameControl(DC: HDC; var Rect : TRect;
  uType, uState : Cardinal) : Boolean;
const
  ADJUST_FLAG: array[Boolean] of Integer = (0, BF_ADJUST);
  PUSH_EDGE_FLAG: array[Boolean] of Integer = (EDGE_RAISED, EDGE_SUNKEN);
  PUSH_EDGE_FLAG2: array[Boolean] of Integer = (0, BF_FLAT);
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
          Shadow:=GTK_SHADOW_NONE;
        end else begin
          // button up
          Shadow:=GTK_SHADOW_OUT;
        end;
      end;

      aStyle := GetStyle('button');
      If aStyle = nil then
        aStyle := Widget^.theStyle
      else
        If State = GTK_STATE_SELECTED then
          State := GTK_STATE_ACTIVE;
          
      aDC:=TDeviceContext(DC);
      DCOrigin:=GetDCOffset(aDC);

      If (DFCS_FLAT and uState)<>0 then
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

    Style := GetStyle('checkbox');
    
    If Style = nil then
      Style := GetStyle('gtk_default');

    If Style <> nil then
      Style := gtk_style_attach(gtk_style_ref(Style),aDC.Drawable);

    Widget := GetStyleWidget('checkbox');
    If Widget = nil then
      Widget := GetStyleWidget('default');
    If (Widget <> nil) and (Style <> nil) then begin
      Widget^.Window := aDC.Drawable;
      gtk_paint_check(Style,aDC.Drawable, State,
        Shadow, nil, Widget, 'checkbutton',
        Rect.Left+DCOrigin.X,Rect.Top+DCOrigin.Y,
        Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);
      Result := True;
    end
    else begin
      {$IfNDef Win32}
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
        Assert(False, Format('Trace:  [TgtkObject.DrawFrameControl] DFC_BUTTON --> draw rect = %d,%d,%d,%d',[Rect.Left,Rect.Top,REct.Right,REct.Bottom]));
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
          WriteLn(Format('ERROR: [TgtkObject.DrawFrameControl] Unknown State 0x%x', [uState]));
        end;
      end;
  else  
    WriteLn(Format('ERROR: [TgtkObject.DrawFrameControl] Unknown type %d', [uType]));
  end;
end;

{------------------------------------------------------------------------------
  Function: DrawEdge
  Params:   DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal
  Returns:  Boolean

  Draws one or more edges of a rectangle. The rectangle is the area
  Left to Right-1 and Top to Bottom-1.
 ------------------------------------------------------------------------------}
function TgtkObject.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal;
  grfFlags: Cardinal): Boolean;
Var
  InnerTL, OuterTL,
  InnerBR, OuterBR: TGDKColor;
  BInner, BOuter: Boolean;
  Width, Height: Integer;
  R: TRect;
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TgtkObject.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, Rect.Left, Rect.Top,Rect.Right, Rect.Bottom]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      Assert(False, 'Trace:[TgtkObject.DrawEdge] Uninitialized GC');
      Result := False;
    end
    else begin
      R := Rect;
      Dec(R.Right);
      Dec(R.Bottom);
      
      // try to use the gdk functions, so that the current theme is used
      BInner := False;
      BOuter := False;

      // TODO: changeThis to real colors
      if (edge and BDR_RAISEDINNER) = BDR_RAISEDINNER
      then begin
        InnerTL := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
        InnerBR := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
//        gdk_color_white(gdk_colormap_get_system, @InnerTL);
//        gdk_color_black(gdk_colormap_get_system, @InnerBR);
        BInner := True;
      end;
      if (edge and BDR_SUNKENINNER) = BDR_SUNKENINNER
      then begin
        InnerTL := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
        InnerBR := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
//        gdk_color_black(gdk_colormap_get_system, @InnerTL);
//        gdk_color_white(gdk_colormap_get_system, @InnerBR);
        BInner := True;
      end;
      if (edge and BDR_RAISEDOUTER) = BDR_RAISEDOUTER
      then begin
        OuterTL := AllocGDKColor(GetSysColor(COLOR_BTNFACE));
        OuterBR := AllocGDKColor(GetSysColor(COLOR_WINDOWFRAME));
//        gdk_color_white(gdk_colormap_get_system, @OuterTL);
//        gdk_color_black(gdk_colormap_get_system, @OuterBR);
        BOuter := True;
      end;
      if (edge and BDR_SUNKENOUTER) = BDR_SUNKENOUTER
      then begin
        OuterTL := AllocGDKColor(GetSysColor(COLOR_WINDOWFRAME));
        OuterBR := AllocGDKColor(GetSysColor(COLOR_BTNFACE));
//        gdk_color_black(gdk_colormap_get_system, @OuterTL);
//        gdk_color_white(gdk_colormap_get_system, @OuterBR);
        BOuter := True;
      end;

      SelectedColors := dcscCustom;
      gdk_gc_set_fill(GC, GDK_SOLID);

      // Draw outer rect
      if Bouter
      then with R do
      begin
        gdk_gc_set_foreground(GC, @OuterTL);
        if (grfFlags and BF_TOP) = BF_TOP
        then gdk_draw_line(Drawable, GC, Left, Top, Right, Top);
        if (grfFlags and BF_LEFT) = BF_LEFT
        then gdk_draw_line(Drawable, GC, Left, Top, Left, Bottom);

        gdk_gc_set_foreground(GC, @OuterBR);
        if (grfFlags and BF_BOTTOM) = BF_BOTTOM
        then gdk_draw_line(Drawable, GC, Left, Bottom, Right + 1, Bottom);
        if (grfFlags and BF_RIGHT) = BF_RIGHT
        then gdk_draw_line(Drawable, GC, Right, Top, Right, Bottom + 1);

        InflateRect(R, -1, -1);
      end;

      // Draw inner rect
      if BInner
      then with R do
      begin
        gdk_gc_set_foreground(GC, @InnerTL);
        if (grfFlags and BF_TOP) = BF_TOP
        then gdk_draw_line(Drawable, GC, Left, Top, Right, Top);
        if (grfFlags and BF_LEFT) = BF_LEFT
        then gdk_draw_line(Drawable, GC, Left, Top, Left, Bottom);

        gdk_gc_set_foreground(GC, @InnerBR);
        if (grfFlags and BF_BOTTOM) = BF_BOTTOM
        then gdk_draw_line(Drawable, GC, Left, Bottom, Right + 1, Bottom);
        if (grfFlags and BF_RIGHT) = BF_RIGHT
        then gdk_draw_line(Drawable, GC, Right, Top, Right, Bottom + 1);
  
        InflateRect(R, -1, -1);
      end;

//      gdk_colormap_free_colors(gdk_colormap_get_system, @OuterTL, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @OuterBR, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @InnerTL, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @InnerBR, 1);
      
      //Draw interiour
      if (grfFlags and BF_MIDDLE) = BF_MIDDLE
      then begin
        Width := R.Right - R.Left + 1;
        Height := R.Bottom - R.Top + 1;
        SelectGDKBrushProps(DC);
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        gdk_draw_rectangle(Drawable, GC, 1, R.Left+DCOrigin.X, R.Top+DCOrigin.Y,
                           Width, Height);
      end;

      // adjust rect if needed
      if (grfFlags and BF_ADJUST) = BF_ADJUST
      then Rect := R;

      Result := True;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, Rect.Left, Rect.Top,Rect.Right, Rect.Bottom]));
end;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function Tgtkobject.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
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
  Assert(False, Format('trace:> [TgtkObject.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));

  Result := Longint(IsValidDC(DC));
  if Boolean(Result)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.DrawText] Uninitialized GC');
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
  Assert(False, Format('trace:> [TgtkObject.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));
end;

{------------------------------------------------------------------------------
  Function: EnableMenuItem
  Params: hndMenu:
          uIDEnableItem:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.EnableMenuItem(hndMenu: HMENU; uIDEnableItem: Integer;
  bEnable: Boolean): Boolean;
begin
  if hndMenu <> 0
  then gtk_widget_set_sensitive(pgtkwidget(hndMenu), bEnable);
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: EnableScrollBar
  Params:  Wnd, wSBflags, wArrows
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.EnableScrollBar]');
  //TODO: Implement this;
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: EnableWindow
  Params: hWnd:
          bEnable:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Assert(False, Format('Trace:  [TGTKObject.EnableWindow] hWnd: 0x%x, Enable: %s', [hwnd, BOOL_TEXT[bEnable]]));
  if hWnd <> 0 then
    gtk_widget_set_sensitive(pgtkwidget(hWnd), bEnable);
  Result:=true;
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing
  
  Use Ellipse to draw a filled circle or ellipse.

 ------------------------------------------------------------------------------}
function TgtkObject.Ellipse(DC: HDC; 
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
      WriteLn('WARNING: [TgtkObject.Ellipse] Uninitialized GC');
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
function TgtkObject.ExcludeClipRect(dc: hdc;
  Left, Top, Right, Bottom : Integer) : Integer;
begin
  If not IsValidDC(DC) then
    Result := ERROR;
  if Result <> ERROR
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.ExcludeClipRect] Uninitialized GC');
      Result := ERROR;
    end
    else
      Result := Inherited ExcludeClipRect(DC, Left, Top, Right, Bottom);
  end;
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  LineStart, LineEnd, StrEnd: PChar;
  Width, Height: Integer;
  AY, Num : Integer;
  TXTPt : TPoint;
  //ADC : hDC;
  UseFont : PGDKFont;
  UnRef : Boolean;
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TgtkObject.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFont := GetDefaultFont;
        UnRef := True;
      end
      else begin
        UseFont := CurrentFont^.GDIFontObject;
        UnRef := False;
      end;
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.ExtTextOut] Uninitialized GC');
      Result := False;
    end
    else if UseFont = nil then begin
      WriteLn('WARNING: [TgtkObject.ExtTextOut] Missing Font');
      Result := False;
    end
    else if ((Options and (ETO_OPAQUE+ETO_CLIPPED)) <> 0)
    and (Rect=nil) then begin
      WriteLn('WARNING: [TgtkObject.ExtTextOut] Rect=nil');
      Result := False;
    end else begin
      // TODO: implement other parameters.
      //ADC := SaveDC(DC);
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      if ((Options and ETO_OPAQUE) <> 0) then
      begin
        Width := Rect^.Right - Rect^.Left;
        Height := Rect^.Bottom - Rect^.Top;
        SelectedColors := dcscCustom;
        EnsureGCColor(DC, dccCurrentBackColor, True, False);
        gdk_draw_rectangle(Drawable, GC, 1,
                           Rect^.Left+DCOrigin.X, Rect^.Top+DCOrigin.Y,
                           Width, Height);
      end;
      if (Options and ETO_CLIPPED) <> 0 then
      begin
        X := Rect^.Left;
        Y := Rect^.Top;
        IntersectClipRect(DC, Rect^.Left, Rect^.Top,
                          Rect^.Right, Rect^.Bottom);
      end;
      Num := FindChar(#10,Str,Count);
      AY := Y;
      UpdateDCTextMetric(TDeviceContext(DC));
      TxtPt.X := X;
      {$IfDef Win32}
      TxtPt.Y := AY + DCTextMetric.TextMetric.tmHeight div 2;
      {$Else}
      TxtPt.Y := AY + DCTextMetric.TextMetric.tmAscent;
      {$EndIf}
      SelectGDKTextProps(DC);
      if Num < 0 then begin
        if Count> 0 then
          gdk_draw_text(Drawable, UseFont, GC,
                        TxtPt.X+DCOrigin.X, TxtPt.Y+DCOrigin.Y, Str, Count);
      end else
      Begin  //write multiple lines
        LineStart:=Str;
        StrEnd:=Str+Count;
        while LineStart < StrEnd do begin
          LineEnd:=LineStart+Num;
          if Num>0 then
            gdk_draw_text(Drawable, UseFont, GC,
                          TxtPt.X+DCOrigin.X, TxtPt.Y+DCOrigin.Y, LineStart, Num);
          AY := TxtPt.Y;
          {$IfDef Win32}
            TxtPt.Y := AY + DCTextMetric.TextMetric.tmHeight div 2;
          {$Else}
            TxtPt.Y := AY + DCTextMetric.TextMetric.tmAscent;
          {$EndIf}
          LineStart:=LineEnd+1; // skip #10
          if (LineStart<StrEnd) and (LineStart^=#13) then
            inc(LineStart); // skip #10
          Count:=StrEnd-LineStart;
          Num:=FindChar(#10,LineStart,Count);
          if Num<0 then
            Num:=Count;
        end;
      end;
      //RestoreDC(DC, ADC);
      If UnRef then
        GDK_Font_UnRef(UseFont);
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
end;

{------------------------------------------------------------------------------
  Function: FillRect
  Params: none
  Returns: Nothing

  The FillRect function fills a rectangle by using the specified brush.
  This function includes the left and top borders, but excludes the right and
  bottom borders of the rectangle.
 ------------------------------------------------------------------------------}
function TgtkObject.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  Width, Height: Integer;
  OldCurrentBrush: PGdiObject;
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TgtkObject.FillRect] DC:0x%x; Rect: ((%d,%d)(%d,%d)); brush: %x', [Integer(DC), Rect.left, rect.top, rect.right, rect.bottom, brush]));
  Result := IsValidDC(DC) and IsValidGDIObject(Brush);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.FillRect] Uninitialized GC');
      Result := False;
    end
    else begin
      Width := Rect.Right - Rect.Left;
      Height := Rect.Bottom - Rect.Top;
      // Temporary hold the old brush to 
      // replace it with the given brush
      OldCurrentBrush := CurrentBrush;
      if PGdiObject(Brush)<>OldCurrentBrush then begin
        OldCurrentBrush := CurrentBrush;
        CurrentBrush := PGdiObject(Brush);
        SelectedColors:=dcscCustom;
      end;
      SelectGDKBrushProps(DC);
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      gdk_draw_rectangle(Drawable, GC, 1,
                         Rect.Left+DCOrigin.X, Rect.Top+DCOrigin.Y,
                         Width, Height);
      // Restore current brush
      if PGdiObject(Brush)<>OldCurrentBrush then begin
        SelectedColors:=dcscCustom;
        CurrentBrush := OldCurrentBrush;
      end;

      Result := True;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.FillRect] DC:0x%x; Rect: ((%d,%d)(%d,%d)); brush: %x', [Integer(DC), Rect.left, rect.top, rect.right, rect.bottom, brush]));
end;

{------------------------------------------------------------------------------
  Function: Frame3d
  Params: -
  Returns: Nothing

  Draws a 3d border in GTK native style.
 ------------------------------------------------------------------------------}
function TGtkObject.Frame3d(DC : HDC; var ARect : TRect;
  const FrameWidth : integer; const Style : TBevelCut) : boolean;
  
const GTKShadowType: array[TBevelCut] of integer =
  (GTK_SHADOW_NONE, GTK_SHADOW_IN, GTK_SHADOW_OUT);
  
var
  Widget, ClientWidget: PGtkWidget;
  i : integer;
  DCOrigin: TPoint;
begin
  Result := IsValidDC(DC);
  if Result then
    with TDeviceContext(DC) do
    begin
      if GC = nil then begin
        Result:= False;
      end
      else begin
        Widget:=PGtkWidget(TDeviceContext(DC).Wnd);
        ClientWidget:=GetFixedWidget(Widget);
        if ClientWidget=nil then
          ClientWidget:=Widget;
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        for i:= 1 to FrameWidth do begin
          gtk_draw_shadow(ClientWidget^.thestyle,
                          GetControlWindow(ClientWidget), GTK_STATE_NORMAL,
                          GtkShadowType[Style],
                          ARect.Left+DCOrigin.X, ARect.Top+DCOrigin.Y,
                          ARect.Right - ARect.Left-1, ARect.Bottom-ARect.Top-1);
          InflateRect(ARect, -1, -1);
        end;
      end;
    end;
end;

{------------------------------------------------------------------------------
  Function: GetActiveWindow
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.GetActiveWindow : HWND;
var
  List: PGList;
  Widget: PGTKWidget;
  Window: PGTKWindow;
begin
  List := gdk_window_get_toplevels;

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
          Exit;
        end;
      end;
    end;
    list := g_list_next(list);
  end;

  // If we are here we didn't find anything
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: GetCapture
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetCapture: HWND;
begin
  Result := HWnd(gtk_grab_get_current);
end;

{------------------------------------------------------------------------------
  Function: GetCaretPos
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetCaretPos(var lpPoint: TPoint): Boolean;
var
  //FocusObject: PGTKObject;
  modmask : TGDKModifierType;
begin
{  Assert(False, 'Trace:TODO: [TgtkObject.GetCaretPos] finish');

  FocusObject := PGTKObject(GetFocus);
  Result := FocusObject <> nil;

  if Result
  then begin
//    Assert(False, Format('Trace:[TgtkObject.GetCaretPos] Got focusObject 0x%x --> %s', [Integer(FocusObject), gtk_type_name(FocusObject^.Klass^.theType)]));

    if gtk_type_is_a(gtk_object_type(FocusObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_GetCaretPos(PGTKAPIWidget(FocusObject), lpPoint.X, lpPoint.Y);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else WriteLn('[TgtkObject.GetCaretPos] got focusObject nil');
 }

Assert(False, 'Trace:GetCaretPos');
  gdk_window_get_pointer(nil,@lpPoint.X,@lpPoint.y,@modmask);
Assert(False, 'Trace:GetCaretPos');
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetCharABCWidths        pbd
  Params:  Don't care yet
  Returns: False so that the font cache in the newest mwEdit will use
           TextMetrics info which is working already


 ------------------------------------------------------------------------------}
function TgtkObject.GetCharABCWidths(DC: HDC; p2, p3: UINT;
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
Function TGTKObject.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
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
      if MainWindow=gtk_widget_get_parent_window(Widget) then begin
        // the widget is using its parent window
        // -> adjust the coordinates
        inc(MainOrigin.X,Widget^.Allocation.X);
        inc(MainOrigin.Y,Widget^.Allocation.Y);
      end;
      if ClientWindow<>nil then
        gdk_window_get_origin(ClientWindow,@ClientOrigin.X,@ClientOrigin.Y)
      else begin
        // client widget not realized
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
Function TGTKObject.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
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
    writeln('GetClientRect  Widget=',HexStr(Cardinal(handle),8),
       ' Client=',HexStr(Cardinal(ClientWidget),8),
       ' WindowSize=',ARect.Right,',',ARect.Bottom,
       ' Allocation=',ClientWidget^.Allocation.Width,',',ClientWidget^.Allocation.Height
       );
  end else begin
    writeln('GetClientRect  Widget=',HexStr(Cardinal(handle),8),
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
Function TGTKObject.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
var
  CRect : TGDKRectangle;
  X, Y : Longint;
  DCOrigin: Tpoint;
begin
  If not IsValidDC(DC) then
    Result := ERROR;
  If lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);
  if Result <> ERROR
  then with TDeviceContext(DC) do
  begin
    DCOrigin:=GetDCOffset(TDeviceContext(DC));
    If Not IsValidGDIObject(ClipRegion) then begin
      gdk_window_get_size(Drawable, @X, @Y);  
      lpRect^ := Rect(-DCOrigin.X, -DCOrigin.Y, X, Y);
      Result := SIMPLEREGION;
    end
    else begin
      Result := RegionType(PGDIObject(ClipRegion)^.GDIRegionObject);
      gdk_region_get_clipbox(PGDIObject(ClipRegion)^.GDIRegionObject, 
        @CRect);
      With lpRect^,CRect do begin
        Left   := X-DCOrigin.X;
        Top    := Y-DCOrigin.Y;
        Right  := Left + Width;
        Bottom := Top + Height;
      end;  
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
Function TGTKObject.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
var
  CRect : TGDKRectangle;
begin
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
        Bottom := X + Height;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer
 
  Returns the current Clipping Region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
Function TGTKObject.GetClipRGN(DC : hDC; RGN : hRGN) : longint;
begin
  If not IsValidDC(DC) then
    Result := ERROR;
  if Result <> ERROR
  then with TDeviceContext(DC) do
  begin
    If Not IsValidGDIObject(RGN) then begin
      Result := ERROR;
      WriteLn('WARNING: [TgtkObject.CombineRgn] Invalid HRGN');
    end
    else begin
      If Not IsValidGDIObject(ClipRegion) then begin
        Result := 0;
      end
      else begin
        Result := CombineRGN(RGN, ClipRegion, ClipRegion, RGN_COPY);
        If Result = NULLREGION then
          Result := 0
        else
          If Result <> ERROR then
            Result := 1;  
      end;
    end;  
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
Function TGTKObject.GetCmdLineParamDescForInterface: string;
const
  e = {$IfDef win32}#13+{$EndIf}#10;
begin
  Result:=
    '--gtk-module module   Load the specified module at startup.'+e+
    e+
    '--g-fatal-warnings    Warnings and errors generated by Gtk+/GDK will'+e+
    '                      halt the application.'+e+
    e+
    '--gtk-debug flags     Turn on specific Gtk+ trace/debug messages.'+e+
    e+
    '--gtk-no-debug flags  Turn off specific Gtk+ trace/debug messages.'+e+
    e+
    '--gdk-debug flags     Turn on specific GDK trace/debug messages.'+e+
    e+
    '--gdk-no-debug flags  Turn off specific GDK trace/debug messages.'+e+
    e+
    '--display h:s:d       Connect to the specified X server, where "h" is'+e+
    '                      the hostname, "s" is the server number (usually'+e+
    '                      0), and "d" is the display number (typically'+e+
    '                      omitted). If --display is not specified, the'+e+
    '                      DISPLAY environment variable is used.'+e+
    e+
    '--sync                Call XSynchronize (display, True) after the X'+e+
    '                      server connection has been established. This'+e+
    '                      makes debugging X protocol erros easier,'+e+
    '                      because X request buffering will be disabled and'+e+
    '                      X errors will be received immediatey after the'+e+
    '                      protocol request that generated the error has'+e+
    '                      been processed by the X server.'+e+
    e+
    '--no-xshm             Disable use of the X Shared Memory Extension.'+e+
    e+
    '--name programe       Set program name to "progname". If not'+e+
    '                      specified, program name will be set to'+e+
    '                      ParamStr(0).'+e+
    e+
    '--class classname     Following Xt conventions, the class of a'+e+
    '                      program is the program name with the initial'+e+
    '                      character capitalized. For example, the class'+e+
    '                      name for gimp is "Gimp". If --class is'+e+
    '                      specified, the class of the program will be'+e+
    '                      set to "classname".'+e;
end;

{------------------------------------------------------------------------------
  Function: GetDC
  Params:  none
  Returns: Nothing

  hWnd is any widget.
  The DC will be created for the client area.
 ------------------------------------------------------------------------------}
function TgtkObject.GetDC(hWnd: HWND): HDC;
begin
  Result:=CreateDCForWidget(PGtkWidget(hWnd),nil);
end;

{------------------------------------------------------------------------------
  function TgtkObject.GetDeviceCaps(DC: HDC; Index: Integer): Integer;


 ------------------------------------------------------------------------------}
function TgtkObject.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := -1;
  If DC = 0 then begin
    DC := GetDC(0);
    If DC = 0 then
      exit;
    Result := GetDeviceCaps(DC, Index);
    ReleaseDC(0, DC);
  end;
  if IsValidDC(DC)
  then with TDeviceContext(DC) do
  begin
    Case Index of
      //The important ones I know how to do
      HORZRES : { Horizontal width in pixels }
        If Drawable = nil then
          Result := GetSystemMetrics(SM_CXSCREEN)
        else
          gdk_window_get_geometry(Drawable, nil, nil, @Result, nil, nil);

      VERTRES : { Vertical height in pixels }
        If Drawable = nil then
          Result := GetSystemMetrics(SM_CYSCREEN)
        else
          gdk_window_get_geometry(Drawable, nil, nil, nil, @Result, nil);

      BITSPIXEL : { Number of bits per pixel }
        If Drawable = nil then
          Result := GDK_Visual_Get_System^.Depth
        else
          gdk_window_get_geometry(Drawable, nil, nil, nil, nil, @Result);

      //For Size in MM, MM = (Pixels*100)/(PPI*25.4)
      
      HORZSIZE : { Horizontal size in millimeters }
        Result := Round((GetDeviceCaps(DC, HORZRES) * 100) /
                        (GetDeviceCaps(DC, LOGPIXELSX) * 25.4));

      VERTSIZE : { Vertical size in millimeters }
        Result := Round((GetDeviceCaps(DC, VERTRES) * 100) /
                        (GetDeviceCaps(DC, LOGPIXELSY) * 25.4));

      //So long as gdk_screen_width_mm is acurate, these should be
      //acurate for Screen GDKDrawables. Once we get Metafiles
      //we will also have to add internal support for Papersizes etc..
      
      LOGPIXELSX : { Logical pixels per inch in X }
        Result := Round(gdk_screen_width / (gdk_screen_width_mm / 25.4));

      LOGPIXELSY : { Logical pixels per inch in Y }
        Result := Round(gdk_screen_height / (gdk_screen_height_mm / 25.4));
    end;
  end;
end;

{------------------------------------------------------------------------------
  function GetDeviceSize(DC: HDC; var p: TPoint): boolean;

  Retrieves the width and height of the device context in pixels.
 ------------------------------------------------------------------------------}
function TgtkObject.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
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
      RaiseException('TGTKObject.GetDeviceSize Window=nil');
      {$ENDIF}
      writeln('TgtkObject.GetDeviceSize:',
        ' WARNING: DC ',HexStr(Cardinal(DC),8),' without gdkwindow.',
        ' Widget=',HexStr(Cardinal(wnd),8));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetFocus
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TgtkObject.GetFocus: HWND;
var
  List: PGList;
  Widget: PGTKWidget;
  Window: PGTKWindow;
begin
  List := gdk_window_get_toplevels;

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
          Exit;
        end;
      end;
    end;
    list := g_list_next(list);
  end;

  // If we are here we didn't find anything
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: GetKeyState
  Params:  nVirtKey: The requested key
  Returns: If the function succeeds, the return value specifies the status of
           the given virtual key. If the high-order bit is 1, the key is down;
           otherwise, it is up. If the low-order bit is 1, the key is toggled.

  The GetKeyState function retrieves the status of the specified virtual key.
 ------------------------------------------------------------------------------}
function TgtkObject.GetKeyState(nVirtKey: Integer): Smallint;
const
  KEYSTATE: array[Boolean] of Smallint = (0, -32768 { $8000});
  TOGGLESTATE: array[Boolean] of Smallint = (0, 1);
begin
  case nVirtKey of
    VK_LSHIFT:   nVirtKey := VK_SHIFT;
    VK_LCONTROL: nVirtKey := VK_CONTROL;
    VK_LMENU:    nVirtKey := VK_MENU;
  end;
  Result := KEYSTATE[FKeyStateList.IndexOf(Pointer(nVirtKey)) <> -1];

  // try extended keys
  if Result = 0 
  then begin
    nVirtKey := nVirtKey or KEYMAP_EXTENDED;
    Result := KEYSTATE[FKeyStateList.IndexOf(Pointer(nVirtKey)) <> -1];
  end;
  
  // add toggle
  if Result <> 0 then
    Result := Result or TOGGLESTATE[FKeyStateList.IndexOf(Pointer(
                                             nVirtKey or KEYMAP_TOGGLE)) <> -1];

  //Assert(False, Format('Trace:[TgtkObject.GetKeyState] %d -> 0x%x', [nVirtKey, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
var
  NumColors : Longint;
  BitmapSection : TDIBSECTION;
begin
  Assert(False, 'trace:[TgtkObject.GetObject]');
  Result := 0;
  if IsValidGDIObject(GDIObj)
  then begin
    case PGDIObject(GDIObj)^.GDIType of
      gdiBitmap:
        begin
          Assert(False, 'Trace:FINISH: [TgtkObject.GetObject] gdiBitmap');
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
              case GDIBitmapType of
                gbBitmap:
                  If GDIBitmapObject <> nil then begin
                    GDK_WINDOW_GET_SIZE(GDIBitmapObject, @biWidth, @biHeight);
                    NumColors := 2;
                    biBitCount := 1;
                  end;
                gbPixmap:
                  If GDIPixmapObject <> nil then begin
                    gdk_window_get_geometry(GDIPixmapObject, nil, nil,
                      @biWidth, @biHeight, @biBitCount);
                  end;
                gbImage :
                  If GDIRawImageObject <> nil then
                    With GDIRawImageObject^ do begin
                      biHeight := Height;
                      biWidth := Width;
                      biBitCount := Depth;
                    end;
              end;

              If Visual = nil then begin
                Visual := gdk_visual_get_best_with_depth(biBitCount);
                If Visual = nil then begin//Depth not supported?
                  Visual := gdk_visual_get_system;
                  gdk_visual_ref(Visual);
                end;
                If Colormap <> nil then
                  gdk_colormap_unref(Colormap);
                ColorMap := gdk_colormap_new(Visual, 1);
              end else
                biBitCount := Visual^.Depth;

              If biBitCount < 24 then
                NumColors := Colormap^.Size;

              biSizeImage := (((biBitCount*biWidth+31) shr 5) shl 2)*biHeight;

              If GetSystemMetrics(SM_CXSCREEN) >= biWidth then
                biXPelsPerMeter := GetDeviceCaps(0, LOGPIXELSX)
              else
                biXPelsPerMeter := Round((biWidth / GetSystemMetrics(SM_CXSCREEN)) *
                                          GetDeviceCaps(0, LOGPIXELSX));

              If GetSystemMetrics(SM_CYSCREEN) >= biHeight then
                biYPelsPerMeter := GetDeviceCaps(0, LOGPIXELSY)
              else
                biYPelsPerMeter := Round((biHeight / GetSystemMetrics(SM_CYSCREEN)) *
                                          GetDeviceCaps(0, LOGPIXELSY));

              bmWidth := biWidth;
              bmHeight := biHeight;
              bmBitsPixel := biBitCount;
              
              //Need to retrieve actual Number of Colors if Indexed Image
              if (bmBitsPixel < 24) then begin
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
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiBrush');
        end;
      gdiFont:
        begin
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
        end;
      gdiPen:
        begin
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiPen');
        end;
      gdiRegion:
        begin
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiRegion');
        end;
    else
      WriteLn(Format('WARNING: [TgtkObject.GetObject] Unknown type %d', [Integer(PGDIObject(GDIObj)^.GDIType)]));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetParent
  Params: Handle:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.GetParent(Handle : HWND): HWND;
begin
  //writeln('TGTKObject.GetParent ',HexStr(Cardinal(Handle),8));
  Result:=0;
  if Handle<>0 then
    Result:=HWnd(PGtkWidget(Handle)^.Parent);
end;


{------------------------------------------------------------------------------
  Function: GetProp
  Params: Handle: Str
  Returns: Pointer

 ------------------------------------------------------------------------------}
Function TgtkObject.GetProp(Handle : hwnd; Str : PChar): Pointer;
Begin
  result := gtk_object_get_data(pgtkobject(Handle),Str);
end;


{------------------------------------------------------------------------------
  Function: GetScrollInfo
  Params:  Handle, BarFlag, ScrollInfo
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetScrollInfo(Handle: HWND; BarFlag: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.GetScrollInfo]');
  Result := False;
end;

Function TgtkObject.CreateSystemFont : hFont;
var
  GDIObj : PGDIObject;
begin
  GDIObj := NewGDIObject(gdiFont);
  GDIObj^.GDIFontObject:= GetDefaultFont;
  Result := hFont(GDIObj);
end;


{------------------------------------------------------------------------------
  Function: GetStockObject
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetStockObject(Value: Integer): LongInt;
begin
  Assert(False, Format('Trace:> [TgtkObject.GetStockObject] %d', [Value]));
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
    DEFAULT_GUI_FONT:    // Default font for user interface objects such as menus and dialog boxes.
      begin
        Result := GetStockObject(SYSTEM_FONT);
      end;
(*    OEM_FIXED_FONT:      // Original equipment manufacturer (OEM) dependent fixed-pitch (monospace) font.
      begin
      end;
*)
    SYSTEM_FONT:         // System font. By default, Windows uses the system font to draw menus, dialog box controls, and text. In Windows versions 3.0 and later, the system font is a proportionally spaced font; earlier versions of Windows used a monospace system font.
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
    Assert(False, Format('Trace:TODO: [TgtkObject.GetStockObject] Implement value: %d', [Value]));
  end;
  Assert(False, Format('Trace:< [TgtkObject.GetStockObject] %d --> 0x%x', [Value, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TgtkObject.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) 
  then begin
    Result := 0;
    // raise an exception
    WriteLn(Format('ERROR: [TgtkObject.GetSysColor] Bad Value: %8x Valid Range between 0 and %d', [nIndex, MAX_SYS_COLORS]));
  end
  else Result := SysColorMap[nIndex];
  //Assert(False, Format('Trace:[TgtkObject.GetSysColor] Index %d --> %8x', [nIndex, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetSystemMetrics(nIndex: Integer): Integer;
var
  P : Pointer;
begin
  Assert(False, Format('Trace:> [TgtkObject.GetSystemMetrics] %d', [nIndex]));
  case nIndex of
    SM_ARRANGE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_ARRANGE          ');
      end;
    SM_CLEANBOOT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CLEANBOOT        ');
      end;
    SM_CMOUSEBUTTONS:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXBORDER         ');
      end;
    SM_CYBORDER:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYBORDER         ');
      end;
    SM_CXCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXCURSOR         ');
      end;
    SM_CYCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYCURSOR         ');
      end;
    SM_CXDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
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
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXEDGE           ');
      end;
    SM_CYEDGE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYEDGE           ');
      end;
    SM_CXFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHSCROLL:
      begin
        P := GTK_hscrollbar_new(nil);
        gtk_widget_show(P);
        Result := GTK_Widget(P)^.requisition.Width;
        GTK_Widget_Destroy(P);
      end;
    SM_CYHSCROLL:
      begin
        P := GTK_hscrollbar_new(nil);
        gtk_widget_show(P);
        Result := GTK_Widget(P)^.requisition.Height;
        GTK_Widget_Destroy(P);
      end;
    SM_CXHTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXICON           ');
      end;
    SM_CYICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYICON           ');
      end;
    SM_CXICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMENUSIZE       ');
      end;
    SM_CYMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMENUSIZE       ');
      end;
    SM_CXMIN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMINTRACK       ');
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
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSIZEFRAME      ');
      end;
    SM_CYSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSIZEFRAME      ');
      end;
    SM_CXSMICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSMICON         ');
      end;
    SM_CYSMICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSMICON         ');
      end;
    SM_CXSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVSCROLL:
      begin
        P := GTK_vscrollbar_new(nil);
        gtk_widget_show(P);
        Result := GTK_Widget(P)^.requisition.Width;
        GTK_Widget_Destroy(P);
      end;
    SM_CYVSCROLL:
      begin
        P := GTK_vscrollbar_new(nil);
        gtk_widget_show(P);
        Result := GTK_Widget(P)^.requisition.Height;
        GTK_Widget_Destroy(P);
      end;
    SM_CYCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYCAPTION        ');
      end;
    SM_CYKANJIWINDOW:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
    else Result := 0;
  end;
  Assert(False, Format('Trace:< [TgtkObject.GetSystemMetrics] %d --> 0x%x (%d)', [nIndex, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TgtkObject.GetTextColor(DC: HDC) : TColorRef;
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
function TgtkObject.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
var
  lbearing, rbearing, width, ascent,descent: LongInt;
  UseFont : PGDKFont;
  UnRef : Boolean;
begin
  Assert(False, 'trace:> [TgtkObject.GetTextExtentPoint]');
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      UseFont := GetDefaultFont;
      UnRef := True;
    end
    else begin
      UseFont := CurrentFont^.GDIFontObject;
      UnRef := False;
    end;
    If UseFont = nil then
      WriteLn('WARNING: [TgtkObject.GetTextExtentPoint] Missing font')
    else begin
      gdk_text_extents(UseFont, Str, Count,
                       @lbearing, @rBearing, @width, @ascent, @descent);
      Size.cX := Width;
      //I THINK this is accurate...
      Size.cY := GDK_String_Height(UseFont, Str)
        {$IfNDef Win32} + descent div 2{$EndIf};
      If UnRef then
        GDK_Font_UnRef(UseFont);
    end;
  end;
  Assert(False, 'trace:< [TgtkObject.GetTextExtentPoint]');
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Assert(False, Format('Trace:> TODO FINISH[TgtkObject.GetTextMetrics] DC: 0x%x', [DC]));

  Result := IsValidDC(DC);
  if Result then begin
    UpdateDCTextMetric(TDeviceContext(DC));
    TM:=TDeviceContext(DC).DCTextMetric.TextMetric;
  end;

  Assert(False, Format('Trace:< TODO FINISH[TgtkObject.GetTextMetrics] DC: 0x%x', [DC]));
end;

{------------------------------------------------------------------------------
  Function: GetWindowLong
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
Function TgtkObject.GetWindowLong(Handle : hwnd; int : Integer): Longint;
var
  //Data : Tobject;
  P : Pointer;
begin
  //TODO:Started but not finished
  Assert(False, Format('Trace:> [TgtkObject.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d)', [Handle, int, int]));

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

  Assert(False, Format('Trace:< [TgtkObject.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d) --> 0x%x (%d)', [Handle, int, int, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetWindowOrgEx
  Params:  none
  Returns: Nothing

  Returns the x-coordinates and y-coordinates of the window origin for the
  specified device context.
 ------------------------------------------------------------------------------}
function TgtkObject.GetWindowOrgEx(dc : hdc; var P : TPoint): Integer;
var
  DCOrigin: TPoint;
begin
//  gdk_window_get_deskrelative_origin(pgtkwidget(TDeviceContext(DC).hwnd)^.window, @P.X, @P.Y);
//write('[TgtkObject.GetWindowOrgEx] ',p.x,' ',p.y);
//  gdk_window_get_root_origin(pgtkwidget(TDeviceContext(DC).hwnd)^.window, @P.X, @P.Y);
//write(' / ',p.x,' ',p.y);
  Result := 0;
  P := Point(0,0);
  // ToDo: fix this, when Designer is ready
  If IsValidDC(DC) then
  with TDeviceContext(DC) do begin
    DCOrigin:=GetDCOffset(TDeviceContext(DC));
    if Drawable<>nil then begin
      gdk_window_get_origin(PGdkWindow(Drawable), @P.X, @P.Y);
      inc(P.X,DCOrigin.X);
      inc(P.Y,DCOrigin.Y);
      Result := 1;
    end else begin
      {$IFDEF RaiseExceptionOnNilPointers}
      RaiseException('TGTKObject.GetWindowOrgEx Window=nil');
      {$ENDIF}
      writeln('TgtkObject.GetWindowOrgEx:',
        ' WARNING: DC ',HexStr(Cardinal(DC),8),' without gdkwindow.',
        ' Widget=',HexStr(Cardinal(wnd),8));
    end;
  end;
//writeln(' / ',p.x,' ',p.y);
end;


{------------------------------------------------------------------------------
  Function: GetWindowRect
  Params:  none
  Returns: 0
  
  After the call, Rect will be the control area in screen coordinates.
  That means, Left and Top will be the screen coordinate of the TopLeft pixel
  of the Handle object and Right and Bottom will be the screen coordinate of
  the BottomRight pixel.
 ------------------------------------------------------------------------------}
function TgtkObject.GetWindowRect(Handle: hwnd; var ARect: TRect): Integer;
var
  X, Y, W, H: Integer;
  Widget: PGTKWidget;
  Window: PGdkWindow;
begin
  //Writeln('GetWindowRect');
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
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  returns the current widget Width and Height
 ------------------------------------------------------------------------------}
Function TgtkObject.GetWindowSize(Handle : hwnd;
  var Width, Height: integer): boolean;
begin
  if Handle<>0 then begin
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
function TgtkObject.GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
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
    R, G, B : Byte;
    NewBrush : TLogBrush;
  begin
    R := GetRValue(BeginColor);
    G := GetGValue(BeginColor);
    B := GetBValue(BeginColor);

    R := R + (Position*(GetRValue(EndColor) - R) div TotalSteps);
    G := G + (Position*(GetGValue(EndColor) - G) div TotalSteps);
    B := B + (Position*(GetBValue(EndColor) - B) div TotalSteps);

    With NewBrush do begin
      lbStyle := BS_SOLID;
      lbColor := RGB(R,G,B);
    end;

    If GradientBrush <> 0 then
      LCLLinux.DeleteObject(GradientBrush);
    GradientBrush := LCLLinux.CreateBrushIndirect(NewBrush);
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
            LCLLinux.FillRect(DC, Rect(TL.X, TL.Y + I, BR.X, TL.Y + I + 1),
              UseBrush)
        end
      end
      else begin
        Steps := Min(BR.X - TL.X, MaxSteps);
        for I := 0 to  Steps - 1 do begin
          GetGradientBrush(StartColor, EndColor, I, Steps, UseBrush);
          LCLLinux.FillRect(DC, Rect(TL.X + I, TL.Y, TL.X + I + 1, BR.Y),
            UseBrush);
        end;
      end;
      If UseBrush <> 0 then
        LCLLinux.DeleteObject(UseBrush);
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
function TgtkObject.HideCaret(hWnd: HWND): Boolean;
var
  GTKObject: PGTKObject;
begin
//writeln('[TgtkObject.HideCaret] A');
  Assert(False, Format('Trace:  [TgtkObject.HideCaret] HWND: 0x%x', [hWnd]));
//TODO: [TgtkObject.HideCaret] Finish (in gtkwinapi.inc)

  GTKObject := PGTKObject(HWND);
  Result := GTKObject <> nil;

  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_HideCaret(PGTKAPIWidget(GTKObject));
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else WriteLn('WARNING: [TgtkObject.HideCaret] Got null HWND');

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
function TGTKObject.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
begin
  If not IsValidDC(DC) then
    Result := ERROR;
  if Result <> ERROR
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.IntersectClipRect] Uninitialized GC');
      Result := ERROR;
    end
    else begin
      Result := Inherited IntersectClipRect(DC, Left, Top, Right, Bottom);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.InvalidateRect(aHandle : HWND; Rect : pRect;
  bErase : Boolean) : Boolean;
var
  gdkRect : TGDKRectangle;
  Widget: PGtkWidget;
begin
//  Writeln(format('Rect = %d,%d,%d,%d',[rect^.left,rect^.top,rect^.Right,rect^.Bottom]));
  Result := True;
  gdkRect.X := Rect^.Left;
  gdkRect.Y := Rect^.Top;
  gdkRect.Width := (Rect^.Right - Rect^.Left);
  gdkRect.Height := (Rect^.Bottom - Rect^.Top);
  
  Widget:=GetFixedWidget(PGtkWidget(aHandle));
  if Widget=nil then Widget:=PgtkWidget(aHandle);

{$IfNDef Win32}
  if bErase then
    gtk_widget_queue_clear_area(Widget,
                          gdkRect.X,gdkRect.Y,gdkRect.Width,gdkRect.Height);

  gtk_widget_queue_draw_area(Widget, gdkRect.X,gdkRect.Y,gdkRect.Width,gdkRect.Height);
{$Else}
  if bErase then
    gdk_window_clear_area(GetControlWindow(Widget),
                          gdkRect.X,gdkRect.Y,gdkRect.Width,gdkRect.Height);

  gtk_widget_draw(Widget, @gdkRect);
{$EndIf}  
end;

{------------------------------------------------------------------------------
  Function: KillTimer
  Params: hWnd:
          nIDEvent:
  Returns:

  WARNING: There seems to be a bug in gtk-1.2.x which breaks gtk_timeout_remove
           thus we can't dispose PGtkITimerinfo here (s.a. gtkTimerCB). 
 ------------------------------------------------------------------------------}
function TGTKObject.KillTimer(hWnd : HWND; uIDEvent : cardinal) : boolean;
var
  n : integer;
  TimerInfo : PGtkITimerinfo;
begin
  Assert(False, 'Trace:removing timer!!!');
  n := FTimerData.Count;
  while (n > 0) do begin
    dec (n);
    TimerInfo := PGtkITimerinfo (FTimerData.Items[n]);
    if ((pointer (hWnd) <> nil) and (hWnd = TimerInfo^.Handle))
    and (uIDEvent = TimerInfo^.IDEvent) then
    begin
      {$IFDEF VerboseTimer}
      writeln('TGTKObject.KillTimer TimerInfo=',HexStr(Cardinal(TimerInfo),8),' ID=',uIDEvent,' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
      {$ENDIF}
      gtk_timeout_remove (TimerInfo^.TimerHandle);
      // mark as invalid
      TimerInfo^.TimerHandle:= 0;
      TimerInfo^.TimerFunc := nil;
      FTimerData.Delete(n);
      //FOldTimerData.Add(TimerInfo);
      Dispose (TimerInfo);
    end;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TgtkObject.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
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
        gdk_draw_line(Drawable, GC, PenPos.X+DCOrigin.X, PenPos.Y+DCOrigin.Y,
                      X+DCOrigin.X, Y+DCOrigin.Y);
        PenPos:= Point(X, Y);
      end else
        Result := False;
    end else begin
      WriteLn('WARNING: [TgtkObject.LineTo] Uninitialized GC');
      Result := False;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

{------------------------------------------------------------------------------
  Function: MaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The MaskBlt function copies a bitmap from a source context into a destination 
  context using the specified mask and raster operation.
 ------------------------------------------------------------------------------}
function TgtkObject.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; 
  SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; 
  Rop: DWORD): Boolean; 
begin
  Result:=false;
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
writeln('[MessageButtonClicked] ',Integer(data^),' ',Integer(gtk_object_get_data(PGtkObject(Widget), 'modal_result')));
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
    else WriteLn('Do not close !!!');
  end else Result:= false;    
end;

function TgtkObject.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
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
      gtk_signal_connect(PGtkObject(AButton), 'clicked', TGtkSignalFunc(@MessageButtonClicked), @ADialogResult);
      gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.action_area), AButton);
    end;

begin
  ButtonCount:= 0;
  { Determine which is the default button }
  DefButton:= ((uType and $00000300) shr 8) + 1;
  Assert(False, 'Trace:Default button is ' + IntToStr(DefButton));

  ADialogResult:= 0;
  Dialog:= gtk_dialog_new;
  gtk_signal_connect(PGtkObject(Dialog), 'delete-event', TGtkSignalFunc(@MessageBoxClosed), @ADialogResult);
  gtk_window_set_default_size(PGtkWindow(Dialog), 100, 100);
  ALabel:= gtk_label_new(lpText);
  gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.vbox), ALabel);    
  DialogType:= (uType and $0000000F);
  if DialogType = MB_OKCANCEL 
  then begin
    CreateButton(SOK, IDOK);
    CreateButton(SCancel, IDCANCEL);
  end 
  else begin
    if DialogType = MB_ABORTRETRYIGNORE 
    then begin
      CreateButton(SAbort, IDABORT);
      CreateButton(SRetry, IDRETRY);
      CreateButton(SIgnore, IDIGNORE);
    end 
    else begin
      if DialogType = MB_YESNOCANCEL 
      then begin
        CreateButton(SYes, IDYES);
        CreateButton(SNo, IDNO);
        CreateButton(SCancel, IDCANCEL);
      end 
      else begin
        if DialogType = MB_YESNO 
        then begin
          CreateButton(SYes, IDYES);
          CreateButton(SNo, IDNO);
        end 
        else begin
          if DialogType = MB_RETRYCANCEL 
          then begin
            CreateButton(SRetry, IDRETRY);
            CreateButton(SCancel, IDCANCEL);
          end 
          else begin
            { We have no buttons to show. Create the default of OK button }
            CreateButton(SOK, IDOK);
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
  DestroyWidget(Dialog);
  Result:= ADialogResult;
end;

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Assert(False, Format('trace:> [TgtkObject.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if OldPoint <> nil then OldPoint^ := PenPos;
    PenPos := Point(X, Y);
  end;
  Assert(False, Format('trace:< [TgtkObject.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

{------------------------------------------------------------------------------
  function MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean; override;

  Move the origin of all operations of a DeviceContext.
  For example:
  Moving the Origin to 10,20 and drawing a point to 50,50, results in
  drawing a point to 60,70.
 ------------------------------------------------------------------------------}
function TgtkObject.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
begin
  Result:=IsValidDC(DC);
  if Result then
  with TDeviceContext(DC) do begin
    //writeln('[TgtkObject.MoveWindowOrgEx] B DC=',HexStr(Cardinal(DC),8),
    //  ' Old=',Origin.X,',',Origin.Y,' d=',dX,',',dY,' ');
    inc(Origin.X,dX);
    inc(Origin.Y,dY);
  end;
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
function TgtkObject.PeekMessage(var lpMsg: TMsg; Handle : HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): Boolean;
var
  AMessage: PMsg;
begin 
  //TODO Filtering
  
  Result := FMessageQueue.Count > 0;
  if Result 
  then begin
    AMessage := FMessageQueue.First^.Data;
    lpMsg := AMessage^;
    if (wRemoveMsg and PM_REMOVE) = PM_REMOVE 
    then begin
      if (AMessage^.Message=LM_PAINT) or (AMessage^.Message=LM_GtkPAINT) then
      begin
        FPaintMessages.Remove(FMessageQueue.First);
        // don't free the DC, this is work for the caller
      end;
      FMessageQueue.Delete(FMessageQueue.First);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:   Pie
  Params:   DC,x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use Pie to draw a filled pie-shaped wedge on the canvas.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position. 

 ------------------------------------------------------------------------------}
function TgtkObject.Pie(DC: HDC;
  x,y,width,height,angle1,angle2 : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.Pie] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited Pie(DC, x, y, width, height, angle1, angle2);
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
Function TgtkObject.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
Begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.Polygon] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
  end;
End;

{------------------------------------------------------------------------------
  Method:   TgtkObject.Polygon
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
function TgtkObject.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: Boolean): boolean;
var
  i: integer;
  PointArray: PGDKPoint;
  Tmp, RGN : hRGN;
  ClipRect : TRect;
  DCOrigin: Tpoint;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.Polygon] Uninitialized GC');
      Result := False;
    end
    else begin
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
      if NumPts<=0 then exit;
      
      GetMem(PointArray,SizeOf(TGdkPoint)*(NumPts+1)); // +1 for return line
      for i:=0 to NumPts-1 do begin
        PointArray[i].x:=Points[i].x+DCOrigin.X;
        PointArray[i].y:=Points[i].y+DCOrigin.Y;
      end;

      If (Points[NumPts-1].X <> Points[0].X) or
        (Points[NumPts-1].Y <> Points[0].Y)
      then begin
        Inc(NumPts);
        PointArray[NumPts - 1].x:=PointArray[0].x;
        PointArray[NumPts - 1].y:=PointArray[0].y;
      end;

      // first draw interior in brush color
      SelectGDKBrushProps(DC);

      if Winding then begin
        Tmp := CreateRectRGN(0,0,0,0);
        GetClipRGN(DC, Tmp);
        RGN := CreatePolygonRgn(Points, NumPts, True);
        ExtSelectClipRGN(DC, RGN, RGN_AND);
        DeleteObject(RGN);
        GetClipBox(DC, @ClipRect);
	FillRect(DC, ClipRect, HBrush(CurrentBrush));
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

      FreeMem(PointArray);

      Result := True;
    end;
  end;
end;

function TgtkObject.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
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
      WriteLn('WARNING: [TgtkObject.Polyline] Uninitialized GC');
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
        if (not CurrentPen^.IsNullPen) then
          gdk_draw_lines(Drawable, GC, PointArray, NumPts);
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
function TGTKObject.PostMessage(Handle: HWND; Msg: Cardinal; wParam: LongInt;
  lParam: LongInt): Boolean;
  
  procedure DeletePaintMessageForHandle(hnd: HWnd);
  var
    OldPaintMessage: PLazQueueItem;
    OldMessage: PMsg;
  begin
    if (hnd=0) then exit;
    OldPaintMessage:=FindPaintMessage(hnd);
    if OldPaintMessage<>nil then begin
      // delete paint message from queue
      OldMessage:=PMsg(OldPaintMessage^.Data);
      FPaintMessages.Remove(OldPaintMessage);
      FMessageQueue.Delete(OldPaintMessage);
      if OldMessage^.Message=LM_PAINT then
        ReleaseDC(0,OldMessage^.WParam);
      Dispose(OldMessage);
    end;
  end;
  
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
        if FindPaintMessage(ParentHandle)<>nil then begin
          Result:=true;
        end;
        Parent:=Parent.Parent;
      end;
    end;
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
    // paint messages are the most expensive messages in the LCL
    // A paint message to a control will also repaint all child controls.
    // -> check if there is already a paint message for one of its parents
    // if yes, then skip this message
    {if ParentPaintMessageInQueue then begin
      if AMessage^.Message=LM_PAINT then
        ReleaseDC(0,AMessage^.WParam);
      exit;
    end;}
  
    // delete old paint message to this widget,
    // so that the widget repaints only once
    DeletePaintMessageForHandle(Handle);

    FMessageQueue.AddLast(AMessage);
    FPaintMessages.Add(FMessageQueue.Last);
  end else begin
    FMessageQueue.AddLast(AMessage);
  end;
end;

{------------------------------------------------------------------------------
  Method:   RadialArc
  Params:   DC,x,y,width,height,sx,sy,ex,ey 
  Returns:  Nothing

  Use RadialArc to draw an elliptically curved line with the current Pen. The 
  values sx,sy, and ex,ey represent the starting and ending radial-points 
  between which the Arc is drawn.

------------------------------------------------------------------------------}
function TgtkObject.RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
Begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.RadialArc] Uninitialized GC');
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
function TgtkObject.RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.RadialChord] Uninitialized GC');
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
function TgtkObject.RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.RadialPie] Uninitialized GC');
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
function TgtkObject.RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean;
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
function TgtkObject.RealizePalette(DC: HDC): Cardinal;
begin
  Assert(False, 'Trace:FINISH: [TgtkObject.RealizePalette]');
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
function TgtkObject.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  Left, Top, Width, Height: Integer;
  DCOrigin: TPoint;
begin
  Assert(False, Format('trace:> [TgtkObject.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.Rectangle] Uninitialized GC');
      Result := False;
    end
    else begin
      if X1<=X2 then begin
        Left:=X1;
        Width:=X2 - X1;
      end else begin
        Left:=X2;
        Width:=X1 - X2;
      end;
      if Y1<=Y2 then begin
        Top:=Y1;
        Height:=Y2 - Y1;
      end else begin
        Top:=Y2;
        Height:=Y1 - Y2;
      end;
      // first draw interior in brush color
      SelectGDKBrushProps(DC);
      DCOrigin:=GetDCOffset(TDeviceContext(DC));
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
        
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
end;

{------------------------------------------------------------------------------
  Function: RectVisible
  Params:  dc : hdc; ARect: TRect
  Returns: True if ARect is not completely clipped away.
 ------------------------------------------------------------------------------}
function TgtkObject.RectVisible(dc : hdc; ARect: TRect) : Boolean;
begin
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: RegroupMenuItem
  Params:  hndMenu: HMENU; GroupIndex: integer
  Returns: Nothing

  Move a menuitem into another group
 ------------------------------------------------------------------------------}
function TgtkObject.RegroupMenuItem(hndMenu: HMENU;
  GroupIndex: Integer): Boolean;
var
  RadioGroup: PGSList;
begin
  if GTK_IS_RADIO_MENU_ITEM(Pointer(hndMenu)) then begin
    // set group
    RadioGroup:=GetRadioMenuItemGroup(PGtkRadioMenuItem(hndMenu));
    gtk_radio_menu_item_set_group(PGtkRadioMenuItem(hndMenu),RadioGroup);
    RadioGroup:=gtk_radio_menu_item_group(PGtkRadioMenuItem(hndMenu));
    UpdateRadioGroupChecks(RadioGroup);
    Result:=true;
  end else
    Result:=false;
end;

{------------------------------------------------------------------------------
  Function: ReleaseCapture
  Params:  none
  Returns: True if succesful

  The ReleaseCapture function releases the mouse capture from a window 
  and restores normal mouse input processing.
 ------------------------------------------------------------------------------}
function TgtkObject.ReleaseCapture: Boolean;
begin
  SetCapture(0);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: ReleaseDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
var
  aDC, pSavedDC: TDeviceContext;
begin
//writeln('[TgtkObject.ReleaseDC] ',HexStr(DC,8),'  ',FDeviceContexts.Count);
  Assert(False, Format('trace:> [TgtkObject.ReleaseDC] DC:0x%x', [DC]));
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
      { Release all graphic objects }
      DeleteObject(HGDIObj(aDC.CurrentBrush));
      DeleteObject(HGDIObj(aDC.CurrentPen));
      DeleteObject(HGDIObj(aDC.CurrentFont));
      DeleteObject(HGDIObj(aDC.CurrentBitmap));
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
          writeln('TgtkObject.ReleaseDC: ',E.Message);
        end;
      end;
      
      DisposeDC(aDC);
      Result := 1;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.ReleaseDC] FDeviceContexts DC:0x%x', [DC]));
end;

{------------------------------------------------------------------------------
  Function: RestoreDC
  Params:  none
  Returns: Nothing


-------------------------------------------------------------------------------}
function TgtkObject.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  aDC, pSavedDC: TDeviceContext;
  Count: Integer;
begin
  Assert(False, Format('Trace:> [TgtkObject.RestoreDC] DC:0x%x, SavedDC: %d', [DC, SavedDC]));
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
    
    if (aDC.ClipRegion<>0) and (pSavedDC.ClipRegion <> aDC.ClipRegion) then
    begin
      // clipping region has changed
      // clipping regions are extraordinary gdiobjects. Users can not set them
      // or read them. If a clipping region is changed, it is always created new
      // -> destroy the current clipping region
      DeleteObject(aDC.ClipRegion);
      aDC.ClipRegion := 0;
    end;
      
    if aDC.GC<>nil then begin
      gdk_gc_unref(aDC.GC);
      aDC.GC:=nil;
    end;
    
    Result := CopyDCData(aDC, pSavedDC);
    aDC.SavedContext := pSavedDC.SavedContext;
    pSavedDC.SavedContext := nil;

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
  Assert(False, Format('Trace:< [TgtkObject.RestoreDC] DC:0x%x, Saved: %d --> %s', [Integer(DC), SavedDC, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Function: RightJustifyMenuItem
  Params:  HndMenu: HMenu; bRightJustify: boolean
  Returns: true on success

  Sets left or justification of a menuitem
-------------------------------------------------------------------------------}
function TgtkObject.RightJustifyMenuItem(HndMenu: HMenu;
  bRightJustify: boolean): Boolean;
var
  MenuItemWidget: PGtkMenuItem;
begin
  MenuItemWidget:=PGtkMenuItem(HndMenu);
  if bRightJustify then
    MenuItemWidget^.flag0:=MenuItemWidget^.flag0 or bm_right_justify
  else
    MenuItemWidget^.flag0:=MenuItemWidget^.flag0 and (not bm_right_justify);
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
Function TgtkObject.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Assert(False, Format('trace:> [TgtkObject.RoundRect] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d, RX:%d, RY:%d', [DC, X1, Y1, X2, Y2, RX, RY]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.RoundRect] Uninitialized GC');
      Result := False;
    end
    else
      Result := Inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
  end;
  Assert(False, Format('trace:< [TgtkObject.RoundRect] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d, RX:%d, RY:%d', [DC, X1, Y1, X2, Y2, RX, RY]));
end;

{------------------------------------------------------------------------------
  Function: SaveDc
  Params:  DC: a DC to save
  Returns: 0 if the functions fails otherwise a positive integer identifing 
           the saved DC

  The SaveDC function saves the current state of the specified device 
  context (DC) by copying its elements to a context stack.
-------------------------------------------------------------------------------}
function TgtkObject.SaveDC(DC: HDC): Integer;
var
  aDC, aSavedDC: TDeviceContext;
begin
  Assert(False, Format('Trace:> [TgtkObject.SaveDC]  0x%x', [Integer(DC)]));
  
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
  
  Assert(False, Format('Trace:< [TgtkObject.SaveDC]  0x%x --> %d', [Integer(DC), Result]));
end;

{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params: Handle:
          P:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
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
      if Window<>nil then
        gdk_window_get_origin(Window, @X, @Y)
      else begin
        X:=0;
        Y:=0;
      end;
    end;
  end;

  //writeln('[TGTKObject.ScreenToClient] ',x,',',y,' P=',P.X,',',P.Y);
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
function TgtkObject.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; 
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
Function TgtkObject.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  If not IsValidDC(DC) then
    Result := ERROR;
  if Result <> ERROR then
    with TDeviceContext(DC) do
    begin
      if (GC = nil) and (RGN <> 0)
      then begin
        WriteLn('WARNING: [TgtkObject.SelectClipRGN] Uninitialized GC');
        Result := ERROR;
      end
      else begin
        If (GC = nil) or (RGN = 0) then begin
          DeleteObject(ClipRegion);
          ClipRegion := 0;
          if GC<>nil then
            SelectGDIRegion(DC);
        end
        else
          If IsValidGDIObject(RGN) then begin
            DeleteObject(ClipRegion);
            ClipRegion := CreateRectRGN(0,0,0,0);
            Result := CombineRGN(ClipRegion, RGN, RGN, RGN_COPY);
            SelectGDIRegion(DC);
          end
          else begin
            Result := ERROR;
            WriteLn('WARNING: [TgtkObject.SelectClipRGN] Invalid RGN');
          end;
      end;
    end;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
//var
//  Color: TGdkColor;
begin
  //TODO: Finish this;
  Assert(False, Format('trace:> [TgtkObject.SelectObject] DC: 0x%x', [DC]));

  Result := 0;
  if IsValidDC(DC) and IsValidGDIObject(GDIObj)
  then begin
    case PGdiObject(GDIObj)^.GDIType of
      gdiBitmap:
        with TDeviceContext(DC) do
        begin
          Assert(False, Format('trace:  [TgtkObject.SelectObject] DC: 0x%x, Type: Bitmap', [DC]));
          Result := HBITMAP(CurrentBitmap);
          CurrentBitmap := PGDIObject(GDIObj);
          if GC <> nil then gdk_gc_unref(GC);
          with PGdiObject(GDIObj)^ do
            case GDIBitmapType of
              gbPixmap: Drawable := GDIPixmapObject;
              gbBitmap: Drawable := GDIBitmapObject;
              gbImage:  Drawable := nil;//GDIRawImageObject;
            else
              Drawable := nil;
            end;
          GC := gdk_gc_new(Drawable);

          gdk_gc_set_function(GC, GDK_COPY);
          SelectedColors := dcscCustom;
        end;
      gdiBrush:
        with TDeviceContext(DC), PGdiObject(GDIObj)^ do
        begin
          Assert(False, Format('trace:  [TgtkObject.SelectObject] DC: 0x%x, Type: Brush', [DC]));
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
          Assert(False, Format('trace:  [TgtkObject.SelectObject] DC: 0x%x, Type: Font', [DC]));
          Result := HFONT(CurrentFont);
          CurrentFont := PGDIObject(GDIObj);
          if GC <> nil
          then begin
            gdk_gc_set_font(GC, PGdiObject(GDIObj)^.GDIFontObject);
          end;
          Exclude(DCFlags,dcfTextMetricsValid);
          SelectedColors := dcscCustom;
        end;
      gdiPen:
        with TDeviceContext(DC) do
        begin
          Result := HPEN(CurrentPen);
          CurrentPen := PGDIObject(GDIObj);
          DCFlags:=DCFlags-[dcfPenSelected,dcfPenInvalid];
          if GC <> nil then SelectGDKPenProps(DC);
          SelectedColors := dcscCustom;
        end;
      gdiRegion:
      begin
        with TDeviceContext(DC) do
        begin
          Result := ClipRegion;
          ClipRegion := 0;
          if GC <> nil then SelectClipRGN(DC, GDIObj);
        end;
      end;
    end;
  end;
//writeln('[TgtkObject.SelectObject] GDI=',HexStr(Cardinal(GDIObj),8)
// ,'  Old=',Hexstr(Cardinal(Result),8));
  Assert(False, Format('trace:< [TgtkObject.SelectObject] DC: 0x%x  --> 0x%x', [DC, Result]));
end;

{------------------------------------------------------------------------------
  Function: SelectPalette
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.SelectPalette]');
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
function TGTKObject.SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: LongInt;
  lParam: LongInt): Integer;
var
  AMessage: TLMessage;
  Target: TObject;
  //ParentControl: TWinControl;
  //ParentHandle: HWnd;
begin
  AMessage.Msg := Msg;
  AMessage.WParam := WParam;
  AMessage.LParam := LParam;
  AMessage.Result := 0;
  
  Target := GetLCLObject(Pointer(HandleWnd));

  if Target<>nil then begin
    if (Msg=LM_PAINT) or (Msg=LM_GtkPaint) then begin
      (* MG: old trick. Not used anymore, but it might be, that someday there
             will be component, that works better with this, so it is kept.
      
        The LCL repaints controls in a top-down hierachy. But the gtk sends
        gtkdraw events bottom-up. So, controls at the bottom are repainted
        many times. To avoid this the queue is checked for LM_PAINT messages
        for the parent control. If there is a parent LM_PAINT, this message
        is ignored.
      {if (Target is TControl) then begin
        ParentControl:=TControl(Target).Parent;
        while ParentControl<>nil do begin
          ParentHandle:=TWinControl(ParentControl).Handle;
          if FindPaintMessage(ParentHandle)<>nil then begin
            if Msg=LM_PAINT then
              ReleaseDC(0,AMessage.WParam);
            exit;
          end;
          ParentControl:=ParentControl.Parent;
        end;
      end;} *)
      if Msg=LM_GtkPAINT then begin
        // convert LM_GtkPAINT to LM_PAINT
        AMessage.Msg := LM_PAINT;
        AMessage.WParam := GetDC(THandle(HandleWnd));
      end;
    end;

    // deliver it
    Result := DeliverMessage(Target, AMessage);
    
    if (AMessage.Msg=LM_PAINT) and (AMessage.WParam<>0) then begin
      // free DC
      ReleaseDC(0,AMessage.WParam);
      
      if (csDesigning in TComponent(Target).ComponentState)
      and (TObject(Target) is TWinControl) then
        SendPaintMessagesForInternalWidgets(TWinControl(Target));
    end;
  end;
end;

{------------------------------------------------------------------------------
  function SetActiveWindow(Handle: HWND): HWND;
  
  
------------------------------------------------------------------------------}
function TgtkObject.SetActiveWindow(Handle: HWND): HWND;
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
function TgtkObject.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Assert(False, Format('trace:> [TgtkObject.SetBKColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := CLR_INVALID;
  if IsValidDC(DC)
  then begin
    with TDeviceContext(DC) do
    begin
      Result := CurrentBackColor.ColorRef;
      SetGDIColorRef(CurrentBackColor,Color);
    end;

  end;
  Assert(False, Format('trace:< [TgtkObject.SetBKColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Function: SetBkMode
  Params: DC:
          bkMode:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function TGTKObject.SetComboMinDropDownSize(Handle: HWND;
    MinItemsWidth, MinItemsHeight: integer): boolean;

 ------------------------------------------------------------------------------}
Function TGTKObject.SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
var
  ComboWidget: PGtkCombo;
  DropDownWidget, ListWidget, FirstChildWidget: PGtkWidget;
  FirstChild: PGList;
  CurX, CurY, CurWidth, CurHeight, CurItemHeight, BorderX, BorderY,
  NewWidth, NewHeight: integer;
  ComboPopup: PGtkScrolledWindow;
begin
  Result:=true;
  if not (GtkWidgetIsA(PgtkWidget(Handle),GTK_COMBO_TYPE)) then
    RaiseException('TGTKObject.SetComboMinDropDownSize invalid handle');

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
      CurItemHeight:=FirstChildWidget^.Allocation.Height;
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
  //writeln('NewWidth=',NewWidth,' NewHeight=',NewHeight,' CurWidth=',CurWidth,' CurHeight=',CurHeight);
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
function TgtkObject.SetCapture(Value: Longint): Longint;
{$IfDef VerboseMouseCapture}
var
  Sender : TObject;
  CurMouseCaptureHandle: PGtkWidget;
{$EndIf}
begin
  Assert(False, Format('Trace:> [TgtkObject.SetCapture] 0x%x', [Value]));
  {$IfDef VerboseMouseCapture}
  if Value<>0 then
    Sender:=GetLCLObject(Pointer(Value))
  else
    Sender:=nil;
  write('TgtkObject.SetCapture New=',HexStr(Cardinal(Value),8),' ');
  if Sender=nil then
    writeln('Sender=nil')
  else
    writeln('Sender=',TControl(Sender).Name,':',Sender.ClassName);

  CurMouseCaptureHandle:=gtk_grab_get_current;
  writeln('  gtk=',HexStr(Cardinal(CurMouseCaptureHandle),8),
    ' MouseCaptureWidget=',HexStr(Cardinal(MouseCaptureWidget),8));
  {$EndIf}

  // return old capture handle
  Result := GetCapture;

  // check that the widget is a widget with a LCL control
  if (Value<>0) and (GetLCLObject(Pointer(Value))=nil) then exit;

  if Result<>Value then begin
    // capture changes
  
    // If the gtk-interface has grabbed the mouse, it is somewhere in the stack
    // of grabs. The gtk uses a grab stack to handle parent-child chains of
    // mouse events. But we stop this chain anyway, the LCL can set and release
    // mouse captures at any time and X can freeze, when a grab is not realeased
    // and the window is destroyed.
    // -> remove all grabs
    ReleaseMouseCapture(false);

    // grab
    if (Value<>0) then begin
      {$IfDef ActivateMouseCapture}
      gtk_grab_add(PgtkWidget(Value));
      {$EndIf}
    end;
    {$IfDef VerboseMouseCapture}
    writeln('TgtkObject.SetCapture RESULT: gtk=',HexStr(Cardinal(gtk_grab_get_current),8));
    {$EndIf}
  end;

  UpdateMouseCaptureControl;
end;

{------------------------------------------------------------------------------
  Function: SetCaretPos
  Params:  new position x, y 
  Returns: true on success

 ------------------------------------------------------------------------------}
function TgtkObject.SetCaretPos(X, Y: Integer): Boolean;
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
function TgtkObject.SetCaretPosEx(Handle: HWNd; X, Y: Integer): Boolean;
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
function TgtkObject.SetCaretRespondToFocus(handle: HWND;
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
function TgtkObject.SetFocus(hWnd: HWND): HWND;
var
  TopLevel: PGTKWidget;
  Widget, ImplWidget: PGtkWidget;
  {$IfDef VerboseFocus}
  LCLObject, AWinControl: TWinControl;
  {$EndIf}
begin
  if hWnd=0 then exit;
  Widget:=PGtkWidget(hWnd);
  {$IfDef VerboseFocus}
  writeln('');
  write('[TgtkObject.SetFocus] A hWnd=',HexStr(Cardinal(hWnd),8));
  LCLObject:=TWinControl(GetLCLObject(Widget));
  if LCLObject<>nil then
    writeln(' LCLObject=',LCLObject.Name,':',LCLObject.ClassName)
  else
    writeln(' LCLObject=nil');
  {$EndIf}
  if hwnd = 0 then
    Result := 0
  else begin
    // return the old focus handle
    Result := GetFocus;
    TopLevel := gtk_widget_get_toplevel(Widget);
    {$IfDef VerboseFocus}
    write('[TgtkObject.SetFocus] B hWnd=',HexStr(Cardinal(hWnd),8));
    write(' TopLevel=',HexStr(Cardinal(TopLevel),8));
    write(' OldFocus=',HexStr(Cardinal(Result),8));
    AWinControl:=TWinControl(GetParentLCLObject(PGtkWidget(Result)));
    if AWinControl<>nil then
      write(' OldLCLParent=',AWinControl.Name,':',AWinControl.ClassName)
    else
      write(' OldLCLParent=nil');
    writeln('');
    {$EndIf}
    if PGtkWidget(GetFocus)=Widget then exit;
    
    if GtkWidgetIsA(TopLevel, gtk_window_get_type)
    then begin
      {$IfDef VerboseFocus}
      AWinControl:=TwinControl(GetParentLCLObject(PGtkWindow(TopLevel)^.focus_widget));
      write('[TgtkObject.SetFocus] C  TopLevel is a gtkwindow ');
      write(' focus_widget=',HexStr(Cardinal(PGtkWindow(TopLevel)^.focus_widget),8));
      if AWinControl<>nil then
        write(' LCLParent=',AWinControl.Name,':',AWinControl.ClassName)
      else
        write(' LCLParent=nil');
      writeln('');
      {$EndIf}
      // TopLevel is a gtkwindow
      if GTK_WIDGET_CAN_FOCUS(TopLevel) then begin
        // TopLevel window can focus
        {$IfDef VerboseFocus}
        writeln('[TgtkObject.SetFocus] D  TopLevel window can focus');
        {$EndIf}
        gtk_window_set_focus(PGTKWindow(TopLevel), Widget)
      end
      else begin
        // TopLevel window can not focus
        {$IfDef VerboseFocus}
        writeln('[TgtkObject.SetFocus] E  TopLevel window can NOT focus');
        writeln('[TgtkObject.SetFocus] F ',
        ' Widget can focus=',GTK_WIDGET_CAN_FOCUS(Widget),
        ', is realized=',GTK_WIDGET_REALIZED(Widget),
        ', is mapped=',GTK_WIDGET_MAPPED(Widget)
        );
        {$EndIf}
        if GtkWidgetIsA(Widget, gtk_combo_get_type)
        then begin
          // handle is a gtk combo
          gtk_widget_grab_focus(PgtkWidget(PGtkCombo(Widget)^.entry));
          {$IfDef VerboseFocus}
          writeln('[TgtkObject.SetFocus] H Entry=',HexStr(Cardinal(PGtkCombo(Widget)^.entry),8),
          ' has focus=',gtk_widget_has_focus(PgtkWidget(PGtkCombo(Widget)^.entry)));
          {$EndIf}
        end
        else begin
          ImplWidget:= GetWidgetInfo(Widget, true)^.ImplementationWidget;
          if ImplWidget <> nil then begin
            {$IfDef VerboseFocus}
            writeln('[TgtkObject.SetFocus] I CoreChild=',HexStr(Cardinal(ImplWidget),8),
              ' Widget can focus=',GTK_WIDGET_CAN_FOCUS(ImplWidget),
              ', is realized=',GTK_WIDGET_REALIZED(ImplWidget),
              ', is mapped=',GTK_WIDGET_MAPPED(ImplWidget)
              );
            {$EndIf}
            gtk_widget_grab_focus(ImplWidget)
          end else begin
            gtk_widget_grab_focus(Widget);
            {$IfDef VerboseFocus}
            writeln('[TgtkObject.SetFocus] J has focus=',gtk_widget_has_focus(Widget));
            {$EndIf}
          end;  
        end;
      end;
    end
    else begin
      if GTK_WIDGET_CAN_FOCUS(Widget) then begin
        gtk_widget_grab_focus(Widget);
      end;
    end;
  end;
  {$IfDef VerboseFocus}
  write('[TgtkObject.SetFocus] END hWnd=',HexStr(Cardinal(hWnd),8));
  ImplWidget:=PGtkWidget(GetFocus);
  write(' NewFocus=',HexStr(Cardinal(ImplWidget),8));
  AWinControl:=TWinControl(GetParentLCLObject(ImplWidget));
  if AWinControl<>nil then
    write(' NewLCLParent=',AWinControl.Name,':',AWinControl.ClassName)
  else
    write(' NewLCLParent=nil');
  writeln('');
  {$EndIf}
end;

Function TgtkObject.SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
Begin
  gtk_object_set_data(pGTKObject(handle),Str,data);
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: SetScrollInfo
  Params:  none
  Returns: The old position value


 ------------------------------------------------------------------------------}
function TgtkObject.SetScrollInfo(Handle : HWND; SBStyle : Integer; 
  ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
const
  POLICY: array[BOOLEAN] of TGTKPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
var
  Adjustment: PGtkAdjustment;
  Scroll : PGTKWidget;
begin
//  Assert(False, 'Trace:[TgtkObject.SetScrollInfo]');
  with ScrollInfo do Assert(False, Format('Trace:> [TgtkObject.SetScrollInfo] Mask:0x%x, Min:%d, Max:%d, Page:%d, Pos:%d', [fMask, nMin, nMax, nPage, nPos]));

  Result := 0;
  if (Handle <> 0)
  then begin
    Adjustment := nil;
    Scroll := GTK_Object_Get_Data(PGTKObject(Handle), 'scroll_area');
    If (Scroll = nil) or not gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
      gtk_scrolled_window_get_type)
    then
      Scroll := PGTKWidget(Handle);

    case SBStyle of
      SB_HORZ:
        If gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
         gtk_scrolled_window_get_type)
        then
          Adjustment := gtk_scrolled_window_get_hadjustment(
                           PGTKScrolledWindow(Scroll))
        else
        if gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
          gtk_hscrollbar_get_type) 
        then
          Adjustment :=  PgtkhScrollBar(Scroll)^.Scrollbar.Range.Adjustment
        else  //clist
        if gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
          gtk_clist_get_type)
        then
          Adjustment :=  {$IfDef Win32}nil{$Else}gtk_clist_get_hadjustment(PgtkCList(Scroll)){$EndIf};

      SB_VERT:
        If gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
         gtk_scrolled_window_get_type)
        then
          Adjustment := gtk_scrolled_window_get_vadjustment(
                           PGTKScrolledWindow(Scroll))
        else
        if gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
          gtk_vscrollbar_get_type) 
        then
          Adjustment :=  PgtkvScrollBar(Scroll)^.Scrollbar.Range.Adjustment
        else  //clist
        if gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
          gtk_clist_get_type)
        then
          Adjustment := {$IfDef Win32}nil{$Else}gtk_clist_get_vadjustment(PgtkCList(Scroll)){$EndIf};

      SB_CTL:
        if gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)), gtk_range_get_type)
        then begin
          Adjustment := gtk_range_get_adjustment(PGTKRange(Scroll));
        end;
        
    end;

    if Adjustment <> nil
    then with ScrollInfo, Adjustment^ do begin
      Result := Round(Value);
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
      
      {writeln('');
      writeln('[TgtkObject.SetScrollInfo] Result=',Result,
      ' Lower=',round(Lower),
      ' Upper=',round(Upper),
      ' Page_Size=',round(Page_Size),
      ' Page_Increment=',round(Page_Increment),
      ' bRedraw=',bRedraw,
      ' Handle=',HexStr(Cardinal(Handle),8));}

      // do we have to set this allways ?
      if bRedraw then
      begin
        if  (Handle <> 0) then
        begin
          if gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
                           gtk_scrolled_window_get_type)
          then
          begin
            if SBStyle in [SB_BOTH, SB_HORZ]
              then gtk_object_set(PGTKObject(Scroll), 'hscrollbar_policy',
                                  [POLICY[bRedraw], nil]);
            if SBStyle in [SB_BOTH, SB_VERT]
              then gtk_object_set(PGTKObject(Scroll), 'vscrollbar_policy',
                                  [POLICY[bRedraw], nil]);
          end
          else
          begin
            if (SBSTYLE = SB_CTL)
            and gtk_type_is_a(gtk_object_type(PGTKObject(Scroll)),
                              gtk_widget_get_type)
            then
              gtk_widget_show(PGTKWidget(Scroll))
            else
              gtk_widget_hide(PGTKWidget(Scroll))
          end;
        end;
        {writeln('');
        writeln('TgtkObject.SetScrollInfo: ',
        ' lower=',round(lower),'/',nMin,
        ' upper=',round(upper),'/',nMax,
        ' value=',round(value),'/',nPos,
        ' step_increment=',round(step_increment),'/',1,
        ' page_increment=',round(page_increment),'/',nPage,
        ' page_size=',round(page_size),'/',nPage,
        '');}

        gtk_adjustment_changed(Adjustment);
      end;
    end;
  end;
  with ScrollInfo do Assert(False, Format('Trace:> [TgtkObject.SetScrollInfo] --> %d', [Result]));
end;

{------------------------------------------------------------------------------
  Function: SetSysColors
  Params:  cElements: the number of elements
           lpaElements: array with element numbers
           lpaRgbValues: array with colors
  Returns: 0 if unsuccesful 

  The SetSysColors function sets the colors for one or more display elements.
 ------------------------------------------------------------------------------}
function TgtkObject.SetSysColors(cElements: Integer; const lpaElements;
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
    //Assert(False, Format('Trace:[TgtkObject.SetSysColor] Index %d (%8x) --> %8x', [PLongArray(lpaElements)^[n], SysColorMap[PLongArray(lpaElements)^[n]], PLongArray(lpaRgbValues)^[n]]));
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
Function TGTKObject.SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
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
function TgtkObject.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Assert(False, Format('trace:> [TgtkObject.SetTextColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := CLR_INVALID;
  if IsValidDC(DC)
  then begin
    with TDeviceContext(DC) do
    begin
      Result := CurrentTextColor.ColorRef;
      SetGDIColorRef(CurrentTextColor,Color);
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.SetTextColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Function: SetTimer
  Params: hWnd:
          nIDEvent:
          uElapse:
          lpTimerFunc:
  Returns: a GTK-timer id

  This function will create a GTK timer object and associate a callback to it.

  Design: Currently only a callback to the TTimer class is implemented.
 ------------------------------------------------------------------------------}
function TGTKObject.SetTimer(hWnd: HWND; nIDEvent, uElapse: integer;
  lpTimerFunc: TFNTimerProc) : integer;
var
  PTimerInfo: PGtkITimerinfo;
begin
  KillTimer(hWnd,nIDEvent);
  
  if ((hWnd = 0) or (lpTimerFunc = nil) or (uElapse<1))
  then
    Result := 0
  else begin
    New (PTimerInfo);
    Fillchar(PTimerInfo^,SizeOf(TGtkITimerinfo),0);
    PTimerInfo^.Handle    := hWND;
    PTimerInfo^.IDEvent   := nIDEvent;
    PTimerInfo^.TimerFunc := lpTimerFunc;
    FTimerData.Add (PTimerInfo);
    {$IFDEF VerboseTimer}
    writeln('TGTKObject.SetTimer ',HexStr(Cardinal(PTimerInfo),8),' ',nIDEvent,' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
    {$ENDIF}
    Result:= gtk_timeout_add(uElapse, @gtkTimerCB, PTimerInfo);
    PTimerInfo^.TimerHandle:=Result;
    if Result=0 then begin
      KillTimer(hWnd,nIDEvent);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Procedure: SetWindowLong
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SetWindowLong(Handle: HWND; Idx: Integer;
  NewLong: Longint): LongInt;
begin
  //TODO: Finish this;
  Assert(False, Format('Trace:> [TgtkObject.SETWINDOWLONG] HWND: 0x%x, idx: 0x%x(%d), Value: 0x%x(%d)', [Handle, idx, idx, newlong, newlong]));
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
  Assert(False, Format('Trace:< [TgtkObject.SETWINDOWLONG] HWND: 0x%x, idx: 0x%x(%d), Value: 0x%x(%d) --> 0x%x(%d)', [Handle, idx, idx, newlong, newlong, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function TgtkObject.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer;
    var OldPoint: TPoint) : Boolean;

  Sets the x-coordinates and y-coordinates of the window origin for the
  specified device context.
 ------------------------------------------------------------------------------}
Function TgtkObject.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer;
  var OldPoint: TPoint) : Boolean;
begin
  //writeln('[TgtkObject.SetWindowOrgEx] ',NewX,' ',NewY);
  GetWindowOrgEx(DC,OldPoint);
  Result := MoveWindowOrgEx(DC,NewX-OldPoint.X,NewY-OldPoint.Y);
end;

{------------------------------------------------------------------------------
  function TgtkObject.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
    X, Y, cx, cy: Integer; uFlags: UINT): Boolean;

 ------------------------------------------------------------------------------}
function TgtkObject.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
  X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
//var Widget: PGTKWidget;
begin
//writeln('[TgtkObject.SetWindowPos] Top=',hWndInsertAfter=HWND_TOP);
{  Widget := GetFixedWidget(pgtkwidget(hWnd));
  if Widget = nil then Widget := pgtkwidget(hWnd); 
  case hWndInsertAfter of
    HWND_BOTTOM: ;  //gdk_window_lower(Widget^.Window);
    HWND_TOP:    gtk_window_set_position(PGtkWindow(hWnd),GTK_WIN_POS_CENTER);
                //gdk_window_raise(Widget^.Window);
  end;
}
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: ShowCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ShowCaret(hWnd: HWND): Boolean;
var
  GTKObject: PGTKObject;
begin
  Assert(False, Format('Trace:> [TgtkObject.ShowCaret] HWND: 0x%x', [hWnd]));

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
  else WriteLn('WARNING: [TgtkObject.ShowCaret] Got null HWND');

  Assert(False, Format('Trace:< [TgtkObject.ShowCaret] HWND: 0x%x --> %s', [hWnd, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Function:  ShowScrollBar
  Params:  Wnd, wBar, bShow
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
const
  POLICY: array[BOOLEAN] of TGTKPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
begin
  Assert(False, 'trace:[TgtkObject.ShowScrollBar]');
  Result:=false;
{  Result := (Handle <> 0);
  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_scrolled_window_get_type)
    then begin
      if wBar in [SB_BOTH, SB_HORZ]
      then gtk_object_set(PGTKObject(Handle), 'hscrollbar_policy', [POLICY[bShow], nil]);
      if wBar in [SB_BOTH, SB_VERT]
      then gtk_object_set(PGTKObject(Handle), 'vscrollbar_policy', [POLICY[bShow], nil]);
    end
    else begin
      if (wBar = SB_CTL) and gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_widget_get_type)
      then begin
        if bShow
        then gtk_widget_show(PGTKWidget(Handle))
        else gtk_widget_hide(PGTKWidget(Handle));
      end;
    end;
  end;
}
end;

{------------------------------------------------------------------------------
  Function: StretchBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           SrcWidth, SrcHeight:   The size of the source rectangle
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a 
  destination rectangle using the specified raster operation. If needed it 
  resizes the bitmap to fit the dimensions of the destination rectangle. 
  Sizing is done according to the stretching mode currently set in the 
  destination device context.
  If SrcDC contains a mask the pixmap will be copied with this transparency.
  
  ToDo: Mirroring, extended NonDrawable support (Image, Bitmap, etc)
 ------------------------------------------------------------------------------}
function TgtkObject.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
type
  TBltFunction = function: Boolean;
var
  fGC : PGDKGC;
  SrcDevContext, DestDevContext: TDeviceContext;
  SrcGDIBitmap: PGdiObject;
  ScaleBMP : hBITMAP;
  Scale : PGdiObject;

  {$IfDef Win32}
  Procedure gdk_window_copy_area(Dest : PGDKWindow; GC : PGDKGC; X,
    Y : Longint; SRC : PGDKWindow; XSRC, YSRC, Width, Height : Longint);
  begin
    gdk_draw_pixmap(Dest, GC, Src, XSrc, YSrc, X, Y, Width, Height);
  End;
  {$EndIf}

  Procedure SetClipping(DestGC : PGDKGC; GDIBitmap : PGdiObject);
  begin
    SelectGDIRegion(DestDC);
    if (GDIBitmap <> NIL) AND (GDIBitmap^.GDIBitmapMaskObject <> nil) then
      begin
        gdk_gc_set_clip_mask(DestGC, GDIBitmap^.GDIBitmapMaskObject);
        gdk_gc_set_clip_origin(DestGC, X, Y);
      end;
  end;

  Procedure ResetClipping(DestGC : PGDKGC);
  begin
    gdk_gc_set_clip_mask (DestGC, nil);
    gdk_gc_set_clip_origin (DestGC, 0,0);
    SelectGDIRegion(DestDC);
  end;

  Procedure SetRasterOperation(ScaleROPGC : PGDKGC);
  begin
    Case ROP of
      WHITENESS,
      BLACKNESS,
      SRCCOPY :
        GDK_GC_Set_Function(ScaleROPGC, GDK_Copy);
      SRCPAINT :
        GDK_GC_Set_Function(ScaleROPGC, GDK_NOOP);
      SRCAND :
        GDK_GC_Set_Function(ScaleROPGC, GDK_Clear);
      SRCINVERT :
        GDK_GC_Set_Function(ScaleROPGC, GDK_XOR);
      SRCERASE :
        GDK_GC_Set_Function(ScaleROPGC, GDK_AND);
      NOTSRCCOPY :
        GDK_GC_Set_Function(ScaleROPGC, GDK_OR_REVERSE);
      NOTSRCERASE :
        GDK_GC_Set_Function(ScaleROPGC, GDK_AND);
      MERGEPAINT :
        GDK_GC_Set_Function(ScaleROPGC, GDK_Copy_Invert);
      DSTINVERT :
        GDK_GC_Set_Function(ScaleROPGC, GDK_INVERT);
      else begin
        gdk_gc_set_function(ScaleROPGC, GDK_COPY);
        WriteLn('WARNING: [TgtkObject.StretchBlt] Got unknown/unsupported CopyMode!!');
      end;
    end;
  end;

  function ScaleBuffer(ScaleGC:PGDKGC) : Boolean;
  {$Ifndef NoGdkPixbufLib}
  var
    ScaleSrc, ScaleDest : PGDKPixbuf;
    ShrinkWidth,
    ShrinkHeight : Boolean;
    ScaleMethod : TGDKINTERPTYPE;
  begin
    Result := False;
    ScaleSRC := nil;
    ScaleDest := nil;
    ShrinkWidth := Width < SrcWidth;
    ShrinkHeight := Height < SrcHeight;
    //GDKPixbuf Scaling is not done in the same way as Windows
    //but by rights ScaleMethod should really be chosen based
    //on the destination device's internal flag
    {GDK_INTERP_NEAREST,GDK_INTERP_TILES,
    GDK_INTERP_BILINEAR,GDK_INTERP_HYPER);}
    If ShrinkWidth and ShrinkHeight then
      ScaleMethod := GDK_INTERP_TILES
    else
      If ShrinkWidth or ShrinkHeight then
        ScaleMethod := GDK_INTERP_BILINEAR//GDK_INTERP_HYPER
      else
        ScaleMethod := GDK_INTERP_BILINEAR;
    ScaleSRC := gdk_pixbuf_get_from_drawable(nil,Scale^.GDIPixmapObject,
      GDK_ColorMap_Get_System,0,0,0,0,SrcWidth,SrcHeight);
    If ScaleSRC = nil then
      exit;
    If (Width > 0) and (Height > 0) then
      ScaleDest := gdk_pixbuf_scale_simple(ScaleSRC,Width,Height,ScaleMethod);
    GDK_Pixbuf_Unref(ScaleSRC);
    If ScaleDest = nil then
      exit;
    DeleteObject(ScaleBMP);
    ScaleBMP := CreateCompatibleBitmap(0, Width, Height);
    Scale := PGdiObject(ScaleBMP);
    gdk_pixbuf_render_pixmap_and_mask(ScaleDest,@Scale^.GDIPixmapObject,
      @Scale^.GDIBitmapMaskObject,0);
    GDK_Pixbuf_Unref(ScaleDest);
    Result := True;
  {$Else not NoGdkPixbufLib}
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] GDKPixbuf support has been disabled, no stretching is available!');
    Result := True;
  {$EndIf}
  end;

  Function ScaleAndROP(ScaleROPGC : PGDKGC; SRC : PGDKDrawable;
    SRCBit : PGDIObject) : Boolean;
  var
    SRCClip : PGDKPixmap;
  begin
    Result := False;
    SRCClip := nil;
    If SRCBit <> nil then
      If SRCBit^.GDIBitmapMaskObject <> nil then
        SRCClip := SRCBit^.GDIBitmapMaskObject;
    if ScaleROPGC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.StretchBlt] Uninitialized GC');
      exit;
    end;

    // create a buffer for raster operations and scaling
    Case ROP of
      WHITENESS,
      BLACKNESS,
      DSTINVERT :
        begin
          ScaleBMP := CreateCompatibleBitmap(0, Width, Height);
          Scale := PGdiObject(ScaleBMP);
          Scale^.GDIBitmapMaskObject := SRCClip;
          SetRasterOperation(ScaleROPGC);
          Result := True;
          exit; //skip scaling
        end;
        else begin
           ScaleBMP := CreateCompatibleBitmap(0, SRCWidth, SRCHeight);
           Scale := PGdiObject(ScaleBMP);
           Scale^.GDIBitmapMaskObject := SRCClip;
        end;
    end;

    // set raster operation to SRCCOPY, or NOTSRCCOPY
    If ROP = NOTSRCERASE then
      GDK_GC_Set_Function(ScaleROPGC, GDK_OR_REVERSE)
    else
      GDK_GC_Set_Function(ScaleROPGC, GDK_Copy);

    GDK_GC_COPY(fGC, ScaleROPGC);
    gdk_gc_set_clip_region(fgc,  nil);
    gdk_gc_set_clip_rectangle (fgc,  nil);

    //copy source into scale buffer
    gdk_window_copy_area(Scale^.GDIPixmapObject, fGC,0, 0,
       SRC, XSRC, YSRC, SRCWidth, SRCHeight);
    // Set raster operation to SRCCOPY
    GDK_GC_Set_Function(ScaleROPGC, GDK_Copy);

    // Scale Buffer if needed
    If (Width <> SrcWidth) or (Height <> SrcHeight) then
      Result := ScaleBuffer(ScaleROPGC)
    else
      Result := True;

    //set raster operation
    If Result then
      SetRasterOperation(ScaleROPGC);
  end;

  Procedure ROPFILLBUFFER(DC : hDC);
  var
    OldCurrentBrush: PGdiObject;
    Brush : hBrush;
  begin
    with TDeviceContext(DC) do
    begin
      // Temporarily hold the old brush to
      // replace it with the given brush
      OldCurrentBrush := CurrentBrush;
      If ROP = WHITENESS then
        Brush := GetStockObject(WHITE_BRUSH)
      else
        Brush := GetStockObject(BLACK_BRUSH);
      CurrentBrush := PGdiObject(Brush);
      SelectedColors := dcscCustom;
      SelectGDKBrushProps(DC);
      gdk_draw_rectangle(Scale^.GDIPixmapObject, GC, 1, 0, 0, Width, Height);
      // Restore current brush
      SelectedColors := dcscCustom;
      CurrentBrush := OldCurrentBrush;
    end;
  end;

  function DrawableToDrawable: Boolean;
  begin
    SrcDevContext:=TDeviceContext(SrcDC);
    DestDevContext:=TDeviceContext(DestDC);
    SrcGDIBitmap:=SrcDevContext.CurrentBitmap;

    fGC := GDK_GC_New(DestDevContext.Drawable);

    // perform raster operation and scaling in a buffer
    DestDevContext.SelectedColors := dcscCustom;
    If not ScaleAndROP(DestDevContext.GC,
      SrcDevContext.Drawable, SrcGDIBitmap)
    then
      exit;

    GDK_GC_Unref(fGC);

    Case ROP of
      WHITENESS, BLACKNESS :
        ROPFILLBUFFER(DestDC);
    end;

    // set clipping mask for transparency
    SetClipping(DestDevContext.GC, Scale);

    // draw image
    gdk_window_copy_area(DestDevContext.Drawable,
      DestDevContext.GC,X, Y, Scale^.GDIPixmapObject,
      0, 0, Width, Height);

    // unset clipping mask for transparency
    ResetClipping(DestDevContext.GC);

    // restore raster operation to SRCCOPY
    GDK_GC_Set_Function(DestDevContext.GC, GDK_Copy);

    // Delete buffer
    DeleteObject(ScaleBMP);

    Result:=True;
  end;

  function PixmapToDrawable: Boolean;
  begin
    SrcDevContext:=TDeviceContext(SrcDC);
    DestDevContext:=TDeviceContext(DestDC);
    SrcGDIBitmap:=SrcDevContext.CurrentBitmap;

    fGC := GDK_GC_New(SrcDevContext.Drawable);

    // perform raster operation and scaling in a buffer
    DestDevContext.SelectedColors := dcscCustom;
    If not ScaleAndROP(DestDevContext.GC, SrcDevContext.Drawable,
      SrcGDIBitmap)
    then
      exit;

    GDK_GC_Unref(fGC);

    Case ROP of
      WHITENESS, BLACKNESS :
        ROPFILLBUFFER(DestDC);
    end;

    // set clipping mask for transparency
    SetClipping(DestDevContext.GC, Scale);

    // draw image
    gdk_window_copy_area(DestDevContext.Drawable,
      DestDevContext.GC,X, Y, Scale^.GDIPixmapObject,
      0, 0, Width, Height);

    // unset clipping mask for transparency
    ResetClipping(DestDevContext.GC);

    // restore raster operation to SRCCOPY
    GDK_GC_Set_Function(DestDevContext.GC, GDK_Copy);

    // Delete buffer
    DeleteObject(ScaleBMP);

    Result := True;
  end;

  function ImageToImage: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] ImageToImage unimplimented!');
    Result:=false;
  end;

  function ImageToDrawable: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] ImageToDrawable unimplimented!');
    Result:=false;
  end;

  function ImageToBitmap: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] ImageToBitmap unimplimented!');
    Result:=false;
  end;

  function PixmapToImage: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] PixmapToImage unimplimented!');
    Result:=false;
  end;

  function PixmapToBitmap: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] PixmapToBitmap unimplimented!');
    Result:=false;
  end;

  function BitmapToImage: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] BitmapToImage unimplimented!');
    Result:=false;
  end;

  function BitmapToPixmap: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] BitmapToPixmap unimplimented!');
    Result:=false;
  end;

  function Unsupported: Boolean;
  begin
    WriteLn('WARNING: [TgtkObject.StretchBlt] Destination and/or Source '
      + 'unsupported!!');
    Result:=false;
  end;

  //----------
  function NoDrawableToNoDrawable: Boolean;
  const //            FROM            TO
    BLT_MATRIX: array[TGDIBitmapType, TGDIBitmapType] of TBltFunction = (
      (@DrawableToDrawable, @BitmapToPixmap, @BitmapToImage),
      (@PixmapToBitmap, @DrawableToDrawable, @PixmapToImage),
      (@ImageToBitmap, @ImageToDrawable, @ImageToImage)
    );
  begin
    If (TDeviceContext(SrcDC).CurrentBitmap <> nil) and
      (TDeviceContext(DestDC).CurrentBitmap <> nil)
    then
      Result := BLT_MATRIX[
        TDeviceContext(SrcDC).CurrentBitmap^.GDIBitmapType,
        TDeviceContext(DestDC).CurrentBitmap^.GDIBitmapType
      ]()
    else
      Result := Unsupported;
  end;

  function NoDrawableToDrawable: Boolean;
  const
    BLT_FUNCTION: array[TGDIBitmapType] of TBltFunction = (
      @PixmapToDrawable, @PixmapToDrawable, @ImageToDrawable
    );
  begin
    If TDeviceContext(SrcDC).CurrentBitmap <> nil then
      Result := BLT_FUNCTION[
        TDeviceContext(SrcDC).CurrentBitmap^.GDIBitmapType
      ]()
    else
      Result := Unsupported;
  end;

  function DrawableToNoDrawable: Boolean;
  const
    BLT_FUNCTION: array[TGDIBitmapType] of TBltFunction = (
      @Unsupported, @Unsupported, @Unsupported
    );
  begin
    If TDeviceContext(DestDC).CurrentBitmap <> nil then
      Result := BLT_FUNCTION[
        TDeviceContext(DestDC).CurrentBitmap^.GDIBitmapType
      ]()
    else
      Result := Unsupported;
  end;

const //                 FROM     TO
  DRAWABLE_MATRIX: array[Boolean, Boolean] of TBltFunction = (
    (@NoDrawableToNoDrawable, @NoDrawableToDrawable),
    (@DrawableToNoDrawable, @DrawableToDrawable)
  );

var DCOrigin: TPoint;
begin
  Assert(True, Format('trace:> [TgtkObject.StretchBlt] DestDC:0x%x; X:%d, Y:%d, Width:%d, Height:%d; SrcDC:0x%x; XSrc:%d, YSrc:%d, SrcWidth:%d, SrcHeight:%d; Rop:0x%x', [DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop]));
  Result := IsValidDC(DestDC) and IsValidDC(SrcDC);
  if Result
  then begin
    with TDeviceContext(DestDC) do begin
      DCOrigin:=GetDCOffset(TDeviceContext(DestDC));
      Inc(X,DCOrigin.X);
      Inc(Y,DCOrigin.Y);
    end;
    with TDeviceContext(SrcDC) do begin
      DCOrigin:=GetDCOffset(TDeviceContext(SrcDC));
      Inc(XSrc,DCOrigin.X);
      Inc(YSrc,DCOrigin.Y);
    end;

    //writeln('TgtkObject.StretchBlt X=',X,' Y=',Y,' Width=',Width,' Height=',Height,
    //  ' XSrc=',XSrc,' YSrc=',YSrc,' SrcWidth=',SrcWidth,' SrcHeight=',SrcHeight);

    If TDeviceContext(SrcDC).Drawable = nil then begin
      If TDeviceContext(DestDC).Drawable = nil then
        Result := NoDrawableToNoDrawable
      else
        Result := NoDrawableToDrawable;
    end
    else begin
      If TDeviceContext(DestDC).Drawable = nil then
        Result := DrawableToNoDrawable
      else
        Result := DrawableToDrawable;
    end;
  end;
  Assert(True, Format('trace:< [TgtkObject.StretchBlt] DestDC:0x%x --> %s', [DestDC, BOOL_TEXT[Result]]));
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
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a 
  destination rectangle using the specified mask and raster operation. If needed
  it resizes the bitmap to fit the dimensions of the destination rectangle. 
  Sizing is done according to the stretching mode currently set in the 
  destination device context.
 ------------------------------------------------------------------------------}
function TgtkObject.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result:=false;
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
Function TGTKObject.TextOut(DC: HDC; X,Y : Integer; Str : Pchar;
  Count: Integer) : Boolean;
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
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.TextOut] Uninitialized GC');
    end
    else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFont := GetDefaultFont;
        Underline := False;
        StrikeOut := False;
        UnRef := True;
      end
      else begin
        UseFont := CurrentFont^.GDIFontObject;
        Underline := LongBool(CurrentFont^.LogFont.lfUnderline);
        StrikeOut := LongBool(CurrentFont^.LogFont.lfStrikeOut);
        UnRef := False;
      end;
      If UseFont = nil then
        WriteLn('WARNING: [TgtkObject.TextOut] Missing Font')
      else begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        GetTextExtentPoint(DC, Str, Count, Sz);
        aRect := Rect(X+DCOrigin.X,Y+DCOrigin.Y,X + Sz.CX, Sz.CY);
        FillRect(DC,aRect,hBrush(CurrentBrush));
        UpdateDCTextMetric(TDeviceContext(DC));
        TxtPt.X := X;
        {$IfDef Win32}
          TxtPt.Y := Y + DCTextMetric.TextMetric.tmHeight div 2;
        {$Else}
          TxtPt.Y := Y + DCTextMetric.TextMetric.tmAscent;
        {$EndIf}
        SelectGDKTextProps(DC);
        gdk_draw_text(Drawable, UseFont,
          GC, TxtPt.X+DCOrigin.X, TxtPt.Y+DCOrigin.Y, Str, Count);
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
          GDK_Font_UnRef(UseFont);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: WindowFromPoint
  Params: Point: Specifies the x and y Coords
  Returns: The handle of the gtkwidget.  If none exist, then NULL is returned.

 ------------------------------------------------------------------------------}
Function TGTKObject.WindowFromPoint(Point : TPoint) : HWND;
var
  ev : TgdkEvent;
  Window : PgdkWindow;
  Widget : PgtkWidget;
begin
  // Check the state of the widget.  IF it's hidden or disabled, don't return it's handle!
  Result := 0;

  Window := gdk_window_at_pointer(@Point.x,@Point.Y);
  if window <> nil then
  Begin
    ev.any.window := Window;
    Widget := gtk_get_event_widget(@ev);
    if widget <> nil then Result := Longint(widget);
    Assert(False, format('Trace:Result = [%d]',[Result]));
  end
  else
    Assert(False, 'Trace:Result = nil');
end;

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

Procedure TGTKObject.InitializeCriticalSection(var CritSection: TCriticalSection);
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

Procedure TGTKObject.EnterCriticalSection(var CritSection: TCriticalSection);
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

Procedure TGTKObject.LeaveCriticalSection(var CritSection: TCriticalSection);
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

Procedure TGTKObject.DeleteCriticalSection(var CritSection: TCriticalSection);
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

//##apiwiz##eps##   // Do not remove

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}

{ =============================================================================

  $Log$
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
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

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

