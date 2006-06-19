{%MainUnit ../interfacebase.pp}
{ $Id$
 ******************************************************************************
                                  TWidgetSet

                           interface communication  stuff

  !! In this file only interface related code as defined in lclintfh.inc
     Most routines implement only the default

  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove

function TWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
begin
  Result := nil;
end;

function TWidgetSet.AddProcessEventHandler(AHandle: THandle; 
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
begin
  Result := nil;
end;

function TWidgetSet.AddPipeEventHandler(AHandle: THandle; 
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
begin
  Result := nil;
end;

procedure TWidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
begin
end;

procedure TWidgetSet.CallDefaultWndHandler(Sender: TObject; var Message);
begin
end;

// the clipboard functions are internally used by TClipboard
function TWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := '';
end;

function TWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat;  Stream: TStream): boolean;
begin
  Result := false;
end;

// ! List will be created. You must free it yourself with FreeMem(List) !
function TWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := true;
  Count := 0;
  List := nil;
end;

function TWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := false;
end;

function TWidgetSet.ClipboardRegisterFormat(
  const AMimeType: string): TClipboardFormat;
begin
  Result := 0;
end;

function TWidgetSet.ComboBoxDropDown(Handle: HWND;
  DropDown: boolean): boolean;
begin
  Result := false;
end;

function TWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
begin
  Bitmap:=0;
  MaskBitmap:=0;
  Result := false;
end;

//todo: remove ?
function TWidgetSet.CreateEmptyRegion: hRGN;
begin
  Result:=CreateRectRGN(0,0,0,0);
end;

function TWidgetSet.CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := 0;
end;

function TWidgetSet.CreateRegionCopy(SrcRGN: hRGN): hRGN;
begin
  // If the interface has a better way to create a copy it can override this
  Result:=CreateEmptyRegion;
  CombineRGN(Result,SrcRGN,SrcRGN,RGN_COPY);
end;

function TWidgetSet.DCClipRegionValid(DC: HDC): boolean;
var
  Clip: hRGN;
begin
  // If the interface has a better way to check a region it can override this
  //debugln('TWidgetSet.DCClipRegionValid DC=',DbgS(DC));
  Clip:=CreateEmptyRegion;
  Result:=GetClipRGN(DC,Clip)>=0;
  DeleteObject(Clip);
end;

procedure TWidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
begin
end;

function TWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=ExtTextOut(DC,X,Y,Options,Rect,Str,Count,Dx);
end;

function TWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean;
begin
  Result:=TextOut(DC,X,Y,Str,Count);
end;


function TWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=false;
end;

function TWidgetSet.Frame(DC: HDC; const ARect: TRect) : integer;
begin
  Result:= 0;
end;

function TWidgetSet.Frame3d(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TGraphicsBevelCut) : boolean;
begin
  Result:= false;
end;

// MWE: Work in progress
function TWidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
  // If the interface has a better way to create a string it can override this

  procedure AddPart(const APart: string);
  begin
    if Result <> '' then Result := Result + '+';
    Result := Result + APart;
  end;

  // Tricky routine. This only works for western languages
  procedure AddKey;
  begin
    case AVKey of
      VK_UNKNOWN    :AddPart(ifsVK_UNKNOWN);
      VK_LBUTTON    :AddPart(ifsVK_LBUTTON);
      VK_RBUTTON    :AddPart(ifsVK_RBUTTON);
      VK_CANCEL     :AddPart(ifsVK_CANCEL);
      VK_MBUTTON    :AddPart(ifsVK_MBUTTON);
      VK_BACK       :AddPart(ifsVK_BACK);
      VK_TAB        :AddPart(ifsVK_TAB);
      VK_CLEAR      :AddPart(ifsVK_CLEAR);
      VK_RETURN     :AddPart(ifsVK_RETURN);
      VK_SHIFT      :AddPart(ifsVK_SHIFT);
      VK_CONTROL    :AddPart(ifsVK_CONTROL);
      VK_MENU       :AddPart(ifsVK_MENU);
      VK_PAUSE      :AddPart(ifsVK_PAUSE);
      VK_CAPITAL    :AddPart(ifsVK_CAPITAL);
      VK_KANA       :AddPart(ifsVK_KANA);
    //  VK_HANGUL     :AddPart('Hangul');
      VK_JUNJA      :AddPart(ifsVK_JUNJA);
      VK_FINAL      :AddPart(ifsVK_FINAL);
      VK_HANJA      :AddPart(ifsVK_HANJA );
    //  VK_KANJI      :AddPart('Kanji');
      VK_ESCAPE     :AddPart(ifsVK_ESCAPE);
      VK_CONVERT    :AddPart(ifsVK_CONVERT);
      VK_NONCONVERT :AddPart(ifsVK_NONCONVERT);
      VK_ACCEPT     :AddPart(ifsVK_ACCEPT);
      VK_MODECHANGE :AddPart(ifsVK_MODECHANGE);
      VK_SPACE      :AddPart(ifsVK_SPACE);
      VK_PRIOR      :AddPart(ifsVK_PRIOR);
      VK_NEXT       :AddPart(ifsVK_NEXT);
      VK_END        :AddPart(ifsVK_END);
      VK_HOME       :AddPart(ifsVK_HOME);
      VK_LEFT       :AddPart(ifsVK_LEFT);
      VK_UP         :AddPart(ifsVK_UP);
      VK_RIGHT      :AddPart(ifsVK_RIGHT);
      VK_DOWN       :AddPart(ifsVK_DOWN);
      VK_SELECT     :AddPart(ifsVK_SELECT);
      VK_PRINT      :AddPart(ifsVK_PRINT);
      VK_EXECUTE    :AddPart(ifsVK_EXECUTE);
      VK_SNAPSHOT   :AddPart(ifsVK_SNAPSHOT);
      VK_INSERT     :AddPart(ifsVK_INSERT);
      VK_DELETE     :AddPart(ifsVK_DELETE);
      VK_HELP       :AddPart(ifsVK_HELP);
      VK_0..VK_9    :AddPart(chr(ord('0')+AVKey-VK_0));
      VK_A..VK_Z    :AddPart(chr(ord('A')+AVKey-VK_A));
      VK_LWIN       :AddPart(ifsVK_LWIN);
      VK_RWIN       :AddPart(ifsVK_RWIN);
      VK_APPS       :AddPart(ifsVK_APPS);
      VK_NUMPAD0..VK_NUMPAD9:  AddPart(Format(ifsVK_NUMPAD,[AVKey-VK_NUMPAD0]));
      VK_MULTIPLY   :AddPart('*');
      VK_ADD        :AddPart('+');
      VK_SEPARATOR  :AddPart('|');
      VK_SUBTRACT   :AddPart('-');
      VK_DECIMAL    :AddPart('.');
      VK_DIVIDE     :AddPart('/');
      VK_F1..VK_F24 : AddPart('F'+IntToStr(AVKey-VK_F1+1));
      VK_NUMLOCK    :AddPart(ifsVK_NUMLOCK);
      VK_SCROLL     :AddPart(ifsVK_SCROLL);
//    VK_EQUAL      :AddPart('=');
//    VK_COMMA      :AddPart(',');
//    VK_POINT      :AddPart('.');
//    VK_SLASH      :AddPart('/');
//    VK_AT         :AddPart('@');
    else
      AddPart(UNKNOWN_VK_PREFIX + IntToStr(AVKey) + UNKNOWN_VK_POSTFIX);
    end;
  end;

begin
  Result := '';
  if ssCtrl in AShiftState then AddPart('Ctrl');
  if ssAlt in AShiftState then AddPart('Alt');
  if ssShift in AShiftState then AddPart('Shift');
  AddKey;
end;

function TWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): boolean;
begin
  Result:=false;
end;

function TWidgetSet.GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  ShowHideOnFocus := true;
  Result := False;
end;

function TWidgetSet.GetClientBounds(Handle : HWND; var ARect: TRect) : Boolean;
begin
  Result := false;
end;

function TWidgetSet.GetCmdLineParamDescForInterface: string;
begin
  Result := '';
end;

function TWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=true;
end;

function TWidgetSet.GetDeviceRawImageDescription(DC: HDC;
  Desc: PRawImageDescription): boolean;
begin
  Result := false;
end;

function TWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  p.X := 0;
  p.Y := 0;
  Result := false;
end;

function TWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  OriginDiff.X:=0;
  OriginDiff.Y:=0;
  Result:=true;
end;

function TWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result:=GetDC(WindowHandle);
end;

function TWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  if Handle <> 0
  then Result := TObject(GetProp(Handle,'WinControl'))
  else Result := nil;
end;

function TWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
end;

function TWidgetSet.GetListBoxItemRect(ListBox: TComponent; Index: integer;
  var ARect: TRect): boolean;
begin
  FillChar(ARect,SizeOf(ARect),0);
  Result:=false;
end;

function TWidgetSet.GetRawImageFromDevice(SrcDC: HDC;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result:=false;
end;

function TWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result:=false;
end;

// TODO: remove
function TWidgetSet.GetScrollBarSize(Handle: HWND;
  SBStyle: Integer): integer;
begin
  Result := GetSystemMetrics(SBStyle);
end;

function TWidgetSet.GetScrollbarVisible(Handle: HWND;
  SBStyle: Integer): boolean;
begin
  Result := false;
end;

function TWidgetSet.GetWindowRelativePosition(Handle : hwnd;
  var Left, Top: integer): boolean;
{ returns the position of the left, top coordinate relative to the clientorigin
  of its parent. This is normally the Left, Top of a TWinControl. But not
  during moving/sizing. }
var
  ChildRect: TRect;
  ParentLeftTop: TPoint;
  ParentHandle: hWnd;
begin
  Result:=false;
  GetWindowRect(Handle,ChildRect);
  Left:=ChildRect.Left;
  Top:=ChildRect.Top;
  ParentHandle:=GetParent(Handle);
  if ParentHandle<>0 then begin
    ParentLeftTop.X:=0;
    ParentLeftTop.Y:=0;
    if not ClientToScreen(ParentHandle,ParentLeftTop) then exit;
    dec(Left,ParentLeftTop.X);
    dec(Top,ParentLeftTop.Y);
  end;
  Result := true;
end;

function TWidgetSet.GetWindowSize(Handle : hwnd;
  var Width, Height: integer): boolean;
// Returns the current Width and Height
begin
  Result:=false;
end;

function TWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result:=false;
end;

Function TWidgetSet.InvalidateFrame(aHandle : HWND; ARect : pRect;
  bErase : Boolean; BorderWidth: integer) : Boolean;

  function Min(i1, i2: integer): integer;
  begin
    if i1<=i2 then Result:=i1 else Result:=i2;
  end;

  function Max(i1, i2: integer): integer;
  begin
    if i1<=i2 then Result:=i2 else Result:=i1;
  end;

var
  BorderRect: TRect;
begin
  Result:=false;
  BorderRect:=ARect^;
  // left
  BorderRect.Right:=Min(BorderRect.Right,BorderRect.Left+BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  BorderRect.Right:=ARect^.Right;
  // top
  BorderRect.Bottom:=Min(BorderRect.Bottom,BorderRect.Top+BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  BorderRect.Bottom:=ARect^.Bottom;
  // right
  BorderRect.Left:=Max(BorderRect.Left,BorderRect.Right-BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  BorderRect.Left:=ARect^.Left;
  // bottom
  BorderRect.Top:=Max(BorderRect.Top,BorderRect.Bottom-BorderWidth);
  if not InvalidateRect(aHandle,@BorderRect,bErase) then exit;
  Result:=true;
end;

Function TWidgetSet.LoadStockPixmap(StockID: longint) : HBitmap;
begin
  Case StockID of
    idButtonOk :
      Result := CreatePixmapIndirect(@IMGOK_Check[0], GetSysColor(COLOR_BTNFACE));
    idButtonYes :
      Result := CreatePixmapIndirect(@IMGOK_Check[0], GetSysColor(COLOR_BTNFACE));
    idButtonNo :
      Result := CreatePixmapIndirect(@IMG_NO[0], GetSysColor(COLOR_BTNFACE));
    idButtonCancel :
      Result := CreatePixmapIndirect(@IMGCancel_X[0], GetSysColor(COLOR_BTNFACE));
    idButtonHelp :
      Result := CreatePixmapIndirect(@IMGHELP[0], GetSysColor(COLOR_BTNFACE));
    idButtonAll :
      Result := CreatePixmapIndirect(@IMGAll_Check[0], GetSysColor(COLOR_BTNFACE));
    idButtonYesToAll :
      Result := CreatePixmapIndirect(@IMGAll_Check[0], GetSysColor(COLOR_BTNFACE));
    idButtonNoToAll :
      Result := CreatePixmapIndirect(@IMGCancel_X[0], GetSysColor(COLOR_BTNFACE));
    idButtonAbort :
      Result := CreatePixmapIndirect(@IMGCancel_X[0], GetSysColor(COLOR_BTNFACE));
    idButtonRetry :
      Result := CreatePixmapIndirect(@IMG_RETRY[0], GetSysColor(COLOR_BTNFACE));
    idButtonIgnore :
      Result := CreatePixmapIndirect(@IMG_IGNIORE[0], GetSysColor(COLOR_BTNFACE));
    idButtonClose :
      Result := CreatePixmapIndirect(@IMGClose[0], GetSysColor(COLOR_BTNFACE));

    idDialogWarning :
      Result := CreatePixmapIndirect(@IMGWarning[0], GetSysColor(COLOR_BTNFACE));
    idDialogError :
      Result := CreatePixmapIndirect(@IMGError[0], GetSysColor(COLOR_BTNFACE));
    idDialogInfo :
      Result := CreatePixmapIndirect(@IMGInfo[0], GetSysColor(COLOR_BTNFACE));
    idDialogConfirm :
      Result := CreatePixmapIndirect(@IMGConfirmation[0], GetSysColor(COLOR_BTNFACE));

    else
      Result := CreatePixmapIndirect(@IMGOK_Check[0], GetSysColor(COLOR_BTNFACE));
  end;
end;

function TWidgetSet.MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean;
var
  P : TPoint;
Begin
  GetWindowOrgEx(dc, @P);
  Result:=SetWindowOrgEx(dc, P.x-dX, P.y-dY, @P);
end;

function TWidgetSet.PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=false;
end;

function TWidgetSet.PairSplitterGetInterfaceInfo: boolean;
begin
  Result:=false;
end;

function TWidgetSet.PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=false;
end;

function TWidgetSet.PairSplitterSetPosition(SplitterHandle: hWnd;
  var NewPosition: integer): Boolean;
begin
  Result:=false;
end;

Function TWidgetSet.PromptUser(const DialogCaption, DialogMessage : String;
  DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
begin
  if PromptDialogFunction<>nil then
    Result:=PromptDialogFunction(DialogCaption, DialogMessage, DialogType,
       Buttons, ButtonCount, DefaultIndex, EscapeResult, true, 0, 0)
  else
    Result:=0;
end;

function TWidgetSet.PromptUserAtXY(const DialogCaption,
  DialogMessage : String;
  DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint;
  X, Y : Longint) : Longint;
begin
  if PromptDialogFunction<>nil then
    Result:=PromptDialogFunction(DialogCaption, DialogMessage, DialogType,
       Buttons, ButtonCount, DefaultIndex, EscapeResult, false, X, Y)
  else
    Result:=0;
end;

function TWidgetSet.RadialArc(DC: HDC;
  left, top, right, bottom, sx, sy, ex, ey: Integer): Boolean;
var
  A1, A2 : Extended;
begin
  Coords2Angles(left, top, right-left, bottom-top, sx, sy, ex, ey, A1, A2);
  Result := Arc(DC, left, top, right, bottom, RoundToInt(A1), RoundToInt(A2));
end;

function TWidgetSet.RadialChord(DC: HDC;
  x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
var
  A1, A2 : Extended;
Begin
  Coords2Angles(x1, y1, x2-x1, y2-y1, sx, sy, ex, ey, A1, A2);
  Result := AngleChord(DC, x1, y1, x2, y2, RoundToInt(A1), RoundToInt(A2));
End;

function TWidgetSet.RadialPie(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
var
  A1, A2 : Extended;
begin
  Coords2Angles(x1, y1, x2-x1, y2-y1, sx, sy, ex, ey, A1, A2);
  Result := RadialPieWithAngles(DC, x1, y1, x2, y2,
                                RoundToInt(A1), RoundToInt(A2));
end;

function TWidgetSet.RadialPieWithAngles(DC: HDC; x1, y1, x2, y2,
  Angle1, Angle2: Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(x1, y1, x2 - x1, y2 - y1, Angle1, Angle2, 0, Points, Count);
  Inc(Count,2);
  ReallocMem(Points, Count*SizeOf(TPoint));
  Points[Count - 2] := CenterPoint(Rect(x1, y1, x2, y2));
  Points[Count - 1] := Points[0];
  Polygon(DC, Points, Count, True);
  ReallocMem(Points, 0);
  Result := True;
end;

function TWidgetSet.RegroupMenuItem(hndMenu: HMENU; GroupIndex: Integer) : Boolean;
begin
  Result := false;
end;

procedure TWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
begin
end;

procedure TWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
end;

procedure TWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
begin
end;

function TWidgetSet.ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := ReleaseDC(hWnd, DC);
end;

function TWidgetSet.ReplaceBitmapMask(var Image, Mask: HBitmap;
  NewMask: HBitmap): boolean;
// the default behaviour is to free 'Mask' and replace it with 'NewMask'.
// Some interfaces like the gtk will free 'Mask' and replace the internal mask
// of 'Handle' with 'NewMask'.
begin
  Result:=true;
  if Mask<>0 then DeleteObject(Mask);
  Mask:=NewMask;
end;

function TWidgetSet.RequestInput(const InputCaption, InputPrompt : String;
  MaskInput : Boolean; var Value : String) : Boolean;
begin
  if InputDialogFunction<>nil then
    Result := InputDialogFunction(InputCaption, InputPrompt, MaskInput, Value)
  else
    Result := false;
end;

procedure TWidgetSet.SendCachedLCLMessages;
begin
end;

function TWidgetSet.DrawSplitter(DC: HDC; const ARect: TRect;
  Horizontal: boolean): boolean;
var
  DrawingRect: TRect;
begin
  DrawingRect:=ARect;
  Result := Frame3D(DC,DrawingRect,1,bvRaised);
end;

function TWidgetSet.SetCaretRespondToFocus(Handle: HWnd;
  ShowHideOnFocus: Boolean): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result := false;
end;

procedure TWidgetSet.SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);
begin
end;

function TWidgetSet.StretchMaskBlt(DestDC: HDC;
  X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := False;
end;

//##apiwiz##eps##   // Do not remove
