{%MainUnit ../lclintf.pp}
{ $Id$
******************************************************************************
  All interface communication related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    InterfaceObject.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

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
 ******************************************************************************

******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring InterfaceObject is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

procedure AttachMenuToWindow(AMenuObject: TComponent);
begin
  InterfaceObject.AttachMenuToWindow(AMenuObject);
end;

procedure CallDefaultWndHandler(Sender: TObject; var Message);
begin
  InterfaceObject.CallDefaultWndHandler(Sender,Message);
end;

// the clipboard functions are internally used by TClipboard
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := InterfaceObject.ClipboardFormatToMimeType(FormatID);
end;

function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result := InterfaceObject.ClipboardGetData(ClipboardType, FormatID, Stream);
end;

// ! List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := InterfaceObject.ClipboardGetFormats(ClipboardType,Count,List);
end;

function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := InterfaceObject.ClipboardGetOwnerShip(ClipboardType, OnRequestProc,
                   FormatCount, Formats);
end;

function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := InterfaceObject.ClipboardRegisterFormat(AMimeType);
end;

function ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean;
begin
  Result := InterfaceObject.ComboBoxDropDown(Handle,DropDown);
end;

function CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
begin
  Result := InterfaceObject.CreateBitmapFromRawImage(RawImage,Bitmap,MaskBitmap,
                                                     AlwaysCreateMask);
end;

function CreateEmptyRegion: hRGN;
begin
  Result := InterfaceObject.CreateEmptyRegion;
end;

function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := InterfaceObject.CreatePixmapIndirect(Data, TransColor);
end;

function CreateRegionCopy(SrcRGN: hRGN): hRGN;
begin
  Result := InterfaceObject.CreateRegionCopy(SrcRGN);
end;

function DCClipRegionValid(DC: HDC): boolean;
begin
  Result := InterfaceObject.DCClipRegionValid(DC);
end;

procedure DrawArrow(Arrow: TComponent; Canvas: TPersistent);
begin
  InterfaceObject.DrawArrow(Arrow, Canvas);
end;

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := InterfaceObject.ExtUTF8Out(DC,X,Y,Options,Rect,Str,Count,Dx);
end;

function FontCanUTF8(Font: HFont): boolean;
begin
  Result := InterfaceObject.FontCanUTF8(Font);
end;

function Frame(DC: HDC; const ARect: TRect): Integer;
begin
  Result := InterfaceObject.Frame(DC, ARect);
end;

function Frame3d(DC : HDC; var ARect : TRect; const FrameWidth : integer;
  const Style: TGraphicsBevelCut) : boolean;
begin
  Result:= InterfaceObject.Frame3d(DC, ARect, FrameWidth, Style);
end;

function GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): boolean;
begin
  Result := InterfaceObject.GetBitmapRawImageDescription(Bitmap,Desc);
end;

function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean;
begin
  Result := InterfaceObject.GetCaretRespondToFocus(handle,ShowHideOnFocus);
end;

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          ARect:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
function GetClientBounds(Handle: HWND; var ARect: TRect): Boolean;
begin
  Result := InterfaceObject.GetClientBounds(Handle, ARect);
end;

{------------------------------------------------------------------------------
  Function: GetCmdLineParamDescForInterface
  Params: none
  Returns: ansistring

  Returns a description of the command line parameters, that are understood by
  the interface.
 ------------------------------------------------------------------------------}
function GetCmdLineParamDescForInterface: string;
begin
  Result := InterfaceObject.GetCmdLineParamDescForInterface;
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds. For instance, vertical scrollbars under gtk are fixed in width. So,
  it sets MinInterfaceWidth and MaxInterfaceWidth. This is used by the auto
  aligning/sizing algorithms of the LCL.
 ------------------------------------------------------------------------------}
function GetControlConstraints(Constraints: TObject): boolean;
begin
  Result := InterfaceObject.GetControlConstraints(Constraints);
end;

function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND;
  var OriginDiff: TPoint): boolean;
begin
  Result := InterfaceObject.GetDCOriginRelativeToWindow(PaintDC,WindowHandle,
                                                        OriginDiff);
end;

function GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result := InterfaceObject.GetDesignerDC(WindowHandle);
end;

function GetDeviceRawImageDescription(DC: HDC;
  Desc: PRawImageDescription): boolean;
begin
  Result := InterfaceObject.GetDeviceRawImageDescription(DC,Desc);
end;

function GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result := InterfaceObject.GetDeviceSize(DC,p);
end;

function GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  Result := InterfaceObject.GetLCLOwnerObject(Handle);
end;

function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := InterfaceObject.GetListBoxIndexAtY(ListBox, y);
end;

function GetListBoxItemRect(ListBox: TComponent; Index: integer;
  var ARect: TRect): boolean;
begin
  Result := InterfaceObject.GetListBoxItemRect(ListBox,Index,ARect);
end;

function GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect;
  var NewRawImage: TRawImage): boolean;
begin
  Result := InterfaceObject.GetRawImageFromDevice(SrcDC,SrcRect,NewRawImage);
end;

function GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result := InterfaceObject.GetRawImageFromBitmap(SrcBitmap,SrcMaskBitmap,
                                                  SrcRect,NewRawImage);
end;

function GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer;
begin
  Result := InterfaceObject.GetScrollBarSize(Handle, SBStyle);
end;

function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
begin
  Result := InterfaceObject.GetScrollbarVisible(Handle, SBStyle);
end;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  Result := InterfaceObject.GetAcceleratorString(AVKey, AShiftState);
end;

{------------------------------------------------------------------------------
  function GetWindowRelativePosition(Handle: hwnd;
    var Left, Top: integer): boolean;

  Returns the Left, Top, relative to the client origin of its parent.
------------------------------------------------------------------------------}
function GetWindowRelativePosition(Handle : hwnd;
  var Left, Top: integer): boolean;
begin
  Result := InterfaceObject.GetWindowRelativePosition(Handle,Left,Top);
end;

{------------------------------------------------------------------------------
  function IntfSendsUTF8KeyPress: Boolean;

  Returns true if interface sends UTF8 KeyPress notifications.
------------------------------------------------------------------------------}
function IntfSendsUTF8KeyPress: Boolean;
begin
  Result := InterfaceObject.IntfSendsUTF8KeyPress;
end;

{------------------------------------------------------------------------------
  Function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean;
    BorderWidth: integer) : Boolean;

  Calls InvalidateRect for the borderframe.
------------------------------------------------------------------------------}
function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean;
begin
  Result := InterfaceObject.InvalidateFrame(aHandle,ARect,bErase,BorderWidth);
end;

function LoadStockPixmap(StockID: longint) : HBitmap;
begin
  Result := InterfaceObject.LoadStockPixmap(StockID);
end;

function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean;
begin
  Result := InterfaceObject.MoveWindowOrgEx(DC, dX, dY);
end;

function PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=InterfaceObject.PairSplitterAddSide(SplitterHandle,SideHandle,Side);
end;

function PairSplitterGetInterfaceInfo: boolean;
begin
  Result:=InterfaceObject.PairSplitterGetInterfaceInfo;
end;

function PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd;Side: integer): Boolean;
begin
  Result:=InterfaceObject.PairSplitterRemoveSide(SplitterHandle,SideHandle, Side);
end;

function PairSplitterSetPosition(SplitterHandle: hWnd; var NewPosition: integer): Boolean;
begin
  Result:=InterfaceObject.PairSplitterSetPosition(SplitterHandle,NewPosition);
end;

function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUser('', DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUser(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUserAtXY('', DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult, X, Y);
end;

function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUserAtXY(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult, X, Y);
end;

function RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := InterfaceObject.RadialChord(DC, x,y,width,height,sx,sy,ex,ey);
end;

function RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := InterfaceObject.RadialArc(DC, x,y,width,height,sx,sy,ex,ey);
end;

function RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := InterfaceObject.RadialPie(DC, x,y,width,height,sx,sy,ex,ey);
end;

function RadialPieWithAngles(DC: HDC; X,Y,Width,Height,
  Angle1,Angle2: Integer): Boolean;
begin
  Result := InterfaceObject.RadialPieWithAngles(DC, X,Y,Width,Height,Angle1,Angle2);
end;

function RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean;
begin
  Result := InterfaceObject.RadioMenuItemGroup(hndMenu,bRadio);
end;

function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer) : Boolean;
begin
  Result := InterfaceObject.RegroupMenuItem(hndMenu,GroupIndex);
end;

function ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := InterfaceObject.ReleaseDesignerDC(hWnd, DC);
end;

function ReplaceBitmapMask(var Image, Mask: HBitmap; NewMask: HBitmap): boolean;
// for Delphi compatibility a TBitmap has a Handle and a MaskHandle.
// Some interfaces have only a combined Handle. To replace the mask use this
// function. Under win32 it will simply replace Mask with NewMask.
// Under gtk Mask will be freed and NewMask will be combined into Handle.
begin
  Result := InterfaceObject.ReplaceBitmapMask(Image,Mask,NewMask);
end;

function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean;
begin
  Result := InterfaceObject.RequestInput(InputCaption, InputPrompt, MaskInput, Value);
end;

procedure SendCachedLCLMessages;
begin
end;

function DrawSplitter(DC: HDC; const ARect: TRect; Horizontal: boolean): boolean;
begin
  Result := InterfaceObject.DrawSplitter(DC,ARect,Horizontal);
end;

function SetCaretRespondToFocus(Handle: hWnd; ShowHideOnFocus: boolean):Boolean;
begin
  Result := InterfaceObject.SetCaretRespondToFocus(Handle,ShowHideOnFocus);
end;

function SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result := InterfaceObject.SetComboMinDropDownSize(Handle,
                                   MinItemsWidth, MinItemsHeight, MinItemCount);
end;

function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := InterfaceObject.StretchMaskBlt(DestDC, X, Y, Width, Height,
               SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
end;

//##apiwiz##eps##   // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

//##apiwiz##spi##   // Do not remove


{------------------------------------------------------------------------------
  Function: ExcludeClipRgn
  Params:  dc, RGN
  Returns: integer

  Subtracts all intersecting points of the passed Region from the Current
  clipping region in the device context (dc).

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function ExcludeClipRgn(dc: hdc; rgn : hrgn) : Integer;
begin
  Result := ExtSelectClipRGN(DC, RGN, RGN_DIFF);
end;

procedure NotifyUser(const DialogMessage : String; DialogType : longint);
begin
  PromptUser(DialogMessage, DialogType, [idButtonOK], -1, -1);
end;

procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
begin
  PromptUser(DialogCaption, DialogMessage, DialogType, [idButtonOK], -1, -1);
end;

procedure NotifyUserAtXY(const DialogMessage : String; DialogType : longint; X, Y : Longint);
begin
  PromptUserAtXY(DialogMessage, DialogType, [idButtonOK], -1, -1, X, Y);
end;

procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);
begin
  PromptUserAtXY(DialogCaption, DialogMessage, DialogType, [idButtonOK], -1, -1, X, Y);
end;

function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := PromptUser(DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult);
end;

function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := PromptUser(DialogCaption, DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult);
end;

function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := PromptUserAtXY(DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult, X, Y);
end;

function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := PromptUserAtXY(DialogCaption, DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult, X, Y);
end;

procedure RaiseLastOSError;
begin
  RaiseGDBException('ToDo: RaiseLastOSError');
end;

//##apiwiz##epi##   // Do not remove

