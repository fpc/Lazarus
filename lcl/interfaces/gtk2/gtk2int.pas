{
 /***************************************************************************
                       gtk2int.pas  -  GTK2 Interface Object
                       -------------------------------------


 ***************************************************************************/

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

unit Gtk2Int;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{$DEFINE USE_PANGO}

uses
  Classes, SysUtils,
  {$IfNDef GTK2_2}
    {$IfNDef Win32}
     X, XLib, XUtil,
    {$EndIf}
  {$EndIf}

  gdk2pixbuf, gtk2, gdk2, glib2, Pango,

  LMessages, Controls, Forms, VclGlobals, LCLProc,
  LCLStrConsts, LCLIntf, LCLType, DynHashArray, LazLinkedList,
  GraphType, GraphMath, Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls,
  ComCtrls, CListBox, KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls,
  Dialogs, ExtDlgs, FileCtrl, LResources, Math, GTKGlobals,

  gtkDef, gtkProc, gtkInt;

type
  TGtk2Object = class(TGtkObject)
  public
    {$Ifdef USE_PANGO} // we should implement pango for gtk2 soon
    function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; override;
    function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;
    function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;
    function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
    function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
    procedure UpdateDCTextMetric(DC: TDeviceContext); override;
    {$EndIf}

    function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;
    Function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; override;

    procedure CreateComponent(Sender : TObject); override;
    function IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; override;
    procedure AppendText(Sender: TObject; Str: PChar); override;
    function GetText(Sender: TComponent; var Text: String): Boolean; override;
    procedure SetLabel(Sender : TObject; Data : Pointer); override;
    procedure HookSignals(Sender: TObject); override;
    function SetProperties(Sender : TObject) : integer; override;

    function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
    function LoadStockPixmap(StockID: longint) : HBitmap; override;
  end;


{$IfDef GTK2_2}//we need a GTK2_2 FLAG somehow
Procedure  gdk_display_get_pointer(display : PGdkDisplay; screen :PGdkScreen; x :Pgint; y : Pgint; mask : PGdkModifierType); cdecl; external gdklib;
function gdk_display_get_default:PGdkDisplay; cdecl; external gdklib;

procedure gdk_draw_pixbuf(drawable : PGdkDrawable; gc : PGdkGC; pixbuf : PGdkPixbuf; src_x, src_y, dest_x, dest_y, width, height : gint;
                                             dither : TGdkRgbDither; x_dither, y_dither : gint); cdecl; external gdklib;
{$Else}
  {$IfNDef Win32}
  Function gdk_x11_drawable_get_xdisplay(drawable : PGdkDrawable) :   PDisplay; cdecl; external gdklib;
  Function gdk_x11_drawable_get_xid(drawable : PGdkDrawable) :  Integer; cdecl; external gdklib;
  {$EndIf}
{$EndIf}

implementation

{$Ifdef USE_PANGO} // we should implement pango for gtk2 soon
{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run
  
  inherited routine apears to work fine so, turned off for now. Doesn't work
  properly, and needs to take & into account before its fully useable....
 ------------------------------------------------------------------------------}
function TGTK2Object.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;

  Function Alignment : TPangoAlignment;
  begin
    If (Flags and DT_Right) = DT_Right then
      Result := PANGO_ALIGN_RIGHT
    else
      If (Flags and DT_CENTER) = DT_CENTER then
        Result := PANGO_ALIGN_CENTER
    else
      Result := PANGO_ALIGN_LEFT;
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

var
  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
  RGBColor : TColor;
  X, Y, Width, Height : Integer;
  DCOrigin: TPoint;
begin
  result :=  inherited DrawText(DC, Str, Count, Rect, Flags);

  exit;

  if (Str=nil) or (Str[0]=#0) then exit;
  Assert(False, Format('trace:> [Tgtk2Object.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));

  Result := Longint(IsValidDC(DC));
  if Boolean(Result)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [Tgtk2Object.DrawText] Uninitialized GC');
      Result := 0;
    end
    else begin
      if (Str<>nil) and (Count>0) then begin
        if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) or
          ((Flags and DT_INTERNAL) = DT_INTERNAL)
        then
          UseFontDesc := GetDefaultFontDesc(false)
        else
          UseFontDesc := CurrentFont^.GDIFontObject;

        DCOrigin:=GetDCOffset(TDeviceContext(DC));

        GetStyle('default');
        Layout := gtk_widget_create_pango_layout (GetStyleWidget('default'), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
        If CurrentFont^.Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(CurrentFont^.StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        SelectedColors := dcscCustom;
        EnsureGCColor(DC, dccCurrentTextColor, True, False);

        Case TColor(CurrentTextColor.ColorRef) of
          clScrollbar..clEndColors:
            RGBColor := GetSysColor(CurrentTextColor.ColorRef and $FF);
          else
            RGBColor := CurrentTextColor.ColorRef and $FFFFFF;
        end;

        Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                        gushort(GetGValue(RGBColor)) shl 8,
                                        gushort(GetBValue(RGBColor)) shl 8);

        pango_attr_list_change(AttrList,Attr);
        //fix me... then generate markup for all this?
        //the same routine could then be used for both
        //DrawText and ExtTextOut

        pango_layout_set_attributes(Layout, AttrList);

        pango_layout_set_single_paragraph_mode(Layout, (Flags and DT_SingleLine) = DT_SingleLine);
        pango_layout_set_wrap(Layout, PANGO_WRAP_WORD);

        If ((Flags and DT_WordBreak) = DT_WordBreak)and not
          Pango_layout_get_single_paragraph_mode(Layout)
        then
          pango_layout_set_width(Layout, (Rect.Right - Rect.Left)*PANGO_SCALE)
        else
          pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, Alignment);
  
        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....
        
        pango_layout_set_text(Layout, Str, Count);
        pango_layout_get_pixel_size(Layout, @Width, @Height);
        Case TopOffset of
          DT_Top : Y := Rect.Top;
          DT_Bottom : Y := Rect.Bottom - Height;
          DT_Center : Y := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Height div 2;
        end;

        Case Alignment of
          PANGO_ALIGN_LEFT : X := Rect.Left;
          PANGO_ALIGN_RIGHT : X := Rect.Right - Width;
          PANGO_ALIGN_CENTER : X := Rect.Left + (Rect.Right - Rect.Left) div 2 - Width div 2;
        end;

        if ((Flags and DT_CalcRect) = DT_CalcRect) then begin
          g_object_unref(Layout);
          Rect.Left := X;
          Rect.Top := Y;
          Rect.Right := X + Width;
          Rect.Bottom := Y + Height;
          result := 0;
          exit;
        end;

        gdk_draw_layout(drawable, gc, X+DCOrigin.X, Y+DCOrigin.Y, Layout);
        g_object_unref(Layout);
        Result := 0;
      end;
    end;
  end;
  Assert(False, Format('trace:> [Tgtk2Object.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function Tgtk2Object.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  LineStart, LineEnd, StrEnd: PChar;
  Width, Height: Integer;
  TopY, LineLen, LineHeight : Integer;
  TxtPt : TPoint;
  DCOrigin: TPoint;

  UnRef,
  Underline,
  StrikeOut : Boolean;

  RGBColor : Longint;

  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;

  procedure DoTextOut(X,Y : Integer; Str : Pchar; Count: Integer);
  var
    aRect : TRect;
  begin
    with TDeviceContext(DC) do begin
      //fix me... and what about UTF-8 conversion?
      //this could be a massive problem since we
      //will need to know before hand what the current
      //locale is, and if we stored UTF-8 string this would break
      //cross-compatibility with GTK1.2 and win32 interfaces.....

      pango_layout_set_text(Layout, Str, Count);

      aRect := Classes.Rect(0,0,0,0);
      pango_layout_get_pixel_size(Layout, @arect.Right, @arect.Bottom);

      OffsetRect(aRect, X+DCOrigin.X,Y+DCOrigin.Y);

      gdk_draw_layout(drawable, gc, aRect.Left, aRect.Top, Layout);
    end;
  end;
  
  procedure DrawTextLine;
  var
    UnderLineLen, Y: integer;
    CurDistX: PInteger;
    CharsWritten, CurX, i: integer;
    LinePos: PChar;
  begin
    with TDeviceContext(DC) do begin
      if (Dx=nil) then begin
        // no dist array -> write as one block
        //fix me... do we even need to do it this way with pango?
        DoTextOut(TxtPt.X, TxtPt.Y, LineStart, LineLen);
      end else begin
        // dist array -> write each char separately
        CharsWritten:=integer(LineStart-Str);
        if DCTextMetric.IsDoubleByteChar then
          CharsWritten:=CharsWritten div 2;
        CurDistX:=Dx+CharsWritten*SizeOf(Integer);
        CurX:=TxtPt.X;
        LinePos:=LineStart;
        for i:=1 to LineLen do begin
          //fix me... do we even need to do it this way with pango?
          DoTextOut(CurX, TxtPt.Y, LinePos, 1);
          inc(LinePos);
          inc(CurX,CurDistX^);
          inc(CurDistX);
        end;
      end;
    end;
  end;

begin
  Assert(False, Format('trace:> [Tgtk2Object.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [Tgtk2Object.ExtTextOut] Uninitialized GC');
      Result := False;
    end
    else if ((Options and (ETO_OPAQUE+ETO_CLIPPED)) <> 0)
    and (Rect=nil) then begin
      WriteLn('WARNING: [Tgtk2Object.ExtTextOut] Rect=nil');
      Result := False;
    end else begin
      // TODO: implement other parameters.

      UseFontDesc:=nil;
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;

      If UseFontDesc = nil then
        WriteLn('WARNING: [Tgtk2Object.ExtTextOut] Missing Font')
      else begin
        // to reduce flickering calculate first and then paint
        DCOrigin:=GetDCOffset(TDeviceContext(DC));

        GetStyle('default');
        Layout := gtk_widget_create_pango_layout (GetStyleWidget('default'), nil);

        pango_layout_set_font_description(Layout, UseFontDesc);

        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        If CurrentFont^.Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(CurrentFont^.StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        if (Options and ETO_CLIPPED) <> 0 then
        begin
          X := Rect^.Left;
          Y := Rect^.Top;
          IntersectClipRect(DC, Rect^.Left, Rect^.Top,
                            Rect^.Right, Rect^.Bottom);
        end;

        LineLen := FindChar(#10,Str,Count);
        TopY := Y;
        UpdateDCTextMetric(TDeviceContext(DC));
        TxtPt.X := X + DCOrigin.X;
        LineHeight := DCTextMetric.TextMetric.tmAscent;
        TxtPt.Y := TopY + LineHeight + DCOrigin.Y;

        SelectedColors := dcscCustom;

        if ((Options and ETO_OPAQUE) <> 0) then
        begin
          Width := Rect^.Right - Rect^.Left;
          Height := Rect^.Bottom - Rect^.Top;
          EnsureGCColor(DC, dccCurrentBackColor, True, False);
          gdk_draw_rectangle(Drawable, GC, 1,
                             Rect^.Left+DCOrigin.X, Rect^.Top+DCOrigin.Y,
                             Width, Height);
        end;

        EnsureGCColor(DC, dccCurrentTextColor, True, False);
        Case TColor(CurrentTextColor.ColorRef) of
          clScrollbar..clEndColors:
            RGBColor := GetSysColor(CurrentTextColor.ColorRef and $FF);
          else
            RGBColor := CurrentTextColor.ColorRef and $FFFFFF;
        end;

        Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                          gushort(GetGValue(RGBColor)) shl 8,
                                          gushort(GetBValue(RGBColor)) shl 8);

        pango_attr_list_change(AttrList,Attr);

        pango_layout_set_attributes(Layout, AttrList);

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

        g_object_unref(Layout);
        Result := True;
        If UnRef then
          pango_font_description_free(UseFontDesc);
      end;
    end;
  end;
  Assert(False, Format('trace:< [Tgtk2Object.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
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
Function TGTK2Object.TextOut(DC: HDC; X,Y : Integer; Str : Pchar;
  Count: Integer) : Boolean;
var
  DCOrigin: TPoint;
  aRect : TRect;

  UnRef,
  Underline,
  StrikeOut : Boolean;

  RGBColor : Longint;

  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
begin
  Result := IsValidDC(DC);
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [Tgtk2Object.TextOut] Uninitialized GC');
    end
    else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;
      
      If UseFontDesc = nil then
        WriteLn('WARNING: [Tgtk2Object.TextOut] Missing Font')
      else begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        GetStyle('default');
        Layout := gtk_widget_create_pango_layout (GetStyleWidget('default'), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
        If Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        SelectedColors := dcscCustom;
        EnsureGCColor(DC, dccCurrentBackColor, True, False);
        EnsureGCColor(DC, dccCurrentTextColor, True, False);

        Case TColor(CurrentTextColor.ColorRef) of
          clScrollbar..clEndColors:
            RGBColor := GetSysColor(CurrentTextColor.ColorRef and $FF);
          else
            RGBColor := CurrentTextColor.ColorRef and $FFFFFF;
        end;

        Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                        gushort(GetGValue(RGBColor)) shl 8,
                                        gushort(GetBValue(RGBColor)) shl 8);

        pango_attr_list_change(AttrList,Attr);
        pango_layout_set_attributes(Layout, AttrList);

        pango_layout_set_single_paragraph_mode(Layout, TRUE);
        pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, PANGO_ALIGN_LEFT);

        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....

        pango_layout_set_text(Layout, Str, Count);

        aRect := Rect(0,0,0, 0);
        pango_layout_get_pixel_size(Layout, @arect.Right, @arect.Bottom);

        OffsetRect(aRect, X+DCOrigin.X,Y+DCOrigin.Y);
        FillRect(DC,aRect,hBrush(CurrentBrush));

        gdk_draw_layout(drawable, gc, aRect.Left, aRect.Top, Layout);
        g_object_unref(Layout);
        Result := True;
        If UnRef then
          pango_font_description_free(UseFontDesc);
      end;
    end;
  end;
end;

function Tgtk2Object.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
var
  GdiObject: PGdiObject;
  FontNameRegistry, Foundry, FamilyName, WeightName,
  Slant, SetwidthName, AddStyleName, PixelSize,
  PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
  CharSetRegistry, CharSetCoding: string;
  FullString : AnsiString;
  
  procedure LoadDefaultFont;
  begin
    DisposeGDIObject(GdiObject);
    GdiObject:=CreateDefaultFont;
  end;

begin
  Result := 0;
  GDIObject := NewGDIObject(gdiFont);
  Try
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

    // check if LongFontName is in XLFD format
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
    end else
      if (LongFontName <> '') and (Screen.Fonts.IndexOf(LongFontName) > 0) then
        FamilyName := LongFontName;

    with LogFont do begin
      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [Tgt2kObject.CreateFontIndirectEx] No fontname');
        Exit;
      end;

      if (FamilyName = '') or (AnsiCompareText(FamilyName,'*')=0) then begin
        FamilyName := StrPas(lfFaceName);
        if AnsiCompareText(FamilyName,'default')=0 then begin
          LoadDefaultFont;
          exit;
        end;
        FullString := AnsiString(FamilyName + ' ' + IntToStr(Abs(lfHeight)));
      end
      else begin
        FullString := AnsiString(FamilyName);
        if (PointSize = '') or (AnsiCompareText(PointSize,'*')=0) then
          FullString := FullString + ' 12'
        else
          FullString := FullString + ' ' + PointSize;
      end;
      
      GdiObject^.GDIFontObject := pango_font_description_from_string(PChar(FullString));
      If lfWeight <> FW_DONTCARE then
        pango_font_description_set_weight(GdiObject^.GDIFontObject, lfWeight);
      
      if lfItalic = 0 then
        pango_font_description_set_style(GdiObject^.GDIFontObject,PANGO_STYLE_NORMAL)
      else
        pango_font_description_set_style(GdiObject^.GDIFontObject,PANGO_STYLE_ITALIC);

      GdiObject^.StrikeOut := lfStrikeOut <> 0;
      GdiObject^.Underline := lfUnderline <> 0;

      Result := HFONT(GdiObject);
    end;
  finally
    if GdiObject^.GDIFontObject = nil
    then begin
      DisposeGDIObject(GdiObject);
      Result := 0;
    end
    else begin
      Result := HFONT(GdiObject);
    end;
  end;
end;

function Tgtk2Object.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
var
  DCOrigin: TPoint;
  aRect : TRect;

  UnRef,
  Underline,
  StrikeOut : Boolean;

  RGBColor : Longint;

  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
begin
  Result := IsValidDC(DC);
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;

      If UseFontDesc = nil then
        WriteLn('WARNING: [Tgtk2Object.GetTextExtentPoint] Missing Font')
      else begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        GetStyle('default');
        Layout := gtk_widget_create_pango_layout (GetStyleWidget('default'), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
        If Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        Case TColor(CurrentTextColor.ColorRef) of
          clScrollbar..clEndColors:
            RGBColor := GetSysColor(CurrentTextColor.ColorRef and $FF);
          else
            RGBColor := CurrentTextColor.ColorRef and $FFFFFF;
        end;

        Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                        gushort(GetGValue(RGBColor)) shl 8,
                                        gushort(GetBValue(RGBColor)) shl 8);

        pango_attr_list_change(AttrList,Attr);
        pango_layout_set_attributes(Layout, AttrList);

        pango_layout_set_single_paragraph_mode(Layout, TRUE);
        pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, PANGO_ALIGN_LEFT);

        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....

        pango_layout_set_text(Layout, Str, Count);

        pango_layout_get_pixel_size(Layout, @Size.cX, @Size.cY);

        g_object_unref(Layout);

        Result := True;
        If UnRef then
          pango_font_description_free(UseFontDesc);
      end;
  end;
end;

{------------------------------------------------------------------------------
  procedure Tgtk2Object.UpdateDCTextMetric(DC: TDeviceContext);
 ------------------------------------------------------------------------------}
procedure Tgtk2Object.UpdateDCTextMetric(DC: TDeviceContext);
const
  TestString = '{Am|g_}';
var
  XT : TSize;
  dummy: LongInt;
  UseFontDesc : PPangoFontDescription;
  UnRef : Boolean;
  AVGBuffer: array[#32..#126] of char;
  AvgLen: integer;
  c: char;

  Underline,
  StrikeOut : Boolean;

  Layout : PPangoLayout;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
  Extents : TPangoRectangle;
begin
  with TDeviceContext(DC) do begin
    if dcfTextMetricsValid in DCFlags then begin
      // cache valid
    end else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;

        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;

        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;
      If UseFontDesc = nil then
        WriteLn('WARNING: [Tgtk2Object.GetTextMetrics] Missing font')
      else begin
        Layout := gtk_widget_create_pango_layout (GetStyleWidget('default'), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
        If Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        pango_layout_set_attributes(Layout, AttrList);

        pango_layout_set_single_paragraph_mode(Layout, TRUE);
        pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, PANGO_ALIGN_LEFT);

        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....

        pango_layout_set_text(Layout,  TestString, length(TestString));

        pango_layout_get_extents(Layout, nil, @Extents);
        g_object_unref(Layout);

        If UnRef then
          pango_font_description_free(UseFontDesc);

        FillChar(DCTextMetric, SizeOf(DCTextMetric), 0);
        with DCTextMetric do begin
          IsDoubleByteChar:=False;//FontIsDoubleByteCharsFont(UseFont);

          for c:=Low(AVGBuffer) to High(AVGBuffer) do
            AVGBuffer[c]:=c;
          lbearing := PANGO_LBEARING(extents) div PANGO_SCALE;
          rBearing := PANGO_RBEARING(extents) div PANGO_SCALE;
          TextMetric.tmAscent := PANGO_ASCENT(extents) div PANGO_SCALE;
          TextMetric.tmDescent := PANGO_DESCENT(extents) div PANGO_SCALE;
          AvgLen:=ord(High(AVGBuffer))-ord(Low(AVGBuffer))+1;
          GetTextExtentPoint(HDC(DC), @AVGBuffer[Low(AVGBuffer)],
                             AvgLen, XT);
          if not IsDoubleByteChar then
            XT.cX := XT.cX div AvgLen
          else
            // Quick hack for double byte char fonts
            XT.cX := XT.cX div (AvgLen div 2);
          TextMetric.tmHeight := XT.cY;
          TextMetric.tmAscent := TextMetric.tmHeight - TextMetric.tmDescent;
          TextMetric.tmAveCharWidth :=  XT.cX;
          if TextMetric.tmAveCharWidth<1 then TextMetric.tmAveCharWidth:=1;
          {temp EVIL hack FIXME -->}
          AVGBuffer[Low(AVGBuffer)]:='M';
          GetTextExtentPoint(HDC(DC), @AVGBuffer[Low(AVGBuffer)],
                             1, XT);
          TextMetric.tmMaxCharWidth := XT.cX;
          AVGBuffer[Low(AVGBuffer)]:='W';
          GetTextExtentPoint(HDC(DC), @AVGBuffer[Low(AVGBuffer)],
                             1, XT);
          TextMetric.tmMaxCharWidth := Max(TextMetric.tmMaxCharWidth,XT.cX);
          {<--  temp EVIL hack FIXME}
          if TextMetric.tmMaxCharWidth<1 then
            TextMetric.tmMaxCharWidth:=1;
        end;
      end;
      Include(DCFlags,dcfTextMetricsValid);
    end;
  end;
end;

{$EndIf}

function Tgtk2Object.BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc;
var
  paintrect : TGDKRectangle;
begin
  result := Inherited BeginPaint(Handle, PS);

  If (Handle <> 0) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) then
  begin
    paintrect.x := PS.rcPaint.Left;
    paintrect.y := PS.rcPaint.Top;
    paintrect.width := PS.rcPaint.Right- PS.rcPaint.Left;
    paintrect.height := PS.rcPaint.Bottom - PS.rcPaint.Top;
    if (paintrect.width <= 0) or (paintrect.height <=0) then begin
      paintrect.x := 0;
      paintrect.y := 0;
      gdk_drawable_get_size(TDeviceContext(Result).Drawable, @paintrect.width, @paintrect.height);
    end;
    gdk_window_freeze_updates(TDeviceContext(Result).Drawable);
    gdk_window_begin_paint_rect (TDeviceContext(Result).Drawable, @paintrect);
  end;
end;

Function Tgtk2Object.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  If (Handle <> 0) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) then
  begin
    if PS.HDC <> 0 then begin
      gdk_window_end_paint (TDeviceContext(PS.HDC).Drawable);
      gdk_window_thaw_updates(TDeviceContext(PS.HDC).Drawable);
    end;
  end;

  result := Inherited EndPaint(Handle, PS);
end;

procedure Tgtk2Object.CreateComponent(Sender : TObject);
var
  Caption : ansistring;          // the caption of "Sender"
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;      // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  CompStyle,                     // componentstyle (type) of GtkWidget which will be created
  TempInt   : Integer;           // local use when neccessary
  // - for csBitBtn
  Box       : Pointer;           // currently only used for TBitBtn
  pixmapwid : pGtkWidget;        // currently only used for TBitBtn
  label1    : pgtkwidget;        // currently only used for TBitBtn
  ParentForm: TCustomForm;
  AccelText : PChar;
  AccelKey  : guint;
  SetupProps : boolean;
  AWindow: PGdkWindow;
begin
  p := nil;
  SetupProps:= false;

  CompStyle := GetCompStyle(Sender);
  Caption   := GetCaption(Sender);

  strTemp := StrAlloc(length(Caption) + 1);
  StrPCopy(strTemp, Caption);

  case CompStyle of
  csEdit :
    begin
      p :=  gtk_entry_new();
      gtk_editable_set_editable (PGtkEditable(P), not TCustomEdit(Sender).ReadOnly);
      gtk_widget_show_all(P);
    end;
    
  csMemo :
    begin
      P := gtk_scrolled_window_new(nil, nil);
      TempWidget := gtk_text_view_new();
      gtk_container_add(p, TempWidget);

      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p),GTK_SHADOW_IN);
      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;

      gtk_text_view_set_editable (PGtkTextView(TempWidget), not TCustomMemo(Sender).ReadOnly);
      if TCustomMemo(Sender).WordWrap then
        gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
      else
        gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);

      gtk_widget_show_all(P);

      SetupProps:= true;
    end;
    else begin
      StrDispose(StrTemp);
      Inherited CreateComponent(Sender);
      Exit;
    end;
  end; //end case
  StrDispose(StrTemp);
  FinishComponentCreate(Sender, P, SetupProps);
end;

{------------------------------------------------------------------------------
  Method: TGtk2Object.IntSendMessage3
  Params:  LM_Message - message to be processed by GTK2
           Sender     - sending control
           data       - pointer to (optional)
  Returns: depends on the message and the sender

  Processes messages from different components.

  WARNING: the result of this function sometimes is not always really an
           integer!!!!!
 ------------------------------------------------------------------------------}
function Tgtk2Object.IntSendMessage3(LM_Message : Integer; Sender : TObject;
  data : pointer) : integer;
var
  handle      : hwnd;                  // handle of sender
  pStr        : PChar;                 // temporary string pointer, must be allocated/disposed when used!
  Widget      : PGtkWidget;            // pointer to gtk-widget (local use when neccessary)
  ChildWidget : PGtkWidget;            // generic pointer to a child gtk-widget (local use when neccessary)
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
  aTextBuffer : PGtkTextBuffer;
  Pos         : Integer;
begin
  Result := 0;   //default value just in case nothing sets it

  Assert(False, 'Trace:Message received');
  if Sender <> nil then
    Assert(False, Format('Trace:  [Tgtk2Object.IntSendMessage3] %s --> Sent LM_Message: $%x (%s); Data: %d', [Sender.ClassName, LM_Message, GetMessageName(LM_Message), Integer(data)]));

  // The following case is now split into 2 separate parts:
  //   1st part should contain all messages which don't need the "handle" variable
  //   2nd part has to contain all parts which need the handle
  // Reason for this split are performance issues since we need RTTI to
  // retrieve the handle
{  case LM_Message of
  else}
    begin
      handle := hwnd(ObjectToGtkObject(Sender));
      Case LM_Message of
      
      LM_GETSELSTART :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
              begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, nil);
                result := gtk_text_iter_get_offset(@aTextIter1);
              end;
            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if not gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@result, nil) then
                   result := gtk_editable_get_position(GTK_EDITABLE(Widget));
              end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
            end
          end
        else
          Result:= 0;
      end;

      LM_GETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
    	      begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                result:= Abs(gtk_text_iter_get_offset(@aTextIter2) - gtk_text_iter_get_offset(@aTextIter1));
  	      end;
            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@result, @Pos) then
                   result := Pos - Result
                else
                  result := 0;
              end;
            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
          end;
      end;

      LM_SETSELSTART:
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
              begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                Writeln('TODO(GTK2): IntSendMessage3, LM_SETSELSTART, csMemo');
                {gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, nil);
                result := gtk_text_iter_get_offset(@aTextIter1);}
              end;

            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),nil, @Pos) then
                  If (Integer(Data) >= 0) and (Integer(Data)<=Pos) then
                    gtk_editable_select_region(GTK_EDITABLE(Widget), Integer(Data), Pos+1);
              end;

            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
        end
        else
          Result:= 0;
      end;

      LM_SETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
            csMemo:
    	      begin
    	        Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
                aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                Writeln('TODO(GTK2): IntSendMessage3, LM_SETSELLEN, csMemo');
                {gtk_text_buffer_get_selection_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                result:= Abs(gtk_text_iter_get_offset(@aTextIter2) - gtk_text_iter_get_offset(@aTextIter1));}
  	      end;

            csEdit:
              begin
    	        Widget:= GTK_WIDGET(Pointer(Handle));
                if gtk_editable_get_selection_bounds(GTK_EDITABLE(Widget),@Pos, nil) then
                  gtk_editable_select_region(GTK_EDITABLE(Widget), Pos, Pos+Integer(Data)+1)
                else
                  gtk_editable_select_region(GTK_EDITABLE(Widget), gtk_editable_get_position(GTK_EDITABLE(Widget)),
                    gtk_editable_get_position(GTK_EDITABLE(Widget)) + Integer(Data)+1)
              end;

            else begin
              result := inherited  IntSendMessage3(LM_Message, Sender, data);
              exit;
            end;
          end;
        end;
      end;

      else begin
        result := inherited  IntSendMessage3(LM_Message, Sender, data);
        exit;
      end; // end of else-part of 2nd case
      end; // end of 2nd case
    end;   // end of else-part of 1st case
//  end;   // end of 1st case
end;

function TGtk2Object.GetText(Sender: TComponent; var Text: String): Boolean;
var
  CS: PChar;
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
begin
  Result := True;
  case TControl(Sender).fCompStyle of
   csEdit: begin
             Widget:= GTK_WIDGET(Pointer(TWinControl(Sender).Handle));
             CS := gtk_editable_get_chars(GTK_EDITABLE(Widget), 0, -1);
             Text := StrPas(CS);
             g_free(CS);
           end;

   csMemo    : begin
      	          Widget:= GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.ImplementationWidget;
                  aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                  gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                  CS := gtk_text_buffer_get_text(aTextBuffer, @aTextIter1, @aTextIter2, True);
                  Text := StrPas(CS);
                  g_free(CS);
               end;
  else
    Result := inherited GetText(Sender, Text);
  end;
end;

{------------------------------------------------------------------------------
  procedure Tgtk2Object.AppendText(Sender: TObject; Str: PChar);
 ------------------------------------------------------------------------------}
procedure Tgtk2Object.AppendText(Sender: TObject; Str: PChar);
var
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
begin
  if Str=nil then exit;

  if (Sender is TWinControl) then begin
    case TWinControl(Sender).fCompStyle of
      csMemo:
      begin
      	Widget:= GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.ImplementationWidget;
        aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
        gtk_text_buffer_begin_user_action(aTextBuffer);
        gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
        gtk_text_buffer_insert(aTextBuffer, @aTextIter2, str, StrLen(str));
        gtk_text_buffer_end_user_action(aTextBuffer);
      end;
      else
        inherited AppendText(Sender, Str);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtk2Object.SetLabel
  Params:  sender - the calling object
           data   - String (PChar) to be set as label for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
procedure Tgtk2Object.SetLabel(Sender : TObject; Data : Pointer);
var
  Widget : PGtkWidget;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
  pLabel : PChar;
begin
  if Sender is TMenuItem then begin
    inherited SetLabel(Sender, Data);
    exit;
  end;

  if Sender is TWinControl
  then Assert(False, Format('Trace:  [Tgtk2Object.SetLabel] %s --> label %s', [Sender.ClassName, TControl(Sender).Caption]))
  else begin
    Assert(False, Format('Trace:WARNING: [Tgtk2Object.SetLabel] %s --> No Decendant of TWinControl', [Sender.ClassName]));
    RaiseException('[Tgtk2Object.SetLabel] ERROR: Sender ('+Sender.Classname+')'
        +' is not TWinControl ');
  end;

  Widget := PGtkWidget(TWinControl(Sender).Handle);
  Assert(Widget = nil, 'Trace:WARNING: [Tgtk2Object.SetLabel] --> got nil pointer');
  Assert(False, 'Trace:Setting Str1 in SetLabel');
  pLabel := pchar(Data);

  case TControl(Sender).fCompStyle of
  csEdit        : begin
                    gtk_entry_set_text(pGtkEntry(Widget), pLabel);
                    {LockOnChange(PGtkObject(Widget),+1);
                    gtk_editable_delete_text(pGtkEditable(P), 0, -1);
                    gtk_editable_insert_text(pGtkEditable(P), pLabel, StrLen(pLabel). 0);
                    LockOnChange(PGtkObject(Widget),-1);}
                  end;

  csMemo        : begin
                    Widget:= PGtkWidget(GetWidgetInfo(Widget, True)^.ImplementationWidget);
                    aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget));
                    gtk_text_buffer_begin_user_action(aTextBuffer);
                    gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_delete(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_get_bounds(aTextBuffer, @aTextIter1, @aTextIter2);
                    gtk_text_buffer_insert(aTextBuffer, @aTextIter1, pLabel, StrLen(pLabel));
                    gtk_text_buffer_end_user_action(aTextBuffer);
                  end;

  else
    inherited SetLabel(Sender, Data);
  end;
  Assert(False, Format('trace:  [Tgtk2Object.SetLabel] %s --> END', [Sender.ClassName]));
end;

procedure Tgtk2Object.HookSignals(Sender: TObject);
begin
  if (Sender is TWinControl) then
     Begin
       inherited HookSignals(Sender);
     End;

  if (Sender is TControl) then
    Begin
      case TControl(sender).FCompStyle of
        csEdit:
        begin
          SetCallback(LM_CHANGED, Sender);
          SetCallback(LM_ACTIVATE, Sender);
          SetCallback(LM_CUTTOCLIP, Sender);
          SetCallback(LM_COPYTOCLIP, Sender);
          SetCallback(LM_PASTEFROMCLIP, Sender);
        end;

	csMemo:
	begin
          SetCallback(LM_CHANGED, Sender);
          SetCallback(LM_ACTIVATE, Sender);
          SetCallback(LM_CUTTOCLIP, Sender);
          SetCallback(LM_COPYTOCLIP, Sender);
          SetCallback(LM_PASTEFROMCLIP, Sender);
	  SetCallback(LM_INSERTTEXT, Sender);
	end;
      end; //case
    end;
end;

{------------------------------------------------------------------------------
  Method: TGtk2Object.SetProperties
  Params:  Sender : the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding GTK2 object.
 ------------------------------------------------------------------------------}
function Tgtk2Object.SetProperties(Sender : TObject) : integer;
var
  wHandle    : Pointer;
  Widget, ImplWidget   : PGtkWidget;
  i : Longint;
  aTextBuffer : PGtkTextBuffer;
  aTextIter1  : TGtkTextIter;
  aTextIter2  : TGtkTextIter;
begin
  Result := 0;     // default if nobody sets it

  if Sender is TWinControl
  then
    Assert(False, Format('Trace:  [Tgtk2Object.SetProperties] %s', [Sender.ClassName]))
  else
    RaiseException('Tgtk2Object.SetProperties: '
                   +' Sender.ClassName='+Sender.ClassName);

  wHandle:= Pointer(TWinControl(Sender).Handle);
  Widget:= GTK_WIDGET(wHandle);

  case TControl(Sender).fCompStyle of
  csEdit :
    with TCustomEdit(Sender) do
      begin
        gtk_editable_set_editable(GTK_ENTRY(wHandle), not (TCustomEdit(Sender).ReadOnly));
        gtk_entry_set_max_length(GTK_ENTRY(wHandle), TCustomEdit(Sender).MaxLength);
        gtk_entry_set_visibility(GTK_ENTRY(wHandle), (TCustomEdit(Sender).EchoMode = emNormal) and (TCustomEdit(Sender).PassWordChar=#0));
        if (TCustomEdit(Sender).EchoMode = emNone) then
          gtk_entry_set_invisible_char(GTK_ENTRY(wHandle), 0)
        else
          gtk_entry_set_invisible_char(GTK_ENTRY(wHandle), Longint(TCustomEdit(Sender).PassWordChar));
      end;

  csMemo:
    begin
      ImplWidget:= GetWidgetInfo(wHandle, true)^.ImplementationWidget;

      gtk_text_view_set_editable (PGtkTextView(ImplWidget), not TCustomMemo(Sender).ReadOnly);
      if TCustomMemo(Sender).WordWrap then
        gtk_text_view_set_wrap_mode(PGtkTextView(ImplWidget), GTK_WRAP_WORD)
      else
        gtk_text_view_set_wrap_mode(PGtkTextView(ImplWidget), GTK_WRAP_NONE);


      case (Sender as TCustomMemo).Scrollbars of
        ssHorizontal:     gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_ALWAYS, GTK_POLICY_NEVER);
        ssVertical:       gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
        ssBoth:           gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
        ssAutoHorizontal: gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
        ssAutoVertical:   gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
        ssAutoBoth:       gtk_scrolled_window_set_policy(
                            GTK_SCROLLED_WINDOW(wHandle),
                            GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      else
        gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(wHandle),
                                       GTK_POLICY_NEVER, GTK_POLICY_NEVER);
      end;

      If (TCustomMemo(Sender).MaxLength >= 0) then begin
          aTextBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(ImplWidget));
	  i:= gtk_text_buffer_get_char_count(aTextBuffer);
	  if i > TCustomMemo(Sender).MaxLength then begin
             gtk_text_buffer_get_bounds(aTextBuffer, nil, @aTextIter2);
             gtk_text_buffer_get_iter_at_offset(aTextBuffer, @aTextIter1, i);
             gtk_text_buffer_delete(aTextBuffer, @aTextIter1, @aTextIter2);
	  end;
      end;
    end;
  else
    Result := inherited SetProperties(Sender);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function Tgtk2Object.GetCursorPos(var lpPoint: TPoint ): Boolean;
{$IfnDef GTK2_2} //we need a GTK2_2 FLAG somehow
  {$IfNDef Win32}
  var
    root, child: pointer;
    winx, winy: Integer;
    xmask: Cardinal;
    TopList, List: PGList;
  {$EndIf}
{$EndIf}
begin
  Result := False;
{$IfDef GTK2_2} //we need a GTK2_2 FLAG somehow
  gdk_display_get_pointer(gdk_display_get_default(), nil, @lpPoint.X, @lpPoint.Y, nil);
  Result := True;
{$Else}
  {$IfNDef Win32}
   TopList := gdk_window_get_toplevels;
   List := TopList;
   while List <> nil do
   begin
     if (List^.Data <> nil)
     and gdk_window_is_visible(List^.Data)
     then begin
       XQueryPointer(gdk_x11_drawable_get_xdisplay (List^.Data),
                     gdk_x11_drawable_get_xid(List^.Data),
                     @root, @child, @lpPoint.X, @lpPoint.Y, @winx, @winy, @xmask);

       Result := True;
       Break;
     end;
     List := g_list_next(List);
   end;

   if TopList <> nil
   then g_list_free(TopList);
   {$Else}
      // Win32 Todo
      writeln('ToDo(Win32): Tgtk2object.GetCursorPos');
  {$EndIf}
{$EndIf}
end;

Function TGtk2Object.LoadStockPixmap(StockID: longint) : HBitmap;
var
  Pixmap : PGDIObject;
  StockName : PChar;
  IconSet : PGtkIconSet;
  Pixbuf : PGDKPixbuf;
begin
  Case StockID Of
    idButtonOk : StockName := GTK_STOCK_OK;
    idButtonCancel : StockName := GTK_STOCK_CANCEL;
    idButtonYes : StockName := GTK_STOCK_YES;
    idButtonNo : StockName := GTK_STOCK_NO;
    idButtonHelp : StockName := GTK_STOCK_HELP;
    idButtonAbort : StockName := GTK_STOCK_CANCEL;
    idButtonClose : StockName := GTK_STOCK_QUIT;
    
    idDialogWarning : StockName := GTK_STOCK_DIALOG_WARNING;
    idDialogError : StockName := GTK_STOCK_DIALOG_ERROR;
    idDialogInfo : StockName := GTK_STOCK_DIALOG_INFO;
    idDialogConfirm : StockName := GTK_STOCK_DIALOG_QUESTION;
   else begin
      Result := inherited LoadStockPixmap(StockID);
      exit;
    end;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    IconSet := gtk_style_lookup_icon_set(GetStyle('button'), StockName)
  else
    IconSet := gtk_style_lookup_icon_set(GetStyle('window'), StockName);

  If (IconSet = nil) then begin
    Result := inherited LoadStockPixmap(StockID);
    exit;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle('button'), GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_BUTTON, GetStyleWidget('button'), nil)
  else
    pixbuf := gtk_icon_set_render_icon(IconSet, GetStyle('window'), GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_DIALOG, GetStyleWidget('window'), nil);

  Pixmap := NewGDIObject(gdiBitmap);
  With Pixmap^ do begin
    GDIBitmapType := gbPixmap;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    gdk_pixbuf_render_pixmap_and_mask(pixbuf, GDIPixmapObject, GDIBitmapMaskObject, 128);
  end;

  gdk_pixbuf_unref(pixbuf);
  Result := HBitmap(Pixmap);
end;

end.

{
  $Log$
  Revision 1.18  2003/09/18 21:36:00  ajgenius
  add csEdit to GTK2 interface to start removing use of GtkOldEditable

  Revision 1.17  2003/09/18 17:23:05  ajgenius
  start using GtkTextView for Gtk2 Memo

  Revision 1.16  2003/09/18 14:06:30  ajgenius
  fixed Tgtkobject.drawtext for Pango till the native pango one works better

  Revision 1.15  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.14  2003/09/17 20:30:57  ajgenius
  fix(?) DCOffset for DrawText

  Revision 1.13  2003/09/17 19:40:46  ajgenius
  Initial DoubleBuffering Support for GTK2

  Revision 1.12  2003/09/15 16:42:02  ajgenius
  mostly fixed ExtTextOut

  Revision 1.11  2003/09/15 03:10:46  ajgenius
  PANGO support for GTK2 now works.. sorta. TextOut/ExtTextOut broken?

  Revision 1.10  2003/09/12 17:40:46  ajgenius
  fixes for GTK2(accel groups, menu accel, 'draw'),
  more work toward Pango(DrawText now works, UpdateDCTextMetric mostly works)

  Revision 1.9  2003/09/10 18:03:47  ajgenius
  more changes for pango -
  partly fixed ref counting,
  added Pango versions of TextOut, CreateFontIndirectEx, and GetTextExtentPoint to the GTK2 interface

  Revision 1.8  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.7  2003/09/09 17:16:24  ajgenius
  start implementing pango routines for GTK2

  Revision 1.6  2003/09/09 04:15:08  ajgenius
  more updates for GTK2, more GTK1 wrappers, removal of more ifdef's, partly fixed signals

  Revision 1.5  2003/09/06 22:56:03  ajgenius
  started gtk2 stock icon overrides
  partial/temp(?) workaround for dc paint offsets

  Revision 1.4  2003/09/06 20:23:53  ajgenius
  fixes for gtk2
  added more wrappers for gtk1/gtk2 converstion and sanity
  removed pointless version $Ifdef GTK2 etc
  IDE now "runs" Tcontrol drawing/using problems
  renders it unuseable however

  Revision 1.3  2003/09/06 17:24:52  ajgenius
  gtk2 changes for pixmap, getcursorpos, mouse events workaround

  Revision 1.2  2003/08/27 20:55:51  mattias
  fixed updating codetools on changing pkg output dir

  Revision 1.1  2002/12/15 11:52:28  mattias
  started gtk2 interface

  Revision 1.15  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.14  2002/11/03 22:40:00  lazarus
  MG: fixed ControlAtPos

  Revision 1.13  2002/10/30 18:45:52  lazarus
  AJ: fixed compiling & removed '_' from custom stock items

  Revision 1.12  2002/10/26 15:15:50  lazarus
  MG: broke LCL<->interface circles

  Revision 1.11  2002/10/25 15:27:02  lazarus
  AJ: Moved form contents creation to gtkproc for code
      reuse between GNOME and GTK, and to make GNOME MDI
      programming easier later on.

  Revision 1.10  2002/10/24 22:10:39  lazarus
  AJ: More changes for better code reuse between gnome & gtk interfaces

  Revision 1.9  2002/10/23 20:47:27  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.8  2002/10/21 13:15:24  lazarus
  AJ:Try and fall back on default style if nil(aka default theme)

  Revision 1.7  2002/10/21 03:23:34  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.6  2002/10/15 22:28:04  lazarus
  AJ: added forcelinebreaks

  Revision 1.5  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.4  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.3  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt
}
