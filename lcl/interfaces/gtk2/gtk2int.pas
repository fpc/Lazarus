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

{off $DEFINE USE_PANGO}

uses
  Classes, SysUtils,
  {$IfNDef GTK2_2}
    {$IfNDef Win32}
     X, XLib, XUtil,
    {$EndIf}
  {$EndIf}

  gdk2pixbuf, gtk2, gdk2, glib2, Pango,

   LMessages, Controls, Forms, VclGlobals, LCLProc,
  LCLStrConsts, LCLLinux, LCLType, DynHashArray, LazLinkedList,
  GraphType, GraphMath, Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls,
  ComCtrls, CListBox, KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls,
  Dialogs, ExtDlgs, FileCtrl, LResources, Math, GTKGlobals,

  gtkDef,   gtkProc, gtkInt;

type
  TGtk2Object = class(TGtkObject)
  public
    function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
    function LoadStockPixmap(StockID: longint) : HBitmap; override;
    {$Ifdef USE_PANGO} // we should implement pango for gtk2 soon
    function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; override;
    function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;
    function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;
    function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
    function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
    procedure UpdateDCTextMetric(DC: TDeviceContext); override;
    {$EndIf}
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
begin
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
        if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then
          UseFontDesc := GetDefaultFontDesc(false)
        else
          UseFontDesc := CurrentFont^.GDIFontObject;

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

        gdk_draw_layout(drawable, gc, X, Y, Layout);
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
  RGBColor : Longint;

  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;

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
        TextOut(DC,TxtPt.X, TxtPt.Y, LineStart, LineLen);
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
          TextOut(DC,CurX, TxtPt.Y, LinePos, 1);
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

      // to reduce flickering calculate first and then paint
      DCOrigin:=GetDCOffset(TDeviceContext(DC));

     { UseFontDesc:=nil;
      if (Str<>nil) and (Count>0) then begin
        if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then
          UseFontDesc := GetDefaultFontDesc(false)
        else
          UseFontDesc := CurrentFont^.GDIFontObject;

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

        if UseFontDesc <> nil then begin           }
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
        {end else begin
          WriteLn('WARNING: [Tgtk2Object.ExtTextOut] Missing Font');
          Result := False;
        end;
      end;  }

      if ((Options and ETO_OPAQUE) <> 0) then
      begin
        Width := Rect^.Right - Rect^.Left;
        Height := Rect^.Bottom - Rect^.Top;
        SelectedColors := dcscCustom;
        EnsureGCColor(DC, dccCurrentBackColor, True, False);
        
      end;

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

      {Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                        gushort(GetGValue(RGBColor)) shl 8,
                                        gushort(GetBValue(RGBColor)) shl 8);

      pango_attr_list_change(AttrList,Attr);

      pango_layout_set_attributes(Layout, AttrList);
       }
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
  FamilyName : string;

  procedure LoadDefaultFont;
  begin
    DisposeGDIObject(GdiObject);
    GdiObject:=CreateDefaultFont;
  end;

begin
  Result := 0;
  GDIObject := NewGDIObject(gdiFont);

  with LogFont do begin
    if lfFaceName[0] = #0
    then begin
      Assert(false,'ERROR: [Tgt2kObject.CreateFontIndirectEx] No fontname');
      Exit;
    end;

    FamilyName := StrPas(lfFaceName); //StringReplace(FaceName, ' ', '*');
    if AnsiCompareText(FamilyName,'default')=0 then begin
      LoadDefaultFont;
      exit;
    end;

    GdiObject^.GDIFontObject := pango_font_description_from_string(PChar(AnsiString(FamilyName + ' ' + IntToStr(Abs(lfHeight)))));
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
    if GC = nil
    then begin
      WriteLn('WARNING: [Tgtk2Object.GetTextExtentPoint] Uninitialized GC');
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
        WriteLn('WARNING: [Tgtk2Object.GetTextExtentPoint] Missing Font')
      else begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));

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
