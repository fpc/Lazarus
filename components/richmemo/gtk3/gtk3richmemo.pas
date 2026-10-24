{
 gtk3richmemo.pas

 Author: Željan Rikalo, copied gtk2 parts authored by Dmitry 'skalogryz' Boyarintsev.

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit gtk3richmemo;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  LazGtk3, LazGdk3, LazGlib2, LazGObject2, LazPango1, LazCairo1, LazGdkPixbuf2,
  // RTL/FCL
  Types, Classes, SysUtils, StrUtils,
  // LCL
  LCLType, Controls, Graphics, LazUTF8, StdCtrls, LCLProc, LazLoggerBase,
  // Gtk3 widget
  gtk3int, gtk3widgets, gtk3objects, gtk3procs, gtk3private,
  Gtk3WSControls, Gtk3WSStdCtrls,
  // RichMemo
  RichMemo, WSRichMemo, RichMemoUtils;

const
  TagNameNumeric = 'numeric';
  TagNameSubOrSuper = 'suborsuper';
  TagNameLink = 'link';
  BulletChar = #$E2#$80#$A2;
  TabChar = #$09;

  { TGtk3WSCustomRichMemo }
type
  TGtk3WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AMemo: TGtk3Memo);
    class procedure GetWidgetBuffer(const AWinControl: TWinControl; var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);

    class function GetAttrAtIter(view: PGtkTextView; const start: TGtkTextIter): PGtkTextAttributes;
    class function GetAttrAtPos(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean = false): PGtkTextAttributes;
    class procedure GetAttributesAt(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean;
      var attr: PGtkTextAttributes; var fp: TFontParams);

    class procedure ApplyTag(abuffer: PGtkTextBuffer; tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
    class procedure FormatSubSuperScript(buffer: PGtkTextBuffer; vs: TVScriptPos; fontSizePts: Double; TextStart, TextLen: Integer);

  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure CutToClipboard(const AWinControl: TWinControl); override;
    class procedure CopyToClipboard(const AWinControl: TWinControl); override;
    class procedure PasteFromClipboard(const AWinControl: TWinControl); override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer;
      var RangeStart, RangeLen: Integer): Boolean; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetric: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AMetric: TIntParaMetric); override;

    class function GetParaNumbering(const AWinControl: TWinControl; TextStart: Integer;
      var ANumber: TIntParaNumbering): Boolean; override;
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart,
      TextLen: Integer; const ANumber: TIntParaNumbering); override;

    class procedure SetParaTabs(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AStopList: TTabStopList); override;
    class function GetParaTabs(const AWinControl: TWinControl; TextStart: integer;
      var AStopList: TTabStopList): Boolean; override;

    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange): Boolean; override;

    class procedure SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ui: TTextUIParam); override;
    class function GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer;
      var ui: TTextUIParam): Boolean; override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function CharAtPos(const AWinControl: TWinControl; x,y: Integer): Integer; override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class function ImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
         const FileNameUTF8: string;
         const AImgSize: TSize
      ): Boolean;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;

    // inline handler
    class function InlineInsert(const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
      const ASize: TSize; AHandler: TRichMemoInline; var wsObj: TRichMemoInlineWSObject): Boolean; override;
    class procedure InlineInvalidate(const AWinControl: TWinControl;
       AHandler: TRichMemoInline; wsObj: TRichMemoInlineWSObject); override;
  end;

  { TGtk3InlineObject }

  TGtk3InlineObject = class(TRichMemoInlineWSObject)
  public
    anch: PGtkTextChildAnchor;
    wgt: PGtkWidget;
    il: TRichMemoInline;
    size: TSize;
    cnv: TCanvas;
    constructor Create;
    destructor Destroy; override;
  end;

const
  SubSuperFontKoef = 0.583; // the koef for the font size of sub/super scripts matching Adover Illustrator and OpenOffice
  SuperRiseKoef = 0.58;
  SubRiseKoef = -0.08;

implementation

type
  TRichMemoData = record
    link: Boolean;
    link_li: integer;
    link_le: integer;
    link_act: TGdkEventType;
    link_btn: guint;
    link_hover: Boolean;
  end;
  PRichMemoData = ^TRichMemoData;

const
  RichMemoDataKey = 'lcl-richmemo-data';
  PageDPI = 72; // not expected to be changed

var
  linkCursor: PGdkCursor = nil;
  textCursor: PGdkCursor = nil;

function ScreenDPI: Double;
begin
  Result := gdk_screen_get_resolution(gdk_screen_get_default);
  if Result <= 0 then
    Result := 96;
end;

function GdkColorToTColor(const c: TGdkColor): TColor;
begin
  Result := RGBToColor(c.red shr 8, c.green shr 8, c.blue shr 8);
end;

procedure FreeRichMemoData(data: gpointer); cdecl;
begin
  Dispose(PRichMemoData(data));
end;

function GetRichMemoData(view: PGtkTextView): PRichMemoData;
begin
  Result := PRichMemoData(g_object_get_data(PGObject(view), RichMemoDataKey));
end;

function GtkTextAttrToFontParams(const textAttr: TGtkTextAttributes; var FontParams: TIntFontParams; const FontKoef: Double = 1.0): Boolean;
var
  st: TPangoStyle;
  pf: PPangoFontDescription;
  sz: double;
begin
  InitFontParams(FontParams);

  pf := textAttr.font;
  Result := Assigned(pf);
  if not Result then Exit;

  FontParams.Name := pango_font_description_get_family(pf);
  sz := pango_font_description_get_size(pf) / PANGO_SCALE;
  if pango_font_description_get_size_is_absolute(pf) then
    sz := sz * PageDPI / ScreenDPI;
  FontParams.Size := round(sz*FontKoef);
  if Ord(pango_font_description_get_weight(pf)) > Ord(PANGO_WEIGHT_NORMAL) then
    Include(FontParams.Style, fsBold);

  st := pango_font_description_get_style(pf);
  if st in [PANGO_STYLE_ITALIC, PANGO_STYLE_OBLIQUE] then
    Include(FontParams.Style, fsItalic);

  if Assigned(textAttr.appearance.rgba[1]) then
    FontParams.Color := TGdkRGBAToTColor(textAttr.appearance.rgba[1]^)
  else
    FontParams.Color := GdkColorToTColor(textAttr.appearance.fg_color);
  if textAttr.appearance.Bitfield0.underline <> 0 then
    Include(FontParams.Style, fsUnderline);
  if textAttr.appearance.Bitfield0.strikethrough <> 0 then
    Include(FontParams.Style, fsStrikeOut);
  FontParams.HasBkClr := textAttr.appearance.Bitfield0.draw_bg <> 0;
  if FontParams.HasBkClr then
  begin
    if Assigned(textAttr.appearance.rgba[0]) then
      FontParams.BkColor := TGdkRGBAToTColor(textAttr.appearance.rgba[0]^)
    else
      FontParams.BkColor := GdkColorToTColor(textAttr.appearance.bg_color);
  end;
end;

type
  TCustomRichMemoInt = class(TCustomRichMemo);

function IsCursorMark(Textbuffer: PGtkTextBuffer; mark: PGtkTextMark): Boolean;
begin
  Result := (mark = gtk_text_buffer_get_insert(Textbuffer)) or
    (mark = gtk_text_buffer_get_selection_bound(Textbuffer));
end;

procedure Gtk3WS_MemoSelChanged_Before(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; Data: TGtk3Widget); cdecl;
var
  tag: PGtkTextTag;
  iter: TGtkTextIter;
begin
  if not IsCursorMark(Textbuffer, mark) then
    exit;
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(TextBuffer)
    , TagNameNumeric);
  if not Assigned(tag) then
    exit;

  iter := StartIter^;
  // navigate "through" numbering characters
  if gtk_text_iter_has_tag(@iter, tag) then
  begin
    // if tried to move at the "endo
    if gtk_text_iter_starts_tag(@iter, tag) then
    begin
      gtk_text_iter_forward_to_tag_toggle(@iter, nil);
      gtk_text_buffer_move_mark(TextBuffer, mark, @iter);
    end else
    begin
      gtk_text_iter_forward_char(@iter);
      if gtk_text_iter_ends_tag(@iter, tag) then
      begin
        gtk_text_iter_backward_to_tag_toggle(@iter, nil);
        gtk_text_iter_backward_char(@iter);
        gtk_text_buffer_move_mark(TextBuffer, mark, @iter);
      end;
    end;
  end;
end;

procedure Gtk3WS_MemoSelChanged (Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; Data: TGtk3Widget); cdecl;
begin
  if not IsCursorMark(Textbuffer, mark) then
    exit;
  if Assigned(Data) and (Data.LCLObject is TCustomRichMemo) then
    TCustomRichMemoInt(Data.LCLObject).DoSelectionChange;
end;

procedure Gtk3WS_RichMemoInsert(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; text: PChar; len: gint; Data: TGtk3Widget); cdecl;
var
  rm: TCustomRichMemo;
  iter: TGtkTextIter;
  tag: PGtkTextTag;
  w: PGtkWidget;
  b: PGtkTextBuffer;
  attr: PGtkTextAttributes;
begin
  if not Assigned(Data) or not (Data.LCLObject is TCustomRichMemo) then
    exit;
  rm := TCustomRichMemo(Data.LCLObject);
  // re-zooming any newly entered (pasted, manually inserted text)
  if (rm.ZoomFactor<>1) then
  begin
    TGtk3WSCustomRichMemo.GetWidgetBuffer(rm, w, b);
    if not Assigned(b) then
      exit;
    iter := StartIter^;
    gtk_text_iter_backward_chars(@iter, UTF8Length(text, len));
    attr := gtk_text_view_get_default_attributes(PGtkTextView(w));
    gtk_text_iter_get_attributes(@iter, attr);

    if attr^.font_scale<>rm.ZoomFactor then
    begin
      tag := gtk_text_buffer_create_tag(b, nil,
          'scale', [gdouble(rm.ZoomFactor),
          'scale-set', gboolean(True),
          nil]);
      gtk_text_buffer_apply_tag(b, tag, @iter, StartIter);
    end;
    gtk_text_attributes_unref(attr);
  end;
end;

procedure Gtk3WS_Backspace(view: PGtkTextView; Data: TGtk3Widget); cdecl;
var
  buf: PGtkTextBuffer;
  mark: PGtkTextMark;
  iend: TGtkTextIter;
  istart: TGtkTextIter;
  tag: PGtkTextTag;
begin
  // this handler checks, if the "numbering" should be erarsed
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then
    exit;
  mark := gtk_text_buffer_get_insert(buf);
  if not Assigned(mark) then
    exit;
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buf)
    , TagNameNumeric);
  if not Assigned(tag) then
    exit;

  // first, check if cursor is right "after" the "numbering characters"
  gtk_text_buffer_get_iter_at_mark(buf, @iend, mark);

  if gtk_text_iter_ends_tag(@iend, tag) then
  begin
    // cursor position is at the beginning of the line - erase all
    // characters that belong to the numbering.
    istart:=iend;
    gtk_text_iter_backward_to_tag_toggle(@istart, tag);
    gtk_text_buffer_delete(buf, @istart, @iend);
    // prevent default backspace
    g_signal_stop_emission_by_name(PGObject(view), 'backspace');
  end;
end;

function GatherNumeirc(buf: PGtkTextBuffer; const istart: TGtkTextIter; var num: TIntParaNumbering): Boolean;
var
  iend: TGtkTextIter;
  ch: Pgchar;
  s: string;
  i: Integer;
  err: Integer;
  tab: Integer;
  us: UnicodeString;
begin
  iend:=istart;
  gtk_text_iter_forward_to_tag_toggle(@iend, nil);
  ch:=gtk_text_iter_get_text(@istart, @iend);
  Result:=Assigned(ch);
  if not Result then
    exit;
  s:=ch;
  g_free(ch);
  Result:=length(s)>1;
  if not Result then
    exit;

  tab := Pos(TabChar, s);
  if tab > 0 then
    s := Copy(s, 1, tab-1);
  Result := s <> '';
  if not Result then
    exit;

  if Copy(s, 1, length(BulletChar)) = BulletChar then
    num.Style:=pnBullet
  else
  if s[1] in ['0'..'9'] then
  begin
    num.Style:=pnNumber;
    i:=1;
    while (i<=length(s)) and (s[i] in ['0'..'9']) do
      inc(i);
    Val( Copy(s, 1, i-1), num.NumberStart, err);
    if i<=length(s) then
      num.SepChar := WideChar(s[i])
    else
      num.SepChar := SepNone;
  end else begin
    num.Style:=pnCustomChar;
    us := UTF8Decode(UTF8Copy(s, 1, 1));
    if us <> '' then
      num.CustomChar := us[1];
  end;
end;

function Gtk3_RichMemoMotion(view: PGtkTextView; Event: PGdkEventMotion;
  Data: TGtk3Widget): gboolean; cdecl;
var
  i: TGtkTextIter;
  tag: PGtkTextTag;
  buf: PGtkTextBuffer;
  w: PGdkWindow;
  bx,by: gint;
  rd: PRichMemoData;
  onlink: Boolean;
begin
  Result:=false;
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then
    exit;
  rd := GetRichMemoData(view);
  if not Assigned(rd) then
    exit;

  tag := gtk_text_tag_table_lookup(
           gtk_text_buffer_get_tag_table(buf), TagNameLink);
  if not Assigned(tag) then
    exit; // which is odd.

  gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT,
    round(Event^.x), round(Event^.y), @bx, @by);

  onlink := gtk_text_view_get_iter_at_location(view, @i, bx,by)
    and gtk_text_iter_has_tag(@i,tag);
  if onlink = rd^.link_hover then
    exit;
  rd^.link_hover := onlink;

  w:= gtk_text_view_get_window(view, GTK_TEXT_WINDOW_TEXT);
  if not Assigned(w) then
    exit;
  if onlink then
  begin
    if not Assigned(linkCursor) then
      linkCursor:=gdk_cursor_new_for_display(gtk_widget_get_display(PGtkWidget(view)), GDK_HAND2);
    gdk_window_set_cursor(w, linkCursor);
  end else
  begin
    if not Assigned(textCursor) then
      textCursor:=gdk_cursor_new_for_display(gtk_widget_get_display(PGtkWidget(view)), GDK_XTERM);
    gdk_window_set_cursor(w, textCursor);
  end;
end;

function Gtk3_RichMemoMouseButton(view: PGtkTextView; Event: PGdkEventButton;
  Data: TGtk3Widget): gboolean; cdecl;
var
  i: TGtkTextIter;
  tag: PGtkTextTag;
  buf: PGtkTextBuffer;
  bx,by: gint;
  mi: TLinkMouseInfo;
  li,le: integer;
  act: TLinkAction;
  rd: PRichMemoData;
begin
  Result:=false;
  if not Assigned(Data) or not (Data.LCLObject is TCustomRichMemo) then
    exit;
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then
    exit;
  rd:=GetRichMemoData(view);
  if not Assigned(rd) then
    exit;

  tag := gtk_text_tag_table_lookup(
           gtk_text_buffer_get_tag_table(buf), TagNameLink);
  if not Assigned(tag) then
    exit; // which is odd.

  gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT,
    round(Event^.x), round(Event^.y), @bx, @by);

  if not gtk_text_view_get_iter_at_location(view, @i, bx,by) then
  begin
    rd^.link:=false;
    exit;
  end;

  if gtk_text_iter_has_tag(@i,tag) then
  begin
    if not gtk_text_iter_starts_tag(@i, tag) then
      gtk_text_iter_backward_to_tag_toggle(@i, tag);
    li:=gtk_text_iter_get_offset(@i);
    gtk_text_iter_forward_to_tag_toggle(@i, tag);
    le:=gtk_text_iter_get_offset(@i)-li;

    case Event^.type_ of
      GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS:
      begin
        rd^.link:=true;
        rd^.link_li:=li;
        rd^.link_le:=le;
        rd^.link_act:=Event^.type_;
        rd^.link_btn:=Event^.button;
      end;

      GDK_BUTTON_RELEASE:
      begin
        act:=laClick;
        if (rd^.link) and (rd^.link_btn=Event^.button) and (rd^.link_li=li) and (le=rd^.link_le) then
        begin
          rd^.link:=false;
          mi.LinkRef:='';
          case Event^.button of
            2: mi.button:=mbMiddle;
            3: mi.button:=mbRight;
          else
            mi.button:=mbLeft;
          end;
          TCustomRichMemoInt(Data.LCLObject).DoLinkAction(act, mi, li, le);
        end;
      end;
    end;
  end else
    rd^.link:=false;
end;


{ TGtk3InlineObject }

constructor TGtk3InlineObject.Create;
begin
  inherited Create;

end;

destructor TGtk3InlineObject.Destroy;
begin
  if Assigned(wgt) then
  begin
    g_signal_handlers_disconnect_matched(PGObject(wgt), [G_SIGNAL_MATCH_DATA], 0, 0, nil, nil, Self);
    g_object_remove_weak_pointer(PGObject(wgt), Pgpointer(@wgt));
    wgt := nil;
  end;
  cnv.Free;
  inherited Destroy;
end;

class procedure TGtk3WSCustomRichMemo.SetCallbacks(const AMemo: TGtk3Memo);
var
  TextBuf: PGtkTextBuffer;
  view: PGtkTextView;
begin
  view := PGtkTextView(AMemo.GetContainerWidget);
  TextBuf := gtk_text_view_get_buffer(view);
  g_signal_connect_data(PGObject(view), 'backspace', TGCallback(@Gtk3WS_Backspace), AMemo, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(TextBuf), 'mark-set', TGCallback(@Gtk3WS_MemoSelChanged_Before), AMemo, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(TextBuf), 'mark-set', TGCallback(@Gtk3WS_MemoSelChanged), AMemo, nil, [G_CONNECT_AFTER]);
  g_signal_connect_data(PGObject(TextBuf), 'insert-text', TGCallback(@Gtk3WS_RichMemoInsert), AMemo, nil, [G_CONNECT_AFTER]);
  g_signal_connect_data(PGObject(view), 'motion-notify-event', TGCallback(@Gtk3_RichMemoMotion), AMemo, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(view), 'button-press-event', TGCallback(@Gtk3_RichMemoMouseButton), AMemo, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(view), 'button-release-event', TGCallback(@Gtk3_RichMemoMouseButton), AMemo, nil, G_CONNECT_DEFAULT);
end;

class procedure TGtk3WSCustomRichMemo.GetWidgetBuffer(const AWinControl: TWinControl;
    var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);
begin
  TextWidget:=nil;
  Buffer:=nil;
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then
    exit;

  TextWidget := TGtk3Widget(AWinControl.Handle).GetContainerWidget;
  if not Assigned(TextWidget) then
    exit;

  Buffer := gtk_text_view_get_buffer(PGtkTextView(TextWidget));
end;

class function TGtk3WSCustomRichMemo.GetAttrAtIter(view: PGtkTextView; const start: TGtkTextIter): PGtkTextAttributes;
var
  attr: PGtkTextAttributes;
begin
  Result:=nil;
  attr := gtk_text_view_get_default_attributes(view);
  if not Assigned(attr) then
    exit;

  gtk_text_iter_get_attributes(@start, attr);
  Result:=attr;
end;

class function TGtk3WSCustomRichMemo.GetAttrAtPos(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean ): PGtkTextAttributes;
var
  TextWidget: PGtkWidget;
  buffer: PGtkTextBuffer;
  iter: TGtkTextIter;
begin
  Result := nil;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then
    exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, TextStart);
  if APara then
    gtk_text_iter_set_line_offset(@iter, 0);

  Result := GetAttrAtIter(PGtkTextView(TextWidget), iter);
end;

class procedure TGtk3WSCustomRichMemo.ApplyTag(abuffer: PGtkTextBuffer;
  tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
var
  istart: TGtkTextIter;
  iend: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset (abuffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (abuffer, @iend, TextStart+TextLen);
  if ToParagraphs then
  begin
    gtk_text_iter_set_line_offset(@istart, 0);
    if not gtk_text_iter_ends_line(@iend) then
      gtk_text_iter_forward_to_line_end(@iend);
  end;
  gtk_text_buffer_apply_tag(abuffer, tag, @istart, @iend);
end;

class function TGtk3WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  AMemo: TGtk3Memo;
  view: PGtkTextView;
  buffer: PGtkTextBuffer;
  gcolor: TGdkRGBA;
  data: PRichMemoData;
begin
  Result := TGtk3WSCustomMemo.CreateHandle(AWinControl, AParams);
  if Result = 0 then
    exit;
  AMemo := TGtk3Memo(Result);

  view := PGtkTextView(AMemo.GetContainerWidget);
  buffer := gtk_text_view_get_buffer(view);

  New(data);
  FillChar(data^, sizeof(TRichMemoData), 0);
  g_object_set_data_full(PGObject(view), RichMemoDataKey, data, @FreeRichMemoData);

  gtk_text_buffer_create_tag (buffer, TagNameNumeric,
      'editable', [gboolean(False),
      'editable-set', gboolean(True),
      nil]);

  gtk_text_buffer_create_tag (buffer, TagNameSubOrSuper, nil, []);

  gcolor := TColortoTGdkRGBA(clBlue);
  gtk_text_buffer_create_tag (buffer, TagNameLink, 'foreground-rgba', [@gcolor,
      'foreground-set', gboolean(True),
      'underline', gint(Ord(PANGO_UNDERLINE_SINGLE)),
      'underline-set', gboolean(True),
      nil] );

  SetCallbacks(AMemo);
end;

class procedure TGtk3WSCustomRichMemo.DestroyHandle(
  const AWinControl: TWinControl);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
begin
  GetWidgetBuffer(AWinControl, w, b);

  // uninstall handlers, to prevent crashes
  if Assigned(b) then
    g_signal_handlers_disconnect_matched(PGObject(b), [G_SIGNAL_MATCH_DATA], 0, 0, nil, nil, Pointer(AWinControl.Handle));
  if Assigned(w) then
    g_signal_handlers_disconnect_matched(PGObject(w), [G_SIGNAL_MATCH_DATA], 0, 0, nil, nil, Pointer(AWinControl.Handle));

  TGtk3WSWinControl.DestroyHandle(AWinControl);
end;

class function TGtk3WSCustomRichMemo.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then
  begin
    Result:=-1;
    exit;
  end;
  Result := 0;
  if not gtk_text_buffer_get_selection_bounds(b, @istart, @iend) then
    exit;

  Result := Abs(gtk_text_iter_get_offset(@iend) - gtk_text_iter_get_offset(@istart));
end;

class function TGtk3WSCustomRichMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  Result := TGtk3MemoStrings.Create(ACustomMemo);
end;

class procedure TGtk3WSCustomRichMemo.CutToClipboard(const AWinControl: TWinControl);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
    exit;
  gtk_text_buffer_cut_clipboard(b,
    gtk_widget_get_clipboard(w, gdk_atom_intern('CLIPBOARD', False)),
    gtk_text_view_get_editable(PGtkTextView(w)));
end;

class procedure TGtk3WSCustomRichMemo.CopyToClipboard(const AWinControl: TWinControl);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
    exit;
  gtk_text_buffer_copy_clipboard(b,
    gtk_widget_get_clipboard(w, gdk_atom_intern('CLIPBOARD', False)));
end;

class procedure TGtk3WSCustomRichMemo.PasteFromClipboard(const AWinControl: TWinControl);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
    exit;
  gtk_text_buffer_paste_clipboard(b,
    gtk_widget_get_clipboard(w, gdk_atom_intern('CLIPBOARD', False)), nil,
    gtk_text_view_get_editable(PGtkTextView(w)));
end;

class function TGtk3WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
  begin
    Result:=false;
    exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart+1);
  if gtk_text_iter_get_offset(@istart)<>TextStart+1 then
  begin
    Result:=false; // TextStart is beyoned the end of text
    exit;
  end;

  gtk_text_iter_backward_to_tag_toggle(@istart, nil);
  RangeStart:=gtk_text_iter_get_offset(@istart);

  gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
  gtk_text_iter_forward_to_tag_toggle(@iend, nil);

  RangeLen:=gtk_text_iter_get_offset(@iend)-RangeStart;
  Result:=true;
end;

class procedure TGtk3WSCustomRichMemo.FormatSubSuperScript(buffer: PGtkTextBuffer; vs: TVScriptPos; fontSizePts: Double; TextStart, TextLen: Integer);
var
  sz: gint;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
  tag: PGtkTextTag;
  scrtag: PGtkTextTag;
  hasscript: Boolean;
  k: Double;
begin
  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  scrtag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buffer), TagNameSubOrSuper );
  if not Assigned(scrtag) then
    exit;
  hasscript := gtk_text_iter_has_tag( @istart, scrtag );

  if not hasscript then
  begin
    iend:=istart;
    gtk_text_iter_forward_to_tag_toggle(@iend, scrtag);
    hasscript:=gtk_text_iter_get_offset(@iend)<TextStart+TextLen;
  end;

  if vs = vpNormal then
  begin
    // no need to do anything, since no modifications were applied;
    if not hasscript then
      exit;
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
    gtk_text_buffer_remove_tag(buffer, scrtag, @istart, @iend);
    tag := gtk_text_buffer_create_tag (buffer, nil,
        'rise', [gint(0),
        'rise-set', gboolean(True),
        nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end else
  begin
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);

    if vs = vpSubScript then
      k := SubRiseKoef
    else
      k := SuperRiseKoef;
    sz := round(fontSizePts * k * PANGO_SCALE);

    tag := gtk_text_buffer_create_tag (buffer, nil,
        'rise', [sz,
        'rise-set', gboolean(True),
        'size-points', gdouble(fontSizePts*SubSuperFontKoef),
        'size-set', gboolean(True),
        nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
    gtk_text_buffer_apply_tag(buffer, scrtag, @istart, @iend);
  end;
end;

class procedure TGtk3WSCustomRichMemo.GetAttributesAt(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean;
  var attr: PGtkTextAttributes; var fp: TFontParams);
var
  iter: TGtkTextIter;
  v: PGtkWidget;
  b: PGtkTextBuffer;
  tag: PGtkTextTag;
begin
  attr:=nil;
  InitFontParams(fp);
  GetWidgetBuffer(AWinControl, v, b);
  if not Assigned(b) then
    exit;

  gtk_text_buffer_get_iter_at_offset(b, @iter, TextStart);
  if APara then gtk_text_iter_set_line_offset(@iter, 0);

  attr:=GetAttrAtIter(PGtkTextView(v), iter);
  if not Assigned(attr) then
    exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), TagNameSubOrSuper );

  if Assigned(tag) and gtk_text_iter_has_tag(@iter, tag) then
  begin
    GtkTextAttrToFontParams(attr^, fp, 1 / SubSuperFontKoef );
    if attr^.appearance.rise < 0 then
      fp.VScriptPos:=vpSubScript
    else
    if attr^.appearance.rise > 0 then
      fp.VScriptPos:=vpSuperScript;
  end else
    GtkTextAttrToFontParams(attr^, fp);
end;


class procedure TGtk3WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams);
var
  TextWidget: PGtkWidget;
  buffer: PGtkTextBuffer;
  tag: PGtkTextTag;
  fgcolor: TGdkRGBA;
  bgcolor: TGdkRGBA;
  nm: string;
const
  pu: array [Boolean] of TPangoUnderline = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  pb: array [Boolean] of TPangoWeight = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  pi: array [Boolean] of TPangoStyle = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  fgcolor := TColortoTGdkRGBA(Params.Color);
  bgcolor := TColortoTGdkRGBA(Params.BkColor);
  nm := Params.Name;
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'family', [PChar(nm),
      'family-set', gboolean(nm <> ''),
      'foreground-rgba', @fgcolor,
      'foreground-set', gboolean(True),
      'background-rgba', @bgcolor,
      'background-set', gboolean(Params.HasBkClr),
      'size-points', gdouble(Params.Size),
      'size-set', gboolean(True),
      'underline', gint(Ord(pu[fsUnderline in Params.Style])),
      'underline-set', gboolean(True),
      'weight', gint(Ord(pb[fsBold in Params.Style])),
      'weight-set', gboolean(True),
      'style', gint(Ord(pi[fsItalic in Params.Style])),
      'style-set', gboolean(True),
      'strikethrough', gboolean(fsStrikeOut in Params.Style),
      'strikethrough-set', gboolean(True),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen);

  FormatSubSuperScript(buffer, Params.VScriptPos, Params.Size, TextStart, TextLen);
end;

class function TGtk3WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: TIntParaAlignment
  ): Boolean;
var
  attr: PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result := Assigned(attr);
  if Result then
  begin
    case attr^.justification of
      GTK_JUSTIFY_LEFT: AAlign:=paLeft;
      GTK_JUSTIFY_RIGHT: AAlign:=paRight;
      GTK_JUSTIFY_CENTER: AAlign:=paCenter;
      GTK_JUSTIFY_FILL: AAlign:=paJustify;
    else
      AAlign:=paLeft;
    end;
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk3WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w: PGtkWidget;
  buffer: PGtkTextBuffer;
  tag: PGtkTextTag;
  val: TGtkJustification;
begin
  case AAlign of
    paRight: val:=GTK_JUSTIFY_RIGHT;
    paCenter: val:=GTK_JUSTIFY_CENTER;
    paJustify: val:=GTK_JUSTIFY_FILL;
  else
    val := GTK_JUSTIFY_LEFT;
  end;
  GetWidgetBuffer(AWinControl, w, buffer);
  if not Assigned(buffer) then
    exit;
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'justification', [gint(Ord(val)),
      'justification-set', gboolean(True),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
end;

class function TGtk3WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetric: TIntParaMetric): Boolean;
var
  attr: PGtkTextAttributes;
  fp: TFontParams;
  PixToPt: Double;
begin
  GetAttributesAt(AWinControl, TextStart, true, attr, fp);
  Result := Assigned(attr);
  if Result then
  begin
    PixToPt := PageDPI / ScreenDPI;
    if attr^.indent<0 then
    begin
      AMetric.FirstLine:=(attr^.left_margin)*PixToPt;
      AMetric.HeadIndent:=(-attr^.indent+attr^.left_margin)*PixToPt;
    end else
    begin
      AMetric.FirstLine:=(attr^.left_margin+attr^.indent)*PixToPt;
      AMetric.HeadIndent:=attr^.left_margin*PixToPt;
    end;
    AMetric.TailIndent:=attr^.right_margin*PixToPt;

    AMetric.SpaceBefore:=attr^.pixels_above_lines*PixToPt;
    AMetric.SpaceAfter:=attr^.pixels_below_lines*PixToPt;
    if fp.Size > 0 then
      AMetric.LineSpacing:=(attr^.pixels_inside_wrap*PixToPt+fp.Size*DefLineSpacing)/fp.Size
    else
      AMetric.LineSpacing:=DefLineSpacing;
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk3WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AMetric: TIntParaMetric);
var
  w: PGtkWidget;
  buffer: PGtkTextBuffer;
  tag: PGtkTextTag;
  h: double;
  fl: double;
  t: double;
  attr: PGtkTextAttributes;
  fp: TFontParams;
  DPIFactor: Double;
  ls: Double;
  wrap: Integer;
begin
  GetWidgetBuffer(AWinControl, w, buffer);
  if not Assigned(buffer) then
    exit;

  DPIFactor := ScreenDPI / PageDPI;
  h:=AMetric.HeadIndent;
  if h<0 then h:=0;
  fl:=AMetric.FirstLine;
  if fl<0 then fl:=0;

  if fl<h then
  begin
    t:=h;
    h:=fl;
    fl:=fl-t;
  end else
    fl:=fl-h;

  GetAttributesAt(AWinControl, TextStart, true, attr, fp);
  if Assigned(attr) then
    gtk_text_attributes_unref(attr);

  ls := AMetric.LineSpacing;
  if ls <= 0 then
    ls := DefLineSpacing;

  wrap := round(fp.Size*(ls-DefLineSpacing)*DPIFactor);
  if wrap < 0 then
    wrap := 0;

  tag := gtk_text_buffer_create_tag (buffer, nil,
      'pixels-above-lines', [gint(round(AMetric.SpaceBefore*DPIFactor)),
      'pixels-above-lines-set', gboolean(True),
      'pixels-below-lines', gint(round(AMetric.SpaceAfter*DPIFactor)),
      'pixels-below-lines-set', gboolean(True),
      'left-margin', gint(round(h*DPIFactor)),
      'left-margin-set', gboolean(True),
      'right-margin', gint(round(AMetric.TailIndent*DPIFactor)),
      'right-margin-set', gboolean(True),
      'indent', gint(round(fl*DPIFactor)),
      'indent-set', gboolean(True),
      'pixels-inside-wrap', gint(wrap),
      'pixels-inside-wrap-set', gboolean(True),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
end;

class function TGtk3WSCustomRichMemo.GetParaNumbering(
  const AWinControl: TWinControl; TextStart: Integer;
  var ANumber: TIntParaNumbering): Boolean;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  tag: PGtkTextTag;
begin
  InitParaNumbering(ANumber);
  GetWidgetBuffer(AWinControl, w, b);
  Result := Assigned(b);
  if not Result then
    exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b)
    , TagNameNumeric);
  if not Assigned(tag) then
    exit;

  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  gtk_text_iter_set_line_offset(@istart, 0);
  if gtk_text_iter_starts_tag(@istart, tag) then
    if not GatherNumeirc(b, istart, ANumber) then
      InitParaNumbering(ANumber);
end;

function NumberingText(const ANumber: TIntParaNumbering; numidx: Integer): string;
begin
  case ANumber.Style of
    pnBullet: Result := BulletChar;
    pnNumber: Result := IntToStr(numidx);
    pnLowLetter: Result := Chr(Ord('a') + ((numidx-1) mod 26));
    pnUpLetter: Result := Chr(Ord('A') + ((numidx-1) mod 26));
    pnLowRoman: Result := LowerCase(IntToRoman(numidx));
    pnUpRoman: Result := IntToRoman(numidx);
    pnCustomChar: Result := UTF8Encode(ANumber.CustomChar);
  else
    Result := '';
  end;
  if not (ANumber.Style in [pnBullet, pnCustomChar]) and (ANumber.SepChar<>SepNone) then
    Result := Result + UTF8Encode(ANumber.SepChar);
  Result := Result + TabChar;
end;

class procedure TGtk3WSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
  txt: String;
  len: Integer;
  ln: Integer;
  ls: Integer;
  ofs: Integer;
  numidx: Integer;
  tag: PGtkTextTag;
begin
  inherited SetParaNumbering(AWinControl, TextStart, TextLen, ANumber);
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) or not Assigned(b) then
    exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b)
    , TagNameNumeric);
  if not Assigned(tag) then
    exit;

  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  iend:=istart;
  gtk_text_iter_forward_chars(@iend, TextLen);
  ln:=gtk_text_iter_get_line(@istart);
  ls:=gtk_text_iter_get_line(@iend);

  numidx:=1;
  if (ANumber.Style in [pnNumber, pnLowLetter, pnUpLetter, pnLowRoman, pnUpRoman])
    and (ANumber.NumberStart > 0) then
    numidx:=ANumber.NumberStart;

  repeat
    gtk_text_iter_set_line_offset(@istart, 0);
    ofs:=gtk_text_iter_get_offset(@istart);

    // remove existing number
    if gtk_text_iter_starts_tag(@istart, tag) then
    begin
      iend:=istart;
      gtk_text_iter_forward_to_tag_toggle(@iend, tag);
      gtk_text_buffer_delete(b, @istart, @iend);
      gtk_text_buffer_get_iter_at_offset (b, @istart, ofs);
    end;

    if ANumber.Style <> pnNone then
    begin
      txt := NumberingText(ANumber, numidx);
      // insert new number
      gtk_text_buffer_insert(b, @istart, PChar(txt), length(txt));

      // restoring iterators
      gtk_text_buffer_get_iter_at_offset (b, @istart, ofs);
      iend := istart;

      len:=UTF8Length(txt);
      gtk_text_iter_forward_chars(@iend, len);
      // aplying tag
      gtk_text_buffer_apply_tag(b, tag, @istart, @iend);
    end;

    // next line!
    if not gtk_text_iter_forward_line(@istart) then
      break;

    inc(ln);
    inc(numidx);
  until ln>ls;
end;

class procedure TGtk3WSCustomRichMemo.SetParaTabs(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AStopList: TTabStopList);
var
  w: PGtkWidget;
  buffer: PGtkTextBuffer;
  tag: PGtkTextTag;
  parr: PPangoTabArray;
  i: Integer;
  DPIFactor: Double;
begin
  GetWidgetBuffer(AWinControl, w, buffer);
  if not Assigned(w) or not Assigned(buffer) then
    exit;

  DPIFactor := ScreenDPI / PageDPI;
  if AStopList.Count=0 then
    parr:=nil
  else
  begin
    parr:=pango_tab_array_new(AStopList.Count, true);
    for i:=0 to AStopList.Count-1 do
    begin
      pango_tab_array_set_tab(parr, i, PANGO_TAB_LEFT, round(AStopList.Tabs[i].Offset * DPIFactor) );
    end;
  end;

  tag := gtk_text_buffer_create_tag (buffer, nil,
      'tabs', [parr,
      'tabs-set', gboolean(AStopList.Count>0),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);

  if Assigned(parr) then
    pango_tab_array_free(parr);
end;

class function TGtk3WSCustomRichMemo.GetParaTabs(
  const AWinControl: TWinControl; TextStart: integer;
  var AStopList: TTabStopList): Boolean;
var
  i: Integer;
  attr: PGtkTextAttributes;
  loc: gint;
  al: TPangoTabAlign;
  f: Double;
begin
  InitTabStopList(AStopList);
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result:=Assigned(attr);
  if not Result then
    exit;
  try
    if not Assigned(attr^.tabs) then
      exit;

    AStopList.Count:=pango_tab_array_get_size(attr^.tabs);
    if AStopList.Count=0 then
      exit;

    f := PageDPI / ScreenDPI;
    if not pango_tab_array_get_positions_in_pixels(attr^.tabs) then
      f:= f / PANGO_SCALE;

    SetLength(AStopList.Tabs, AStopList.Count);
    for i:=0 to AStopList.Count-1 do
    begin
      pango_tab_array_get_tab(attr^.tabs, i, @al, @loc);
      AStopList.Tabs[i].Offset:=loc*f;
      AStopList.Tabs[i].Align:=tabLeft;
    end;
  finally
    gtk_text_attributes_unref(attr);
  end;
end;

class function TGtk3WSCustomRichMemo.GetParaRange(
  const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange
  ): Boolean;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
  begin
    Result:=false;
    exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
  gtk_text_iter_set_line_offset(@istart, 0);
  if not gtk_text_iter_ends_line(@iend) then
    gtk_text_iter_forward_to_line_end(@iend);
  rng.start:=gtk_text_iter_get_offset(@istart);
  rng.lengthNoBr:=gtk_text_iter_get_offset(@iend)-rng.start;

  // if there's a character to move, then it's end of line, if not then it won't change!
  gtk_text_iter_forward_char(@iend);
  rng.length:=gtk_text_iter_get_offset(@iend)-rng.start;
  Result:=true;
end;

class procedure TGtk3WSCustomRichMemo.SetTextUIParams(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ui: TTextUIParam);
var
  TextWidget: PGtkWidget;
  buffer: PGtkTextBuffer;
  table: PGtkTextTagTable;
  tag: PGtkTextTag;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then
    exit;

  table := gtk_text_buffer_get_tag_table(buffer);
  tag := gtk_text_tag_table_lookup(table, TagNameLink);
  if not Assigned(tag) then
    exit;

  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
  if uiLink in ui.features then
  begin
    gtk_text_tag_set_priority(tag, gtk_text_tag_table_get_size(table)-1);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end else
    gtk_text_buffer_remove_tag(buffer, tag, @istart, @iend);
end;

class function TGtk3WSCustomRichMemo.GetTextUIParams(
  const AWinControl: TWinControl; TextStart: Integer;
  var ui: TTextUIParam): Boolean;
var
  TextWidget: PGtkWidget;
  buffer: PGtkTextBuffer;
  tag: PGtkTextTag;
  istart: TGtkTextIter;
begin
  ui.features:=[];
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  Result:=Assigned(buffer);
  if not Result then
    exit;

  tag := gtk_text_tag_table_lookup(
           gtk_text_buffer_get_tag_table(buffer), TagNameLink);
  if Assigned(tag) then
  begin // which is odd.
    gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
    if gtk_text_iter_has_tag(@istart, tag) then
      Include(ui.features, uiLink);
  end;
end;

class procedure TGtk3WSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
    exit;
  gtk_text_buffer_get_iter_at_offset (b, @istart, DstStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, DstStart+DstLen);
  gtk_text_buffer_delete(b, @istart, @iend);
  if length(TextUTF8)>0 then
    gtk_text_buffer_insert(b, @istart, PChar(TextUTF8), length(TextUTF8));
end;

class function TGtk3WSCustomRichMemo.CharAtPos(const AWinControl: TWinControl;
  x, y: Integer): Integer;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  gx, gy: gint;
  trailing: gint;
begin
  Result:=-1;
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) then
    exit;

  trailing:=0;
  gtk_text_view_window_to_buffer_coords(PGtkTextView(w), GTK_TEXT_WINDOW_WIDGET, x, y, @gx, @gy);
  gtk_text_view_get_iter_at_position(PGtkTextView(w), @istart, @trailing, gx,gy);
  Result:=gtk_text_iter_get_offset(@istart)+trailing;
end;

class function TGtk3WSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  TextWidget: PGtkWidget;
  buffer: PGtkTextBuffer;
  istart: TGtkTextIter;
  ilimit: TGtkTextIter;
  start_match: TGtkTextIter;
  end_match: TGtkTextIter;
  Found: Boolean;
  opt: TGtkTextSearchFlags;
  lim: Integer;
begin
  Result := -1;
  if ANiddle = '' then
    exit;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then
  exit;

  opt:=[GTK_TEXT_SEARCH_VISIBLE_ONLY];
  if not (soMatchCase in SearchOpts.Options) then
    Include(opt, GTK_TEXT_SEARCH_CASE_INSENSITIVE);

  gtk_text_buffer_get_iter_at_offset(buffer, @istart, SearchOpts.start);
  if not (soBackward in SearchOpts.Options) then
  begin
    if SearchOpts.len < 0 then
      gtk_text_buffer_get_end_iter(buffer, @ilimit)
    else
      gtk_text_buffer_get_iter_at_offset(buffer, @ilimit, SearchOpts.start+SearchOpts.len);
  end else
  begin
    lim := 0;
    if SearchOpts.len >= 0 then
      lim := SearchOpts.start-SearchOpts.len;
    if lim < 0 then lim := 0;
    gtk_text_buffer_get_iter_at_offset(buffer, @ilimit, lim);
  end;

  repeat
    if not (soBackward in SearchOpts.Options) then
      Found := gtk_text_iter_forward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @ilimit)
    else
      Found := gtk_text_iter_backward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @ilimit);

    if not Found then
      exit;

    if not (soWholeWord in SearchOpts.Options)
      or (gtk_text_iter_starts_word(@start_match) and gtk_text_iter_ends_word(@end_match)) then
    begin
      Result := gtk_text_iter_get_offset(@start_match);
      exit;
    end;

    if not (soBackward in SearchOpts.Options) then
      istart := end_match
    else
      istart := start_match;
  until false;
end;

class function TGtk3WSCustomRichMemo.ImageFromFile(
  const ARichMemo: TCustomRichMemo; APos: Integer; const FileNameUTF8: string;
  const AImgSize: TSize): Boolean;
var
  t: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  pix: PGdkPixbuf;
  scaled: PGdkPixbuf;
  err: PGError;
  DPIFactor: Double;
  k: Double;
begin
  Result:=false;
  GetWidgetBuffer(ARichMemo, t, b);
  if not Assigned(b) then
    exit;

  err:=nil;
  DPIFactor := ScreenDPI / PageDPI;

  if (AImgSize.cx=0) and (AImgSize.cy=0) then
  begin
    pix := gdk_pixbuf_new_from_file(PChar(FileNameUTF8), @err);
    k := ScreenDPI / 96;
    if Assigned(pix) and (k <> 1) then
    begin
      scaled := gdk_pixbuf_scale_simple(pix, round(gdk_pixbuf_get_width(pix) * k),
        round(gdk_pixbuf_get_height(pix) * k), GDK_INTERP_BILINEAR);
      if Assigned(scaled) then
      begin
        g_object_unref(PGObject(pix));
        pix := scaled;
      end;
    end;
  end else
    pix := gdk_pixbuf_new_from_file_at_size(PChar(FileNameUTF8),
      round(AImgSize.cx * DPIFactor), round(AImgSize.cy * DPIFactor), @err);

  Result:=Assigned(pix);
  if Result then
  begin
    gtk_text_buffer_get_iter_at_offset(b, @istart, APos);
    gtk_text_buffer_insert_pixbuf(b, @istart, pix);
    g_object_unref(PGObject(pix));
  end;
  if Assigned(err) then
  begin
    DebugLn(['TGtk3WSCustomRichMemo.ImageFromFile ', err^.message]);
    g_error_free(err);
  end;
end;

class procedure TGtk3WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
var
  TextMark: PGtkTextMark;
  CursorIter: TGtkTextIter;
  w: PGtkWidget;
  b: PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then
    exit;

  if NewStart = -1 then
  begin
    //always scroll so the cursor is visible
    TextMark := gtk_text_buffer_get_insert(b);
    gtk_text_buffer_get_iter_at_mark(b, @CursorIter, TextMark);
  end else
  begin
    //SelStart was used and we should move to that location
    gtk_text_buffer_get_iter_at_offset(b, @CursorIter, NewStart);
    gtk_text_buffer_place_cursor(b, @CursorIter); // needed to move the cursor
    TextMark := gtk_text_buffer_get_insert(b);
  end;
  gtk_text_view_scroll_to_mark(PGtkTextView(w), TextMark, 0, False, 0, 0);
end;

class procedure TGtk3WSCustomRichMemo.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  TextMark: PGtkTextMark;
  StartIter,
  EndIter: TGtkTextIter;
  Offset: Integer;
  w: PGtkWidget;
  b: PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then
    exit;

  TextMark := gtk_text_buffer_get_insert(b);
  gtk_text_buffer_get_iter_at_mark(b, @StartIter, TextMark);

  Offset := gtk_text_iter_get_offset(@StartIter);

  gtk_text_buffer_get_iter_at_offset(b, @EndIter, Offset+NewLength);

  gtk_text_buffer_select_range(b, @StartIter, @EndIter);
end;

class procedure TGtk3WSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  tag: PGtkTextTag;
  istart: TGtkTextIter;
  iend: TGtkTextIter;
  p: PGtkTextAttributes;
  sc: gdouble;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
    exit;

  sc:=1;
  p:=GetAttrAtPos(AWinControl, 0);
  if Assigned(p) then
  begin
    sc:=p^.font_scale;
    gtk_text_attributes_unref(p);
  end;
  if sc=0 then
    sc:=1;
  //restore the scale.
  //for whatever reason, scale is always assumed as a multiplier!
  //thus it is necessary to "unscale" the previous value as well
  sc:=1/sc*AZoomFactor;

  tag := gtk_text_buffer_create_tag(b, nil,
      'scale', [gdouble(sc),
      'scale-set', gboolean(True),
      nil]);

  gtk_text_buffer_get_start_iter(b, @istart);
  gtk_text_buffer_get_end_iter(b, @iend);
  gtk_text_buffer_apply_tag(b, tag, @istart, @iend);
end;

function GtkDrawableDraw(widget: PGtkWidget; cr: Pcairo_t;
  gi: TGtk3InlineObject): gboolean; cdecl;
var
  DC: TGtk3DeviceContext;
begin
  Result:=False;
  if not Assigned(widget) or not Assigned(cr) or not Assigned(gi)
    or not Assigned(gi.il) or not Assigned(gi.cnv) then
      exit;
  DC := TGtk3DeviceContext.CreateFromCairo(widget, cr);
  try
    gi.cnv.Handle := HDC(DC);
    gi.il.Draw(gi.cnv, gi.size);
  finally
    gi.cnv.Handle := 0;
    DC.Free;
  end;
  Result:=True;
end;

class function TGtk3WSCustomRichMemo.InlineInsert(
  const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
  const ASize: TSize; AHandler: TRichMemoInline;
  var wsObj: TRichMemoInlineWSObject): Boolean;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  anch: PGtkTextChildAnchor;
  gi: TGtk3InlineObject;
  draw: PGtkWidget;
  sz: TSize;
  DPIFactor: Double;
begin
  Result:=false;
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then
    exit;

  DPIFactor := ScreenDPI / PageDPI;
  gi:=TGtk3InlineObject.Create;

  gtk_text_buffer_get_iter_at_offset(b, @istart, ATextStart);
  anch:=gtk_text_buffer_create_child_anchor(b, @istart);

  draw:=PGtkWidget(gtk_drawing_area_new);

  sz.cx:=round(ASize.cx * DPIFactor);
  sz.cy:=round(ASize.cy * DPIFactor);

  gi.il:=AHandler;
  gi.anch:=anch;
  gi.wgt:=draw;
  gi.size:=sz;
  gi.cnv:=TCanvas.Create;
  g_object_add_weak_pointer(PGObject(draw), Pgpointer(@gi.wgt));

  g_signal_connect_data(PGObject(draw), 'draw', TGCallback(@GtkDrawableDraw), gi, nil, G_CONNECT_DEFAULT);
  gtk_widget_set_size_request(draw, sz.cx, sz.cy);
  gtk_text_view_add_child_at_anchor(PGtkTextView(w), draw, anch);

  gtk_widget_show(draw);
  gi.il.SetVisible(true);

  wsObj:=gi;
  Result:=true;
end;

class procedure TGtk3WSCustomRichMemo.InlineInvalidate(
  const AWinControl: TWinControl; AHandler: TRichMemoInline;
  wsObj: TRichMemoInlineWSObject);
var
  gi: TGtk3InlineObject;
begin
  if not Assigned(wsObj) or not (wsObj is TGtk3InlineObject) then
    exit;
  gi := TGtk3InlineObject(wsObj);
  if Assigned(gi.wgt) then
    gtk_widget_queue_draw(gi.wgt);
end;


class function TGtk3WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl;
   TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  attr: PGtkTextAttributes;
begin
  GetAttributesAt(AWinControl, TextStart, false, attr, params);
  Result := Assigned(attr);
  if Result then
    gtk_text_attributes_unref(attr);
end;

function GtkInsertImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize
  ): Boolean;
begin
  Result:=TGtk3WSCustomRichMemo.ImageFromFile(ARichMemo, APos, FileNameUTF8, AImgSize);
end;

function GtkWSGetFontParams(fontref: HFONT; var params: TFontParams): Boolean;
var
  AFont: TGtk3Font;
  PangoDesc: PPangoFontDescription;
  sz: Double;
begin
  Result:=(fontref<>0) and Gtk3WidgetSet.IsValidGDIObject(fontref)
    and (TObject(fontref) is TGtk3Font);
  if not Result then
    exit;

  AFont := TGtk3Font(fontref);
  Result := IsFontNameDefault(AFont.LogFont.lfFaceName);
  if not Result then
    exit;

  PangoDesc := AFont.Handle;
  Result := Assigned(PangoDesc);
  if not Result then
    exit;

  //default font name
  params.Name := StrPas(pango_font_description_get_family(PangoDesc));

  //default font size
  if AFont.LogFont.lfHeight = 0 then
  begin
    sz := pango_font_description_get_size(PangoDesc) / PANGO_SCALE;
    if pango_font_description_get_size_is_absolute(PangoDesc) then
      sz := sz * PageDPI / ScreenDPI;
    params.Size := round(sz);
  end;

  // rely on LogFont structure to be initialiazed (as it seems to be)
  if AFont.LogFont.lfItalic > 0 then
    Include(params.Style, fsItalic);
  if AFont.LogFont.lfWeight >= FW_BOLD then
    Include(params.Style, fsBold);
  if AFont.LogFont.lfUnderline > 0 then
    Include(params.Style, fsUnderline);
  if AFont.LogFont.lfStrikeOut > 0 then
    Include(params.Style, fsStrikeOut);
end;

initialization
  InsertImageFromFile := @GtkInsertImageFromFile;
  WSGetFontParams:=@GtkWSGetFontParams;

finalization
  if Assigned(linkCursor) then
    g_object_unref(PGObject(linkCursor));
  if Assigned(textCursor) then
    g_object_unref(PGObject(textCursor));

end.
