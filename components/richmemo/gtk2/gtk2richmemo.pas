{
 gtk2richmemo.pas

 Author: Dmitry 'skalogryz' Boyarintsev

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

unit Gtk2RichMemo;

{$mode objfpc}{$H+}

interface

{$ifdef RMLCLTRUNK}
{$define OneParamGetWidgetInfo}
{$else}
{$if FPC_FULLVERSION<30000}
{$else}
{$define OneParamGetWidgetInfo}
{$endif}
{$endif}


uses
  // Bindings
  gtk2, glib2, gdk2, pango,
  // RTL/FCL
  Types, Classes, SysUtils,
  // LCL
  LCLType, Controls, Graphics, LazUTF8, StdCtrls, LCLProc,
  // Gtk2 widget
  Gtk2Int, Gtk2Def,
  GTK2WinApiWindow, Gtk2Globals, Gtk2Proc,
  gdk2pixbuf, Gtk2WSStdCtrls,
  // RichMemo
  RichMemo, WSRichMemo, RichMemoUtils;

const
  TagNameNumeric = 'numeric';
  TagNameSubOrSuper = 'suborsuper';
  TagNameLink       = 'link';
  BulletChar     = #$E2#$80#$A2;
  TabChar        = #$09;

  { TGtk2WSCustomRichMemo }
type
  TGtk2WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
    class procedure GetWidgetBuffer(const AWinControl: TWinControl; var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);

    class function GetAttrAtIter(view: PGtkTextView; const start: TGtkTextIter): PGtkTextAttributes;
    class function GetAttrAtPos(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean = false): PGtkTextAttributes;
    class procedure GetAttributesAt(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean;
      var attr: PGtkTextAttributes; var fp: TFontParams);

    class procedure ApplyTag(abuffer: PGtkTextBuffer; tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
    class procedure FormatSubSuperScript(buffer: PGtkTextBuffer; vs: TVScriptPos; fontSizePts: Double; TextStart, TextLen: Integer);

  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

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

  { TGtk2InlineObject }

  TGtk2InlineObject = class(TRichMemoInlineWSObject)
  public
    anch   : PGtkTextChildAnchor;
    wgt    : PGtkWidget;
    il     : TRichMemoInline;
    size   : TSize;
    cnv    : TCanvas;
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TGtk2RichMemoStrings }

  TGtk2RichMemoStrings = class(TGtk2MemoStrings)
  protected
    FGtkText : PGtkTextView;
    FGtkBuf: PGtkTextBuffer;
    FOwner: TWinControl;
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
  public
    constructor Create(TextView : PGtkTextView; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    property Owner: TWinControl read FOwner;
  end;


const
  SubSuperFontKoef = 0.583; // the koef for the font size of sub/super scripts matching Adover Illustrator and OpenOffice
  SuperRiseKoef =  0.58;
  SubRiseKoef   = -0.08;

implementation

type
  TRichMemoData = record
    link    : Boolean;
    link_li : integer;
    link_le : integer;
    link_act: TGdkEventType;
    link_btn: guint;
  end;
  PRichMemoData = ^TRichMemoData;

var
  linkCursor: PGdkCursor = nil;// gdk_cursor_new(GDK_DRAFT_LARGE)

// todo: why "shr" on each of this flag test?
function gtktextattr_underline(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_underline) shr bp_TGtkTextAppearance_underline) > 0;
end;

function gtktextattr_strikethrough(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_strikethrough) shr bp_TGtkTextAppearance_strikethrough) > 0;
end;

function gtktextattr_bkcolor(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_draw_bg ) shr bp_TGtkTextAppearance_draw_bg) > 0;
end;


function GtkTextAttrToFontParams(const textAttr: TGtkTextAttributes; var FontParams: TIntFontParams; const FontKoef : Double = 1.0): Boolean;
var
  w   : integer;
  st  : TPangoStyle;
  pf  : PPangoFontDescription;
  sz  : double;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
begin
  InitFontParams(FontParams);

  pf := textAttr.font;
  Result := Assigned(pf);
  if not Result then Exit;

  if Assigned(pf) then begin
    FontParams.Name := pango_font_description_get_family(pf);
    FontParams.Size := pango_font_description_get_size(pf);
    sz:=FontParams.Size / PANGO_SCALE;
    if pango_font_description_get_size_is_absolute(pf) then
      sz:=sz/(ScreenDPI/PageDPI);
    FontParams.Size:=round(sz*FontKoef);
    w := pango_font_description_get_weight(pf);
    if w > PANGO_WEIGHT_NORMAL then Include(FontParams.Style, fsBold);

    st := pango_font_description_get_style(pf);
    if st and PANGO_STYLE_ITALIC > 0 then  Include(FontParams.Style, fsItalic);
  end;

  FontParams.Color := TGDKColorToTColor(textAttr.appearance.fg_color);
  if gtktextattr_underline(textAttr.appearance) then  Include(FontParams.Style, fsUnderline);
  if gtktextattr_strikethrough(textAttr.appearance) then Include(FontParams.Style, fsStrikeOut);
  FontParams.HasBkClr := gtktextattr_bkcolor(textAttr.appearance);
  if FontParams.HasBkClr then
    FontParams.BkColor := TGDKColorToTColor(textAttr.appearance.bg_color);

end;

type
  TGtk2WSCustomMemoInt = class(TGtk2WSCustomMemo);
  TCustomRichMemoInt   = class(TCustomRichMemo);

procedure Gtk2WS_MemoSelChanged_Before(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; WidgetInfo: PWidgetInfo); cdecl;
var
  tag : PGtkTextTag;
begin
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(TextBuffer)
    , TagNameNumeric);

  // navigate "through" numbering characters
  if gtk_text_iter_has_tag( StartIter, tag) then begin
    // if tried to move at the "endo
    if gtk_text_iter_begins_tag(StartIter, tag) then begin
      gtk_text_iter_forward_to_tag_toggle(StartIter, nil);
      gtk_text_buffer_move_mark(TextBuffer, mark, StartIter);
    end else begin
      gtk_text_iter_forward_char(StartIter);
      if gtk_text_iter_ends_tag(StartIter, tag) then begin
        gtk_text_iter_backward_to_tag_toggle(StartIter, nil);
        gtk_text_iter_backward_char(StartIter);
        gtk_text_buffer_move_mark(TextBuffer, mark, StartIter);
      end;
    end;
  end;
end;

procedure Gtk2WS_MemoSelChanged (Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; WidgetInfo: PWidgetInfo); cdecl;
begin
  if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
  begin
    TCustomRichMemoInt(WidgetInfo^.LCLObject).DoSelectionChange;
  end;
end;

procedure Gtk2WS_RichMemoInsert(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; text: PChar; len: gint; WidgetInfo: PWidgetInfo); cdecl;
var
  rm : TCustomRichMemo;
  iter : PGtkTextIter;
  tag  : PGtkTextTag;
  w    : PGtkWidget;
  b    : PGtkTextBuffer;
  attr : PGtkTextAttributes;
begin
  if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
  begin
    rm := TCustomRichMemo(WidgetInfo^.LCLObject);
    // re-zooming any newly entered (pasted, manually inserted text)
    if (rm.ZoomFactor<>1) then begin
      TGtk2WSCustomRichMemo.GetWidgetBuffer(rm, w, b);
      iter:=gtk_text_iter_copy(StartIter);
      gtk_text_iter_backward_chars(iter, len);
      attr := gtk_text_view_get_default_attributes(PGtkTextView(w));
      gtk_text_iter_get_attributes(iter, attr);

      if attr^.font_scale<>rm.ZoomFactor then begin
        tag := gtk_text_buffer_create_tag(b, nil,
            'scale', [   gdouble(rm.ZoomFactor),
            'scale-set', gboolean(gTRUE),
            nil]);
        gtk_text_buffer_apply_tag(b, tag, iter, StartIter);
      end;
      gtk_text_attributes_unref(attr);
    end;
  end;
end;

procedure Gtk2WS_Backspace(view: PGtkTextView; WidgetInfo: PWidgetInfo); cdecl;
var
  buf    : PGtkTextBuffer;
  mark   : PGtkTextMark;
  iend   : TGtkTextIter;
  istart : TGtkTextIter;
  tag    : PGtkTextTag;
begin
  // this handler checks, if the "numbering" should be erarsed
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then Exit;
  mark := gtk_text_buffer_get_mark(buf, 'insert');
  if not Assigned(mark) then Exit;
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buf)
    , TagNameNumeric);
  if not Assigned(tag) then Exit;

  // first, check if cursor is right "after" the "numbering characters"
  gtk_text_buffer_get_iter_at_mark(buf, @iend, mark);

  if gtk_text_iter_ends_tag(@iend, tag) then begin
    // cursor position is at the beginning of the line - erase all
    // characters that belong to the numbering.
    istart:=iend;
    gtk_text_iter_backward_to_tag_toggle(@istart, tag);
    gtk_text_buffer_delete(buf, @istart, @iend);
    // prevent default backspace
    g_signal_stop_emission_by_name(view, 'backspace');
  end;
end;

function GatherNumeirc(buf: PGtkTextBuffer; const istart: TGtkTextIter; var num: TIntParaNumbering): Boolean;
var
  iend : TGtkTextIter;
  ch   : Pgchar;
  s    : string;
  i    : Integer;
  err  : Integer;
begin
  iend:=istart;
  gtk_text_iter_forward_to_tag_toggle(@iend, nil);
  ch:=gtk_text_iter_get_text(@istart, @iend);
  Result:=Assigned(ch);
  if not Result then Exit;
  s:=ch;
  g_free(ch);
  Result:=length(s)>1;
  if not Result then Exit;

  if Pos(BulletChar, s[1]) = 1 then num.Style:=pnBullet
  else if s[1] in ['0'..'9'] then begin
    num.Style:=pnNumber;
    i:=1;
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
    Val( Copy(s, 1, i-1), num.NumberStart, err);
  end else begin
    num.Style:=pnCustomChar;

  end;

  //ch.
  //gtk_text_buffer_insert_range
end;

function Gtk2_RichMemoKeyPress(view: PGtkTextView; Event: PGdkEventKey;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  buf    : PGtkTextBuffer;
  mark   : PGtkTextMark;
  istart : TGtkTextIter;
  tag    : PGtkTextTag;
begin
  if Event^.keyval= GDK_KEY_Return then begin
    //writeln('return !');
    buf:=gtk_text_view_get_buffer(view);
    if not Assigned(buf) then Exit;
    mark := gtk_text_buffer_get_mark(buf, 'insert');
    if not Assigned(mark) then Exit;
    tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buf)
      , TagNameNumeric);
    if not Assigned(tag) then Exit;
    gtk_text_buffer_get_iter_at_mark(buf, @istart, mark);

    gtk_text_iter_set_line_offset(@istart, 0);
    if gtk_text_iter_begins_tag(@istart, tag) then begin
      //writeln('apply!');
      //writeln( 'ofs: ', gtk_text_iter_get_offset(@istart));
    end;
  end;
  Result:=false;
end;


function Gtk2_RichMemoMotion(view: PGtkTextView; Event: PGdkEventKey;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  mt : PGdkEventMotion;
  i  : TGtkTextIter;
  tag : PGtkTextTag;
  buf : PGtkTextBuffer;
  w   : PGdkWindow;
  bx,by: gint;
begin
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then Exit;

  tag := gtk_text_tag_table_lookup(
           gtk_text_buffer_get_tag_table(buf), TagNameLink);
  if not Assigned(tag) then Exit; // which is odd.

  mt:=PGdkEventMotion(Event);
  gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT,
    round(mt^.x), round(mt^.y), @bx, @by);

  gtk_text_view_get_iter_at_location(view, @i, bx,by);

  gdk_cursor_new(GDK_DRAFT_LARGE);
  w:= gtk_text_view_get_window(view, GTK_TEXT_WINDOW_TEXT);
  if gtk_text_iter_has_tag(@i,tag) then begin

    if not Assigned(linkCursor) then
      linkCursor:=gdk_cursor_new(GDK_HAND1);
    gdk_window_set_cursor(w, linkCursor);
  end else
    gdk_window_set_cursor(w, nil);

  Result:=false;
end;

function Gtk2_RichMemoMouseButton(view: PGtkTextView; Event: PGdkEventKey;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  mt : PGdkEventButton;
  i  : TGtkTextIter;
  tag : PGtkTextTag;
  buf : PGtkTextBuffer;
  bx,by: gint;
  mi  : TLinkMouseInfo;
  li,le: integer;
  act : TLinkAction;
  data : PRichMemoData;
begin
  buf:=gtk_text_view_get_buffer(view);
  data:=PRichMemoData(WidgetInfo^.UserData);
  if not Assigned(buf) then Exit;

  tag := gtk_text_tag_table_lookup(
           gtk_text_buffer_get_tag_table(buf), TagNameLink);
  if not Assigned(tag) then Exit; // which is odd.

  mt:=PGdkEventButton(Event);
  gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT,
    round(mt^.x), round(mt^.y), @bx, @by);

  gtk_text_view_get_iter_at_location(view, @i, bx,by);

  if gtk_text_iter_has_tag(@i,tag) then begin
    if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
    begin
      gtk_text_iter_backward_to_tag_toggle(@i, tag);
      li:=gtk_text_iter_get_offset(@i);
      gtk_text_iter_forward_to_tag_toggle(@i, tag);
      le:=gtk_text_iter_get_offset(@i)-li;

      case mt^._type of
        GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS: begin
          data^.link:=true;
          data^.link_li:=li;
          data^.link_le:=le;
          data^.link_act:=mt^._type;
          data^.link_btn:=mt^.button;
        end;

        GDK_BUTTON_RELEASE: begin
          act:=laClick;
          if (data^.link) and (data^.link_btn=mt^.button) and (data^.link_li=li) and (le=data^.link_le) then
          begin
            data^.link:=false;
            case mt^.button of
              2: mi.button:=mbMiddle;
              3: mi.button:=mbRight;
            else
              mi.button:=mbLeft;
            end;
            TCustomRichMemoInt(WidgetInfo^.LCLObject).DoLinkAction(act, mi, li, le);
          end;
        end;
      end;
    end;
  end;

  Result:=false;
end;


{ TGtk2InlineObject }

constructor TGtk2InlineObject.Create;
begin
  inherited Create;

end;

destructor TGtk2InlineObject.Destroy;
begin
  cnv.Free;
  inherited Destroy;
end;

{ TGtk2RichMemoStrings }

function TGtk2RichMemoStrings.GetTextStr: string;
var
  StartIter, EndIter: TGtkTextIter;
  AText: PgChar;
begin
  Result := '';
  gtk_text_buffer_get_start_iter(FGtkBuf, @StartIter);
  gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter);

  AText := gtk_text_iter_get_text(@StartIter, @EndIter);
  Result := StrPas(AText);
  if AText <> nil then
    g_free(AText);
end;

function TGtk2RichMemoStrings.GetCount: integer;
begin
  Result := gtk_text_buffer_get_line_count(FGtkBuf);
  if Get(Result-1) = '' then Dec(Result);
end;

function TGtk2RichMemoStrings.Get(Index: Integer): string;
var
  StartIter, EndIter: TGtkTextIter;
  AText: PgChar;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = gtk_text_buffer_get_line_count(FGtkBuf) then
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  else begin
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index);
    gtk_text_iter_forward_to_line_end(@EndIter);
  end;
  // if a row is blank gtk_text_iter_forward_to_line_end will goto the row ahead
  // this is not desired. so if it jumped ahead a row then the row we want is blank
  if gtk_text_iter_get_line(@StartIter) = gtk_text_iter_get_line(@EndIter) then
  begin
    AText := gtk_text_iter_get_text(@StartIter, @EndIter);
    Result := StrPas(AText);
    g_free(AText);
  end
  else
    Result := '';
end;

constructor TGtk2RichMemoStrings.Create(TextView: PGtkTextView;
  TheOwner: TWinControl);
begin
  inherited Create(TextView, TheOwner);
  if TextView = nil then RaiseGDBException(
    'TGtk2RichMemoStrings.Create Unspecified Text widget');
  FGtkText:= TextView;
  FGtkBuf := gtk_text_view_get_buffer(FGtkText);
  if TheOwner = nil then RaiseGDBException(
    'TGtk2RichMemoStrings.Create Unspecified owner');
  FOwner:=TheOwner;
end;

destructor TGtk2RichMemoStrings.Destroy;
begin

  inherited Destroy;
end;

procedure TGtk2RichMemoStrings.Assign(Source: TPersistent);
var
  S: TStrings;
begin
  if Source is TStrings then
  begin
    S:=TStrings(Source);
    // to prevent Clear and then SetText we need to use our own Assign
    QuoteChar := S.QuoteChar;
    Delimiter := S.Delimiter;
    NameValueSeparator := S.NameValueSeparator;
    TextLineBreakStyle := S.TextLineBreakStyle;
    Text := S.Text;
  end
  else
    inherited Assign(Source);
end;

procedure TGtk2RichMemoStrings.AddStrings(TheStrings: TStrings);
begin
  SetTextStr(GetTextStr + TStrings(TheStrings).Text);
end;

procedure TGtk2RichMemoStrings.Clear;
begin
  SetText('');
end;

procedure TGtk2RichMemoStrings.Delete(Index: integer);
var
StartIter,
EndIter: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = Count-1 then begin
    gtk_text_iter_backward_char(@StartIter);
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  end
  else
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index+1);
  gtk_text_buffer_delete(FGtkBuf, @StartIter, @EndIter);
end;

procedure TGtk2RichMemoStrings.Insert(Index: integer; const S: string);
var
  StartIter,
  CursorIter: TGtkTextIter;
  NewLine: String;
  TextMark: PGtkTextMark;
begin
  if Index < gtk_text_buffer_get_line_count(FGtkBuf) then begin
    //insert with LineEnding
    NewLine := S+LineEnding;
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  end
  else begin
    //append with a preceding LineEnding
    gtk_text_buffer_get_end_iter(FGtkBuf, @StartIter);
    if gtk_text_buffer_get_line_count(FGtkBuf) = Count then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
  end;

  {if FQueueCursorMove = 0 then
  begin}
    TextMark := gtk_text_buffer_get_insert(FGtkBuf);
    gtk_text_buffer_get_iter_at_mark(FGtkBuf, @CursorIter, TextMark);
{    if gtk_text_iter_equal(@StartIter, @CursorIter) then
      QueueCursorMove(-1);
  end;}

  // and finally insert the new text
  gtk_text_buffer_insert(FGtkBuf, @StartIter, PChar(NewLine) ,-1);
end;

procedure TGtk2RichMemoStrings.SetTextStr(const Value: string);
begin
  if (Value <> Text) then
  begin
    LockOnChange({%H-}PGtkObject(Owner.Handle), 1);
    gtk_text_buffer_set_text(FGtkBuf, PChar(Value), -1);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  TextBuf: PGtkTextBuffer;
  view   : PGtkTextView;
begin
  TGtk2WSCustomMemoInt.SetCallbacks(AGtkWidget, AWidgetInfo);

  view:=PGtkTextView(AWidgetInfo^.CoreWidget);
  TextBuf := gtk_text_view_get_buffer(view);
  SignalConnect(PGtkWidget(view), 'backspace', @Gtk2WS_Backspace, AWidgetInfo);
  SignalConnect(PGtkWidget(TextBuf), 'mark-set', @Gtk2WS_MemoSelChanged_Before, AWidgetInfo);
  SignalConnectAfter(PGtkWidget(TextBuf), 'mark-set', @Gtk2WS_MemoSelChanged, AWidgetInfo);
  SignalConnectAfter(PGtkWidget(TextBuf), 'insert-text', @Gtk2WS_RichMemoInsert, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'key-press-event',  @Gtk2_RichMemoKeyPress, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'motion-notify-event', @Gtk2_RichMemoMotion, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'button-press-event', @Gtk2_RichMemoMouseButton, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'button-release-event', @Gtk2_RichMemoMouseButton, AWidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.GetWidgetBuffer(const AWinControl: TWinControl;
    var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);
var
  Widget     : PGtkWidget;
  list       : PGList;
begin
  TextWidget:=nil;
  Buffer:=nil;
  // todo: cache values?
  Widget := PGtkWidget(PtrUInt(AWinControl.Handle));

  list := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(list) then Exit;

  TextWidget := PGtkWidget(list^.data);
  if not Assigned(TextWidget) then Exit;

  buffer := gtk_text_view_get_buffer (PGtkTextView(TextWidget));
end;

class function TGtk2WSCustomRichMemo.GetAttrAtIter(view: PGtkTextView; const start: TGtkTextIter): PGtkTextAttributes;
var
  attr       : PGtkTextAttributes;
begin
  Result:=nil;
  attr := gtk_text_view_get_default_attributes(view);
  if not Assigned(attr) then Exit;

  gtk_text_iter_get_attributes(@start, attr);
  Result:=attr;
end;

class function TGtk2WSCustomRichMemo.GetAttrAtPos(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean ): PGtkTextAttributes;
var
  TextWidget : PGtkWidget;
  buffer     : PGtkTextBuffer;
  iter       : TGtkTextIter;
begin
  Result:=nil;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, TextStart);
  if APara then gtk_text_iter_set_line_offset(@iter, 0);

  Result := GetAttrAtIter(PGtkTextView(TextWidget), iter);
end;

class procedure TGtk2WSCustomRichMemo.ApplyTag(abuffer: PGtkTextBuffer;
  tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
var
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset (abuffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (abuffer, @iend, TextStart+TextLen);
  if ToParagraphs then begin
    gtk_text_iter_set_line_offset(@istart, 0);
    gtk_text_iter_forward_to_line_end(@iend);
  end;
  gtk_text_buffer_apply_tag(abuffer, tag, @istart, @iend);
end;

class function TGtk2WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget,
  TempWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  buffer: PGtkTextBuffer;
  SS:TPoint;
  gcolor  : TGdkColor;
  data: PRichMemoData;
const
  pu: array [Boolean] of gint = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget,dbgsName(AWinControl));
  {$ENDIF}

  New(data);
  FillChar(data^, sizeof(TRichMemoData), 0);
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  WidgetInfo^.DataOwner := True;
  WidgetInfo^.UserData := data;

  TempWidget := gtk_text_view_new();
  gtk_container_add(PGtkContainer(Widget), TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.vscrollbar, GTK_CAN_FOCUS);

  SS:=Gtk2TranslateScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),SS.X, SS.Y);

  // add border for memo
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget),
    BorderStyleShadowMap[TCustomMemo(AWinControl).BorderStyle]);

  SetMainWidget(Widget, TempWidget);
  {$ifdef OneParamGetWidgetInfo}
  GetWidgetInfo(Widget)^.CoreWidget := TempWidget;
  {$else}
  GetWidgetInfo(Widget, True)^.CoreWidget := TempWidget;
  {$endif}

  gtk_text_view_set_editable(PGtkTextView(TempWidget), not TCustomMemo(AWinControl).ReadOnly);
  gtk_text_view_set_justification(PGtkTextView(TempWidget), aGtkJustification[TCustomMemo(AWinControl).Alignment]);
  if TCustomMemo(AWinControl).WordWrap then
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
  else
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);

  gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), TCustomMemo(AWinControl).WantTabs);

  gtk_widget_show_all(Widget);

  buffer := gtk_text_view_get_buffer (PGtkTextView(TempWidget));
  //tag:=gtk_text_tag_new(TagNameNumeric);
  gtk_text_buffer_create_tag (buffer, TagNameNumeric,
      'editable',   [ gboolean(gFALSE),
      'editable-set', gboolean(gTRUE),
      nil]);

  gtk_text_buffer_create_tag (buffer, TagNameSubOrSuper, nil);

  gcolor := TColortoTGDKColor(clBlue);
  gtk_text_buffer_create_tag (buffer, TagNameLink, 'foreground-gdk', [@gcolor,
      'foreground-set', gboolean(gTRUE),
      'underline-set',  gboolean(gTRUE),
      'underline',      gint(pu[true]),
      nil] );

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.DestroyHandle(
  const AWinControl: TWinControl);
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  handlerid: gulong;
begin
  GetWidgetBuffer(AWinControl, w, b);

  // uninstall hanlder, to prevent crashes
  handlerid := g_signal_handler_find(b
    , G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA
    , 0, 0, nil
    , @Gtk2WS_MemoSelChanged, GetWidgetInfo(w));
  g_signal_handler_disconnect (b, handlerid);

  // the proper way of destroying a widgetset
  Gtk2WidgetSet.DestroyLCLComponent(AWinControl);
  // or actually - TGtk2WSWinControl.DestroyHandle(AWinControl)
  // the following call
  //   TWSWinControlClass(Classparent).DestroyHandle(AWinControl);
  // won't work, because TGtk2CustomMemo doesn't have DestoryHandle assigned
end;

class function TGtk2WSCustomRichMemo.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then begin
    Result:=-1;
    Exit;
  end;
  Result := 0;
  if not gtk_text_buffer_get_selection_bounds(b, @istart, @iend) then Exit;

  Result := Abs(gtk_text_iter_get_offset(@iend) - gtk_text_iter_get_offset(@istart));
end;

class function TGtk2WSCustomRichMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
var
  TextView: PGtkTextView;
begin
  {$ifdef OneParamGetWidgetInfo}
  TextView := PGtkTextView(GetWidgetInfo({%H-}Pointer(ACustomMemo.Handle))^.CoreWidget);
  {$else}
  TextView := PGtkTextView(GetWidgetInfo({%H-}Pointer(ACustomMemo.Handle), False)^.CoreWidget);
  {$endif}
  Result := TGtk2RichMemoStrings.Create(TextView, ACustomMemo);
end;

class function TGtk2WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then begin
    Result:=false;
    Exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart+1);
  if gtk_text_iter_get_offset(@istart)<>TextStart+1 then begin
    Result:=false; // TextStart is beyoned the end of text
    Exit;
  end;

  gtk_text_iter_backward_to_tag_toggle(@istart, nil);
  RangeStart:=gtk_text_iter_get_offset(@istart);

  gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
  gtk_text_iter_forward_to_tag_toggle(@iend, nil);

  RangeLen:=gtk_text_iter_get_offset(@iend)-RangeStart;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.FormatSubSuperScript(buffer: PGtkTextBuffer; vs: TVScriptPos; fontSizePts: Double; TextStart, TextLen: Integer);
var
  sz     : gint;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  tag     : PGtkTextTag;
  scrtag   : PGtkTextTag;
  hasscript : Boolean;
  k         : Double;
begin
  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  scrtag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buffer), TagNameSubOrSuper );
  hasscript := gtk_text_iter_has_tag( @istart, scrtag );

  if not hasscript then begin
    iend:=istart;
    gtk_text_iter_forward_to_tag_toggle(@iend, scrtag);
    hasscript:=gtk_text_iter_get_offset(@iend)<TextStart+TextLen;
  end;

  if vs = vpNormal then begin
    // no need to do anything, since no modifications were applied;
    if not hasscript then Exit;
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
    gtk_text_buffer_remove_tag(buffer, scrtag, @istart, @iend);
    tag := gtk_text_buffer_create_tag (buffer, nil,
        'rise',     [0,
        'rise-set',  gboolean(gTRUE),
        nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end else begin
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);

    if vs = vpSubScript then k := SubRiseKoef
    else k := SuperRiseKoef;
    sz := round(fontSizePts * k * PANGO_SCALE);

    tag := gtk_text_buffer_create_tag (buffer, nil,
        'rise',     [sz,
        'rise-set',  gboolean(gTRUE),
        'size-set',       gboolean(gTRUE),
        'size-points',    gdouble(fontSizePts*SubSuperFontKoef),
        nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
    gtk_text_buffer_apply_tag(buffer, scrtag, @istart, @iend);
  end;
end;

class procedure TGtk2WSCustomRichMemo.GetAttributesAt(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean;
  var attr: PGtkTextAttributes; var fp: TFontParams);
var
  iter : TGtkTextIter;
  v    : PGtkTextView;
  b    : PGtkTextBuffer;
  tag  : PGtkTextTag;
begin
  InitFontParams(fp);
  GetWidgetBuffer(AWinControl, PGtkWidget(v), b);

  gtk_text_buffer_get_iter_at_offset(b, @iter, TextStart);
  if APara then gtk_text_iter_set_line_offset(@iter, 0);

  attr:=GetAttrAtIter(v, iter);
  if not Assigned(attr) then Exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), TagNameSubOrSuper );

  if gtk_text_iter_has_tag(@iter, tag) then begin
    GtkTextAttrToFontParams(attr^, fp, 1 / SubSuperFontKoef );
    if attr^.appearance.rise < 0 then fp.VScriptPos:=vpSubScript
    else if attr^.appearance.rise > 0 then fp.VScriptPos:=vpSuperScript;
  end else
    GtkTextAttrToFontParams(attr^, fp);

end;


class procedure TGtk2WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams);
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  gcolor  : TGdkColor;
  bgcolor : TGdkColor;
  nm      : string;
const
  pu: array [Boolean] of gint = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  pb: array [Boolean] of gint = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  pi: array [Boolean] of gint = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gcolor := TColortoTGDKColor(Params.Color);
  bgcolor := TColortoTGDKColor(Params.BkColor);
  nm := Params.Name;
  if nm = '' then nm := #0;
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'family-set',     [gboolean(gTRUE),
      'family',         @nm[1],
      'foreground-gdk', @gcolor,
      'foreground-set', gboolean(gTRUE),
      'background-gdk', @bgcolor,
      'background-set', gboolean(Params.HasBkClr),
      'size-set',       gboolean(gTRUE),
      'size-points',    gdouble(Params.Size),
      'underline-set',  gboolean(gTRUE),
      'underline',      gint(pu[fsUnderline in Params.Style]),
      'weight-set',     gboolean(gTRUE),
      'weight',         gint(pb[fsBold in Params.Style]),
      'style-set',      gboolean(gTRUE),
      'style',          gint(pi[fsItalic in Params.Style]),
      'strikethrough-set', gboolean(gTRUE),
      'strikethrough',    gboolean(fsStrikeOut in Params.Style),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen);

  FormatSubSuperScript(buffer, Params.VScriptPos, Params.Size, TextStart, TextLen);
end;

class function TGtk2WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: TIntParaAlignment
  ): Boolean;
var
  attr       : PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result := Assigned(attr);
  if Result then begin
    case attr^.justification of
      GTK_JUSTIFY_LEFT:   AAlign:=paLeft;
      GTK_JUSTIFY_RIGHT:  AAlign:=paRIGHT;
      GTK_JUSTIFY_CENTER: AAlign:=paCenter;
      GTK_JUSTIFY_FILL:   AAlign:=paJustify;
    else
      AAlign:=paLeft;
    end;
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  val    : Integer;
begin
  case AAlign of
    paRight:   val:=GTK_JUSTIFY_RIGHT;
    paCenter:  val:=GTK_JUSTIFY_CENTER;
    paJustify: val:=GTK_JUSTIFY_FILL;
  else
    val := GTK_JUSTIFY_LEFT;
  end;
  GetWidgetBuffer(AWinControl, w, buffer);
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'justification', [   gint(val),
      'justification-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
end;

class function TGtk2WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetric: TIntParaMetric): Boolean;
var
  attr : PGtkTextAttributes;
  fp   : TFontParams;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  PixToPt   = PageDPI / ScreenDPI;
begin
  GetAttributesAt(AWinControl, TextStart, true, attr, fp);
  Result := Assigned(attr);
  if Result then begin
    if attr^.indent<0 then begin
      AMetric.FirstLine:=(attr^.left_margin)*PixToPt;
      AMetric.HeadIndent:=(-attr^.indent+attr^.left_margin)*PixToPt;
    end else begin
      AMetric.FirstLine:=(attr^.left_margin+attr^.indent)*PixToPt;
      AMetric.HeadIndent:=attr^.left_margin*PixToPt;
    end;
    AMetric.TailIndent:=attr^.right_margin*PixToPt;

    AMetric.SpaceAfter:=attr^.pixels_above_lines*PixToPt;
    AMetric.SpaceBefore:=attr^.pixels_below_lines*PixToPt;
    AMetric.LineSpacing:=(attr^.pixels_inside_wrap*PixToPt+fp.Size)/(fp.Size);
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AMetric: TIntParaMetric);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  h      : double;
  fl     : double;
  t      : double;
  attr   : PGtkTextAttributes;
  fp     : TFontParams;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
  h:=AMetric.HeadIndent;
  if h<0 then h:=0;
  fl:=AMetric.FirstLine;
  if fl<0 then fl:=0;

  if fl<h then begin
    t:=h;
    h:=fl;
    fl:=fl-t;
  end else
    fl:=fl-h;

  GetAttributesAt(AWinControl, TextStart, true, attr, fp);
  gtk_text_attributes_unref(attr);

  GetWidgetBuffer(AWinControl, w, buffer);
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'pixels-above-lines',   [ gint(round(AMetric.SpaceBefore*DPIFactor)),
      'pixels-above-lines-set', gboolean(gTRUE),
      'pixels-below-lines',     gint(round(AMetric.SpaceAfter*DPIFactor)),
      'pixels-below-lines-set', gboolean(gTRUE),
      'left-margin',            gint(round(h*DPIFactor)),
      'left-margin-set',        gboolean(gTRUE),
      'right-margin',           gint(round(AMetric.TailIndent*DPIFactor)),
      'right-margin-set',       gboolean(gTRUE),
      'indent',                 gint(round(fl*DPIFactor)),
      'indent-set',             gboolean(gTRUE),
      'pixels-inside-wrap',     gint((round(fp.Size*(AMetric.LineSpacing-DefLineSpacing)*DPIFactor))),
      'pixels-inside_wrap-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
end;

class procedure TGtk2WSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  txt    : String;
  len    : Integer;
  ln     : Integer;
  ls     : Integer;
  ofs    : Integer;
  numidx : Integer;
  tag    : PGtkTextTag;
begin
  inherited SetParaNumbering(AWinControl, TextStart, TextLen, ANumber);
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) or not Assigned(b) then Exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b)
    , TagNameNumeric);

  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  iend:=istart;
  gtk_text_iter_forward_chars(@iend, TextLen);
  ln:=gtk_text_iter_get_line(@istart);
  ls:=gtk_text_iter_get_line(@iend);


  numidx:=1;
  if ANumber.Style=pnNumber then numidx:=ANumber.NumberStart;

  repeat
    gtk_text_iter_set_line_offset(@istart, 0);
    case ANumber.Style of
      pnBullet: txt := BulletChar;
      pnNumber: txt := IntToStr(numidx);
      pnLowLetter: txt := 'a';
      pnLowRoman:  txt := 'i';
      pnUpLetter:  txt := 'A';
      pnUpRoman:   txt := 'I';
      pnCustomChar: txt:= UTF8Encode(ANumber.CustomChar);
    end;
    if not (ANumber.Style in [pnBullet, pnCustomChar]) and (ANumber.SepChar<>#0) then
      txt:=txt+UTF8Encode(ANumber.SepChar);
    txt:=txt+TabChar;

    //gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
    //gtk_text_buffer_move
    ofs:=gtk_text_iter_get_offset(@istart);

    // remove existing number
    if gtk_text_iter_begins_tag(@istart, tag) then begin
      iend:=istart;
      gtk_text_iter_forward_to_tag_toggle(@iend, nil);
      gtk_text_buffer_delete(b, @istart, @iend);
      gtk_text_buffer_get_iter_at_offset (b, @istart, ofs);
    end;
    // insert new number
    gtk_text_buffer_insert(b, @istart, @txt[1], length(txt));

    // restoring iterators
    gtk_text_buffer_get_iter_at_offset (b, @istart, ofs);
    gtk_text_iter_set_line_offset(@istart, 0);
    iend := istart;

    len:=UTF8Length(txt);
    gtk_text_iter_forward_chars(@iend, len);
    // aplying tag
    gtk_text_buffer_apply_tag_by_name(b, TagNameNumeric, @istart, @iend);

    // next line!
    gtk_text_iter_forward_line(@istart);

    inc(ln);
    inc(numidx);
  until ln>ls;


end;

class procedure TGtk2WSCustomRichMemo.SetParaTabs(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AStopList: TTabStopList);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  parr   : PPangoTabArray;
  i      : Integer;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
  GetWidgetBuffer(AWinControl, w, buffer);
  if not Assigned(w) or not Assigned(buffer) then Exit;

  GetWidgetBuffer(AWinControl, w, buffer);

  if AStopList.Count=0 then
    parr:=nil
  else begin
    parr:=pango_tab_array_new(AStopList.Count, true);
    for i:=0 to AStopList.Count-1 do begin
      pango_tab_array_set_tab(parr, i, PANGO_TAB_LEFT, round(AStopList.Tabs[i].Offset * DPIFactor) );
    end;
  end;

  tag := gtk_text_buffer_create_tag (buffer, nil,
      'tabs',   [ parr,
      'tabs-set', gboolean(AStopList.Count>0),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);

  if Assigned(parr) then pango_tab_array_free(parr);
end;

class function TGtk2WSCustomRichMemo.GetParaTabs(
  const AWinControl: TWinControl; TextStart: integer;
  var AStopList: TTabStopList): Boolean;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  PixToPt   = PageDPI / ScreenDPI;
var
  i      : Integer;
  attr   : PGtkTextAttributes;
  loc    : gint;
  al     : TPangoTabAlign;
  f      : Double;
begin
  InitTabStopList(AStopList);
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result:=Assigned(attr);
  if not Result then Exit;
  if not Assigned(attr^.tabs) then Exit;

  AStopList.Count:=pango_tab_array_get_size(attr^.tabs);
  if AStopList.Count=0 then Exit;

  f := PixToPt;
  if not pango_tab_array_get_positions_in_pixels(attr^.tabs) then
    f:= f / PANGO_SCALE;

  SetLength(AStopList.Tabs, AStopList.Count);
  for i:=0 to AStopList.Count-1 do begin
    pango_tab_array_get_tab(attr^.tabs, i, @al, @loc);
    AStopList.Tabs[i].Offset:=loc*f;
    AStopList.Tabs[i].Align:=tabLeft;
  end;
  gtk_text_attributes_unref(attr);
end;

class function TGtk2WSCustomRichMemo.GetParaRange(
  const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange
  ): Boolean;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then begin
    Result:=false;
    Exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
  gtk_text_iter_set_line_offset(@istart, 0);
  gtk_text_iter_forward_to_line_end(@iend);
  rng.start:=gtk_text_iter_get_offset(@istart);
  rng.lengthNoBr:=gtk_text_iter_get_offset(@iend)-rng.start;

  // if there's a character to move, then it's end of line, if not then it won't change!
  gtk_text_iter_forward_char(@iend);
  rng.length:=gtk_text_iter_get_offset(@iend)-rng.start;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.SetTextUIParams(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ui: TTextUIParam);
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
  if uiLink in ui.features then begin
    gtk_text_buffer_apply_tag_by_name(buffer, TagNameLink, @istart, @iend);
  end else begin
    tag := gtk_text_tag_table_lookup(
         gtk_text_buffer_get_tag_table(buffer), TagNameLink);
    if Assigned(tag) then
      gtk_text_buffer_remove_tag(buffer, tag, @istart, @iend);
  end;
end;

class function TGtk2WSCustomRichMemo.GetTextUIParams(
  const AWinControl: TWinControl; TextStart: Integer;
  var ui: TTextUIParam): Boolean;
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : PGtkTextTag;
  istart : TGtkTextIter;
begin
  ui.features:=[];
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  Result:=Assigned(buffer);
  if not Result then Exit;

  tag := gtk_text_tag_table_lookup(
           gtk_text_buffer_get_tag_table(buffer), TagNameLink);
  if Assigned(tag) then begin // which is odd.
    gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
    if gtk_text_iter_has_tag(@istart, tag) then
      Include(ui.features, uiLink);
  end;
end;

class procedure TGtk2WSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;
  gtk_text_buffer_get_iter_at_offset (b, @istart, DstStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, DstStart+DstLen);
  gtk_text_buffer_delete(b, @istart, @iend);
  if length(TextUTF8)>0 then
    gtk_text_buffer_insert(b, @istart, @textUTF8[1], length(TextUTF8));
end;

class function TGtk2WSCustomRichMemo.CharAtPos(const AWinControl: TWinControl;
  x, y: Integer): Integer;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  gx, gy : gint;
  trailing: gint;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) then Exit;

  gtk_text_view_window_to_buffer_coords(PGtkTextView(w), GTK_TEXT_WINDOW_WIDGET, x, y, @gx, @gy);
  gtk_text_view_get_iter_at_position(PGtkTextView(w), @istart, @trailing, gx,gy);
  Result:=gtk_text_iter_get_offset(@istart)+trailing;
end;

procedure UTF8CharsToWideString(const p: Pchar; var w: WideString);
var
  slen : Integer;
  cnt: Integer;
  sz: SizeUInt;
begin
  if not Assigned(p) then begin
    w:='';
    Exit;
  end;
  slen:=strlen(p);
  if slen=0 then begin
    w:='';
    Exit;
  end;
  cnt:=UTF8Length(p, slen);
  SetLength(w, cnt);
  if cnt>0 then
   ConvertUTF8ToUTF16( @w[1], length(w), p, slen, [toInvalidCharToSymbol], sz);
  SetLength(w, sz);
end;

class function TGtk2WSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  TextWidget   : PGtkWidget;
  buffer       : PGtkTextBuffer;
  istart       : TGtkTextIter;
  iend         : TGtkTextIter;
  start_match  : TGtkTextIter;
  end_match    : TGtkTextIter;
  Found        : Boolean;
  opt          : TGtkTextSearchFlags;
  gstr         : PChar;
  txt          : WideString;
  sub          : WIdeString;
const
  GTK_TEXT_SEARCH_VISIBLE_ONLY     = 1 shl 0;  (* values of TGtkTextSearchFlags *)
  {%H-}GTK_TEXT_SEARCH_TEXT_ONLY        = 1 shl 1;
  GTK_TEXT_SEARCH_CASE_INSENSITIVE = 1 shl 2;
begin
  Result := -1;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  opt:=GTK_TEXT_SEARCH_VISIBLE_ONLY;
  if not (soMatchCase in SearchOpts.Options) then begin
    opt:=opt or GTK_TEXT_SEARCH_CASE_INSENSITIVE; // doesn't work anyway! it works in gtk3 only

    gtk_text_buffer_get_iter_at_offset (buffer, @istart, SearchOpts.start);
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, SearchOpts.start+SearchOpts.len);

    gtk_text_buffer_get_text(buffer, @istart, @iend, false);
    gstr := gtk_text_buffer_get_text(Buffer, @istart, @iend, False);
    if Assigned(gstr) then begin
      UTF8CharsToWideString(gstr, txt);
      g_free(gstr);
      txt:=WideUpperCase(txt);
      sub:=WideUpperCase(UTF8Decode(ANiddle));
      Result:=Pos(sub,txt);
      if Result>0 then
        Result:=Result-1+SearchOpts.start
      else
        Result:=-1;
    end else
      Result:=-1;
  end else begin
    gtk_text_buffer_get_iter_at_offset(buffer, @istart, SearchOpts.start );
    if not (soBackward in SearchOpts.Options) then
    begin
      gtk_text_buffer_get_iter_at_offset(buffer, @iend, SearchOpts.start+SearchOpts.len );
      Found := gtk_text_iter_forward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @iend)
    end else begin
      gtk_text_buffer_get_iter_at_offset(buffer, @iend, SearchOpts.start-SearchOpts.len);
      Found := gtk_text_iter_backward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @iend)
    end;

    if Found
      then Result := gtk_text_iter_get_offset(@start_match)
      else Result := -1;
  end;
end;

class function TGtk2WSCustomRichMemo.ImageFromFile(
  const ARichMemo: TCustomRichMemo; APos: Integer; const FileNameUTF8: string;
  const AImgSize: TSize): Boolean;
var
  t: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  pix: PGdkPixbuf;
  err: PGError;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
  Result:=false;
  GetWidgetBuffer(ARichMemo, t, b);
  if not Assigned(b) then Exit;

  err:=nil;

  if (AImgSize.cx=0) and (AImgSize.cy=0) then
    pix := gdk_pixbuf_new_from_file(PChar(FileNameUTF8), @err)
  else
    pix := gdk_pixbuf_new_from_file_at_size(PChar(FileNameUTF8),
      round(AImgSize.cx * DPIFactor),  round(AImgSize.cy * DPIFactor), @err);

  Result:=Assigned(pix);
  if Result then begin
    gtk_text_buffer_get_iter_at_offset(b, @istart, APos);
    gtk_text_buffer_insert_pixbuf(b, @istart, pix);
  end else
    writeln(err^.message);
end;

class procedure TGtk2WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
var
  TextMark: PGtkTextMark;
  CursorIter: TGtkTextIter;
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then Exit;

  if NewStart = -1 then
  begin
    // always scroll so the cursor is visible
    TextMark := gtk_text_buffer_get_insert(b);
    gtk_text_buffer_get_iter_at_mark(b, @CursorIter, TextMark);
  end
  else begin
    // SelStart was used and we should move to that location
    gtk_text_buffer_get_iter_at_offset(b, @CursorIter, NewStart);
    gtk_text_buffer_place_cursor(b, @CursorIter); // needed to move the cursor
    TextMark := gtk_text_buffer_get_insert(b);
  end;
  gtk_text_view_scroll_to_mark(PGtkTextView(w), TextMark, 0, True, 0, 1);
end;

class procedure TGtk2WSCustomRichMemo.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  TextMark: PGtkTextMark;
  StartIter,
  EndIter: TGtkTextIter;
  Offset: Integer;
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then Exit;

  TextMark := gtk_text_buffer_get_insert(b);
  gtk_text_buffer_get_iter_at_mark(b, @StartIter, TextMark);

  Offset := gtk_text_iter_get_offset(@StartIter);

  gtk_text_buffer_get_iter_at_offset(b, @EndIter, Offset+NewLength);

  gtk_text_buffer_select_range(b, @StartIter, @EndIter);
end;

class procedure TGtk2WSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  tag    : PGtkTextTag;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  p      : PGtkTextAttributes;
  sc     : gdouble;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;

  p:=GetAttrAtPos(AWinControl, 0);
  sc:=p^.font_scale;
  if sc=0 then sc:=1;
  gtk_text_attributes_unref(p);
  // restore the scale.
  // for whatever reason, scale is always assumed as a multiplier!
  // thus it is necessary to "unscale" the previous value as well
  sc:=1/sc*AZoomFactor;

  tag := gtk_text_buffer_create_tag(b, nil,
      'scale', [   gdouble(sc),
      'scale-set', gboolean(gTRUE),
      nil]);

  //gtk_text_buffer_get_start_iter(b, @istart);
  gtk_text_buffer_get_iter_at_offset(b, @istart, 0);
  gtk_text_buffer_get_end_iter(b, @iend);
  gtk_text_buffer_apply_tag(b, tag, @istart, @iend);

  //todo: set default font with scale
end;

function GtkDrawableDraw(widget: PGtkWidget;
  event : PGdkEventExpose; gi: TGtk2InlineObject): gboolean; cdecl;
begin
  if not Assigned(widget) then Exit;
  if not gi.cnv.HandleAllocated then
    gi.cnv.Handle:=GTK2WidgetSet.CreateDCForWidget(widget, event^.Window,false);
  gi.il.Draw( gi.cnv, gi.Size);
  Result:=gTRUE;
end;

class function TGtk2WSCustomRichMemo.InlineInsert(
  const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
  const ASize: TSize; AHandler: TRichMemoInline;
  var wsObj: TRichMemoInlineWSObject): Boolean;
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  istart : TGtkTextIter;
  anch   : PGtkTextChildAnchor;
  gi     : TGtk2InlineObject;
  draw   : PGtkWidget;
  sz     : TSize;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
  Result:=false;
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;

  gi:=TGtk2InlineObject.Create;

  gtk_text_buffer_get_iter_at_offset(b, @istart, ATextStart);
  anch:=gtk_text_buffer_create_child_anchor(b, @istart);

  draw:=gtk_drawing_area_new;
  ConnectSignal( PGtkObject(draw), 'expose-event', @GtkDrawableDraw, gi);

  sz.cx:=round(ASize.cx * DPIFactor);
  sz.cy:=round(ASize.cy * DPIFactor);
  gtk_widget_set_size_request(draw, sz.cx, sz.cy);
  gtk_text_view_add_child_at_anchor(PGtkTextView(w), draw, anch);

  gi.il:=AHandler;
  gi.anch:=anch;
  gi.wgt:=draw;
  gi.size:=sz;
  gi.cnv:=TCanvas.Create;

  gtk_widget_show(draw);
  gi.il.SetVisible(true);

  wsObj:=gi;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.InlineInvalidate(
  const AWinControl: TWinControl; AHandler: TRichMemoInline;
  wsObj: TRichMemoInlineWSObject);
var
  gi : TGtk2InlineObject;
begin
  if not Assigned(wsObj) or not (wsObj is TGtk2InlineObject) then Exit;
  gi := TGtk2InlineObject(wsObj);
  if Assigned(gi.wgt) then
    gtk_widget_queue_draw(gi.wgt);
end;


class function TGtk2WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl;
   TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  attr : PGtkTextAttributes;
begin
  GetAttributesAt(AWinControl, TextStart, false, attr, params);
  Result := Assigned(attr);
  if Result then gtk_text_attributes_unref(attr);
end;

function GtkInsertImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize
  ): Boolean;
begin
  Result:=TGtk2WSCustomRichMemo.ImageFromFile(ARichMemo, APos, FileNameUTF8, AImgSize);
end;

function GtkWSGetFontParams(fontref: HFONT; var params: TFontParams): Boolean;
var
  gtkobj: PGDIObject;
  pangofont: PPangoLayout;
  PangoDesc: PPangoFontDescription;
  sz       : Integer;
  isSzAbs  : Boolean;
begin
  gtkobj:=PGDIObject(fontref);
  Result:=Assigned(gtkobj) and (gtkobj^.GDIType=gdiFont);
  if not Result then Exit;

  if gtkobj^.LogFont.lfFaceName = 'default' then begin
    pangofont:=GTK2WidgetSet.GetDefaultGtkFont(False);
    Result:=PANGO_IS_LAYOUT(pangofont);
    if Result then
    begin
      // default font name
      PangoDesc := pango_layout_get_font_description(pangofont);
      if not Assigned(PangoDesc) then
        PangoDesc := pango_context_get_font_description(pango_layout_get_context(pangofont));
      params.Name := StrPas(pango_font_description_get_family(PangoDesc));

      // default font size
      if gtkobj^.LogFont.lfHeight = 0 then begin
        isSzAbs := pango_font_description_get_size_is_absolute(PangoDesc);
        sz := pango_font_description_get_size(PangoDesc);
        if not isSzAbs then
          params.Size := round(sz / PANGO_SCALE)
        else
          params.Size := round(sz/ScreenInfo.PixelsPerInchY*72);
      end;

      // rely on LogFont structure to be initialiazed (as it seems to be)
      if gtkobj^.LogFont.lfItalic > 0 then Include(params.Style, fsItalic);
      if gtkobj^.LogFont.lfWeight >= FW_BOLD then Include(params.Style, fsBold);
      if gtkobj^.LogFont.lfUnderline > 0 then Include(params.Style, fsUnderline);
      if gtkobj^.LogFont.lfStrikeOut > 0 then Include(params.Style, fsStrikeOut);
    end;
  end else
    Result:=false;
end;

initialization
  InsertImageFromFile := @GtkInsertImageFromFile;
  WSGetFontParams:=@GtkWSGetFontParams;

end.

