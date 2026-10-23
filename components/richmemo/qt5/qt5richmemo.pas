unit qt5richmemo;

interface

{$mode delphi}

{$define RMQT5_TEXTFORMATS} // the feature is available in Qt5 Trunk
                            // it allows to query Qt5TextEdit for character formats array
{$ifdef RMQT5_NOTEXTFORMATS}{$undef RMQT5_TEXTFORMATS}{$endif}

uses
  Types, Classes, SysUtils, LazUTF8,
  LCLType, Controls, StdCtrls, Graphics, Forms,
  qt5, qtobjects, qtwidgets, qtprivate, QtWSControls,
  WSProc,
  RichMemo, WSRichMemo, RichMemoUtils;

type
  { TQtRichMemoStrings }

  TQtRichMemoStrings = class(TQtMemoStrings)
  protected
    procedure SetUpdateState(Updating: Boolean); override;
  end;

  { TQtWSCustomRichMemo }

  TQtWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;

    class procedure CutToClipboard(const AWinControl: TWinControl); override;
    class procedure CopyToClipboard(const AWinControl: TWinControl); override;
    class procedure PasteFromClipboard(const AWinControl: TWinControl); override;
    class function CanPasteFromClipboard(const AWinControl: TWinControl): Boolean; override;

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
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ANumber: TIntParaNumbering); override;
    class procedure SetParaTabs(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AStopList: TTabStopList); override;
    class function GetParaTabs(const AWinControl: TWinControl; TextStart: integer;
      var AStopList: TTabStopList): Boolean; override;
    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer;
      var rng: TParaRange): Boolean; override;

    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;

    class procedure SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ui: TTextUIParam); override;
    class function GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer;
      var ui: TTextUIParam): Boolean; override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String;
      DstStart, DstLen: Integer); override;
    class function CharAtPos(const AWinControl: TWinControl; x, y: Integer): Integer; override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;
    class function GetZoomFactor(const AWinControl: TWinControl; var AZoomFactor: Double): Boolean; override;

    class procedure Redo(const AWinControl: TWinControl); override;
    class function GetCanRedo(const AWinControl: TWinControl): Boolean; override;

    class function isInternalChange(const AWinControl: TWinControl; Params: TTextModifyMask
      ): Boolean; override;
    class procedure SetTextAttributesInternal(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AModifyMask: TTextModifyMask; const Params: TIntFontParams); override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;

    class procedure SetTransparentBackground(const AWinControl: TWinControl;
      ATransparent: Boolean); override;
  end;

type
  TEditorState = record
    selst: integer; // selection start
    sellen: integer; // selection length
  end;

  // no sanity check is done in these functions
  procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
  procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);

implementation

const
  WordWrapMap: array[Boolean] of QTextEditLineWrapMode =
  (
    QTextEditNoWrap,
    QTextEditWidgetWidth
  );

  AlignmentMap: array[TIntParaAlignment] of QtAlignment =
  (
    QtAlignLeft,
    QtAlignRight,
    QtAlignHCenter,
    QtAlignJustify
  );

  VAlignMap: array[TVScriptPos] of QTextCharFormatVerticalAlignment =
  (
    QTextCharFormatAlignNormal,
    QTextCharFormatAlignSubScript,
    QTextCharFormatAlignSuperScript
  );

  PageDPI = 72;

type
  TCustomRichMemoInt = class(TCustomRichMemo);

  { TQtRichMemoHelper }

  TQtRichMemoHelper = class(TObject)
  public
    TextEdit: TQtTextEdit;
    Hook: QObject_hookH;
    Zoom: Double;
    OverLink: Boolean;
    LinkDown: Boolean;
    LinkStart: Integer;
    LinkLen: Integer;
    LinkBtn: QtMouseButton;
    constructor Create(ATextEdit: TQtTextEdit);
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
  end;

var
  Helpers: TFPList = nil;
  ImageCounter: Integer = 0;

function FindHelper(te: TQtTextEdit): TQtRichMemoHelper;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(Helpers) then Exit;
  for i := 0 to Helpers.Count - 1 do
    if TQtRichMemoHelper(Helpers[i]).TextEdit = te then
    begin
      Result := TQtRichMemoHelper(Helpers[i]);
      Exit;
    end;
end;

function ScreenDPI: Double;
begin
  Result := Screen.PixelsPerInch;
  if Result <= 0 then
    Result := 96;
end;

function GetEdit(const AWinControl: TWinControl; const AName: string;
  out te: TQtTextEdit; out w: QTextEditH): Boolean;
begin
  te := nil;
  w := nil;
  Result := WSCheckHandleAllocated(AWinControl, AName);
  if not Result then Exit;
  te := TQtTextEdit(AWinControl.Handle);
  w := QTextEditH(te.Widget);
  Result := Assigned(w);
end;

function GetZoom(te: TQtTextEdit): Double;
var
  h: TQtRichMemoHelper;
begin
  Result := 1;
  h := FindHelper(te);
  if Assigned(h) and (h.Zoom > 0) then
    Result := h.Zoom;
end;

function DocLen(doc: QTextDocumentH): Integer;
begin
  Result := QTextDocument_characterCount(doc) - 1;
  if Result < 0 then Result := 0;
end;

function ClampPos(doc: QTextDocumentH; APos: Integer): Integer;
begin
  Result := APos;
  if Result < 0 then Result := 0;
  if Result > DocLen(doc) then Result := DocLen(doc);
end;

function NewCursor(w: QTextEditH): QTextCursorH;
begin
  Result := QTextCursor_Create(QTextEdit_document(w));
end;

procedure SelectRange(cur: QTextCursorH; doc: QTextDocumentH; AStart, ALen: Integer);
begin
  QTextCursor_setPosition(cur, ClampPos(doc, AStart));
  QTextCursor_setPosition(cur, ClampPos(doc, AStart + ALen), QTextCursorKeepAnchor);
end;

procedure CharFormatAt(w: QTextEditH; APos: Integer; fmt: QTextCharFormatH);
var
  doc: QTextDocumentH;
  cur: QTextCursorH;
  p: Integer;
begin
  doc := QTextEdit_document(w);
  cur := QTextCursor_Create(doc);
  try
    p := APos + 1;
    if p > DocLen(doc) then p := DocLen(doc);
    QTextCursor_setPosition(cur, ClampPos(doc, p));
    QTextCursor_charFormat(cur, fmt);
  finally
    QTextCursor_Destroy(cur);
  end;
end;

procedure DefaultFontInfo(w: QTextEditH; out AFamily: WideString; out ASize: Double);
var
  f: QFontH;
begin
  f := QFont_Create();
  try
    QTextDocument_defaultFont(QTextEdit_document(w), f);
    AFamily := '';
    QFont_family(f, @AFamily);
    ASize := QFont_pointSizeF(f);
  finally
    QFont_Destroy(f);
  end;
  if ASize <= 0 then ASize := 10;
end;

procedure SetBrushProp(fmt: QTextFormatH; AColor: TColor; AForeground: Boolean);
var
  clr: TQColor;
  br: QBrushH;
begin
  ColorRefToTQColor(ColorToRGB(AColor), clr);
  br := QBrush_Create(@clr, QtSolidPattern);
  try
    if AForeground then
      QTextFormat_setForeground(fmt, br)
    else
      QTextFormat_setBackground(fmt, br);
  finally
    QBrush_Destroy(br);
  end;
end;

function GetBrushProp(fmt: QTextFormatH; AForeground: Boolean; out AColor: TColor): Boolean;
var
  br: QBrushH;
  cr: TColorRef;
begin
  br := QBrush_Create();
  try
    if AForeground then
      QTextFormat_foreground(fmt, br)
    else
      QTextFormat_background(fmt, br);
    Result := QBrush_style(br) <> QtNoBrush;
    if Result then
    begin
      TQColorToColorRef(QBrush_color(br)^, cr);
      AColor := TColor(cr);
    end;
  finally
    QBrush_Destroy(br);
  end;
end;

function DefaultTextColor(const AWinControl: TWinControl): TColor;
begin
  Result := AWinControl.Font.Color;
  if Result = clDefault then
    Result := clWindowText;
  Result := ColorToRGB(Result);
end;

function LinkRangeAt(w: QTextEditH; APos: Integer; out AStart, ALen: Integer; out AHref: WideString): Boolean;
var
  doc: QTextDocumentH;
  fmt: QTextCharFormatH;
  len: Integer;
  s, e: Integer;
  href: WideString;

  function IsLinkAt(i: Integer): Boolean;
  begin
    Result := False;
    if (i < 0) or (i >= len) then Exit;
    CharFormatAt(w, i, fmt);
    Result := QTextCharFormat_isAnchor(fmt);
    if Result then
    begin
      href := '';
      QTextCharFormat_anchorHref(fmt, @href);
      Result := href = AHref;
    end;
  end;

begin
  Result := False;
  AStart := APos;
  ALen := 0;
  AHref := '';
  doc := QTextEdit_document(w);
  len := DocLen(doc);
  fmt := QTextCharFormat_Create();
  try
    if (APos < 0) or (APos >= len) then Exit;
    CharFormatAt(w, APos, fmt);
    if not QTextCharFormat_isAnchor(fmt) then Exit;
    QTextCharFormat_anchorHref(fmt, @AHref);
    s := APos;
    while IsLinkAt(s - 1) do Dec(s);
    e := APos;
    while IsLinkAt(e + 1) do Inc(e);
    AStart := s;
    ALen := e - s + 1;
    Result := True;
  finally
    QTextCharFormat_Destroy(fmt);
  end;
end;

{ TQtRichMemoStrings }

procedure TQtRichMemoStrings.SetUpdateState(Updating: Boolean);
begin
  // rich content lives in the QTextDocument, the plain text cache must never
  // be written back on EndUpdate - it would erase all formatting
end;

{ TQtRichMemoHelper }

constructor TQtRichMemoHelper.Create(ATextEdit: TQtTextEdit);
begin
  inherited Create;
  TextEdit := ATextEdit;
  Zoom := 1;
  Hook := QObject_hook_Create(TextEdit.viewportWidget);
  QObject_hook_hook_events(Hook, EventFilter);
  QWidget_setMouseTracking(TextEdit.viewportWidget, True);
end;

destructor TQtRichMemoHelper.Destroy;
begin
  if Assigned(Hook) then
    QObject_hook_Destroy(Hook);
  Hook := nil;
  inherited Destroy;
end;

function TQtRichMemoHelper.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
var
  w: QTextEditH;
  pt: TQtPoint;
  href: WideString;
  cur: QTextCursorH;
  pos: Integer;
  s, l: Integer;
  onlink: Boolean;
  qc: QCursorH;
  mi: TLinkMouseInfo;
  rm: TCustomRichMemo;
begin
  Result := False;
  if not Assigned(TextEdit) or not Assigned(TextEdit.LCLObject)
    or not (TextEdit.LCLObject is TCustomRichMemo) then Exit;
  rm := TCustomRichMemo(TextEdit.LCLObject);
  w := QTextEditH(TextEdit.Widget);

  case QEvent_type(Event) of
    QEventMouseMove:
      begin
        QMouseEvent_pos(QMouseEventH(Event), @pt);
        href := '';
        QTextEdit_anchorAt(w, @href, @pt);
        onlink := href <> '';
        if onlink <> OverLink then
        begin
          OverLink := onlink;
          if onlink then
            qc := QCursor_Create(QtPointingHandCursor)
          else
            qc := QCursor_Create(QtIBeamCursor);
          QWidget_setCursor(TextEdit.viewportWidget, qc);
          QCursor_Destroy(qc);
        end;
      end;

    QEventMouseButtonPress, QEventMouseButtonRelease:
      begin
        QMouseEvent_pos(QMouseEventH(Event), @pt);
        href := '';
        QTextEdit_anchorAt(w, @href, @pt);
        if href = '' then
        begin
          LinkDown := False;
          Exit;
        end;
        cur := QTextCursor_Create();
        try
          QTextEdit_cursorForPosition(w, cur, @pt);
          pos := QTextCursor_position(cur);
        finally
          QTextCursor_Destroy(cur);
        end;
        if not LinkRangeAt(w, pos, s, l, href) then
          if not LinkRangeAt(w, pos - 1, s, l, href) then Exit;

        if QEvent_type(Event) = QEventMouseButtonPress then
        begin
          LinkDown := True;
          LinkStart := s;
          LinkLen := l;
          LinkBtn := QMouseEvent_button(QMouseEventH(Event));
        end else
        begin
          if LinkDown and (LinkStart = s) and (LinkLen = l)
            and (LinkBtn = QMouseEvent_button(QMouseEventH(Event))) then
          begin
            LinkDown := False;
            case LinkBtn of
              QtRightButton: mi.Button := mbRight;
              QtMidButton: mi.Button := mbMiddle;
            else
              mi.Button := mbLeft;
            end;
            mi.LinkRef := UTF8Encode(href);
            TCustomRichMemoInt(rm).DoLinkAction(laClick, mi, s, l);
          end;
        end;
      end;
  end;
end;

{ TQtWSCustomRichMemo }

class function TQtWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtTextEdit;
begin
  QtTextEdit := TQtTextEdit.Create(AWinControl, AParams);
  QtTextEdit.AcceptRichText := True;
  QtTextEdit.ClearText;
  QtTextEdit.setBorder(TCustomMemo(AWinControl).BorderStyle = bsSingle);
  QtTextEdit.setReadOnly(TCustomMemo(AWinControl).ReadOnly);
  QtTextEdit.setLineWrapMode(WordWrapMap[TCustomMemo(AWinControl).WordWrap]);
  // create our FList helper
  QtTextEdit.FList := TQtRichMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);
  QtTextEdit.AttachEvents;

  if not Assigned(Helpers) then
    Helpers := TFPList.Create;
  Helpers.Add(TQtRichMemoHelper.Create(QtTextEdit));

  Result := TLCLHandle(QtTextEdit);
end;

class procedure TQtWSCustomRichMemo.DestroyHandle(const AWinControl: TWinControl);
var
  h: TQtRichMemoHelper;
begin
  if AWinControl.HandleAllocated then
  begin
    h := FindHelper(TQtTextEdit(AWinControl.Handle));
    if Assigned(h) then
    begin
      Helpers.Remove(h);
      h.Free;
    end;
  end;
  TQtWSWinControl.DestroyHandle(AWinControl);
end;

class procedure TQtWSCustomRichMemo.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
var
  te: TQtTextEdit;
  w: QTextEditH;
  ws: WideString;
begin
  if not GetEdit(ACustomEdit, 'SetSelText', te, w) then Exit;
  ws := UTF8Decode(NewSelText);
  QTextEdit_insertPlainText(w, @ws);
end;

class procedure TQtWSCustomRichMemo.CutToClipboard(const AWinControl: TWinControl);
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  if GetEdit(AWinControl, 'CutToClipboard', te, w) then
    QTextEdit_cut(w);
end;

class procedure TQtWSCustomRichMemo.CopyToClipboard(const AWinControl: TWinControl);
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  if GetEdit(AWinControl, 'CopyToClipboard', te, w) then
    QTextEdit_copy(w);
end;

class procedure TQtWSCustomRichMemo.PasteFromClipboard(const AWinControl: TWinControl);
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  if GetEdit(AWinControl, 'PasteFromClipboard', te, w) then
    QTextEdit_paste(w);
end;

class function TQtWSCustomRichMemo.CanPasteFromClipboard(const AWinControl: TWinControl): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  Result := GetEdit(AWinControl, 'CanPasteFromClipboard', te, w) and QTextEdit_canPaste(w);
end;

class procedure TQtWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  bf: QTextBlockFormatH;
begin
  if not GetEdit(AWinControl, 'SetParaAlignment', te, w) then Exit;
  cur := NewCursor(w);
  bf := QTextBlockFormat_Create();
  try
    SelectRange(cur, QTextEdit_document(w), TextStart, TextLen);
    QTextBlockFormat_setAlignment(bf, AlignmentMap[AAlign]);
    QTextCursor_mergeBlockFormat(cur, bf);
  finally
    QTextBlockFormat_Destroy(bf);
    QTextCursor_Destroy(cur);
  end;
end;

class function TQtWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  bf: QTextBlockFormatH;
  al: QtAlignment;
begin
  Result := GetEdit(AWinControl, 'GetParaAlignment', te, w);
  if not Result then Exit;
  cur := NewCursor(w);
  bf := QTextBlockFormat_Create();
  try
    QTextCursor_setPosition(cur, ClampPos(QTextEdit_document(w), TextStart));
    QTextCursor_blockFormat(cur, bf);
    al := QTextBlockFormat_alignment(bf);
  finally
    QTextBlockFormat_Destroy(bf);
    QTextCursor_Destroy(cur);
  end;
  if QtAlignJustify and al > 0 then AAlign := paJustify
  else if QtAlignHCenter and al > 0 then AAlign := paCenter
  else if (QtAlignRight or QtAlignTrailing) and al > 0 then AAlign := paRight
  else AAlign := paLeft;
end;

class function TQtWSCustomRichMemo.GetParaMetric(const AWinControl: TWinControl;
  TextStart: Integer; var AMetric: TIntParaMetric): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  bf: QTextBlockFormatH;
  k: Double;
begin
  InitParaMetric(AMetric);
  Result := GetEdit(AWinControl, 'GetParaMetric', te, w);
  if not Result then Exit;
  k := PageDPI / ScreenDPI;
  cur := NewCursor(w);
  bf := QTextBlockFormat_Create();
  try
    QTextCursor_setPosition(cur, ClampPos(QTextEdit_document(w), TextStart));
    QTextCursor_blockFormat(cur, bf);
    AMetric.HeadIndent := QTextBlockFormat_leftMargin(bf) * k;
    AMetric.FirstLine := AMetric.HeadIndent + QTextBlockFormat_textIndent(bf) * k;
    AMetric.TailIndent := QTextBlockFormat_rightMargin(bf) * k;
    AMetric.SpaceBefore := QTextBlockFormat_topMargin(bf) * k;
    AMetric.SpaceAfter := QTextBlockFormat_bottomMargin(bf) * k;
    if (QTextBlockFormat_lineHeightType(bf) = Ord(QTextBlockFormatProportionalHeight))
      and (QTextBlockFormat_lineHeight(bf) > 0) then
      AMetric.LineSpacing := QTextBlockFormat_lineHeight(bf) / 100 * DefLineSpacing
    else
      AMetric.LineSpacing := DefLineSpacing;
  finally
    QTextBlockFormat_Destroy(bf);
    QTextCursor_Destroy(cur);
  end;
end;

class procedure TQtWSCustomRichMemo.SetParaMetric(const AWinControl: TWinControl;
  TextStart, TextLen: Integer; const AMetric: TIntParaMetric);
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  bf: QTextBlockFormatH;
  k: Double;
  h: Double;
begin
  if not GetEdit(AWinControl, 'SetParaMetric', te, w) then Exit;
  k := ScreenDPI / PageDPI;
  h := AMetric.HeadIndent;
  if h < 0 then h := 0;
  cur := NewCursor(w);
  bf := QTextBlockFormat_Create();
  try
    SelectRange(cur, QTextEdit_document(w), TextStart, TextLen);
    QTextBlockFormat_setLeftMargin(bf, h * k);
    QTextBlockFormat_setTextIndent(bf, (AMetric.FirstLine - h) * k);
    QTextBlockFormat_setRightMargin(bf, AMetric.TailIndent * k);
    QTextBlockFormat_setTopMargin(bf, AMetric.SpaceBefore * k);
    QTextBlockFormat_setBottomMargin(bf, AMetric.SpaceAfter * k);
    if AMetric.LineSpacing > 0 then
      QTextBlockFormat_setLineHeight(bf, AMetric.LineSpacing / DefLineSpacing * 100,
        Ord(QTextBlockFormatProportionalHeight))
    else
      QTextBlockFormat_setLineHeight(bf, 100, Ord(QTextBlockFormatProportionalHeight));
    QTextCursor_mergeBlockFormat(cur, bf);
  finally
    QTextBlockFormat_Destroy(bf);
    QTextCursor_Destroy(cur);
  end;
end;

class function TQtWSCustomRichMemo.GetParaNumbering(const AWinControl: TWinControl;
  TextStart: Integer; var ANumber: TIntParaNumbering): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  blk: QTextBlockH;
  lst: QTextListH;
  fmt: QTextFormatH;
  lf: QTextListFormatH;
  ws: WideString;
begin
  InitParaNumbering(ANumber);
  Result := GetEdit(AWinControl, 'GetParaNumbering', te, w);
  if not Result then Exit;
  blk := QTextBlock_Create();
  try
    QTextDocument_findBlock(QTextEdit_document(w), blk, ClampPos(QTextEdit_document(w), TextStart));
    lst := QTextBlock_textList(blk);
    if not Assigned(lst) then Exit;
    fmt := QTextFormat_Create();
    lf := QTextListFormat_Create();
    try
      QTextObject_format(lst, fmt);
      QTextFormat_toListFormat(fmt, lf);
      case QTextListFormat_style(lf) of
        QTextListFormatListDecimal: ANumber.Style := pnNumber;
        QTextListFormatListLowerAlpha: ANumber.Style := pnLowLetter;
        QTextListFormatListUpperAlpha: ANumber.Style := pnUpLetter;
        QTextListFormatListLowerRoman: ANumber.Style := pnLowRoman;
        QTextListFormatListUpperRoman: ANumber.Style := pnUpRoman;
      else
        ANumber.Style := pnBullet;
      end;
      ANumber.NumberStart := 1;
      if ANumber.Style in [pnNumber, pnLowLetter, pnUpLetter, pnLowRoman, pnUpRoman] then
      begin
        ws := '';
        QTextListFormat_numberSuffix(lf, @ws);
        if ws <> '' then
          ANumber.SepChar := ws[1]
        else
          ANumber.SepChar := SepNone;
      end;
    finally
      QTextListFormat_Destroy(lf);
      QTextFormat_Destroy(fmt);
    end;
  finally
    QTextBlock_Destroy(blk);
  end;
end;

class procedure TQtWSCustomRichMemo.SetParaNumbering(const AWinControl: TWinControl;
  TextStart, TextLen: Integer; const ANumber: TIntParaNumbering);
var
  te: TQtTextEdit;
  w: QTextEditH;
  doc: QTextDocumentH;
  cur: QTextCursorH;
  lf: QTextListFormatH;
  bf: QTextBlockFormatH;
  ws: WideString;
  lastpos: Integer;
begin
  if not GetEdit(AWinControl, 'SetParaNumbering', te, w) then Exit;
  doc := QTextEdit_document(w);
  cur := NewCursor(w);
  try
    QTextCursor_beginEditBlock(cur);
    if ANumber.Style = pnNone then
    begin
      lastpos := ClampPos(doc, TextStart + TextLen);
      QTextCursor_setPosition(cur, ClampPos(doc, TextStart));
      bf := QTextBlockFormat_Create();
      try
        repeat
          QTextCursor_blockFormat(cur, bf);
          QTextFormat_setObjectIndex(bf, -1);
          QTextBlockFormat_setIndent(bf, 0);
          QTextCursor_setBlockFormat(cur, bf);
          if not QTextCursor_movePosition(cur, QTextCursorNextBlock) then Break;
        until QTextCursor_position(cur) > lastpos;
      finally
        QTextBlockFormat_Destroy(bf);
      end;
    end else
    begin
      SelectRange(cur, doc, TextStart, TextLen);
      lf := QTextListFormat_Create();
      try
        case ANumber.Style of
          pnNumber: QTextListFormat_setStyle(lf, QTextListFormatListDecimal);
          pnLowLetter: QTextListFormat_setStyle(lf, QTextListFormatListLowerAlpha);
          pnUpLetter: QTextListFormat_setStyle(lf, QTextListFormatListUpperAlpha);
          pnLowRoman: QTextListFormat_setStyle(lf, QTextListFormatListLowerRoman);
          pnUpRoman: QTextListFormat_setStyle(lf, QTextListFormatListUpperRoman);
        else
          QTextListFormat_setStyle(lf, QTextListFormatListDisc);
        end;
        QTextListFormat_setIndent(lf, 1);
        if ANumber.Style in [pnNumber, pnLowLetter, pnUpLetter, pnLowRoman, pnUpRoman] then
        begin
          if ANumber.SepChar <> SepNone then
            ws := ANumber.SepChar
          else
            ws := '';
          QTextListFormat_setNumberSuffix(lf, @ws);
        end;
        QTextCursor_createList(cur, lf);
      finally
        QTextListFormat_Destroy(lf);
      end;
    end;
    QTextCursor_endEditBlock(cur);
  finally
    QTextCursor_Destroy(cur);
  end;
end;

class procedure TQtWSCustomRichMemo.SetParaTabs(const AWinControl: TWinControl;
  TextStart, TextLen: Integer; const AStopList: TTabStopList);
var
  te: TQtTextEdit;
  w: QTextEditH;
  doc: QTextDocumentH;
  opt: QTextOptionH;
  arr: TPtrIntArray;
  i: Integer;
  k: Double;
begin
  // Qt5Pas does not expose per-block tab positions, tabs are document-wide
  if not GetEdit(AWinControl, 'SetParaTabs', te, w) then Exit;
  doc := QTextEdit_document(w);
  k := ScreenDPI / PageDPI;
  arr := nil;
  SetLength(arr, AStopList.Count);
  for i := 0 to AStopList.Count - 1 do
    arr[i] := Round(AStopList.Tabs[i].Offset * k);
  opt := QTextOption_Create();
  try
    QTextDocument_defaultTextOption(doc, opt);
    QTextOption_setTabArray(opt, @arr);
    QTextDocument_setDefaultTextOption(doc, opt);
  finally
    QTextOption_Destroy(opt);
  end;
end;

class function TQtWSCustomRichMemo.GetParaTabs(const AWinControl: TWinControl;
  TextStart: integer; var AStopList: TTabStopList): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  opt: QTextOptionH;
  arr: TPtrIntArray;
  i: Integer;
  k: Double;
begin
  InitTabStopList(AStopList);
  Result := GetEdit(AWinControl, 'GetParaTabs', te, w);
  if not Result then Exit;
  k := PageDPI / ScreenDPI;
  arr := nil;
  opt := QTextOption_Create();
  try
    QTextDocument_defaultTextOption(QTextEdit_document(w), opt);
    QTextOption_tabArray(opt, @arr);
  finally
    QTextOption_Destroy(opt);
  end;
  AStopList.Count := Length(arr);
  SetLength(AStopList.Tabs, AStopList.Count);
  for i := 0 to AStopList.Count - 1 do
  begin
    AStopList.Tabs[i].Offset := arr[i] * k;
    AStopList.Tabs[i].Align := tabLeft;
  end;
end;

class function TQtWSCustomRichMemo.GetParaRange(const AWinControl: TWinControl;
  TextStart: Integer; var rng: TParaRange): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  blk: QTextBlockH;
  nxt: QTextBlockH;
begin
  Result := GetEdit(AWinControl, 'GetParaRange', te, w);
  if not Result then Exit;
  blk := QTextBlock_Create();
  nxt := QTextBlock_Create();
  try
    QTextDocument_findBlock(QTextEdit_document(w), blk, ClampPos(QTextEdit_document(w), TextStart));
    rng.start := QTextBlock_position(blk);
    rng.lengthNoBr := QTextBlock_length(blk) - 1;
    QTextBlock_next(blk, nxt);
    if QTextBlock_isValid(nxt) then
      rng.length := rng.lengthNoBr + 1
    else
      rng.length := rng.lengthNoBr;
  finally
    QTextBlock_Destroy(nxt);
    QTextBlock_Destroy(blk);
  end;
end;

class function TQtWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  fmt: QTextCharFormatH;
  ws: WideString;
  dfam: WideString;
  dsz: Double;
  sz: Double;
  c: TColor;
begin
  InitFontParams(Params);
  Result := GetEdit(AWinControl, 'GetTextAttributes', te, w);
  if not Result then Exit;

  DefaultFontInfo(w, dfam, dsz);
  fmt := QTextCharFormat_Create();
  try
    CharFormatAt(w, TextStart, fmt);

    ws := '';
    QTextCharFormat_fontFamily(fmt, @ws);
    if ws = '' then ws := dfam;
    Params.Name := UTF8Encode(ws);

    sz := QTextCharFormat_fontPointSize(fmt);
    if sz <= 0 then sz := dsz;
    Params.Size := Round(sz / GetZoom(te));

    if QTextCharFormat_fontWeight(fmt) >= Ord(QFontBold) then Include(Params.Style, fsBold);
    if QTextCharFormat_fontItalic(fmt) then Include(Params.Style, fsItalic);
    if QTextCharFormat_fontUnderline(fmt) then Include(Params.Style, fsUnderline);
    if QTextCharFormat_fontStrikeOut(fmt) then Include(Params.Style, fsStrikeOut);

    if GetBrushProp(fmt, True, c) then
      Params.Color := c
    else
      Params.Color := DefaultTextColor(AWinControl);

    Params.HasBkClr := GetBrushProp(fmt, False, c);
    if Params.HasBkClr then
      Params.BkColor := c;

    case QTextCharFormat_verticalAlignment(fmt) of
      QTextCharFormatAlignSuperScript: Params.VScriptPos := vpSuperScript;
      QTextCharFormatAlignSubScript: Params.VScriptPos := vpSubScript;
    else
      Params.VScriptPos := vpNormal;
    end;
  finally
    QTextCharFormat_Destroy(fmt);
  end;
end;

class procedure TQtWSCustomRichMemo.SetTextAttributes(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const Params: TIntFontParams);
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  fmt: QTextCharFormatH;
  br: QBrushH;
  ws: WideString;
const
  QIsBold: array [Boolean] of Integer = (Ord(QFontNormal), Ord(QFontBold));
begin
  if not GetEdit(AWinControl, 'SetTextAttributes', te, w) then Exit;
  cur := NewCursor(w);
  fmt := QTextCharFormat_Create();
  try
    SelectRange(cur, QTextEdit_document(w), TextStart, TextLen);

    ws := UTF8Decode(Params.Name);
    if ws <> '' then QTextCharFormat_setFontFamily(fmt, @ws);
    if Params.Size > 0 then QTextCharFormat_setFontPointSize(fmt, Params.Size * GetZoom(te));
    QTextCharFormat_setFontWeight(fmt, QIsBold[fsBold in Params.Style]);
    QTextCharFormat_setFontItalic(fmt, fsItalic in Params.Style);
    QTextCharFormat_setFontUnderline(fmt, fsUnderline in Params.Style);
    QTextCharFormat_setFontStrikeOut(fmt, fsStrikeOut in Params.Style);
    QTextCharFormat_setVerticalAlignment(fmt, VAlignMap[Params.VScriptPos]);

    SetBrushProp(fmt, Params.Color, True);
    if Params.HasBkClr then
      SetBrushProp(fmt, Params.BkColor, False)
    else
    begin
      br := QBrush_Create(QtNoBrush);
      try
        QTextFormat_setBackground(fmt, br);
      finally
        QBrush_Destroy(br);
      end;
    end;

    QTextCursor_mergeCharFormat(cur, fmt);
  finally
    QTextCharFormat_Destroy(fmt);
    QTextCursor_Destroy(cur);
  end;
end;

class procedure TQtWSCustomRichMemo.SetTextUIParams(const AWinControl: TWinControl;
  TextStart, TextLen: Integer; const ui: TTextUIParam);
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  fmt: QTextCharFormatH;
  ws: WideString;
begin
  if not GetEdit(AWinControl, 'SetTextUIParams', te, w) then Exit;
  cur := NewCursor(w);
  fmt := QTextCharFormat_Create();
  try
    SelectRange(cur, QTextEdit_document(w), TextStart, TextLen);
    if uiLink in ui.features then
    begin
      ws := UTF8Decode(ui.linkref);
      if ws = '' then
        ws := UTF8Decode(TCustomRichMemo(AWinControl).GetText(TextStart, TextLen));
      QTextCharFormat_setAnchor(fmt, True);
      QTextCharFormat_setAnchorHref(fmt, @ws);
      QTextCharFormat_setFontUnderline(fmt, True);
      SetBrushProp(fmt, clBlue, True);
    end else
    begin
      ws := '';
      QTextCharFormat_setAnchor(fmt, False);
      QTextCharFormat_setAnchorHref(fmt, @ws);
      QTextCharFormat_setFontUnderline(fmt, False);
      SetBrushProp(fmt, DefaultTextColor(AWinControl), True);
    end;
    QTextCursor_mergeCharFormat(cur, fmt);
  finally
    QTextCharFormat_Destroy(fmt);
    QTextCursor_Destroy(cur);
  end;
end;

class function TQtWSCustomRichMemo.GetTextUIParams(const AWinControl: TWinControl;
  TextStart: Integer; var ui: TTextUIParam): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  fmt: QTextCharFormatH;
  ws: WideString;
begin
  ui.features := [];
  ui.linkref := '';
  Result := GetEdit(AWinControl, 'GetTextUIParams', te, w);
  if not Result then Exit;
  fmt := QTextCharFormat_Create();
  try
    CharFormatAt(w, TextStart, fmt);
    if QTextCharFormat_isAnchor(fmt) then
    begin
      Include(ui.features, uiLink);
      ws := '';
      QTextCharFormat_anchorHref(fmt, @ws);
      ui.linkref := UTF8Encode(ws);
    end;
  finally
    QTextCharFormat_Destroy(fmt);
  end;
end;

class procedure TQtWSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  ws: WideString;
begin
  if not GetEdit(AWinControl, 'InDelText', te, w) then Exit;
  cur := NewCursor(w);
  try
    SelectRange(cur, QTextEdit_document(w), DstStart, DstLen);
    ws := UTF8Decode(TextUTF8);
    if ws <> '' then
      QTextCursor_insertText(cur, @ws)
    else
      QTextCursor_removeSelectedText(cur);
  finally
    QTextCursor_Destroy(cur);
  end;
end;

class function TQtWSCustomRichMemo.CharAtPos(const AWinControl: TWinControl;
  x, y: Integer): Integer;
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  pt: TQtPoint;
begin
  Result := -1;
  if not GetEdit(AWinControl, 'CharAtPos', te, w) then Exit;
  pt.x := x;
  pt.y := y;
  cur := QTextCursor_Create();
  try
    QTextEdit_cursorForPosition(w, cur, @pt);
    Result := QTextCursor_position(cur);
  finally
    QTextCursor_Destroy(cur);
  end;
end;

class function TQtWSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  te: TQtTextEdit;
  w: QTextEditH;
  doc: QTextDocumentH;
  cur: QTextCursorH;
  ws: Widestring;
  fl: QTextDocumentFindFlags;
  p: Integer;
begin
  Result := -1;
  if ANiddle = '' then
    exit;
  if not GetEdit(AWinControl, 'Search', te, w) then
    exit;
  doc := QTextEdit_document(w);

  fl := 0;
  if soMatchCase in SearchOpts.Options then
    fl := fl or QTextDocumentFindCaseSensitively;
  if soWholeWord in SearchOpts.Options then
    fl := fl or QTextDocumentFindWholeWords;
  if soBackward in SearchOpts.Options then
    fl := fl or QTextDocumentFindBackward;

  ws := UTF8Decode(ANiddle);
  cur := QTextCursor_Create();
  try
    QTextDocument_find(doc, cur, @ws, ClampPos(doc, SearchOpts.start), fl);
    if QTextCursor_isNull(cur) then
      exit;
    p := QTextCursor_selectionStart(cur);
    if SearchOpts.len >= 0 then
    begin
      if soBackward in SearchOpts.Options then
      begin
        if p < SearchOpts.start - SearchOpts.len then
          exit;
      end else
      begin
        if QTextCursor_selectionEnd(cur) > SearchOpts.start + SearchOpts.len then
          exit;
      end;
    end;
    Result := p;
  finally
    QTextCursor_Destroy(cur);
  end;
end;

class procedure TQtWSCustomRichMemo.SetZoomFactor(const AWinControl: TWinControl;
  AZoomFactor: Double);
var
  te: TQtTextEdit;
  w: QTextEditH;
  h: TQtRichMemoHelper;
  doc: QTextDocumentH;
  blk: QTextBlockH;
  nxt: QTextBlockH;
  cur: QTextCursorH;
  fmt: QTextCharFormatH;
  mfmt: QTextCharFormatH;
  rng: array of TTextRange;
  cnt: Integer;
  i: Integer;
  bpos: Integer;
  ratio: Double;
  sz: Double;
  dfam: WideString;
  dsz: Double;
begin
  if not GetEdit(AWinControl, 'SetZoomFactor', te, w) then
    exit;
  if AZoomFactor <= 0 then AZoomFactor := 1;
  h := FindHelper(te);
  if not Assigned(h) then
    exit;
  ratio := AZoomFactor / h.Zoom;
  h.Zoom := AZoomFactor;

  if ratio = 1 then
    exit;

  DefaultFontInfo(w, dfam, dsz);
  doc := QTextEdit_document(w);
  rng := nil;
  blk := QTextBlock_Create();
  cur := QTextCursor_Create(doc);
  fmt := QTextCharFormat_Create();
  try
    QTextCursor_beginEditBlock(cur);
    QTextDocument_findBlockByNumber(doc, blk, 0);
    while QTextBlock_isValid(blk) do
    begin
      bpos := QTextBlock_position(blk);
      cnt := QTextBlock_textFormatsCount(blk);
      SetLength(rng, cnt);
      if cnt > 0 then
        QTextBlock_textFormatsRanges(blk, PTextRangeArray(@rng[0]), cnt);
      for i := 0 to cnt - 1 do
      begin
        QTextCursor_setPosition(cur, bpos + rng[i].start + 1);
        QTextCursor_charFormat(cur, fmt);
        sz := QTextCharFormat_fontPointSize(fmt);
        if sz <= 0 then sz := dsz;
        QTextCursor_setPosition(cur, bpos + rng[i].start);
        QTextCursor_setPosition(cur, bpos + rng[i].start + rng[i].length, QTextCursorKeepAnchor);
        mfmt := QTextCharFormat_Create();
        try
          QTextCharFormat_setFontPointSize(mfmt, sz * ratio);
          QTextCursor_mergeCharFormat(cur, mfmt);
        finally
          QTextCharFormat_Destroy(mfmt);
        end;
      end;
      nxt := QTextBlock_Create();
      QTextBlock_next(blk, nxt);
      QTextBlock_Destroy(blk);
      blk := nxt;
    end;
    QTextCursor_endEditBlock(cur);
  finally
    QTextCharFormat_Destroy(fmt);
    QTextCursor_Destroy(cur);
    QTextBlock_Destroy(blk);
  end;
end;

class function TQtWSCustomRichMemo.GetZoomFactor(const AWinControl: TWinControl;
  var AZoomFactor: Double): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  Result := GetEdit(AWinControl, 'GetZoomFactor', te, w);
  if Result then
    AZoomFactor := GetZoom(te);
end;

class procedure TQtWSCustomRichMemo.Redo(const AWinControl: TWinControl);
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  if GetEdit(AWinControl, 'Redo', te, w) then
    QTextEdit_redo(w);
end;

class function TQtWSCustomRichMemo.GetCanRedo(const AWinControl: TWinControl): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  Result := GetEdit(AWinControl, 'GetCanRedo', te, w)
    and QTextDocument_isRedoAvailable(QTextEdit_document(w));
end;

class function TQtWSCustomRichMemo.isInternalChange(
  const AWinControl: TWinControl; Params: TTextModifyMask): Boolean;
begin
  Result := false;
end;

class procedure TQtWSCustomRichMemo.SetTextAttributesInternal(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AModifyMask: TTextModifyMask; const Params: TIntFontParams);
begin
  SetTextAttributes(AWinControl, TextStart, TextLen, Params);
end;

class function TQtWSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  qblck: QTextBlockH;
  i: Integer;
  cnt: Integer;
  rng: array of TTextRange;
  blckofs: Integer;
begin
  RangeStart:=TextStart;
  RangeLen:=0;
  Result := GetEdit(AWinControl, 'GetStyleRange', te, w);
  if not Result then
    exit;

  //  only returns within blocks (paragraphs).  Does not search prev/next block.
  Result:=false;
  rng:=nil;
  {$ifndef RMQT5_TEXTFORMATS}
  Exit;
  {$else}
  qblck := QTextBlock_Create();
  try
    QTextDocument_findBlock(QTextEdit_document(w), qblck, ClampPos(QTextEdit_document(w), TextStart));

    cnt := QTextBlock_textFormatsCount(qblck);
    SetLength(rng, cnt);
    if cnt>0 then
    begin
      QTextBlock_textFormatsRanges(qblck, PTextRangeArray(@rng[0]), cnt);
      blckofs := QTextBlock_position(qblck);
      TextStart := TextStart - blckofs;
      for i:=0 to cnt-1 do
      begin
        if (TextStart >= rng[i].start) and (TextStart<rng[i].start+rng[i].length) then
        begin
          RangeStart := rng[i].start + blckofs;
          RangeLen := rng[i].length;
          break;
        end;
      end;
    end;
    Result:=true;
  finally
    QTextBlock_Destroy(qblck);
  end;
  {$endif}
end;

class procedure TQtWSCustomRichMemo.SetTransparentBackground(
  const AWinControl: TWinControl; ATransparent: Boolean);
var
  te: TQtTextEdit;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTransparentBackground') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  QWidget_setAutoFillBackground(te.viewportWidget, not ATransparent);
end;

procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
begin
  backup.selst:=te.getSelectionStart;
  backup.sellen:=te.getSelectionLength;
end;

procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);
begin
  te.setSelection(backup.selst, backup.sellen);
end;

function QtInsertImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
  const FileNameUTF8: string; const AImgSize: TSize): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  cur: QTextCursorH;
  img: QImageH;
  scl: QImageH;
  ws: WideString;
  nm: WideString;
  k: Double;
begin
  Result := False;
  if not GetEdit(ARichMemo, 'InsertImageFromFile', te, w) then
    exit;
  ws := UTF8Decode(FileNameUTF8);
  img := QImage_Create(PWideString(@ws));
  try
    if QImage_isNull(img) then
      exit;
    if (AImgSize.cx > 0) and (AImgSize.cy > 0) then
    begin
      k := ScreenDPI / PageDPI;
      scl := QImage_Create();
      QImage_scaled(img, scl, Round(AImgSize.cx * k), Round(AImgSize.cy * k),
        QtIgnoreAspectRatio, QtSmoothTransformation);
      QImage_Destroy(img);
      img := scl;
    end;
    Inc(ImageCounter);
    nm := UTF8Decode('richmemo-image-' + IntToStr(ImageCounter));
    cur := NewCursor(w);
    try
      QTextCursor_setPosition(cur, ClampPos(QTextEdit_document(w), APos));
      QTextCursor_insertImage(cur, img, @nm);
    finally
      QTextCursor_Destroy(cur);
    end;
    Result := True;
  finally
    QImage_Destroy(img);
  end;
end;

procedure FreeHelpers;
var
  i: Integer;
begin
  if not Assigned(Helpers) then
    exit;
  for i := 0 to Helpers.Count - 1 do
    TQtRichMemoHelper(Helpers[i]).Free;
  FreeAndNil(Helpers);
end;

initialization
  InsertImageFromFile := @QtInsertImageFromFile;

finalization
  FreeHelpers;

end.
