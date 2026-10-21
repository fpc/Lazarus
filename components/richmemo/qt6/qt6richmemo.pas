unit qt6richmemo;

interface
{$ifdef LCLQt6}
{$mode delphi}

{$define RMQT6_TEXTFORMATS} // the feature is available in Qt6 Trunk
                            // it allows to query Qt6TextEdit for character formats array
{$ifdef RMQT6_NOTEXTFORMATS}{$undef RMQT6_TEXTFORMATS}{$endif}

//
// Following class methods are need for the implementation
//  QTextCharFormatH
//  QTextBlockFormatH
uses
  LCLType, Controls, StdCtrls, Graphics,
  qt6, qtobjects, qtwidgets, qtprivate,
  WSProc,
  RichMemo, WSRichMemo;

type
  { TQtWSCustomRichMemo }

  TQtWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

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
    selst  : integer; // selection start
    sellen : integer; // selection length
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
  QtTextEdit.FList := TQtMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);
  QtTextEdit.AttachEvents;

  Result := TLCLHandle(QtTextEdit);
end;

class procedure TQtWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  // alignment
  QTextEdit_setAlignment(w, AlignmentMap[AAlign]);

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : WideString;
  clr: TQColor;
  bck : TEditorState;
  qfh : QFontH;
begin
  InitFontParams(Params);
  if not WSCheckHandleAllocated(AWinControl, 'GetTextAttributes') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  MakeBackup(te, bck);

  te.setSelection(TextStart, 1);

  ws:='';
  QTextEdit_fontFamily(w, @ws);

  if ws='' then
    begin
      qfh:=QFont_Create();
      QApplication_font(qfh, w);
      QFont_family(qfh, @ws);
      Params.Size := QFont_pointSize(qfh);
      if QFont_weight(qfh)>=QtFontWeight_Bold then Include(Params.Style, fsBold);
      if QFont_italic(qfh) then Include(Params.Style, fsItalic);
      if QFont_underline(qfh) then Include(Params.Style, fsUnderline);
      QFont_Destroy(qfh);
    end
  else
    begin
      Params.Size:=round(QTextEdit_fontPointSize(w));
      if QTextEdit_fontWeight(w)>=QtFontWeight_Bold then Include(Params.Style, fsBold);
      if QTextEdit_fontItalic(w) then Include(Params.Style, fsItalic);
      if QTextEdit_fontUnderline(w) then Include(Params.Style, fsUnderline);
    end;

  Params.Name:=UTF8Encode(ws);
  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.Color));

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textBackgroundColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.BkColor));

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textBackgroundColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(Params.BkColor));
  Params.HasBkClr:=Params.BkColor > 0;

  ApplyBackup(te, bck);

  Result:=true;
end;

class procedure TQtWSCustomRichMemo.SetTextAttributes(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const Params: TIntFontParams);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
  ws : WideString;
  clr: TQColor;
const
  QIsBold: array [Boolean] of Integer = (QtFontWeight_Normal, QtFontWeight_Bold);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTextAttributes') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  ws:=UTF8Decode(Params.Name);
  if ws<>'' then QTextEdit_setFontFamily(w, @ws);
  if Params.Size>0 then QTextEdit_setFontPointSize(w, Params.Size);
  QTextEdit_setFontUnderline(w, fsUnderline in Params.Style);
  QTextEdit_setFontWeight(w, QisBold[fsBold in Params.Style]);
  QTextEdit_setFontItalic(w, fsItalic in Params.Style);

  ColorRefToTQColor(Params.Color, clr);
  QTextEdit_setTextColor(w, @clr);

  if Params.HasBkClr then
    begin
      ColorRefToTQColor(Params.BkColor, clr);
      QTextEdit_setTextBackgroundColor(w, @clr);
    end;

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : Widestring;
  fl : QTextDocumentFindFlags;
begin
  if not WSCheckHandleAllocated(AWinControl, 'Search') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  fl:=0;
  if soMatchCase in SearchOpts.Options then fl:=fl or QTextDocumentFindCaseSensitively;
  if soWholeWord in SearchOpts.Options then fl:=fl or QTextDocumentFindWholeWords;
  if soBackward in SearchOpts.Options then fl:=fl or QTextDocumentFindBackward;

  //todo: range filtering in Serach Opts
  ws:=UTF8Decode(ANiddle);
  if QTextEdit_find(w, @ws, fl) then  Result:=te.getSelectionStart
  else Result:=-1;
end;

class function TQtWSCustomRichMemo.isInternalChange(
  const AWinControl: TWinControl; Params: TTextModifyMask): Boolean;
begin
  Result := false;

  //Result:=inherited isInternalChange(AWinControl, Params);
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
  te : TQtTextEdit;
  bck : TEditorState;
  qcur : QTextCursorH;
  qblck : QTextBlockH;
  i   : Integer;
  cnt : Integer;
  rng : array of TTextRange;
  blckofs: Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetStyleRange') then begin
    Result:=false;
    Exit;
  end;

  //  only returns within blocks (paragraphs).  Does not search prev/next block.
  RangeStart:=TextStart;
  RangeLen:=0;
  Result:=false;
  rng:=nil;

  {$ifndef RMQT6_TEXTFORMATS}
  Exit;
  {$else}

  te:=TQtTextEdit(AWinControl.Handle);
  MakeBackup(te, bck);
  qcur := QTextCursor_Create();
  qblck := QTextBlock_Create();
  try
    te.setSelection(TextStart, 0);
    QTextEdit_textCursor(QTextEditH(te.Widget), qcur);
    QTextCursor_block(qcur, qblck);

    cnt := QTextBlock_textFormatsCount(qblck);
    SetLength(rng, cnt);
    QTextBlock_textFormatsRanges(qblck, PTextRangeArray(@rng[0]), cnt);

    if cnt>0 then begin
      blckofs := QTextBlock_position(qblck);
      TextStart := TextStart - blckofs;
      for i:=0 to cnt-1 do begin
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
    QTextCursor_Destroy(qcur);
    ApplyBackup(te, bck);
  end;
  {$endif}
end;

class procedure TQtWSCustomRichMemo.SetTransparentBackground(
  const AWinControl: TWinControl; ATransparent: Boolean);
var
  te : TQtTextEdit;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTransparentBackground') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  QWidget_setAutoFillBackground(te.viewportWidget, not ATransparent);
end;

class function TQtWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  te : TQtTextEdit;
  al : QtAlignment;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetParaAlignment') then begin
    Result:=false;
    Exit;
  end;
  te:=TQtTextEdit(AWinControl.Handle);
  al:=QTextEdit_alignment(QTextEditH(te.Widget));
  if QtAlignLeading and al > 0 then AAlign:=paLeft
  else if QtAlignTrailing and al > 0 then AAlign:=paRight
  else if QtAlignCenter and al > 0 then AAlign:=paCenter
  else if QtAlignJustify and al > 0 then AAlign:=paJustify
  else AAlign:=paLeft;
  Result:=true;
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

{$else}
implementation
// dummy empty unit.
{$endif}  //{$ifdef LCLQt6}
end.

