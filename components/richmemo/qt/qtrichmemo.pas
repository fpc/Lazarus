unit QtRichMemo;

interface

{$mode delphi}

//
// Following class methods are need for the implementation
//  QTextCharFormatH
//  QTextBlockFormatH
uses
  LCLType, Controls, StdCtrls, Graphics,
  qt4, qtobjects, qtwidgets, qtprivate,
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
  end;

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

  Result := TLCLIntfHandle(QtTextEdit);
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

const
  QNormal = 50;
  QBold   = 75;

class function TQtWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
  ws : WideString;
  clr: TQColor;
begin
  InitFontParams(Params);
  if not WSCheckHandleAllocated(AWinControl, 'GetTextAttributes') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, 1);

  //todo!
  ws:='';
  QTextEdit_fontFamily(w, @ws);
  writeln(wS);
  if ws<>'' then Params.Name:=ws;

  Params.Size:=round(QTextEdit_fontPointSize(w));
  if QTextEdit_fontWeight(w)>=QBold then Include(Params.Style, fsBold);
  if QTextEdit_fontItalic(w) then Include(Params.Style, fsItalic);
  if QTextEdit_fontUnderline(w) then Include(Params.Style, fsUnderline);

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.Color));

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textBackgroundColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.BkColor));
  //todo!
  params.HasBkClr:=false;

  te.setSelection(ss, sl);
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
  QNormal = 50;
  QBold   = 75;
const
  QIsBold: array [Boolean] of integer = (QNormal, QBold);
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

  //todo:
  {
  if not Params.HasBkClr then begin
    ColorRefToTQColor(Params.BkColor, clr);
    clr.Alpha:=0;
  end else
    ColorRefToTQColor(Params.BkColor, clr);
  QTextEdit_setTextBackgroundColor(w, @clr);
  }

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
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
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

end.
