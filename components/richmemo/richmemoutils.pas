{
 richmemoutils.pas

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
unit RichMemoUtils;

interface

{$mode objfpc}{$h+}

uses
  Types, SysUtils, Classes, Graphics, LazFileUtils, LazUTF8, RichMemo;

const
  NoResize : TSize = ( cx: 0; cy : 0 );

var
  { Disclaimer: the function would insert an image file into RichMemo
    (if implemented by the widgetset) But in a very inefficient way.
    The image would be read again and the memory would be re-allocated for
    the image every time. So please, don't use it for smileys in
    your chat instant messaging.  A better API (with data caching) is considered.
    (That's why this method is not part of TCustomRichMemo class)

    APos - position in the text
    AImgSize - size to be inserted (in POINTS, not pixels!).
        if both width and height are 0, the image would not be resized at all.
    }
  InsertImageFromFile : function (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize
  ): Boolean = nil;

function InsertImageFromFileNoResize (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string): Boolean;

procedure LoadRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);

procedure SaveRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);

procedure InsertStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AStyle: TFontStyles;
  InsPos : Integer = -1 );
procedure InsertColorStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AColor: TColor; AStyle: TFontStyles;
  InsPos : Integer = -1 );
procedure InsertFontText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; const prms: TFontParams;
  InsPos : Integer = -1 );

type
  TCopyOptions = set of (
    coCopyBuf            // copy via buffer. Always used by default, if RichMemo tries to copy in itself,
                         // the flag is only helpful if rich memos are different
  );

// copies text from one rich memo to another
// if ASrc and ADst are the same rich memo, calls to CopyRichTextViaBuf()
// returns the number of characters copied
function CopyRichText(const ASrc: TCustomRichMemo;
  SrcStart, SrcLen: Integer;
  const ADst: TCustomRichMemo;
  DstStart, DstLen: integer;
  const CopyOpts: TCopyOptions = []
): Integer;

procedure AddFontStyle(rm: TCustomRichMemo; fs: TFontStyles);
procedure AddFontStyle(rm: TCustomRichMemo; fs: TFontStyles; ofs, len: integer);
procedure RemoveFontStyle(rm: TCustomRichMemo; fs: TFontStyles);
procedure RemoveFontStyle(rm: TCustomRichMemo; fs: TFontStyles; ofs, len: integer);

implementation

procedure AddFontStyle(rm: TCustomRichMemo; fs: TFontStyles);
begin
  if (rm = nil) then Exit;
  AddFontStyle(rm, fs, rm.SelStart, rm.SelLength);
end;

procedure AddFontStyle(rm: TCustomRichMemo; fs: TFontStyles; ofs, len: integer);
var
  p : TFontParams;
begin
  if (rm = nil) then Exit;
  InitFontParams(p);
  rm.SetRangeParams(ofs, len, [tmm_Styles], p, fs, []);
end;

procedure RemoveFontStyle(rm: TCustomRichMemo; fs: TFontStyles);
begin
  if (rm = nil) then Exit;
  RemoveFontStyle(rm, fs, rm.SelStart, rm.SelLength);
end;

procedure RemoveFontStyle(rm: TCustomRichMemo; fs: TFontStyles; ofs, len: integer);
var
  p : TFontParams;
begin
  if (rm = nil) then Exit;
  InitFontParams(p);
  rm.SetRangeParams(ofs, len, [tmm_Styles], p, [], fs);
end;

procedure InsertFontText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; const prms: TFontParams;
  InsPos : Integer = -1 );
var
  len : Integer;
begin
  if InsPos<0 then InsPos:=UTF8Length(ARichMemo.Text);
  len:=ARichMemo.InDelText(TextUTF8, InsPos, 0);
  ARichMemo.SetTextAttributes(InsPos, len, prms);
end;

procedure InsertColorStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AColor: TColor; AStyle: TFontStyles;
  InsPos : Integer = -1 );
var
  sel: Integer;
  ft : TFontParams;
begin
  if InsPos<0 then InsPos:=UTF8Length(ARichMemo.Text);
  ARichMemo.GetTextAttributes(InsPos, ft);
  ft.Style:=AStyle;
  ft.Color:=AColor;
  InsertFontText(ARichMemo, TextUTF8, ft, InsPos);
end;

procedure InsertStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AStyle: TFontStyles;
  InsPos : Integer );
var
  sel: Integer;
  ft : TFontParams;
begin
  if InsPos<0 then InsPos:=UTF8Length(ARichMemo.Text);
  ARichMemo.GetTextAttributes(InsPos, ft);
  ft.Style:=AStyle;
  InsertFontText(ARichMemo, TextUTF8, ft, InsPos);
end;

function InsertImageFileDummy(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize): Boolean;
begin
  Result:=false;
end;

function InsertImageFromFileNoResize (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string): Boolean;
begin
  Result:=InsertImageFromFile(ARichMemo, APos, FileNameUTF8, NoResize);
end;

{$IFDEF USELCLUtf8}
procedure LoadRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);
var
  fs : THandleStream;
  h  : THandle;
begin
  if not Assigned(ARichMemo) then Exit;
  h:= FileOpenUTF8(FileNameUTF8, fmShareDenyNone or fmOpenRead);
  fs := THandleStream.Create( h );
  try
    ARichMemo.LoadRichText(fs);
  finally
    fs.Free;
  end;
  FileClose(h);
end;
{$ENDIF}

procedure LoadRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);
var
  fs : TFileStream;
begin
  if not Assigned(ARichMemo) then Exit;
  fs:= TFileStream.Create( UTF8Decode(FileNameUTF8), fmShareDenyNone or fmOpenRead);
  try
    ARichMemo.LoadRichText(fs);
  finally
    fs.Free;
  end;
end;

procedure SaveRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);
var
  fs : TFileStream;
begin
  if not Assigned(ARichMemo) then Exit;
  fs:= TFileStream.Create( UTF8Decode(FileNameUTF8), fmCreate);
  try
    ARichMemo.SaveRichText(fs);
  finally
    fs.Free;
  end;
end;

type
  TBuffer = class(TObject)
    text   : string;
    len    : Integer;
    font   : TFontParams;
    para   : TParaMetric;
    palign : TParaAlignment;
    next   : TBuffer;
  end;

type
  TCopyResult = record
    TotalLen : integer;
  end;

procedure _CopyBuf(const ASrc: TCustomRichMemo;
  SrcStart, SrcLen: Integer;
  const ADst: TCustomRichMemo;
  DstStart, DstLen: integer;
  const CopyOpts: TCopyOptions;
  out Result: TcopyResult);
var
  bf : TBuffer;
  hd : TBuffer;
  lt : TBuffer;
  i,iend : integer;
  ofs,ln : integer;
  l: integer;
begin
  lt := nil;
  hd := nil;
  Result.TotalLen := 0;
  try
    i:=SrcStart;
    iend := SrcStart + SrcLen;

    while i < iend do begin
      bf := TBuffer.Create;
      ASrc.GetStyleRange(i, ofs, ln);
      if ofs + ln > iend then ln := iend - ofs;
      l := ln+ofs - i;
      bf.text := ASrc.GetText(i, l);
      ASrc.GetTextAttributes(ofs, bf.font);
      bf.len := l;
      if hd = nil then hd := bf;
      if lt <> nil then lt.Next := bf;
      lt := bf;
      inc(i, ln);
    end;

    while Assigned(hd) do begin
      ADst.InDelText(hd.text, DstStart, DstLen);
      ADst.SetTextAttributes(DstStart, hd.len, hd.font);
      inc(DstStart, hd.len);
      inc(Result.TotalLen, hd.len);
      if DstLen > 0 then DstLen := 0;
      lt := hd;
      hd := hd.Next;
      lt.Free;
    end;
  finally
    while Assigned(hd) do begin // an exception occurred?
      lt := hd;
      hd := hd.Next;
      lt.Free;
    end;
  end;
end;

// consume less memory, BUT can be slower. (depending on WS implementation)
procedure _CopyDirect(const ASrc: TCustomRichMemo;
  SrcStart, SrcLen: Integer;
  const ADst: TCustomRichMemo;
  DstStart, DstLen: integer;
  const CopyOpts: TCopyOptions;
  out Result: TcopyResult);
var
  i : integer;
  l : integer;
  iend: Integer;
  isFirst: boolean;
  txt: string;
  ofs: integer;
  ln: integer;
  fnt: TFontParams;
  copyParaParams: Boolean;
begin

  i:=SrcStart;
  iend := SrcStart + SrcLen;
  copyParaParams := false;
  Result.TotalLen := 0;
  InitFontParams(fnt);
  while i < iend do begin
    ASrc.GetStyleRange(i, ofs, ln);
    if ofs + ln > iend then ln := iend - ofs;
    l := ln+ofs - i;
    txt := ASrc.GetText(i, l);
    ASrc.GetTextAttributes(ofs, fnt);

    ADst.InDelText(txt, DstStart, DstLen);
    ADst.SetTextAttributes(DstStart, l, fnt);

    inc(DstStart, l);
    inc(i, l);
    inc(Result.TotalLen, l);
    if isFirst then begin
      DstLen := 0;
      isFirst := false;
    end;
  end;
end;

function CopyRichText(const ASrc: TCustomRichMemo;
  SrcStart, SrcLen: Integer;
  const ADst: TCustomRichMemo;
  DstStart, DstLen: integer;
  const CopyOpts: TCopyOptions): Integer;
var
  isSame : Boolean;
  copyBuf : boolean;
  res : TCopyResult;
  _initDst : integer;
  _initLen : integer;
begin
  if (ASrc = nil) or (ADst = nil) then begin
    Result := 0;
    Exit;
  end;
  isSame := (ASrc = ADst);

  if isSame then copyBuf := true
  else copyBuf := coCopyBuf in CopyOpts;

  ASrc.Lines.BeginUpdate;
  if not isSame then ADst.Lines.BeginUpdate;
  try
    if SrcStart < 0 then SrcStart := 0;
    if SrcLen < 0 then SrcLen := ASrc.GetTextLen;
    if SrcLen = 0 then begin
      Result := 0; // nothing to be copied over
      Exit;
    end;
    if DstStart < 0 then DstStart := 0;
    if DstLen < 0 then DstLen := ADst.GetTextLen;

    if copyBuf
      then _CopyBuf   (ASrc, SrcStart, SrcLen, ADst, DstStart, DstLen, CopyOpts, res)
      else _CopyDirect(ASrc, SrcStart, SrcLen, ADst, DstStart, DstLen, CopyOpts, res);

    Result := res.TotalLen;
  finally
    if not isSame then ADst.Lines.EndUpdate;
    ASrc.Lines.EndUpdate;
  end;
end;

initialization
  if not Assigned(InsertImageFromFile)  then
    InsertImageFromFile := @InsertImageFileDummy;

end.