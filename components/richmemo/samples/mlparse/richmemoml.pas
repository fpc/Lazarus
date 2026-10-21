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
unit RichMemoML;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, RichMemoUtils;

type
  TMarkupFormatHandler = procedure (Sender: TObject;
    var tag: string; tagattr: TStrings;
    var font: TFontParams; var txt: string; var tagCloses: Boolean ) of object;

  TMarkupEntityReplace = procedure (Sender: TObject; var txt: string; tagsStack: TStrings) of object;

  { TFormatStack }

  TFormatStack = class(Tobject)
  public
    tagName : string;
    fmt     : TFontParams;
    constructor Create(const atag: string; afmt: TFontParams);
  end;

  { TMarkupHandler }

  TMarkupHandler = class(TObject)
  private
    fOnMarkup   : TMarkupFormatHandler;
    fOnReplace  : TMarkupEntityReplace;
    fStack      : TList;
    fStackNames : TStrings;
    fRichMemo   : TRichMemo;
    procedure Append(const s: string; const fmt: TFontParams);
    procedure EntityReplace(var s: string);
    procedure GetTagFmt(var atag: string; st: TStrings; var fmt: TFontParams; var AText: string; var toBeClosed: Boolean);
    procedure AddStack(const atag: string; const fmt: TFontParams);
    procedure PopStack(const atag: string; var fmt: TFontParams);
    procedure Clear;
  public
    DefParams: TFontParams;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(ARichMemo: TRichMemo; const atext: string);
    property OnFormatSelect:  TMarkupFormatHandler read fOnMarkup write fOnMarkup;
    property OnEntityReplace: TMarkupEntityReplace read fOnReplace write fOnReplace;
  end;

procedure Parse(const atext: string; ADstMemo: TRichMemo; AFormat: TMarkupFormatHandler; AReplace: TMarkupEntityReplace = nil);

implementation

procedure Parse(const atext: string; ADstMemo: TRichMemo; AFormat: TMarkupFormatHandler; AReplace: TMarkupEntityReplace);
var
  h : TMarkupHandler;
begin
  if not Assigned(ADstMemo) or (atext='') then Exit;
  h := TMarkupHandler.Create;
  try
    h.OnFormatSelect:=AFormat;
    h.OnEntityReplace:=AReplace;
    h.DefParams:=GetFontParams(ADstMemo.Font);
    h.Parse(ADstMemo, atext);
  finally
    h.Free;
  end;
end;

{ TFormatStack }

constructor TFormatStack.Create(const atag: string; afmt: TFontParams);
begin
  inherited Create;
  tagName:=atag;
  fmt:=afmt;
end;

{ TMarkupHandler }

procedure TMarkupHandler.Append(const s: string; const fmt: TFontParams);
begin
  InsertFontText(fRichMemo, s, fmt);
end;

procedure TMarkupHandler.EntityReplace(var s: string);
var
  i : integer;
begin
  //todo: more effecient?
  fStackNames.Clear;
  for i:=0 to fStack.Count-1 do
    fStackNames.Add( TFormatStack(fStack[i]).tagName );

  if Assigned(OnEntityReplace) then OnEntityReplace(Self, s, fStackNames);
end;

procedure TMarkupHandler.GetTagFmt(var atag: string; st: TStrings;
  var fmt: TFontParams; var AText: string; var toBeClosed: Boolean);
begin
  if Assigned(OnFormatSelect) then
    OnFormatSelect(Self, atag, st, fmt, atext, toBeClosed);
end;

procedure TMarkupHandler.AddStack(const atag: string; const fmt: TFontParams);
begin
  fStack.Add(TFormatStack.Create(atag, fmt));
end;

procedure TMarkupHandler.PopStack(const atag: string; var fmt: TFontParams);
var
  i   : integer;
  j   : integer;
  fs  : TFormatStack;
begin
  i:=fStack.Count-1;
  while i>=0 do begin
    fs:=TFormatStack(fStack[i]);
    if fs.tagName=atag then begin
      for j:=fStack.Count-1 downto i do begin
        TFormatStack(fStack[j]).Free;
        fStack[j]:=nil;
      end;
      fStack.Pack;

      dec(i);
      if i>=0 then fmt:=TFormatStack(fStack[i]).fmt
      else fmt:=DefParams;
      Exit;
    end else
      dec(i);
  end;
  // do nothing. unknown closing tag
end;

procedure TMarkupHandler.Clear;
var
  i : Integer;
begin
  for i:=0 to fStack.Count-1 do
    TFormatStack(fStack[i]).free;
  fStack.Clear;
end;

constructor TMarkupHandler.Create;
begin
  inherited;
  fStack:=TList.Create;
  fStackNames:=TStringList.Create;
end;

destructor TMarkupHandler.Destroy;
begin
  Clear;
  fStackNames.Free;
  fStack.Free;
  inherited Destroy;
end;

procedure TMarkupHandler.Parse(ARichMemo: TRichMemo; const atext: string);
var
  i : integer;
  j : integer;
  sb : string;
  fmt : TFontParams;
  newfmt : TFontParams;
  tag : string;
  openTag : Boolean;
  tagAttr:  TStringList;
  tobeClosed : Boolean;
begin
  if not Assigned(ARichMemo) then Exit;
  Clear;
  fmt:=DefParams;
  i:=1;
  j:=1;
  fRichMemo:=ARichMemo;
  fRichMemo.Lines.BeginUpdate;
  tagAttr:=TStringList.Create;
  try
    while i<=length(atext) do begin
      if atext[i]='<' then begin

        tagattr.Clear;
        if j<i then begin
          sb:=Copy(atext, j, i-j);
          EntityReplace(sb);
          Append(sb, fmt);
        end;

        inc(i);
        j:=i;
        while (i<=length(atext)) and (atext[i]<>'>') do inc(i);

        if (i>j+1) and (i<=length(atext)) and (atext[j]='/') then begin
          openTag:=false;
          inc(j);
        end else begin
          openTag:=true;
        end;
        tag:=Copy(atext, j, i-j);
        tag:=AnsiLowerCase(tag);

        if openTag then begin
          tobeClosed:=true;
          sb:='';
          newfmt:=fmt;
          GetTagFmt(tag, tagattr, newfmt, sb, tobeClosed);

          if sb<>'' then Append(sb, newfmt);

          if tobeClosed then fmt:=newfmt;
          AddStack(tag, fmt);
        end else begin
          PopStack(tag, fmt);
        end;

        inc(i);
        j:=i;
        // parsing tag!
      end else
        inc(i);
    end;
    i:=length(atext)+1;
    if j<i then begin
      sb:=Copy(atext, j, i-j);
      Append( sb, fmt );
    end;
  finally
    fRichMemo.Lines.EndUpdate;
    tagAttr.Free;
  end;
end;

end.

