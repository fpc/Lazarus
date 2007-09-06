
{*****************************************}
{                                         }
{             FastReport v2.3             }
{           Text export filter            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_E_TXT;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,
  StdCtrls,

  LCLType,LCLIntf,LR_Class;

type

  { TfrTextExport }

  TfrTextExport = class(TComponent)
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  TfrTextExportFilter = class(TfrExportFilter)
  public
    constructor Create(AStream: TStream); override;
    procedure OnEndPage; override;
    procedure OnBeginPage; override;
    procedure OnText(X, Y: Integer; const Text: String; View: TfrView); override;
  end;


implementation

uses LR_Utils, LR_Const;


var
  UsedFont: Integer = 16;

constructor TfrTextExportFilter.Create(AStream: TStream);
var
  s: String;
  n: Integer;
begin
  inherited;
  s := InputBox(sFilter, sFilterParam, '16');
  Val(s, UsedFont, n);
end;

procedure TfrTextExportFilter.OnEndPage;
var
  i, n, x, tc1: Integer;
  p: PfrTextRec;
  s: String;
  function Dup(Count: Integer): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Count do
      Result := Result + ' ';
  end;

begin
  n := Lines.Count - 1;
  while n >= 0 do
  begin
    if Lines[n] <> nil then break;
    Dec(n);
  end;

  for i := 0 to n do
  begin
    s := '';
    tc1 := 0;
    p := PfrTextRec(Lines[i]);
    while p <> nil do
    begin
      x := Round(p^.X / 6.5);
      s := s + Dup(x - tc1) + p^.Text;
      tc1 := x + Length(p^.Text);
      p := p^.Next;
    end;
    s := s + #13#10;
    Stream.Write(s[1], Length(s));
  end;
  s := #12#13#10;
  Stream.Write(s[1], Length(s));
end;

procedure TfrTextExportFilter.OnBeginPage;
var
  i: Integer;
begin
  ClearLines;
  for i := 0 to 200 do Lines.Add(nil);
end;

procedure TfrTextExportFilter.OnText(X, Y: Integer; const Text: String;
  View: TfrView);
var
  p, p1, p2: PfrTextRec;
begin
  if View = nil then Exit;
  Y := Round(Y / UsedFont);
  p1 := PfrTextRec(Lines[Y]);
  GetMem(p, SizeOf(TfrTextRec));
  FillChar(p^, SizeOf(TfrTextRec), 0);
  p^.Next := nil;
  p^.X := X;
  p^.Text := Text;
  if View is TfrMemoView then
    with View as TfrMemoView do
    begin
      p^.FontName := Font.Name;
      p^.FontSize := Font.Size;
      p^.FontStyle := frGetFontStyle(Font.Style);
      p^.FontColor := Font.Color;
      p^.FontCharset := Font.Charset;
    end;
  p^.FillColor := View.FillColor;
  if p1 = nil then
    Lines[Y] := TObject(p)
  else
  begin
    p2 := p1;
    while (p1 <> nil) and (p1^.X < p^.X) do
    begin
      p2 := p1;
      p1 := p1^.Next;
    end;
    if p2 <> p1 then
    begin
      p2^.Next := p;
      p^.Next := p1;
    end
    else
    begin
      Lines[Y] := TObject(p);
      p^.Next := p1;
    end;
  end;
end;


{ TfrTextExport }

constructor TfrTextExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrTextExportFilter, sTextFile + ' (*.txt)', '*.txt');
end;

end.
