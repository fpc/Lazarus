
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            HTM export filter            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_E_HTM;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs, LR_E_TXT,
  LCLType,LCLIntf,LR_Class;

type

  { TfrHTMExport }

  TfrHTMExport = class(TComponent)
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  TfrHTMExportFilter = class(TfrTextExportFilter)
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnEndPage; override;
  end;


implementation

uses LR_Const;


constructor TfrHTMExportFilter.Create(AStream: TStream);
var
  s: String;
begin
  inherited Create(AStream);

  s :=  '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"' + LineEnding +
        '    "http://www.w3.org/TR/html4/loose.dtd">' + LineEnding +
        '<html><head>' + LineEnding +
        '<meta name="generator" content="LazReport html exporter">' + LineEnding +
        '<title>LazReport Exported Report</title>' + LineEnding +  // TODO: improve
        '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' + LineEnding +
        '</head><body><table>' + LineEnding;

  Stream.Write(s[1], Length(s));
end;

destructor TfrHTMExportFilter.Destroy;
var
  s: String;
begin
  s := '</table></body></html>' + LineEnding;
  Stream.Write(s[1], Length(s));
  inherited Destroy;
end;

procedure TfrHTMExportFilter.OnEndPage;
var
  i, n: Integer;
  p: PfrTextRec;
  s, s1, s2, s3: String;

  function GetHTMLFontSize(Size: Integer): String;
  begin
    case Size of
      6, 7: Result := '1';
      8, 9: Result := '2';
      14..17: Result := '4';
      18..23: Result := '5';
      24..35: Result := '6'
    else
      Result := '7';
    end;
  end;

  function GetHTMLFontStyle(Style: Integer): String;
  begin
    Result := '';
    if (Style and $1) <> 0 then Result := '<i>';
    if (Style and $2) <> 0 then Result := Result + '<b>';
    if (Style and $4) <> 0 then Result := Result + '<u>';
  end;

  function GetEndHTMLFontStyle(Style: Integer): String;
  begin
    Result := '';
    if (Style and $4) <> 0 then Result := '</u>';
    if (Style and $2) <> 0 then Result := Result + '</b>';
    if (Style and $1) <> 0 then Result := Result + '</i>';
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
    p := PfrTextRec(Lines[i]);
    s := '';
    while p <> nil do
    begin
      s1 := ''; s2 := ''; s3 := '';
      if (p^.FontColor = clWhite) or (p^.FontColor = clNone) then
        p^.FontColor := clBlack;
      if p^.FontColor <> clBlack then
      begin
        s1 := IntToHex(p^.FontColor, 6);
        s1 := 'Color="#' + Copy(s1, 5, 2) + Copy(s1, 3, 2) +
          Copy(s1, 1, 2) + '"';
      end;
// most reports is done with font size = 10..13 - treat it as default font
      if not (p^.FontSize in [10..13]) then
        s1 := s1 + ' Size=' + GetHTMLFontSize(p^.FontSize);
      if p^.FontStyle <> 0 then
      begin
        s2 := GetHTMLFontStyle(p^.FontStyle);
        s3 := GetEndHTMLFontStyle(p^.FontStyle);
      end;
      if s1 <> '' then s1 := '<Font ' + s1 + '>';
      s := s + '<td>' + s1 + s2 + p^.Text + s3;
      if s1 <> '' then s := s + '</Font>';
      s := s + '</td>';
      p := p^.Next;
    end;
    if s='' then
      s := '<td></td>';
    s := '<tr>' + s + '</tr>' + LineEnding;
    Stream.Write(s[1], Length(s));
  end;
end;


{ TfrHTMExport }

constructor TfrHTMExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrHTMExportFilter, sHTMFile + ' (*.htm)', '*.htm');
end;

end.
