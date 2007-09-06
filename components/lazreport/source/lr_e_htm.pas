
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
  Graphics,GraphType, Controls, Forms, Dialogs,
  StdCtrls,LR_E_TXT,

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
  s := '<HTML>'#13#10'<Body bgColor="#FFFFFF">'#13#10'<Table>'#13#10;
  Stream.Write(s[1], Length(s));
end;

destructor TfrHTMExportFilter.Destroy;
var
  s: String;
begin
  s := '</Table>'#13#10'</Body>'#13#10'</HTML>'#13#10;
  Stream.Write(s[1], Length(s));
  inherited Destroy;
end;

procedure TfrHTMExportFilter.OnEndPage;
var
  i, n: Integer;
  p: PfrTextRec;
  s, s1, s2: String;

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
    s := '<tr>';
    while p <> nil do
    begin
      s1 := ''; s2 := '';
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
        s2 := GetHTMLFontStyle(p^.FontStyle);
      if s1 <> '' then s1 := '<Font ' + s1 + '>';
      s := s + '<td>' + s1 + s2 + p^.Text + '</td>';
      p := p^.Next;
    end;
    s := s + '</tr>'#13#10;
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
