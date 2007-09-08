
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            CSV export filter            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_E_CSV;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,
  StdCtrls,LR_E_TXT,

  LCLType,LCLIntf,LR_Class;

type

  TfrCSVExport = class(TComponent) 
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  TfrCSVExportFilter = class(TfrTextExportFilter)
  public
    procedure OnEndPage; override;
  end;


implementation

uses LR_Const;


procedure TfrCSVExportFilter.OnEndPage;
var
  i, j, n, tc1, tc2: Integer;
  p: PfrTextRec;
  s: String;
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
      tc2 := p^.X div 64;
      for j := 0 to tc2 - tc1 - 1 do
        s := s + ';';
      s := s + p^.Text;
      tc1 := tc2;
      p := p^.Next;
    end;
    s := s + #13#10;
    Stream.Write(s[1], Length(s));
  end;
end;

constructor TfrCSVExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrCSVExportFilter, sCSVFile + ' (*.csv)', '*.csv');
end;

end.

