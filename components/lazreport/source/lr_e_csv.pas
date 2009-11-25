
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
  Graphics,GraphType, Controls, Forms, Dialogs,LR_E_TXT,
  LCLType,LCLIntf,LR_Class;

type

  TfrCSVExport = class(TComponent) 
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TfrCSVExportFilter }

  TfrCSVExportFilter = class(TfrTextExportFilter)
  private
    FIntervals: TFPList;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnText(X, Y: Integer; const Text: String; View: TfrView); override;
  end;


implementation

uses LR_Const;

const
  FIELD_GRAIN  = 32;  // granularity of fields when converting pixel positions

constructor TfrCSVExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FIntervals := TFPList.Create;
end;

destructor TfrCSVExportFilter.Destroy;
begin
  FIntervals.Free;
  inherited destroy;
end;

procedure TfrCSVExportFilter.OnBeginPage;
begin
  inherited OnBeginPage;
  FIntervals.Clear;
end;

function CompareIntervals(Item1, Item2: Pointer): Integer;
begin
  result := PtrInt(Item1)-PtrInt(Item2);
end;

procedure TfrCSVExportFilter.OnEndPage;
var
  i, j, k, n, tc1, tc2: Integer;
  MaxCols: Integer;
  p, q: PfrTextRec;
  s,str: String;
begin

  n := Lines.Count - 1;
  while n >= 0 do
  begin
    if Lines[n] <> nil then break;
    Dec(n);
  end;

  if FIntervals.Count = 0 then
    exit;

  FIntervals.Sort(@CompareIntervals);

  for i := 0 to n do
  begin
    p := PfrTextRec(Lines[i]);
    if p = nil then
      continue;

    s := '';
    for j := 0 to FIntervals.Count-1 do
    begin

      if (P <> nil) and ((P^.X div FIELD_GRAIN) = PtrInt(FIntervals[j])) then
      begin
        Str := p^.Text;
        p := p^.Next;
      end else
        Str := '';

      if j=0 then
        s := str
      else
        s := s + ';' + str;

    end;

    s := s + LineEnding;
    Stream.Write(s[1], Length(s));
  end;
end;

procedure TfrCSVExportFilter.OnText(X, Y: Integer; const Text: String;
  View: TfrView);
begin
  inherited OnText(X, Y, Text, View);
  x := x div FIELD_GRAIN;
  if FIntervals.IndexOf(pointer(ptrint(x)))<0 then
    FIntervals.Add(pointer(ptrint(x)));
end;

constructor TfrCSVExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrCSVExportFilter, sCSVFile + ' (*.csv)', '*.csv');
end;

end.

