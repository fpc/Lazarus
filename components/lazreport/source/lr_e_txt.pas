
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
  Classes, SysUtils, LazUTF8, LResources, Graphics, GraphType, Controls, Forms,
  Dialogs, LCLType, LCLIntf, LR_Class;

type

  { TfrTextExport }

  TfrTextExport = class(TComponent)
  private
    function GetShowExportParamsDlg: boolean;
    procedure SetShowExportParamsDlg(AValue: boolean);
  public
    constructor Create(aOwner: TComponent); override;
  published
    property ShowExportParamsDlg:boolean read GetShowExportParamsDlg write SetShowExportParamsDlg;
  end;

  { TfrTextExportFilter }

  TfrTextExportFilter = class(TfrExportFilter)
  private
    FUseBOM: boolean;
    FUsedFont: Integer;

    FDeleteEmptyLine: boolean;
    FPageBreaks: boolean;
  protected
    procedure GetUsedFont; virtual;
    function Setup:boolean; override;
    procedure NewRec(View: TfrView; const AText:string; var p:pointer); override;
    procedure CalcXCoords(var {%H-}x,{%H-}w: integer); virtual;
    function  CheckView(View: TfrView): boolean; override;
  public
    constructor Create(AStream: TStream); override;
    procedure OnBeginDoc; override;
    procedure OnEndPage; override;
    procedure OnBeginPage; override;
    procedure OnText({%H-}X, Y: Integer; const Text: String; View: TfrView); override;

    property UsedFont: integer read FUsedFont write FUsedFont;
    property UseBOM: boolean read FUseBOM write FUseBOM;
  end;


implementation

uses LR_Const, LR_E_TXT_Params;

var
  FShowExportParamsDlg: boolean;


procedure TfrTextExportFilter.GetUsedFont;
var
  s: String;
  n: Integer;
begin
  s := InputBox(sFilter, sFilterParam, '10');
  Val(s, FUsedFont, n);
  if n<>0 then
    FUsedFont := 10;
end;

function TfrTextExportFilter.Setup: boolean;
var
  lrExpTxtParamsForm: TlrExpTxtParamsForm;
begin
  Result:=inherited Setup;
  if FUsedFont<=0 then
    GetUsedFont;

  if FShowExportParamsDlg then
  begin
    lrExpTxtParamsForm:=TlrExpTxtParamsForm.Create(Application);
    lrExpTxtParamsForm.CheckBox1.Checked:=FDeleteEmptyLine;
    lrExpTxtParamsForm.CheckBox2.Checked:=FPageBreaks;
    Result:=lrExpTxtParamsForm.ShowModal = mrOk;
    if Result then
    begin
      FPageBreaks:=lrExpTxtParamsForm.CheckBox2.Checked;
      FDeleteEmptyLine:=lrExpTxtParamsForm.CheckBox1.Checked;
    end;
    lrExpTxtParamsForm.Free;
  end;
end;

procedure TfrTextExportFilter.NewRec(View: TfrView; const AText: string;
  var p:pointer);
begin
  inherited NewRec(View, AText, p);
  CalcXCoords(PfrTextRec(p)^.X, PfrTextRec(p)^.W);
end;

procedure TfrTextExportFilter.CalcXCoords(var x, w: integer);
begin
end;

function TfrTextExportFilter.CheckView(View: TfrView): boolean;
begin
  Result:= View.Typ in [gtMemo,gtAddin];
end;

constructor TfrTextExportFilter.Create(AStream: TStream);
begin
  inherited;
  FUsedFont := 10;
  FUseBOM := false;
  FPageBreaks:=true;
  FDeleteEmptyLine:=false;
end;

procedure TfrTextExportFilter.OnBeginDoc;
begin
  if FUseBOM then begin
    Stream.WriteByte($EF);
    Stream.WriteByte($BB);
    Stream.WriteByte($BF);
  end;
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
    if Lines[n] <> nil then
      break;
    Dec(n);
  end;

  for i := 0 to n do
  begin
    s  := '';
    tc1:= 0;
    p  := PfrTextRec(Lines[i]);
    while p <> nil do
    begin
      x  := Round(p^.X / 6.5);
      s  := s + Dup(x - tc1) + p^.Text;
      tc1:= x + UTF8Length(p^.Text);
      p  := p^.Next;
    end;

    if FDeleteEmptyLine and (S = '') then
      Continue;

    if FPageBreaks then
    begin
      S :=S+ #12+LineEnding;
      Stream.Write(s[1], Length(s));
    end;
  end;
  s := #12+LineEnding;
  Stream.Write(s[1], Length(s));
end;

procedure TfrTextExportFilter.OnBeginPage;
var
  i: integer;
begin
  ClearLines;
  for i := 0 to 200 do
    Lines.Add(nil);
end;

procedure TfrTextExportFilter.OnText(X, Y: Integer; const Text: String;
  View: TfrView);
var
  p: PfrTextRec;
begin
  if View = nil then
    Exit;
  Y := Round(Y / UsedFont);

  p:=nil;
  NewRec(View, Text, p);
  AddRec(Y, p);
end;


{ TfrTextExport }

function TfrTextExport.GetShowExportParamsDlg: boolean;
begin
  Result:=FShowExportParamsDlg;
end;

procedure TfrTextExport.SetShowExportParamsDlg(AValue: boolean);
begin
  FShowExportParamsDlg:=AValue;
end;

constructor TfrTextExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrTextExportFilter, sTextFile + ' (*.txt)', '*.txt');
end;

end.
