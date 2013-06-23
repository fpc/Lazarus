unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAFuncSeries, TAGraph;

type
  TForm1 = class(TForm)
    btnRefresh: TButton;
    Chart1: TChart;
    Chart1FuncSeries1: TFuncSeries;
    edFormula: TEdit;
    Panel1: TPanel;
    procedure btnRefreshClick(Sender: TObject);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses
  uPSCompiler, uPSRuntime, uPSUtils;

type
  TUserFunction = function (AX: Double): Double of object;

var
  exec: TPSExec;
  userFunc: TUserFunction;

function ScriptOnExportCheck(
  ASender: TPSPascalCompiler; AProc: TPSInternalProcedure;
  const AProcDecl: string): Boolean;
begin
  if AProc.Name = 'FN' then
    if not ExportCheck(ASender, AProc, [btDouble, btDouble], [pmIn]) then begin
      ASender.MakeError('', ecTypeMismatch, '');
      exit(false);
    end;
  Result := true;
end;

function ScriptOnUses(ASender: TPSPascalCompiler; const AName: string): Boolean;
begin
  Result := AName = 'SYSTEM';
end;

procedure CompileScript(const AScript: string);
var
  data: string;
begin
  FreeAndNil(exec);

  with TPSPascalCompiler.Create do
    try
      OnUses := @ScriptOnUses;
      OnExportCheck := @ScriptOnExportCheck;
      AllowNoBegin := true;
      AllowNoEnd := true;
      if not Compile(AScript) then exit;
      GetOutput(data);
    finally
      Free;
    end;

  exec := TPSExec.Create;

  if not exec.LoadData(data) then begin
    FreeAndNil(exec);
    exit;
  end;

  userFunc := TUserFunction(exec.GetProcAsMethodN('FN'));
  if userFunc = nil then
    FreeAndNil(exec);
end;

{ TForm1 }

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  CompileScript(
    'function fn(x: Double): Double; begin Result := ' + edFormula.Text +
    '; end;');
  with edFormula do
    if exec = nil then
      Color := clRed
    else
      Color := clDefault;
  Chart1FuncSeries1.Active := true;
  Chart1.Invalidate;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  if exec = nil then
    AY := AX
  else
    AY := userFunc(AX);
end;

finalization
  FreeAndNil(exec);

end.

