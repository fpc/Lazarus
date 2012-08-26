unit EMScriptClasses;
{
  Classes that can be accessed from Scripts
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, uPSCompiler, uPSRuntime;

//type
(*
  { TSrcEdit }

  TSrcEdit = class
  private
    FSynEdit: TCustomSynEdit;
    function GetCaretX: Integer;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer;
    function GetLine(Y: Integer): String;
    function GetLineAtCaret: String;
    function GetLogicalCaretX: Integer;
    function GetLogicalCaretXY: TPoint;
    procedure SetCaretX(AValue: Integer);
    procedure SetCaretXY(AValue: TPoint);
    procedure SetCaretY(AValue: Integer);
    procedure SetLogicalCaretX(AValue: Integer);
    procedure SetLogicalCaretXY(AValue: TPoint);
  public
    // Method for macro use
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property CaretX: Integer read GetCaretX write SetCaretX;
    property CaretY: Integer read GetCaretY write SetCaretY;
    property LogicalCaretXY: TPoint read GetLogicalCaretXY write SetLogicalCaretXY;
    property LogicalCaretX: Integer read GetLogicalCaretX write SetLogicalCaretX;
    property Line[Y: Integer]: String read GetLine;
    property LineAtCaret: String read GetLineAtCaret;
  public
    // Method  NOT  for macro use
    constructor Create(ASynEdit: TCustomSynEdit);
    property SynEdit: TCustomSynEdit read FSynEdit write FSynEdit;
  end;
*)

procedure CompRegisterTSynEdit(Cl: TPSPascalCompiler);
procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);


implementation



procedure TSynEdit_CaretXY_W(Self: TSynEdit; const P: TPoint);
begin   Self.CaretXY := P;   end;
procedure TSynEdit_CaretXY_R(Self: TSynEdit; var P: TPoint);
begin   P := Self.CaretXY;   end;

procedure TSynEdit_CaretX_W(Self: TSynEdit; const I: Integer);
begin   Self.CaretX := I;   end;
procedure TSynEdit_CaretX_R(Self: TSynEdit; var I: Integer);
begin   I := Self.CaretX;   end;

procedure TSynEdit_CaretY_W(Self: TSynEdit; const I: Integer);
begin   Self.CaretY := I;   end;
procedure TSynEdit_CaretY_R(Self: TSynEdit; var I: Integer);
begin   I := Self.CaretY;   end;

procedure TSynEdit_LogicalCaretXY_W(Self: TSynEdit; const P: TPoint);
begin   Self.LogicalCaretXY := P;   end;
procedure TSynEdit_LogicalCaretXY_R(Self: TSynEdit; var P: TPoint);
begin   P := Self.LogicalCaretXY;   end;

procedure TSynEdit_LogicalCaretX_W(Self: TSynEdit; const I: Integer);
begin   Self.LogicalCaretXY := Point(I, Self.CaretY);   end;
procedure TSynEdit_LogicalCaretX_R(Self: TSynEdit; var I: Integer);
begin   I := Self.LogicalCaretXY.X;   end;

procedure TSynEdit_Lines_R(Self: TSynEdit; var S: string; I: Longint);
begin   S := Self.Lines[I];   end;

procedure TSynEdit_LineAtCaret_R(Self: TSynEdit; var S: string);
begin   S := Self.Lines[Self.CaretY-1];   end;


procedure CompRegisterTSynEdit(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TOBJECT'), 'TSynEdit') do
  begin
    RegisterProperty('CaretXY', 'TPoint', iptRW);
    RegisterProperty('CaretX',  'Integer', iptRW);
    RegisterProperty('CaretY',  'Integer', iptRW);
    RegisterProperty('LogicalCaretXY', 'TPoint', iptRW);
    RegisterProperty('LogicalCaretX',  'Integer', iptRW);
    RegisterProperty('Lines', 'String Integer', iptR);

    RegisterProperty('LineAtCaret', 'String', iptR);
  end;
end;

procedure ExecRegisterTSynEdit(cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSynEdit) do
  begin
    //RegisterConstructor(@TSynEdit.CREATE, 'CREATE');

    RegisterPropertyHelper(@TSynEdit_CaretXY_R, @TSynEdit_CaretXY_W, 'CARETXY');
    RegisterPropertyHelper(@TSynEdit_CaretX_R,  @TSynEdit_CaretX_W,  'CARETX');
    RegisterPropertyHelper(@TSynEdit_CaretY_R,  @TSynEdit_CaretY_W,  'CARETY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretXY_R, @TSynEdit_LogicalCaretXY_W, 'LOGICALCARETXY');
    RegisterPropertyHelper(@TSynEdit_LogicalCaretX_R,  @TSynEdit_LogicalCaretX_W,  'LOGICALCARETX');
    RegisterPropertyHelper(@TSynEdit_Lines_R, nil, 'LINES');

    RegisterPropertyHelper(@TSynEdit_LineAtCaret_R, nil, 'LINEATCARET');
  end;
end;

{ TSynEdit }
(*
function TSynEdit.GetCaretXY: TPoint;
begin
  Result := SynEdit.CaretXY;
end;

function TSynEdit.GetCaretX: Integer;
begin
  Result := SynEdit.CaretX;
end;

function TSynEdit.GetCaretY: Integer;
begin
  Result := SynEdit.CaretY;
end;

function TSynEdit.GetLine(Y: Integer): String;
begin
  if Y >= SynEdit.Lines.Count then begin
    Result := '';
    exit;
  end;
  Result := SynEdit.Lines[Y - 1];
end;

function TSynEdit.GetLineAtCaret: String;
begin
  Result := GetLine(SynEdit.CaretY);
end;

function TSynEdit.GetLogicalCaretX: Integer;
begin
  Result := SynEdit.LogicalCaretXY.X;
end;

function TSynEdit.GetLogicalCaretXY: TPoint;
begin
  Result := SynEdit.LogicalCaretXY;
end;

procedure TSynEdit.SetCaretX(AValue: Integer);
begin
  SynEdit.CaretX := AValue;
end;

procedure TSynEdit.SetCaretXY(AValue: TPoint);
begin
  SynEdit.CaretXY := AValue;
end;

procedure TSynEdit.SetCaretY(AValue: Integer);
begin
  SynEdit.CaretY := AValue;
end;

procedure TSynEdit.SetLogicalCaretX(AValue: Integer);
begin
  SynEdit.LogicalCaretXY := Point(AValue, SynEdit.CaretY);;
end;

procedure TSynEdit.SetLogicalCaretXY(AValue: TPoint);
begin
  SynEdit.LogicalCaretXY := AValue;
end;

constructor TSynEdit.Create(ASynEdit: TCustomSynEdit);
begin
  FSynEdit := ASynEdit;
end;
*)

end.

