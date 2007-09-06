unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LR_DSet,
  LR_Class, LR_Pars, Buttons;

{.$DEFINE DEBUG}

type

  { TForm1 }

  TForm1 = class(TForm)
    btnShowReport: TButton;
    Button1: TButton;
    Button2: TButton;
    frReport1: TfrReport;
    MasterDS: TfrUserDataset;
    ColumnsDS: TfrUserDataset;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ColumnsDSCheckEOF(Sender: TObject; var IsEof: Boolean);
    procedure ColumnsDSFirst(Sender: TObject);
    procedure ColumnsDSNext(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnShowReportClick(Sender: TObject);
    procedure frReport1BeginBand(Band: TfrBand);
    procedure frReport1BeginColumn(Band: TfrBand);
    procedure frReport1BeginDoc;
    procedure frReport1BeginPage(pgNo: Integer);
    procedure frReport1EndBand(Band: TfrBand);
    procedure frReport1EndDoc;
    procedure frReport1EndPage(pgNo: Integer);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
    procedure frReport1GetValue(const ParName: String; var ParValue: Variant);
    procedure frReport1PrintColumn(ColNo: Integer; var AWidth: Integer);
    procedure frReport1Progress(n: Integer);
    procedure frReport1UserFunction(const AName: String; p1, p2, p3: Variant;
      var Val: Variant);
    procedure MasterDSCheckEOF(Sender: TObject; var ISEof: Boolean);
    procedure MasterDSFirst(Sender: TObject);
    procedure MasterDSNext(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FRow: Integer;
    FCol: Integer;
  end; 

var
  Form1: TForm1; 

implementation

uses lclproc, unit2;

{ TForm1 }

procedure TForm1.frReport1GetValue(const ParName: String; var ParValue: Variant
  );
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1GetValue: ParName=',ParName);
  {$ENDIF}
  if ParName='I' then
    ParValue:=FCol
  else
  if ParName='J' then
    ParValue:=FRow
  else
  if ParName='S1' then begin
    if FRow=0 then
      ParValue:=''
    else
      ParValue:='Row'+IntToStr(FRow);
  end;
end;

procedure TForm1.frReport1PrintColumn(ColNo: Integer; var AWidth: Integer);
begin
  aWidth:=60;
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1PrintColumn: ColNo',COlNo,' Width=',AWidth);
  {$ENDIF}
end;

procedure TForm1.frReport1Progress(n: Integer);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1Progress: n=',n);
  {$ENDIF}
end;

procedure TForm1.frReport1UserFunction(const AName: String; p1, p2, p3: Variant;
  var Val: Variant);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1UserFuncion: Name=',AName,' P1=',P1,' P2=',P2,' P3=',P3, ' Val=',Val);
  {$ENDIF}
end;

procedure TForm1.frReport1EnterRect(Memo: TStringList; View: TfrView);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1EnterRect: INI, Memo: "', Dbgstr(Memo.Text),'" View=', View.Name,' View.FillColor=', ColorToString(View.FillColor)  );
  {$ENDIF}
  if FRow=0 then
    view.FillColor := clWhite
  else
  if FCol mod 4=0 then
    view.FillColor := clYellow;
    
  if (FCol=3)and(FRow=3) then
    view.Memo.Text := 'HELLO';
    
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1EnterRect: END, Memo: "', Dbgstr(Memo.Text),'" View=', View.Name,' View.FillColor=', ColorToString(View.FillColor)  );
  {$ENDIF}
end;

procedure TForm1.frReport1BeginBand(Band: TfrBand);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1BeginBand: Band=',Band.Name);
  {$ENDIF}
end;

procedure TForm1.btnShowReportClick(Sender: TObject);
begin
  frReport1.ShowReport;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  frReport1.LoadFromFile('usuario.lrf');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.DesignReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  form2.show;
end;

procedure TForm1.ColumnsDSFirst(Sender: TObject);
begin
  FCol:=1;
  {$IFDEF DEBUG}
  WriteLn('TForm1.ColumnsDSFirst: FCol=',FCol);
  {$ENDIF}
end;

procedure TForm1.ColumnsDSNext(Sender: TObject);
begin
  Inc(FCol);
  {$IFDEF DEBUG}
  WriteLn('TForm1.ColumnsDSNext: FCol=',FCol);
  {$ENDIF}
end;

procedure TForm1.ColumnsDSCheckEOF(Sender: TObject; var ISEof: Boolean);
begin
  IsEOF:=FCol>9;
  {$IFDEF DEBUG}
  WriteLn('TForm1.ColumnsDSCheckEOF: FCol=',FCol,' IsEOF=',IsEOF);
  {$ENDIF}
end;

procedure TForm1.frReport1BeginColumn(Band: TfrBand);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1BeginColumn: Band=',Band.Name);
  {$ENDIF}
end;

procedure TForm1.frReport1BeginDoc;
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1BeginDoc;');
  {$ENDIF}
end;

procedure TForm1.frReport1BeginPage(pgNo: Integer);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1BeginPage: PgNo=',PgNo);
  {$ENDIF}
end;

procedure TForm1.frReport1EndBand(Band: TfrBand);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1EndBand: Band=',Band.Name);
  {$ENDIF}
end;

procedure TForm1.frReport1EndDoc;
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1EndDoc:');
  {$ENDIF}
end;

procedure TForm1.frReport1EndPage(pgNo: Integer);
begin
  {$IFDEF DEBUG}
  WriteLn('TForm1.frReport1EndPage: PgNo=',PgNo);
  {$ENDIF}
end;

procedure TForm1.MasterDSFirst(Sender: TObject);
begin
  FRow:=0;
  {$IFDEF DEBUG}
  WriteLn('TForm1.MasterDSFirst: FRow=',FRow);
  {$ENDIF}
end;

procedure TForm1.MasterDSNext(Sender: TObject);
begin
  Inc(FRow);
  {$IFDEF DEBUG}
  WriteLn('TForm1.MasterDSNext: FRow=',FRow);
  {$ENDIF}
end;

procedure TForm1.MasterDSCheckEOF(Sender: TObject; var IsEof: Boolean);
begin
  IsEof:=FRow>40;
  {$IFDEF DEBUG}
  WriteLn('TForm1.MasterDSCheckEOF: FRow=',FRow,' IsEOF=',IsEOF);
  {$ENDIF}
end;

initialization
  {$I unit1.lrs}

end.

