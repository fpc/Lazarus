unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_Desgn, LRDialogControls, Forms,
  Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    LRDialogControls1: TLRDialogControls;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    function ReportFileName:string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FileExistsUTF8(ReportFileName) then
    frReport1.LoadFromFile(ReportFileName)
  else
    frReport1.FileName:=ReportFileName;
  frReport1.DesignReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if FileExistsUTF8(ReportFileName) then
  begin
    frReport1.LoadFromFile(ReportFileName);
    frReport1.ShowReport;
  end;
end;

function TForm1.ReportFileName: string;
begin
  Result:=AppendPathDelim(ExtractFileDir(ParamStrUTF8(0)))+'demo_form_storage.lrf';
end;

end.

