unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_Desgn, lr_CrossTab, Forms, Controls,
  Graphics, Dialogs, StdCtrls, sqldb, IBConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    IBConnection1: TIBConnection;
    Label1: TLabel;
    Label2: TLabel;
    lrCrossObject1: TlrCrossObject;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function RepName:string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LazFileUtils, LazUtf8;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FileExistsUTF8(RepName) then
    frReport1.LoadFromFile(RepName)
  else
    frReport1.FileName:=RepName;
  frReport1.DesignReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if FileExistsUTF8(RepName) then
  begin
    frReport1.LoadFromFile(RepName);
    frReport1.ShowReport;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IBConnection1.Connected:=true;
  SQLQuery1.Open;
end;

function TForm1.RepName: string;
begin
  Result:=AppendPathDelim(ExtractFileDir(ParamStrUTF8(0)))+'demo_cross.lrf';
end;

end.

