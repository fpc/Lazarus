unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_Desgn, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure frReport1MouseOverObject(View: TfrView; var ACursor: TCursor);
    procedure frReport1ObjectClick(View: TfrView);
  private
    FRepFileName:string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses LazUTF8;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FRepFileName:=AppendPathDelim(ExtractFileDir(ParamStrUTF8(0))) + 'url_demo.lrf';
  if FileExistsUTF8(FRepFileName) then
    frReport1.LoadFromFile(FRepFileName)
  else
    frReport1.FileName:=FRepFileName;
end;

procedure TForm1.frReport1MouseOverObject(View: TfrView; var ACursor: TCursor);
begin
  if Pos('url', UTF8LowerCase(View.Tag))>0 then
    ACursor:=crHandPoint;
end;

procedure TForm1.frReport1ObjectClick(View: TfrView);
begin
  if Pos('url_ext', UTF8LowerCase(View.Tag))>0 then
    ShowMessage('Selected adres: '+View.URLInfo);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.ShowReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frReport1.DesignReport;
end;

end.

