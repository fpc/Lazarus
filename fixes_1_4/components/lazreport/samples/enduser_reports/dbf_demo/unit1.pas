unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lrTDbfData, LRDialogControls, LR_Class, LR_Desgn,
  Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    LRDialogControls1: TLRDialogControls;
    lrTDbfData1: TlrTDbfData;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FReportFileName:string;
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
  frReport1.LoadFromXMLFile(FReportFileName);
  frReport1.ShowReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frReport1.LoadFromXMLFile(FReportFileName);
  frReport1.DesignReport;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FReportFileName:=ExtractFileDir(ParamStr(0)) + DirectorySeparator +'dbf_demo.lrf';
end;

end.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lrTDbfData, LRDialogControls, LR_Class, LR_Desgn,
  Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    LRDialogControls1: TLRDialogControls;
    lrTDbfData1: TlrTDbfData;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FReportFileName:string;
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
  frReport1.LoadFromXMLFile(FReportFileName);
  frReport1.ShowReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frReport1.LoadFromXMLFile(FReportFileName);
  frReport1.DesignReport;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FReportFileName:=ExtractFileDir(ParamStr(0)) + DirectorySeparator +'dbf_demo.lrf';
end;

end.

