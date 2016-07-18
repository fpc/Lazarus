unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_PQConnection, LR_IBConnection,
  LRDialogControls, LR_View, LR_Class, LR_Desgn, Forms, Controls, Graphics,
  Dialogs, StdCtrls, sqldb, IBConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    IBConnection1: TIBConnection;
    ListBox1: TListBox;
    LRDialogControls1: TLRDialogControls;
    LR_IBConnection1: TLR_IBConnection;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FReportFolders:string;
    procedure RefreshReportList;
    function CurReportName:string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LazFileUtils;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FReportFolders:=ExtractFileDir(ParamStr(0))+DirectorySeparator+'reports';
  IBConnection1.Connected:=true;
  SQLTransaction1.StartTransaction;
  RefreshReportList;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RefreshReportList;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex>-1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
  begin
    frReport1.LoadFromFile(CurReportName);
    frReport1.ShowReport;
  end
  else
    ShowMessage('Error! Not selected report.');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  frReport1.LoadFromFile(CurReportName);
  frReport1.DesignReport;
  RefreshReportList;
end;

procedure TForm1.RefreshReportList;
var
  R:TSearchRec;
  Code:integer;
begin
  ListBox1.Clear;
  Code:=FindFirstUTF8(FReportFolders+DirectorySeparator+'*.lrf', faAnyFile, R);
  while Code = 0 do
  begin
    if R.Attr and faDirectory = 0 then
    begin
      ListBox1.Items.Add(R.Name);
    end;
    Code:=FindNextUTF8(R);
  end;
  FindCloseUTF8(R);
end;

function TForm1.CurReportName: string;
begin
  Result:=FReportFolders + DirectorySeparator;
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex>-1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
    Result:=Result + ListBox1.Items[ListBox1.ItemIndex]
  else
    Result:=Result + 'New_Report.lrf';
end;

end.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_PQConnection, LR_IBConnection,
  LRDialogControls, LR_View, LR_Class, LR_Desgn, Forms, Controls, Graphics,
  Dialogs, StdCtrls, sqldb, IBConnection;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    IBConnection1: TIBConnection;
    ListBox1: TListBox;
    LRDialogControls1: TLRDialogControls;
    LR_IBConnection1: TLR_IBConnection;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FReportFolders:string;
    procedure RefreshReportList;
    function CurReportName:string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FReportFolders:=ExtractFileDir(ParamStr(0))+DirectorySeparator+'reports';
  IBConnection1.Connected:=true;
  SQLTransaction1.StartTransaction;
  RefreshReportList;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RefreshReportList;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex>-1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
  begin
    frReport1.LoadFromFile(CurReportName);
    frReport1.ShowReport;
  end
  else
    ShowMessage('Error! Not selected report.');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  frReport1.LoadFromFile(CurReportName);
  frReport1.DesignReport;
  RefreshReportList;
end;

procedure TForm1.RefreshReportList;
var
  R:TSearchRec;
  Code:integer;
begin
  ListBox1.Clear;
  Code:=FindFirstUTF8(FReportFolders+DirectorySeparator+'*.lrf', faAnyFile, R);
  while Code = 0 do
  begin
    if R.Attr and faDirectory = 0 then
    begin
      ListBox1.Items.Add(R.Name);
    end;
    Code:=FindNextUTF8(R);
  end;
  FindCloseUTF8(R);
end;

function TForm1.CurReportName: string;
begin
  Result:=FReportFolders + DirectorySeparator;
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex>-1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
    Result:=Result + ListBox1.Items[ListBox1.ItemIndex]
  else
    Result:=Result + 'New_Report.lrf';
end;

end.

