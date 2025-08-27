unit main;

{$mode objfpc}{$H+}

{$DEFINE USENEW_DBLOADSAVE}

interface

uses
  Classes, ExtCtrls, SysUtils, BufDataset, DB, FileUtil,
  LazLogger, Forms, Controls, Graphics, Dialogs,
  ComCtrls, DBCtrls, DBGrids, StdCtrls,
  LR_Const, LR_Class, LR_DBSet, LR_Desgn;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBNavigator2: TDBNavigator;
    Label1: TLabel;
    Panel2: TPanel;
    radLoadFromDesigner: TRadioButton;
    radLoadFromRecord: TRadioButton;
    reportsDS: TDataSource;
    reportsDB: TBufDataset;
    mushroomsDB: TBufDataset;
    gridMushrooms: TDBGrid;
    gridReports: TDBGrid;
    frDbMushrooms: TfrDBDataSet;
    Img: TDBImage;
    Notes: TDBMemo;
    DBNavigator1: TDBNavigator;
    mushroomsDS: TDataSource;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    Memo1: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    BtnDesign: TToolButton;
    BtnPreview: TToolButton;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    procedure BtnDesignClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure frDesigner1LoadReport(Report: TfrReport; var ReportName: String);
    procedure frDesigner1SaveReport(Report: TfrReport; var ReportName: String;
      SaveAs: Boolean; var Saved: Boolean);
    procedure reportsDBAfterScroll(DataSet: TDataSet);
  private
    DBName: string;
    procedure UpdateCaption;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  // mushrooms database
  DBName := ProgramDirectoryWithBundle + 'mushrooms.bds';
  if FileExists(DBName) then begin
    mushroomsDB.LoadFromFile(DBName);
    Notes.DataField := 'Notes';
    Img.DataField := 'Picture';
    Caption := format('RecodCount=%d', [mushroomsDB.RecordCount]);
  end else begin
    ShowMessage('mushrooms database not found!');
    Application.Terminate;
    exit;
  end;

  // reports database
  DBName := ProgramDirectoryWithBundle + 'reports.bds';
  if FileExists(DBName) then begin
    reportsDB.LoadFromFile(DBName);
  end else begin
    reportsDB.Close;
    with ReportsDB.FieldDefs do begin
      Clear;
      Add('Key', ftAutoInc);
      Add('Title', ftString, 100);
      Add('Report', ftMemo);
    end;
    reportsDB.CreateDataset;
  end;

  UpdateCaption;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  reportsDB.SaveToFile(DBName);
end;

procedure TForm1.BtnDesignClick(Sender: TObject);
begin
  if radLoadFromRecord.Checked then begin

    // method 1. load the report before opening designer
    //

    // Save any pending updates first
    if reportsDB.State in [dsEdit, dsInsert] then
      reportsDB.Post;

    if (reportsDB.RecordCount=0) or (reportsDB.FieldByName('Report').IsNull) then

      // 'mark' the report as new
      frReport1.FileName:= SUntitled

    else begin

      // load the report form the field 'Report'
      frReport1.LoadFromDB(reportsDB.FieldByName('Report'));

      // set the report name
      frReport1.FileName := reportsDB.FieldByName('Title').AsString;
    end

  end else begin

    // method 2. first show the designer and then use the designer Open.. menu, to open the
    //           the report in the reportsDB current record

  end;

  frReport1.DesignReport;

  UpdateCaption;
end;

procedure TForm1.BtnPreviewClick(Sender: TObject);
begin
  if reportsDB.RecordCount>0 then begin
    frReport1.LoadFromDB(reportsDB.FieldByName('Report'));
    frReport1.PrepareReport;
    frReport1.ShowPreparedReport;
  end
  else
    ShowMessage('There is no report!');
end;

{$REGION 'Designer' -fold}
procedure TForm1.frDesigner1SaveReport(Report: TfrReport;
  var ReportName: String; SaveAs: Boolean; var Saved: Boolean);
var
  book: TBookMark;
  nameChanged: Boolean;
begin

  // Ask the user for the new report name
  nameChanged := (reportsDB.RecordCount=0) or (reportsDB.FieldByName('Title').IsNull);
  if nameChanged then begin
    if not InputQuery('Report name', 'Report Name', ReportName) then begin
      Saved := false;
      exit;
    end;
  end;


  if SaveAs then begin //add new report

    // if reportsDB is not empty, check if the report name already exists
    // to avoid duplicates
    if ReportsDB.RecordCount>0 then begin
      book := ReportsDB.GetBookmark;
      try
        if ReportsDB.Locate('Title', ReportName, [loCaseInsensitive]) then begin
          ShowMessageFmt('A report named %s already exists', [ReportName]);
          Saved := false;
          exit;
        end;
      finally
        ReportsDB.GotoBookmark(book);
        ReportsDB.FreeBookmark(book);
      end;
    end;

    // append a new report
    reportsDB.Append;
  end else
    // the report is in the current record, enter edit state
    reportsDB.Edit;

  // update the report name
  if nameChanged then
    reportsDB.FieldByName('Title').Asstring := ReportName;

  // save the report into the database
  Report.SaveToDB(reportsDB.FieldByName('Report'));

  Saved:=true;

  // post the changes
  ReportsDB.Post;

  UpdateCaption;
end;

procedure TForm1.reportsDBAfterScroll(DataSet: TDataSet);
begin
  //Do not load in AfterScroll, in frReport1.LoadFromDB the table is scrolled from beginning to end
  //until the row with reportsDB.Fields[0].AsInteger is found. Not well done, it needs to be
  //corrected. This causes this function to run indefinitely.
  //if (not reportsDB.EOF)and (reportsDB.Fields[1].AsString<>'') then
  //frReport1.LoadFromDB(reportsDB,reportsDB.Fields[0].AsInteger);

  // That's okay, you can use it like that because in the new function, the data pointer is not moved.
  //frReport1.LoadFromDB(reportsDB.Fields[1]);
end;

procedure TForm1.UpdateCaption;
begin
  Caption := format('Reports: %d, Mushrooms: %d', [reportsDB.RecordCount, mushroomsDB.RecordCount]);
end;


procedure TForm1.frDesigner1LoadReport(Report: TfrReport; var ReportName: String
  );
begin
  if not reportsDB.FieldByName('Report').IsNull then begin
    frReport1.LoadFromDB(reportsDB.FieldByName('Report'));
    ReportName:=reportsDB.FieldByName('Title').asString; //Todo real name
  end;

  UpdateCaption;
end;
{$ENDREGION}

end.

