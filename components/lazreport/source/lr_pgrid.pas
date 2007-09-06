
{*****************************************}
{                                         }
{             FastReport v2.3             }
{         Print DBGrid component          }
{                                         }
{  FR_PGrid.pas:                          }
{  Copyright (c) 1999 by                  }
{  Butov Konstantin <kos@sp.iae.nsk.su>   }
{                                         }
{  FastReport:                            }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_PGrid;

interface

{$I LR_Vers.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBGrids, Printers, LR_DSet, LR_DBSet, LR_Class;

type

  TFrPrintGrid = class;

  TColumnInfo=record
    Column: Integer;
    ColumnWidth: Integer;
  end;
  
  TSetupColumnEvent=procedure(Sender:TFrPrintGrid; const Column: TColumn;
    var PrintColumn:boolean; var ColumnWidth:Integer) of object;

  { TfrPrintGrid }

  TfrPrintGrid = class(TComponent)
  private
    FDBGrid               : TDBGrid;
    FOnSetUpColumn: TSetupColumnEvent;
    FReport               : TfrReport;
    FReportDataSet        : TfrDBDataSet;
    FColumnDataSet        : TfrUserDataSet;
    FOrientation          : TPrinterOrientation;
    FFont, FTitleFont     : TFont;
    fShowProgress         : Boolean;
    fShowHdOnAllPage      : boolean;
    FCaption              : String;
    FShowCaption          : Boolean;
    FWidth                : Integer;
    FDataSet              : TDataset;
    FColumnsInfo          : array of TColumnInfo;
    
    procedure OnEnterRect(Memo: TStringList; View: TfrView);
    procedure OnPrintColumn(ColNo: Integer; var Width: Integer);
    procedure SetDBGrid(const AValue: TDBGrid);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetupColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure PreviewReport;
  published
    property DBGrid: TDBGrid read FDBGrid write SetDBGrid;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property Font: TFont read FFont write FFont;
    property TitleFont : TFont read FTitleFont write FTitleFont;
    property Caption: String read FCaption write FCaption;
    property ShowCaption: Boolean read FShowCaption write FShowCaption;
    property ShowHeaderOnAllPage : boolean read fShowHdOnAllPage write fShowHdOnAllPage default True;
    property ShowProgress : Boolean read fShowProgress write fShowProgress default false;
    property OnSetupColumn: TSetupColumnEvent read FOnSetUpColumn write FOnSetupColumn;
 end;


implementation

type
  THackDBGrid = class(TDBGrid)
  end;

{ TfrPrintGrid }

constructor TfrPrintGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fShowHdOnAllPage:=True;
  FFont := TFont.Create;
  FFont.Name := 'default';
  FFont.Charset := frCharset;
  FFont.Size := 0;
  FTitleFont := TFont.Create;
  FTitleFont.Assign(FFont);
  FTitleFont.Style := [fsBold];
  FCaption := 'Grid';
  FShowCaption := True;
  fShowProgress:=False;
end;

destructor TfrPrintGrid.Destroy;
begin
  SetLength(FColumnsInfo, 0);
  FFont.Free;
  FTitleFont.Free;
  inherited Destroy;
end;

procedure TfrPrintGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DBGrid) then
    DBGrid := nil;
end;

procedure TfrPrintGrid.SetupColumns;
var
  PrintColumn: Boolean;
  i,j,ColumnWidth: Integer;
begin
  SetLength(FColumnsInfo, 0);
  for i:=0 to DbGrid.Columns.Count-1 do begin

    PrintColumn := DbGrid.Columns[i].Visible;
    ColumnWidth := DbGrid.Columns[i].Width;
    
    if Assigned(FOnSetupColumn) then
      FOnSetupColumn(Self, TColumn(DbGrid.Columns[i]), PrintColumn, ColumnWidth);
      
    if PrintColumn then begin
      j:=Length(FColumnsInfo);
      SetLength(FColumnsInfo, j+1);
      FColumnsInfo[j].Column := i;
      FColumnsInfo[j].ColumnWidth := ColumnWidth;
    end;
    
  end;
end;

procedure TfrPrintGrid.SetDBGrid(const AValue: TDBGrid);
begin
  fDBGrid:=aValue;
  if (csDesigning in ComponentState) and Assigned(fDBGrid) then
  begin
    fFont.Assign(fDBGrid.Font);
    FTitleFont.Assign(fDBGrid.TitleFont);
  end;
end;

procedure TfrPrintGrid.PreviewReport;
var
  v: TfrView;
  b: TfrBandView;
  Page: TfrPage;
  BM  : TBookMark;
begin
  if (FDBGrid = nil) or (DBGrid.Datasource = nil) or
     (DBGrid.Datasource.Dataset = nil) then Exit;

  FDataSet := DBGrid.Datasource.Dataset;

  FReport := TfrReport.Create(Self);
  FReport.OnEnterRect  :=@OnEnterRect;
  FReport.OnPrintColumn:=@OnPrintColumn;
  FReport.ShowProgress :=fShowProgress;
  
  FReportDataSet := TfrDBDataSet.Create(Self);
  FReportDataSet.Name := 'frGridDBDataSet1';
  FReportDataSet.DataSet := FDataSet;

  SetupColumns;
  
  FColumnDataSet := TfrUserDataSet.Create(Self);
  FColumnDataSet.Name := 'frGridUserDataSet1';
  FColumnDataSet.RangeEnd := reCount;
  FColumnDataSet.RangeEndCount := Length(FColumnsInfo);

  try
    FReportDataSet.DataSource := DBGrid.DataSource;
    FReport.Pages.Add;
    Page := FReport.Pages[0];
    with Page do
      ChangePaper(pgSize, Width, Height, FOrientation);

    if FShowCaption then
    begin
      b := TfrBandView(frCreateObject(gtBand, ''));
      b.SetBounds(10, 20, 1000, 25);
      b.BandType := btReportTitle;
      Page.Objects.Add(b);
      v := frCreateObject(gtMemo, '');
      v.SetBounds(20, 20, Page.PrnInfo.PgW - 40, 25);
      TfrMemoView(v).Alignment:=taCenter;
      TfrMemoView(v).Font.Assign(FTitleFont);
      v.Memo.Add(FCaption);
      Page.Objects.Add(v);
    end;

    b := TfrBandView(frCreateObject(gtBand, ''));
    b.BandType := btMasterHeader;
    if self.fShowHdOnAllPage then
      b.Flags:=b.Flags+flBandRepeatHeader;
    b.SetBounds(20, 60, 1000, 20);
    Page.Objects.Add(b);

    v := frCreateObject(gtMemo, '');
    v.SetBounds(20, 60, 20, 20);
    TfrMemoView(v).Alignment:=taCenter;
    TfrMemoView(v).FillColor := clSilver;
    TfrMemoView(v).Font.Assign(FTitleFont);
    TfrMemoView(v).Frames:=frAllFrames;
    TfrMemoView(v).Layout:=tlCenter;
    v.Memo.Add('[Header]');
    Page.Objects.Add(v);

    b := TfrBandView(frCreateObject(gtBand, ''));
    b.BandType := btMasterData;
    b.Dataset := FReportDataSet.Name;
    b.SetBounds(0, 100, 1000, 18);
    Page.Objects.Add(b);

    b := TfrBandView(frCreateObject(gtBand, ''));
    b.BandType := btCrossData;
    b.Dataset := FColumnDataSet.Name;
    b.SetBounds(20, 0, 20, 1000);
    Page.Objects.Add(b);

    v := frCreateObject(gtMemo, '');
    v.SetBounds(20, 100, 20, 18);
    v.Memo.Add('[Cell]');
    TfrMemoView(v).Font.Assign(FFont);
    TfrMemoView(v).Frames:=frAllFrames;
    TfrMemoView(v).Layout:=tlCenter;
    Page.Objects.Add(v);

    FDataSet.DisableControls;
    BM:=FDataSet.GetBookmark;
    try
      FReport.ShowReport;
    finally
      FDataSet.GotoBookmark(BM);
      FDataSet.FreeBookmark(BM);
      FDataSet.EnableControls;
    end;
  finally
    FReport.Free;
    FReportDataSet.Free;
    FColumnDataSet.Free;
  end;
end;

procedure TfrPrintGrid.OnEnterRect(Memo: TStringList; View: TfrView);
var
  C: TColumn;
  i: Integer;
begin
  i := FColumnDataset.RecNo;

  if (i<0) or (i>Length(FColumnsInfo)-1) then
    exit;
    
  C := TColumn(DbGrid.Columns[FColumnsInfo[i].Column]);
  if C<>nil then
  begin
    if (Memo[0]='[Cell]') and (C.Field<>nil) then
    begin
      Memo[0] := C.Field.DisplayText;
      View.dx := FColumnsInfo[i].ColumnWidth;
      TfrMemoView(View).Alignment:=C.Alignment;
    end else
    if Memo[0]='[Header]' then
    begin
      Memo[0] := C.Title.Caption;
      View.dx := FColumnsInfo[i].ColumnWidth;
    end;
  end;
end;

procedure TfrPrintGrid.OnPrintColumn(ColNo: Integer; var Width: Integer);
begin
  if (ColNo<1) or (ColNo>Length(FColumnsInfo)) then
    exit;
  Width := FColumnsInfo[ColNo-1].ColumnWidth;
end;


end.
