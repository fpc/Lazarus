
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
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, PropEdits,
  DB, DBGrids, Printers, LR_DSet, LR_DBSet, LR_Class;

type

  TFrPrintGrid = class;

  TColumnInfo=record
    Column: Integer;
    ColumnWidth: Integer;
  end;

  TColumnInfoArr = Array of  TColumnInfo;

  TSetupColumnEvent=procedure(Sender:TFrPrintGrid; const Column: TColumn;
    var PrintColumn:boolean; var ColumnWidth:Integer) of object;

  TFinalSetupEvent=procedure(Sender:TFrPrintGrid; var FReport  : TfrReport;
     var FColumnsInfo : TColumnInfoArr ) of object;

  { TfrPrintGrid }

  TfrPrintGrid = class(TComponent)
  private
    FDBGrid               : TCustomDBGrid;
    FOnGetValue: TDetailEvent;
    FOnSetUpColumn: TSetupColumnEvent;
    FOnFinalSetup         : TFinalSetupEvent;
    FPrinterIndex         : Integer;
    FReport               : TfrReport;
    FReportDataSet        : TfrDBDataSet;
    FColumnDataSet        : TfrUserDataSet;
    FOrientation          : TPrinterOrientation;
    FFont, FTitleFont     : TFont;
    fShowProgress         : Boolean;
    fShowHdOnAllPage      : boolean;
    FCaption              : String;
    FShowCaption          : Boolean;
    FDataSet              : TDataset;
    FColumnsInfo          : TColumnInfoArr;
    FTemplate             : string;

    procedure OnEnterRect(Memo: TStringList; View: TfrView);
    procedure OnPrintColumn(ColNo: Integer; var Width: Integer);
    procedure SetDBGrid(const AValue: TCustomDBGrid);
    procedure SetFont(AValue: TFont);
    procedure SetTitleFont(AValue: TFont);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetupColumns;
    function FindBand(APage: TFrPage; AType: TfrBandType): TFrBandView;
    procedure ReplaceTemplate(APage:TFrPage; ABand: TFrBandView; ATemplate,AReplace:String);
    procedure FindFreeSpace(APage: TfrPage; out XPos,YPos:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PreviewReport;
  published
    property DBGrid: TCustomDBGrid read FDBGrid write SetDBGrid;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property Font: TFont read FFont write SetFont;
    property TitleFont : TFont read FTitleFont write SetTitleFont;
    property Caption: String read FCaption write FCaption;
    property Template: string read FTemplate write FTemplate;
    property PrinterIndex: Integer read FPrinterIndex write FPrinterIndex;
    property ShowCaption: Boolean read FShowCaption write FShowCaption;
    property ShowHeaderOnAllPage : boolean read fShowHdOnAllPage write fShowHdOnAllPage default True;
    property ShowProgress : Boolean read fShowProgress write fShowProgress default false;
    property OnSetupColumn: TSetupColumnEvent read FOnSetUpColumn write FOnSetupColumn;
    property OnGetValue: TDetailEvent read FOnGetValue write FOnGetValue;
    property OnFinalSetup: TFinalSetupEvent read FOnFinalSetup write FOnFinalSetup;

 end;


implementation

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
  FPrinterIndex := -1;
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
  for i:=0 to TDBGrid(DbGrid).Columns.Count-1 do begin

    PrintColumn := TDBGrid(DbGrid).Columns[i].Visible;
    ColumnWidth := TDBGrid(DbGrid).Columns[i].Width;

    if Assigned(FOnSetupColumn) then
      FOnSetupColumn(Self, TColumn(TDBGrid(DbGrid).Columns[i]), PrintColumn, ColumnWidth);

    if PrintColumn then begin
      j:=Length(FColumnsInfo);
      SetLength(FColumnsInfo, j+1);
      FColumnsInfo[j].Column := i;
      FColumnsInfo[j].ColumnWidth := ColumnWidth;
    end;

  end;
end;

function TfrPrintGrid.FindBand(APage: TFrPage; AType:TfrBandType): TFrBandView;
var
  i: Integer;
begin
  for i:=0 to APage.Objects.Count-1 do begin
    if not (TObject(APage.Objects[i]) is TFrBandView) then
      continue;
    Result := TFrBandView(APage.Objects[i]);
    if Result.BandType=AType then
      exit;
  end;
  result := nil;
end;

procedure TfrPrintGrid.ReplaceTemplate(APage: TFrPage; ABand: TFrBandView;
  ATemplate, AReplace: String);
var
  i: Integer;
  Obj: TfrObject;
begin
  for i:=0 to APage.Objects.Count-1 do begin
    Obj :=  TfrObject(APage.Objects[i]);
    if Obj is TfrMemoView then begin
      if (Obj.y>=ABand.y) and (Obj.y<(ABand.Y+ABand.Dy)) then begin
        // this memo is on ABand
        TfrMemoView(Obj).Memo.Text := StringReplace(TfrMemoView(Obj).Memo.Text,
          ATemplate, AReplace, [rfReplaceAll, rfIgnoreCase]);
      end;
    end;
  end;
end;

procedure TfrPrintGrid.FindFreeSpace(APage: TfrPage; out XPos, YPos: Integer);
var
  i: Integer;
  Ydone,Xdone: boolean;
begin

  YPos := 0;
  XPos := 20;

  YDone:= false;
  XDone:= false;

  for i:=0 to APage.Objects.Count-1 do begin
    if not (TObject(APage.Objects[i]) is TFrBandView) then
      continue;

    with TfrBandView(APage.Objects[i]) do begin
      if BandType in [btCrossHeader, btCrossData, btCrossFooter] then begin
        if not XDone then begin
          if x - XPos > 20 then
            XDone := true
          else
            XPos := x + dx + 1;
        end;
      end else begin
        if not YDone then begin
          if y - YPos > 40 then
            YDone := true
          else
            YPos := y + dy + 1;
        end;
      end;
    end;

  end;
end;

procedure TfrPrintGrid.SetDBGrid(const AValue: TCustomDBGrid);
begin
  fDBGrid:=aValue;
  if (csDesigning in ComponentState) and Assigned(fDBGrid) then
  begin
    fFont.Assign(fDBGrid.Font);
    FTitleFont.Assign(TDBGrid(fDBGrid).TitleFont);
  end;
end;

procedure TfrPrintGrid.SetFont(AValue: TFont);
begin
  if FFont.IsEqual(AValue) then exit;
  FFont.Assign(AValue);
end;

procedure TfrPrintGrid.SetTitleFont(AValue: TFont);
begin
  if FTitleFont.IsEqual(AValue) then exit;
  FTitleFont.Assign(AValue);
end;

procedure TfrPrintGrid.PreviewReport;
var
  v: TfrView;
  b,h: TfrBandView;
  Page: TfrPage;
  BM  : TBookMark;
  XPos,YPos: Integer;
  SaveDesign:TfrReportDesigner;
begin
  if (FDBGrid = nil) or (TDBGrid(DBGrid).Datasource = nil) or
     (TDBGrid(DBGrid).Datasource.Dataset = nil) then Exit;

  if (FTemplate<>'') and not FileExists(FTemplate) then
      raise Exception.CreateFmt('Template file %s does not exists',[FTemplate]);

  SaveDesign:=frDesigner;
  frDesigner:=nil;

  FReport := TfrReport.Create(Self);
  if FTemplate<>'' then
    FReport.LoadFromFile(FTemplate);

  FDataSet := TDBGrid(DBGrid).Datasource.Dataset;

  FReport.OnEnterRect  :=@OnEnterRect;
  FReport.OnPrintColumn:=@OnPrintColumn;
  FReport.ShowProgress :=fShowProgress;
  FReport.OnGetValue   :=FOnGetValue;

  FReportDataSet := TfrDBDataSet.Create(Self);
  FReportDataSet.Name := 'frGridDBDataSet1';
  FReportDataSet.DataSet := FDataSet;

  SetupColumns;

  FColumnDataSet := TfrUserDataSet.Create(Self);
  FColumnDataSet.Name := 'frGridUserDataSet1';
  FColumnDataSet.RangeEnd := reCount;
  FColumnDataSet.RangeEndCount := Length(FColumnsInfo);

  try
    FReportDataSet.DataSource := TDBGrid(DBGrid).DataSource;
    if FReport.Pages.Count=0 then
      FReport.Pages.add;
    Page := FReport.Pages[FReport.Pages.Count-1];

    with Page do
      ChangePaper(pgSize, Width, Height, FOrientation);

    b := FindBand(Page, btReportTitle);
    if b<>nil then begin
      if FShowCaption then
        ReplaceTemplate(Page, b, '<title>', FCaption);
    end;

    h := FindBand(Page, btPageHeader);
    if h<>nil then begin
      if FShowCaption then
        ReplaceTemplate(Page, h, '<title>', FCaption);
    end;

    if FShowCaption and (b=nil) and (h=nil) then
    begin
      b := TfrBandView(frCreateObject(gtBand, '', Page));
      b.SetBounds(10, 20, 1000, 25);
      b.BandType := btReportTitle;
//      Page.Objects.Add(b);
      v := frCreateObject(gtMemo, '', Page);
      v.SetBounds(20, 20, Page.PrnInfo.PgW - 40, 25);
      TfrMemoView(v).Alignment:=taCenter;
      TfrMemoView(v).Font.Assign(FTitleFont);
      v.Memo.Add(FCaption);
//      Page.Objects.Add(v);
    end;

    // if we have a template we need to be sure that bands on template
    // do not overlap with bands we are about to add, we need exactly
    // 40 pixels of free height space and 20 pixels width for cross band
    FindFreeSpace(Page, XPos, YPos);

    b := TfrBandView(frCreateObject(gtBand, '', Page));
    b.BandType := btMasterHeader;
    if self.fShowHdOnAllPage then
      b.Flags:=b.Flags+flBandRepeatHeader;
    b.SetBounds(XPos, YPos, 1000, 20);
    b.Flags:=b.Flags or flStretched;
//    Page.Objects.Add(b);

    v := frCreateObject(gtMemo, '', Page);
    v.SetBounds(XPos, YPos, 20, 20);
    TfrMemoView(v).Alignment:=taCenter;
    TfrMemoView(v).FillColor := clSilver;
    TfrMemoView(v).Font.Assign(FTitleFont);
    TfrMemoView(v).Frames:=frAllFrames;
    TfrMemoView(v).Layout:=tlTop;
    v.Memo.Add('[Header]');
//    Page.Objects.Add(v);

    YPos := YPos + 22;

    b := TfrBandView(frCreateObject(gtBand, '', Page));
    b.BandType := btMasterData;
    b.Dataset := FReportDataSet.Name;
    b.SetBounds(0, YPos, 1000, 18);
    b.Flags:=b.Flags or flStretched;
//    Page.Objects.Add(b);

    b := TfrBandView(frCreateObject(gtBand, '', Page));
    b.BandType := btCrossData;
    b.Dataset := FColumnDataSet.Name;
    b.SetBounds(XPos, 0, 20, 1000);
//    Page.Objects.Add(b);

    v := frCreateObject(gtMemo, '', Page);
    v.SetBounds(XPos, YPos, 20, 18);
    v.Memo.Add('[Cell]');
    V.Flags:=V.Flags or flStretched;
    TfrMemoView(v).Font.Assign(FFont);
    TfrMemoView(v).Frames:=frAllFrames;
    TfrMemoView(v).Layout:=tlTop;
//    Page.Objects.Add(v);

    FDataSet.DisableControls;
    BM:=FDataSet.GetBookmark;
    try
      if (FPrinterIndex <> -1) then
       begin
         FReport.ChangePrinter(Printer.PrinterIndex, FPrinterIndex);
         FReport.PrepareReport;
       end;

      if Assigned( OnFinalSetup ) then
        OnFinalSetup( Self, FReport, FColumnsInfo  );

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
  // ToDo: invert this assignment. Now SaveDesign is only assigned but not used.
  SaveDesign:=frDesigner;
end;

procedure TfrPrintGrid.OnEnterRect(Memo: TStringList; View: TfrView);
var
  C: TColumn;
  i: Integer;
begin
  i := FColumnDataset.RecNo;

  if (i<0) or (i>Length(FColumnsInfo)-1) then
    exit;

  C := TColumn(TDBGrid(DbGrid).Columns[FColumnsInfo[i].Column]);
  if (C<>nil)and(Memo.Count>0) then
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

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TFrPrintGrid,'Template',TFileNamePropertyEditor);
end.
