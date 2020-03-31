unit ResultDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ClipBrd, LCLType, LCLProc, ComCtrls, Menus, SynEdit,
  SynHighlighterPo, PoFamilies, PoFamilyLists, GraphStat, PoCheckerConsts,
  PoCheckerSettings, Types;

type

  { TResultDlgForm }

  TResultDlgForm = class(TForm)
    GraphStatBtn: TBitBtn;
    CopyMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    MemoPopupMenu: TPopupMenu;
    StatMemo: TSynEdit;
    ResultPageControl: TPageControl;
    CloseBtn: TBitBtn;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    FLog: TStringList;
    FStatLog: TStringList;
    FDupLog: TStringList;
    LogMemo: TSynEdit;
    GeneralTabSheet: TTabSheet;
    StatisticsTabSheet: TTabSheet;
    DuplicatesTabSheet: TTabSheet;
    DupMemo: TSynEdit;
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GraphStatBtnClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
  private
    PoHL: TSynPoSyn;
    FPoFamilyStats: TPoFamilyStats;
    FSettings: TPoCheckerSettings;
    procedure GetCurrentMemo(var CurrentMemo: TSynEdit);
    procedure LoadConfig;
    procedure SaveConfig;
  public
    // The following fields keep translation statistics calculated when tests were performed.
    // They will allow to avoid recalculation of these values in GraphStat form.
    FTotalTranslated: Integer;
    FTotalUntranslated: Integer;
    FTotalFuzzy: Integer;
    FTotalPercTranslated: Double;
    property Log: TStringList read FLog write FLog;
    property StatLog: TStringList read FStatLog write FStatLog;
    property DupLog: TStringList read FDupLog write FDupLog;
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats write FPoFamilyStats;
    property Settings: TPoCheckerSettings read FSettings write FSettings;
  end; 

implementation

{$R *.lfm}

{ TResultDlgForm }

procedure TResultDlgForm.FormCreate(Sender: TObject);
begin
  Caption := sResults;
  GeneralTabSheet.Caption := sGeneralInfo;
  StatisticsTabSheet.Caption := sTranslationStatistics;
  DuplicatesTabSheet.Caption := sDuplicateOriginalsTab;
  CopyMenuItem.Caption := sCopy;
  SaveAsMenuItem.Caption := sSaveAs;

  LogMemo.Lines.Clear;
  StatMemo.Lines.Clear;
  FLog := TStringList.Create;
  FStatLog := TStringList.Create;
  FDupLog := TStringList.Create;
  PoHL := TSynPoSyn.Create(Self);
  LogMemo.Highlighter := PoHL;
  GraphStatBtn.Caption := sShowStatGraph;
  FTotalTranslated := 0;
  FTotalUntranslated := 0;
  FTotalFuzzy := 0;
end;

procedure TResultDlgForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FLog.Clear;
  FStatLog.Clear;
  FDupLog.Clear;
end;

procedure TResultDlgForm.CopyMenuItemClick(Sender: TObject);
var
  CurMemo: TSynEdit;
begin
  GetCurrentMemo(CurMemo);
  if CurMemo <> nil then
    ClipBoard.AsText := CurMemo.Text;
end;

procedure TResultDlgForm.FormDestroy(Sender: TObject);
begin
  FLog.Free;
  FStatLog.Free;
  FDupLog.Free;
  SaveConfig;
end;

procedure TResultDlgForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CurMemo: TSynEdit;
begin
  GetCurrentMemo(CurMemo);
  if (Key = VK_Tab) and (Shift = []) and (Assigned(CurMemo) and CurMemo.Focused) then
  begin
    //Workaroud: cannot tab out of LogMemo/StatMemo
    GraphStatBtn.SetFocus;
    //debugln('Tab');
    Key := 0;
  end;
end;

procedure TResultDlgForm.FormShow(Sender: TObject);
begin
  LogMemo.Lines.Assign(FLog);
  StatMemo.Lines.Assign(FStatLog);
  DupMemo.Lines.Assign(FDupLog);
  GraphStatBtn.Visible := (PoFamilyStats <> nil) and (PoFamilyStats.Count > 0);
  LoadConfig;
  WindowState := Settings.ResultsFormWindowState;
end;

procedure TResultDlgForm.GraphStatBtnClick(Sender: TObject);
var
  mr: TModalResult;
begin
  GraphStatForm := TGraphStatForm.Create(nil);
  try
    GraphStatForm.PoFamilyStats := Self.PoFamilyStats;
    GraphStatForm.Settings := Self.Settings;

    if PoFamilyList.LangID <> lang_all then
    begin
      GraphStatForm.TranslatedLabel.Caption := Format(sTranslatedStringsTotal, [
        IntToStr(FTotalTranslated), FTotalPercTranslated]);
      GraphStatForm.UnTranslatedLabel.Caption := Format(sUntranslatedStringsTotal
        , [IntToStr(FTotalUntranslated)]);
      GraphStatForm.FuzzyLabel.Caption := Format(sFuzzyStringsTotal, [IntToStr(
        FTotalFuzzy)]);
    end
    else
    begin
      GraphStatForm.TranslatedLabel.Caption := sTranslatedStrings;
      GraphStatForm.UnTranslatedLabel.Caption := sUntranslatedStrings;
      GraphStatForm.FuzzyLabel.Caption := sFuzzyStrings;
    end;
    mr := GraphStatForm.ShowModal;
    if mr = mrOpenEditorFile then ModalResult := mr; // To inform pocheckermain
  finally
    FreeAndNil(GraphStatForm);
  end;
end;

procedure TResultDlgForm.SaveAsMenuItemClick(Sender: TObject);
var
  CurMemo: TSynEdit;
begin
  GetCurrentMemo(CurMemo);
  if (CurMemo <> nil) and (SaveDialog.Execute) then
  begin
    try
      CurMemo.Lines.SaveToFile(SaveDialog.FileName);
    except
      on E: EStreamError do MessageDlg('POChecker',Format(sSaveError,[SaveDialog.FileName]),mtError, [mbOk],0);
    end;
  end;
end;

procedure TResultDlgForm.GetCurrentMemo(var CurrentMemo: TSynEdit);
begin
  case ResultPageControl.PageIndex of
    0: CurrentMemo := LogMemo;
    1: CurrentMemo := StatMemo;
    2: CurrentMemo := DupMemo;
  else
    CurrentMemo := nil;
  end;
end;

procedure TResultDlgForm.LoadConfig;
var
  ARect: TRect;
begin
  if not Assigned(FSettings) then Exit;
  ARect := FSettings.ResultsFormGeometry;
  //debugln('TResultDlgForm.LoadConfig: ARect = ',dbgs(ARect));
  if not IsDefaultRect(ARect) and IsValidRect(ARect) then
  begin
    ARect := FitToRect(ARect, Screen.WorkAreaRect);
    BoundsRect := ARect;
  end;
  if Settings.DisableAntialiasing then
  begin
    LogMemo.Font.Quality := fqNonAntialiased;
    StatMemo.Font.Quality := fqNonAntialiased;
    DupMemo.Font.Quality := fqNonAntialiased;
  end
  else
  begin
    LogMemo.Font.Quality := fqDefault;
    StatMemo.Font.Quality := fqDefault;
    DupMemo.Font.Quality := fqDefault;
  end;
end;

procedure TResultDlgForm.SaveConfig;
begin
  //debugln('TResultDlgForm.SaveConfig: BoundsRect = ',dbgs(BoundsRect));
  if not Assigned(FSettings) then Exit;
  Settings.ResultsFormWindowState := WindowState;
  if (WindowState = wsNormal) then
    Settings.ResultsFormGeometry := BoundsRect
  else
    Settings.ResultsFormGeometry := Rect(RestoredLeft, RestoredTop, RestoredLeft + RestoredWidth, RestoredTop + RestoredHeight);
end;

end.

