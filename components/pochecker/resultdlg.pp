unit ResultDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ClipBrd, LCLType, LCLProc, SynEdit, SynHighlighterPo,
  PoFamilies, GraphStat, PoCheckerConsts;

type

  { TResultDlgForm }

  TResultDlgForm = class(TForm)
    GraphStatBtn: TBitBtn;
    CopyBtn: TBitBtn;
    SaveBtn: TBitBtn;
    CloseBtn: TBitBtn;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    FLog: TStringList;
    LogMemo: TSynEdit;
    procedure CopyBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GraphStatBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    PoHL: TSynPoSyn;
    FPoFamilyStats: TPoFamilyStats;
    procedure SaveToFile;
  public
    property Log: TStringList read FLog write FLog;
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats write FPoFamilyStats;
  end; 

implementation

{$R *.lfm}

{ TResultDlgForm }

procedure TResultDlgForm.FormCreate(Sender: TObject);
begin
  Caption := sResults;
  LogMemo.Lines.Clear;
  LogMemo.Align := alClient;
  FLog := TStringList.Create;
  PoHL := TSynPoSyn.Create(Self);
  LogMemo.Highlighter := PoHL;
  SaveBtn.Caption := sSaveCaption;
  CopyBtn.Caption := sCopyCaption;
  GraphStatBtn.Caption := sShowStatGraph;
end;

procedure TResultDlgForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FLog.Clear;
end;

procedure TResultDlgForm.CopyBtnClick(Sender: TObject);
begin
  ClipBoard.AsText := LogMemo.Text;
end;

procedure TResultDlgForm.FormDestroy(Sender: TObject);
begin
  FLog.Free;
end;

procedure TResultDlgForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Tab) and (Shift = []) and LogMemo.Focused then
  begin
    //Workaroud: cannot tab out of LogMemo
    CopyBtn.SetFocus;
    //debugln('Tab');
    Key := 0;
  end;
end;

procedure TResultDlgForm.FormShow(Sender: TObject);
begin
  LogMemo.Lines.Assign(FLog);
  GraphStatBtn.Visible := (PoFamilyStats <> nil) and (PoFamilyStats.Count > 0);
end;

procedure TResultDlgForm.GraphStatBtnClick(Sender: TObject);
begin
  GraphStatForm := TGraphStatForm.Create(nil);
  GraphStatForm.PoFamilyStats := Self.PoFamilyStats;
  GraphStatForm.ShowModal;
  FreeAndNil(GraphStatForm);
end;

procedure TResultDlgForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    try
      LogMemo.Lines.SaveToFile(SaveDialog.FileName);
    except
      on E: EStreamError do MessageDlg('Po-checker',Format(sSaveError,[SaveDialog.FileName]),mtError, [mbOk],0);
    end;
  end;
end;

procedure TResultDlgForm.SaveToFile;
begin
  if SaveDialog.Execute then
  begin
    try
      LogMemo.Lines.SaveToFile(SaveDialog.FileName);
    except
      MessageDlg('Po-checker',Format(sSaveError,[SaveDialog.FileName]), mtError, [mbOk], 0);
    end;
  end;
end;

end.

