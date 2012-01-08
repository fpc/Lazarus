unit ResultDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ClipBrd, LCLType, LCLProc, SynEdit, SynHighlighterPo;

type

  { TResultDlgForm }

  TResultDlgForm = class(TForm)
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
    procedure SaveBtnClick(Sender: TObject);
  private
    PoHL: TSynPoSyn;
    procedure SaveToFile;
  public
    property Log: TStringList read FLog write FLog;
  end; 

implementation

{$R *.lfm}

ResourceString
  sSaveError = 'Error saving file:' + LineEnding + '%s';
  sSaveCaption = 'Save to file';
  sCopyCaption = 'Copy to clipboard';


{ TResultDlgForm }

procedure TResultDlgForm.FormCreate(Sender: TObject);
begin
  LogMemo.Lines.Clear;
  LogMemo.Align := alClient;
  FLog := TStringList.Create;
  PoHL := TSynPoSyn.Create(Self);
  LogMemo.Highlighter := PoHL;
  SaveBtn.Caption := sSaveCaption;
  CopyBtn.Caption := sCopyCaption;
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
      MessageDlg('GPoCheck',Format(sSaveError,[SaveDialog.FileName]), mtError, [mbOk], 0);
    end;
  end;
end;

end.

