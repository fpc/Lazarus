unit UfrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms,
  Controls, Graphics, Dialogs, ComCtrls, ActnList, StdActns, LCLType,
  uFrameSearch;

type

  { TForm1 }

  TForm1 = class(TForm)
    actSearch   :TAction;
    ActionList1 :TActionList;
    actExit     :TFileExit;
    ActLoad     :TFileOpen;
    actSaveAs   :TFileSaveAs;
    frSearch: TFrame1;
    ImageList1  :TImageList;
    StatusBar1  :TStatusBar;
    SynEdit1    :TSynEdit;
    ToolBar1    :TToolBar;
    ToolButton1 :TToolButton;
    ToolButton2 :TToolButton;
    ToolButton3 :TToolButton;
    ToolButton4 :TToolButton;
    ToolButton5 :TToolButton;
    ToolButton6 :TToolButton;
    procedure ActLoadAccept(Sender :TObject);
    procedure actSaveAsAccept(Sender :TObject);
    procedure actSearchExecute(Sender :TObject);
    procedure actSearchUpdate(Sender :TObject);
    procedure SpeedButton4Click(Sender :TObject);
    procedure SynEdit1ReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: integer;
      var ReplaceAction: TSynReplaceAction);
  private
    procedure CloseFrame(Sender:TObject);
    procedure AfterSearch(Sender:TObject; cnt: Integer);
  public
    constructor Create(aOwner :TComponent); override;
  end;

var
  Form1 : TForm1;

implementation

{$R *.lfm}
const
  cDefaultFiles = 'Known Files (*.txt,*.pas,*.dpr,*.dpk,*.inc,*.C,*.cpp)|*.txt,*.pas;*.dpr;*.dpk;*.inc;*.C;*.cpp';
{ TForm1 }

procedure TForm1.actSearchExecute(Sender :TObject);
begin
  frSearch.Visible := True;
  frSearch.EditSearch.SetFocus;
end;

procedure TForm1.actSearchUpdate(Sender :TObject);
begin
  actSearch.Enabled := SynEdit1.Lines.Count > 0;
end;

procedure TForm1.SpeedButton4Click(Sender :TObject);
begin
  frSearch.Visible := False;
end;

procedure TForm1.SynEdit1ReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction
  );
var
  a: TModalResult;
  p: TPoint;
begin
  p := SynEdit1.RowColumnToPixels(Point(Column,Line));
  a:=MessageDlgPos('Replace "'+ASearch+'" with "'+AReplace+'"?',mtconfirmation,
            [mbYes,mbYesToAll,mbNo,mbCancel],0,Left+50+p.x,Top+100+p.y);

  case a of
    mrYes:ReplaceAction:=raReplace;
    mrNo :ReplaceAction:=raSkip;
    mrAll,mrYesToAll:ReplaceAction:=raReplaceAll;
  else
    ReplaceAction:=raCancel;
  end;
end;

procedure TForm1.CloseFrame(Sender :TObject);
begin
  frSearch.Hide;
  SynEdit1.SetFocus;
end;

procedure TForm1.AfterSearch(Sender: TObject; cnt: Integer);
begin
  StatusBar1.Panels[0].Text := 'Last search : "'+frSearch.EditSearch.Text+'" matched: '+inttostr(cnt);
end;

procedure TForm1.ActLoadAccept(Sender :TObject);
begin
  SynEdit1.Lines.LoadFromFile(ActLoad.Dialog.FileName);
end;

procedure TForm1.actSaveAsAccept(Sender :TObject);
begin
  SynEdit1.Lines.SaveToFile(actSaveAs.Dialog.FileName);
end;

constructor TForm1.Create(aOwner :TComponent);
begin
  inherited Create(aOwner);
  ActLoad.Dialog.Filter   := cDefaultFiles;
  actSaveAs.Dialog.Filter := cDefaultFiles;
  frSearch.Editor         := SynEdit1;
  frSearch.OnCloseFrame   := @CloseFrame;
  frSearch.OnAfterSearch  := @AfterSearch;
end;

end.



