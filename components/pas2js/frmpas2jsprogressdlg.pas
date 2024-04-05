unit FrmPas2jsProgressDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  StrPas2JSDesign;

type

  { TPas2jsProgressDialog }

  TPas2jsProgressDialog = class(TForm)
    CancelButton: TButton;
    NoteLabel: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
  end;

var
  Pas2jsProgressDialog: TPas2jsProgressDialog;

implementation

{$R *.lfm}

{ TPas2jsProgressDialog }

procedure TPas2jsProgressDialog.FormCreate(Sender: TObject);
begin
  CancelButton.Caption:='Cancel';
  Pas2jsProgressDialog:=Self;
end;

procedure TPas2jsProgressDialog.FormDestroy(Sender: TObject);
begin
  if Pas2jsProgressDialog=Self then
    Pas2jsProgressDialog:=nil;
end;

end.

