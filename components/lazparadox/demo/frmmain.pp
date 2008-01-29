unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, DbCtrls, DBGrids, db, paradox, ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    DSPDX: TDatasource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    FEPX: TFileNameEdit;
    Label1: TLabel;
    PDX: TParadox;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation


{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  PDX.Close;
  PDX.FileName:=FEPX.FileName;
  PDX.Open;
end;


initialization
  {$I frmmain.lrs}

end.

