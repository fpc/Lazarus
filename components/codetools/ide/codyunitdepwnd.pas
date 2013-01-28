unit CodyUnitDepWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons;

type

  { TUnitDependenciesDialog }

  TUnitDependenciesDialog = class(TForm)
    BtnPanel: TPanel;
    CloseBitBtn: TBitBtn;
    Timer1: TTimer;
    procedure CloseBitBtnClick(Sender: TObject);
  private
  public
  end;

var
  UnitDependenciesDialog: TUnitDependenciesDialog;

procedure ShowUnitDependenciesDialog(Sender: TObject);

implementation

procedure ShowUnitDependenciesDialog(Sender: TObject);
var
  Dlg: TUnitDependenciesDialog;
begin
  Dlg:=TUnitDependenciesDialog.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{ TUnitDependenciesDialog }

procedure TUnitDependenciesDialog.CloseBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

{$R *.lfm}

end.

