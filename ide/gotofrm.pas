unit GotoFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LazarusIDEStrConsts, LCLType;

type

  { TfrmGoto }

  TfrmGoto = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    btnOK: TBitbtn;
    btnCancel: TBitBtn;
    procedure Edit1KeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoShow; override;
  end;

implementation

{ TfrmGoto }

constructor TfrmGoto.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := lisMenuGotoLine;
  Label1.Caption := lisUEGotoLine;
  Edit1.Caption := '';
end;

procedure TfrmGoto.DoShow;
begin
  Edit1.SelectAll;
  Edit1.SetFocus;
  inherited DoShow;
end;

procedure TfrmGoto.Edit1KeyDown(Sender: TObject; var Key:Word;
   Shift:TShiftState);
begin
  if (Key=VK_RETURN) then
  begin
    ModalResult:=mrOk;
    Key := 0;
  end;
  if (Key=VK_ESCAPE) then
  begin
    ModalResult:=mrCancel;
    Key := 0;
  end;
end;

initialization
  {$I gotofrm.lrs}

end.

