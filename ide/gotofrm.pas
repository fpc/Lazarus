unit GotoFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LazarusIDEStrConsts, LCLType, ButtonPanel;

type

  { TfrmGoto }

  TfrmGoto = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Edit1: TEdit;
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

initialization
  {$I gotofrm.lrs}

end.

