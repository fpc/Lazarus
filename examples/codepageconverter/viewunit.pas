unit viewunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SynHighlighterPas, SynMemo, Menus;

type
  TViewForm = class(TForm)
    FontDialog1: TFontDialog;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    SynMemo1: TSynMemo;
    SynPasSyn1: TSynPasSyn;
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ViewForm: TViewForm;

implementation

{ TViewForm }

procedure TViewForm.MenuItem1Click(Sender: TObject);
begin
  if FontDialog1.Execute then SynMemo1.Font:=FontDialog1.Font;
end;

initialization
  {$I viewunit.lrs}

end.

