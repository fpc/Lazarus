unit udlgpagesetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  framepagesetup, ExtCtrls, StdCtrls;

type

  { TdlgPageSetup }

  TdlgPageSetup = class(TForm)
    btnCancel1: TButton;
    btnPrinter: TButton;
    btnOk: TButton;
    frmPageSetup: TframePageSetup;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dlgPageSetup: TdlgPageSetup;

implementation

{ TdlgPageSetup }

procedure TdlgPageSetup.FormCreate(Sender: TObject);
begin
  frmPageSetup.Initialize(psmFull);
end;

initialization
  {$I udlgpagesetup.lrs}

end.

