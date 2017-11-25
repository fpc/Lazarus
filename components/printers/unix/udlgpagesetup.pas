{
 *****************************************************************************
  This file is part of the Printer4Lazarus package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
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

{$R udlgpagesetup.lfm}

{ TdlgPageSetup }

procedure TdlgPageSetup.FormCreate(Sender: TObject);
begin
  frmPageSetup.Initialize(psmFull);
end;

end.

