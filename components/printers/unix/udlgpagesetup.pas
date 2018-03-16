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
  // LCL
  Forms, ExtCtrls, StdCtrls,
  // Printers
  framePageSetup;

type

  { TDlgPageSetup }

  TDlgPageSetup = class(TForm)
    btnCancel: TButton;
    btnPrinter: TButton;
    btnOk: TButton;
    frmPageSetup: TframePageSetup;
    PanelButtons: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end; 

var
  dlgPageSetup: TDlgPageSetup;

implementation

{$R udlgpagesetup.lfm}

{ TDlgPageSetup }

procedure TDlgPageSetup.FormCreate(Sender: TObject);
begin
  frmPageSetup.Initialize(psmFull);
end;

end.

