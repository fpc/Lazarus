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
  private

  public
    procedure SetControls(AEnablePreview, AEnableMargins, AEnablePapers,
      AEnableOrientation: boolean);
  end; 

var
  dlgPageSetup: TDlgPageSetup;

implementation

{$R udlgpagesetup.lfm}

{ TDlgPageSetup }

procedure TDlgPageSetup.SetControls(AEnablePreview, AEnableMargins, AEnablePapers,
  AEnableOrientation: boolean);
begin
  frmPageSetup.Initialize(AEnablePreview, AEnableMargins, AEnablePapers, AEnableOrientation);
end;

end.

