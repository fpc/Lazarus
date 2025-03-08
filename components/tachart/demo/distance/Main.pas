unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Graphics, Controls, ExtCtrls, ComCtrls,
  frmMainDemo, frmFitDemo;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsMain: TTabSheet;
    tsFit: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FMainDemoFrame: TMainDemoFrame;
    FFitDemoFrame: TFitDemoFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMainDemoFrame := TMainDemoFrame.Create(Self);
  FMainDemoFrame.Parent := tsMain;
  FMainDemoFrame.Align := alClient;

  FFitDemoFrame := TFitDemoFrame.Create(Self);
  FFitDemoFrame.Parent := tsFit;
  FFitDemoFrame.Align := alClient;
end;

end.

