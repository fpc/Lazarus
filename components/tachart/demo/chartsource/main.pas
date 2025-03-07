unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms, Controls, ExtCtrls, ComCtrls,
  frmBasic, frmStatistics, frmDerivative, frmSorted;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    Splitter1: TSplitter;
    tsSorted: TTabSheet;
    tsDerivative: TTabSheet;
    tsStatistics: TTabSheet;
    tsBasic: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FBasicFrame: TBasicFrame;
    FStatisticsFrame: TStatisticsFrame;
    FDerivativeFrame: TDerivativeFrame;
    FSortedFrame: TSortedFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;

  FBasicFrame := TBasicFrame.Create(self);
  FBasicFrame.Parent := tsBasic;
  FBasicFrame.Align := alClient;

  FStatisticsFrame := TStatisticsFrame.Create(self);
  FStatisticsFrame.Parent := tsStatistics;
  FStatisticsFrame.Align := alClient;

  FDerivativeFrame := TDerivativeFrame.Create(self);
  FDerivativeFrame.Parent := tsDerivative;
  FDerivativeFrame.Align := alClient;

  FSortedFrame := TSortedFrame.Create(self);
  FSortedFrame.Parent := tsSorted;
  FSortedFrame.Align := alClient;
end;

end.

