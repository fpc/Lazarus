unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, ComCtrls,
  frmPoints, frmBars;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsPoints: TTabSheet;
    tsBars: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FPointsFrame: TPointsFrame;
    FBarsFrame: TBarsFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RandSeed := 675402;

  FPointsFrame := TPointsFrame.Create(Self);
  FPointsFrame.Parent := tsPoints;
  FPointsFrame.Align := alClient;

  FBarsFrame := TBarsFrame.Create(Self);
  FBarsFrame.Parent := tsBars;
  FBarsFrame.Align := alClient;
end;

end.

