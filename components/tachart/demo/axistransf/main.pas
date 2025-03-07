unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls,
  frmLinear, frmIndependent, frmLogarithm, frmNormDistr, frmUser;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsLinear: TTabSheet;
    tsCumulNormDistr: TTabSheet;
    tsIndependent: TTabSheet;
    tsUser: TTabSheet;
    tsLog: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FLinearFrame: TLinearFrame;
    FIndependentScaleFrame: TIndependentScaleFrame;
    FLogarithmFrame: TLogarithmFrame;
    FNormDistrFrame: TNormDistrFrame;
    FUserFrame: TUserFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  x: Double;
begin
  FLinearFrame := TLinearFrame.Create(Self);
  FLinearFrame.Parent := tsLinear;
  FLinearFrame.Align := alClient;

  FIndependentScaleFrame := TIndependentScaleFrame.Create(Self);
  FIndependentScaleFrame.Parent := tsIndependent;
  FIndependentScaleFrame.Align := alClient;

  FLogarithmFrame := TLogarithmFrame.Create(Self);
  FLogarithmFrame.Parent := tsLog;
  FLogarithmFrame.Align := alClient;

  FNormDistrFrame := TNormDistrFrame.Create(Self);
  FNormDistrFrame.Parent := tsCumulNormDistr;
  FNormDistrFrame.Align := alClient;

  FUserFrame := TUserFrame.Create(Self);
  FUserFrame.Parent := tsUser;
  FUserFrame.Align := alClient;
end;

end.

