unit main; 

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, ComCtrls,
  frmPie, frmPolar;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsPolar: TTabSheet;
    tsPie: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    PieFrame: TPieFrame;
    PolarFrame: TPolarFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PieFrame := TPieFrame.Create(Self);
  PieFrame.Parent := tsPie;
  PieFrame.Align := alClient;

  PolarFrame := TPolarFrame.Create(Self);
  PolarFrame.Parent := tsPolar;
  PolarFrame.Align := alClient;
end;

end.

