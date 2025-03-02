unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, ComCtrls,
  uMachineStateFrame, uSimpleGanttFrame, uGanttFrame;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl1: TPageControl;
    pgMachineStateChart: TTabSheet;
    pgGanttChart: TTabSheet;
    pgSimpleGanttChart: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FMachineStateFrame: TMachineStateFrame;
    FSimpleGanttFrame: TSimpleGanttFrame;
    FGanttFrame: TGanttFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMachineStateFrame := TMachineStateFrame.Create(Self);
  FMachineStateFrame.Parent := pgMachinestateChart;
  FMachineStateFrame.Align := alClient;

  FSimpleGanttFrame := TSimpleGanttFrame.Create(self);
  FSimpleGanttFrame.Parent := pgSimpleGanttChart;
  FSimpleGanttFrame.Align := alClient;

  FGanttFrame := TGanttFrame.Create(self);
  FGanttFrame.Parent := pgGanttChart;
  FGanttFrame.Align := alClient;
end;

end.

