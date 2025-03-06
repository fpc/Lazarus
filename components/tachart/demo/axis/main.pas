unit Main;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, Controls, Forms, ComCtrls,
  frmCustomMarks, frmAxisGroups, frmSubmarks, frmDateTime,
  frmIntervals, frmPosition, frmRotated;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageControl: TPageControl;
    tsRotated: TTabSheet;
    tsPosition: TTabSheet;
    tsIntervals: TTabSheet;
    tsAxisGroup: TTabSheet;
    tsCustomMarks: TTabSheet;
    tsDateTime: TTabSheet;
    tsSubmarks: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FCustomMarksFrame: TCustomMarksFrame;
    FAxisGroupsFrame: TAxisGroupsFrame;
    FSubmarksFrame: TSubMarksFrame;
    FDateTimeFrame: TDateTimeFrame;
    FIntervalsFrame: TIntervalsFrame;
    FPositionFrame: TPositionFrame;
    FRotatedSeriesFrame: TRotatedSeriesFrame;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCustomMarksFrame := TCustomMarksFrame.Create(Self);
  FCustomMarksFrame.Parent := tsCustomMarks;
  FCustomMarksFrame.Align := alClient;

  FAxisGroupsFrame := TAxisGroupsFrame.Create(Self);
  FAxisGroupsFrame.Parent := tsAxisGroup;
  FAxisGroupsFrame.Align := alClient;

  FSubmarksFrame := TSubmarksFrame.Create(Self);
  FSubmarksFrame.Parent := tsSubMarks;
  FSubmarksFrame.Align := alClient;

  FDateTimeFrame := TDateTimeFrame.Create(Self);
  FDateTimeFrame.Parent := tsDateTime;
  FDateTimeFrame.Align := alClient;

  FIntervalsFrame := TIntervalsFrame.Create(Self);
  FIntervalsFrame.Parent := tsIntervals;
  FIntervalsFrame.Align := alClient;

  FPositionFrame := TPositionFrame.Create(self);
  FPositionFrame.Parent := tsPosition;
  FPositionFrame.Align := alClient;

  FRotatedSeriesFrame := TRotatedSeriesFrame.Create(self);
  FRotatedSeriesFrame.Parent := tsRotated;
  FRotatedSeriesFrame.Align := alClient;
end;

end.

