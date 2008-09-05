unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  PairSplitter, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button2: TButton;
    Button3: TButton;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  with PairSplitter1 do
  begin
    Sides[0].Color := clRed;
    Sides[1].Color := clGreen;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  PairSplitterSide1.Width := PairSplitterSide1.Width - 25;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
var
  SplitterWidth : integer;
begin
  if (ParamStr(1)='--runtest') then begin
    SplitterWidth := PairSplitterSide2.Width -
      (PairSplitterSide1.Left + PairSplitterSide1.Width);
    writeln(format('Side 1: %d, Splitter + Side 2: %d',
              [PairSplitterSide1.Width, SplitterWidth + PairSplitterSide2.Width]));
    // splitter is working, so descreasing width of Side1 should increase Side2
    Button2Click(nil);
    writeln(format('Side 1: %d, Splitter + Side 2: %d',
              [PairSplitterSide1.Width, SplitterWidth + PairSplitterSide2.Width]));
    // increase width again
    Button3Click(nil);
    writeln(format('Side 1: %d, Splitter + Side 2: %d',
              [PairSplitterSide1.Width, SplitterWidth + PairSplitterSide2.Width]));
    Close;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PairSplitterSide1.Width := PairSplitterSide1.Width + 25;
end;

initialization
  {$I unit1.lrs}

end.

