unit Test_ParentSizing;
(* Test sizing of containers that depend on their children
   - ScrollBox, depends on child by align alClient
*)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Controls, StdCtrls, ExtCtrls, fpcunit, testregistry;

type

  { TTestParentSizing }

  TTestParentSizing = class(TTestCase)
  private
    FTestForm: TForm;
    FTestScrollBox: TScrollBox;
  protected
    procedure CreateTestForm;
    procedure CreateTestScrollBox;
    procedure TearDown; override;
    procedure AssertNoScroll(AName: String; AScrollBar: TControlScrollBar);
    procedure AssertInRange(AName: String; AValue, AnExpMin, AnExpMax: Integer);
  published
    procedure ScrollBoxWithTabControl;
  end;

implementation

type

  { TTestScrollBox }

  TTestScrollBox = class(TScrollBox)
  protected
    FDoSetBoundsCount: integer;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TTestTabControl }

  TTestTabControl = class(TTabControl)
  protected
    FDoSetBoundsCount: integer;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

{ TTestScrollBox }

procedure TTestScrollBox.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inc(FDoSetBoundsCount);
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
end;

{ TTestTabControl }

procedure TTestTabControl.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inc(FDoSetBoundsCount);
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
end;

{ TTestParentSizing }

procedure TTestParentSizing.CreateTestForm;
begin
  FTestForm.Free;
  FTestForm := TForm.CreateNew(nil);
  FTestForm.SetBounds(10,10,410,410);
    //FTestForm.Show;
end;

procedure TTestParentSizing.CreateTestScrollBox;
begin
  FTestScrollBox := TTestScrollBox.Create(FTestForm);
  FTestScrollBox.Parent := FTestForm;
  FTestScrollBox.BorderStyle := bsNone; // Otherwise there is a widgetset dependent spacing
  FTestScrollBox.SetBounds(10,10,250,200);
  //FTestScrollBox.AutoScroll := True;
end;

procedure TTestParentSizing.TearDown;
begin
  inherited TearDown;
  FTestForm.Free;
  Application.ProcessMessages; // when running real WS
end;

procedure TTestParentSizing.AssertNoScroll(AName: String; AScrollBar: TControlScrollBar);
begin
  AssertTrue(AName, (AScrollBar.Range <= AScrollBar.Page) or (not AScrollBar.Visible));
end;

procedure TTestParentSizing.AssertInRange(AName: String; AValue, AnExpMin, AnExpMax: Integer);
begin
  AssertTrue(Format('%s in %d Range %d..%d', [AName, AValue, AnExpMin, AnExpMax]), (AValue >= AnExpMin) and (AValue <= AnExpMax));
end;

procedure TTestParentSizing.ScrollBoxWithTabControl;
var
  c: TControl;
  t: TTestTabControl absolute c;
  j, i_tabs, i_pos, i_tpos, i_align, i_class: Integer;
begin
  CreateTestForm;
  CreateTestScrollBox;
  FTestForm.Show;

  for i_tabs := 0 to 3 do
  for i_pos := 0 to 3 do
  for i_tpos := 0 to 3 do
  for i_align := 0 to 4 do
  for i_class := 0 to 2 do begin
    TTestScrollBox(FTestScrollBox).FDoSetBoundsCount := 0;
    // Alternative classes don't have tabs
    if (i_class > 0) and ((i_tpos>0) or (i_tabs > 0)) then Continue;
    // Don't repeat all test for right/bottom tabs
    if (i_tpos >=2) {right/bottom} and ( (i_tabs>1) or (i_pos>1) ) then continue;

    case i_class of
      0: begin
          t := TTestTabControl.Create(FTestForm);

          for j := 1 to i_tabs*2-1 do t.Tabs.Add(IntToStr(j)); // test different amount of tabs: 0,1,3,5

          case i_tpos of
            0: t.TabPosition := tpTop;
            1: t.TabPosition := tpLeft;
            2: t.TabPosition := tpRight;
            3: t.TabPosition := tpBottom;
          end;
        end;
      1: c := TPanel.Create(FTestForm);
      2: c := TLabel.Create(FTestForm);
    end;
    c.AutoSize := False;

    case i_pos of
      0: begin c.Top :=  20;     c.Left :=  20; end;
      1: begin c.Top := -20;     c.Left := -20; end;
      2: begin c.Top :=   0;     c.Left :=   0; end;
      3: begin c.Top := 190;     c.Left := 240; end;
    end;
    c.Parent := FTestScrollBox;
    case i_align of
      1,3: t.Height := 200;
      2,4: t.Width  := 250;
    end;

    if i_class = 0 then t.FDoSetBoundsCount := 0;
    case i_align of
      0: c.Align := alClient;
      1: c.Align := alTop;
      2: c.Align := alLeft;
      3: c.Align := alBottom;
      4: c.Align := alRight;
    end;

    AssertNoScroll('', FTestScrollBox.HorzScrollBar);
    AssertNoScroll('', FTestScrollBox.VertScrollBar);
    AssertEquals('Left',     0, c.Left);
    AssertEquals('Top',      0, c.Top);
    AssertEquals('Width',  250, c.Width);
    AssertEquals('Height', 200, c.Height);

    AssertEquals('0 call to change bounds', 0, TTestScrollBox(FTestScrollBox).FDoSetBoundsCount);

    if i_align = 0 then begin
      if (i_class = 0) and (i_pos in [0,2]) then // there was no initial scrollbar
        AssertEquals('1 call to change bounds', 1, t.FDoSetBoundsCount);

      FTestScrollBox.SetBounds(10,10,350,150);
      AssertNoScroll('', FTestScrollBox.HorzScrollBar);
      AssertNoScroll('', FTestScrollBox.VertScrollBar);
      AssertEquals('Left',     0, c.Left);
      AssertEquals('Top',      0, c.Top);
      AssertEquals('Width',  350, c.Width);
      AssertEquals('Height', 150, c.Height);

      FTestScrollBox.SetBounds(10,10,220,160);
      AssertNoScroll('', FTestScrollBox.HorzScrollBar);
      AssertNoScroll('', FTestScrollBox.VertScrollBar);
      AssertEquals('Left',     0, c.Left);
      AssertEquals('Top',      0, c.Top);
      AssertEquals('Width',  220, c.Width);
      AssertEquals('Height', 160, c.Height);

      FTestScrollBox.SetBounds(10,10,250,200);
    end;

    c.Free;
  end;


  (* TODO: Scrollbox with size of 100 => tab control currently forces a minsize of 200
     TODO: borderstyle / borderwidth
  *)
end;

initialization
  RegisterTest(TTestParentSizing);

end.

