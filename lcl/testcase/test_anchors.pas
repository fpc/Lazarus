unit Test_Anchors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, TestBase, Controls, Forms, testregistry;

type

  { TTestAnchors
    When a control uses anchors, then it remembers its distances to the parent,
    even if its actual size is (temporarily) affected by align, constraint,
    AnchorSide, ...
    - The distances are restored when the (tmp) reason is removed.
    - The distances follow resizes of the parent.
  }

  TTestAnchors = class(TTestBase)
  private
  protected
    procedure TearDown; override;
  published
    (* Change size of Parent (by various means: direct, align, constraint, ...)
       Expect an anchored child (without AnchorSide, but for any combination of anchors)
       to keep its distances to the parent on each anchored side.
     *)
    procedure AnchorsFollowParentSize;


    (* Constrain an anchored (no AnchorSide) child.
       Sides with anchor keep or restore their distances
    *)
    procedure AnchorsAndConstraint;


    (* Set and remove an anchor-side control to an anchored child.
       Check pos/size after
    *)
    //procedure AnchorsAndSideControl;



    (* Toggling Autosize on/off resets the distances
       Changing height/width of an AutoSizing parent by
       - changing any child's height/width
       - changing anchor of a child to add/remove it from the parents size calculation
       => and other children in that parent should move (and move back)
          according to their distances
       - Also include bevel/border
    *)
    //procedure AnchorsAndParentAutosize;


    (* TODO:
       - after loading from lfm => form resizes before show
       - scrollbox as parent, changing scrollbar range
    *)
  end;

implementation

{ TTestAnchors }

procedure TTestAnchors.TearDown;
begin
  inherited TearDown;
  FTestForm.Free;
  Application.ProcessMessages; // when running real WS
end;

procedure TTestAnchors.AnchorsFollowParentSize;

  function GetExpRect(AnOrigExp, AnOrigParent, ANewParent: TRect; CurAnch: TAnchors): TRect;
  const akH = [akLeft,akRight]; akV = [akTop,akBottom];
  var
    m, w, h: LongInt;
  begin
    Result := AnOrigExp;
    w := ANewParent.Width - AnOrigParent.Width;
    //if      CurAnch * akH = [akLeft]         then Result.Left  := Result.Left +
         if CurAnch * akH = [akRight]        then Result.Offset(w, 0)
    else if CurAnch * akH = [akLeft,akRight] then Result.Width  := Result.Width  + w
    else if CurAnch * akH = []               then begin
      m := AnOrigExp.Left + AnOrigExp.Width div 2;
      if (AnOrigParent.Width = 0) or (ANewParent.Width = 0) then Result.Offset(-m, 0) // center at 0
      else
        Result.Offset(
          m * ANewParent.Width div AnOrigParent.Width - m,
         0
        );
      end;
    Result.Width := max(0, Result.Width);

    h := ANewParent.Height - AnOrigParent.Height;
    //if      CurAnch * akH = [akTop]         then Result.Top  := Result.Top +
         if CurAnch * akV = [akBottom]        then Result.Offset(0, h)
    else if CurAnch * akV = [akTop,akBottom]  then Result.Height := Result.Height  + h
    else if CurAnch * akV = []                then begin
      m := AnOrigExp.Top + AnOrigExp.Height div 2;
      if (AnOrigParent.Height = 0) or (ANewParent.Height = 0) then Result.Offset(0, -m) // center at 0
      else
        Result.Offset(
          0,
          m * ANewParent.Height div AnOrigParent.Height - m
        );
      end;
    Result.Height := max(0, Result.Height);
  end;

var
  ATestPanel: TTestPanel;
  ATestLabel: TTestLabel;
  ChildOrigBounds, ParentOrigBounds: TRect;
  IdxVisible: Boolean;
  Anch: TAnchors;

  procedure Setup(P, C: TRect; A: TAnchors);
  begin
    CreateTestControl(ATestPanel, TTestPanel, P, [cfFreeOld]);
    CreateTestControl(ATestLabel, TTestLabel, C, ATestPanel, [cfNoAutoSize]);
    ATestLabel.Anchors := A;
    AssertBounds('Label width at begin ', ATestLabel, C);
  end;

  procedure MaybeReset(AResetIdx: integer; out ANewChildExp: TRect; AnCurExpBounds: TRect);
  begin
    if AResetIdx = 0 then begin
      ANewChildExp := AnCurExpBounds;
      exit; // Do not reset
    end;

    ATestLabel.SetBounds(2,2,1,1); // change bounds to diff value
    case AResetIdx of
      1: begin ANewChildExp := AnCurExpBounds;  ATestLabel.BoundsRect := ANewChildExp; end;
      2: begin ANewChildExp := ChildOrigBounds; ATestLabel.BoundsRect := ANewChildExp; end;
      3: begin ANewChildExp := ChildOrigBounds; Setup(ParentOrigBounds, ANewChildExp, Anch); end;
    end;
  end;

  procedure BeforeChange;
  begin
    ResetSetBoundsCounts([ATestPanel, ATestLabel]);
    if IdxVisible then ATestLabel.Visible := False;
  end;

  procedure BeforeCheckExp;
  begin
    if IdxVisible then ATestLabel.Visible := True;
  end;

var
  IdxAnch, IdxReset: Integer;
  IdxAlign: TAlign;
  ExpBnd, ExpBnd2, ExpBndStore: TRect;
begin
  CreateTestForm(10,10,720,620);
  FTestForm.Show;
  ATestPanel := nil;

  for IdxAnch := 0 to 15 do
  for IdxVisible := False to True do
  begin
    Anch := ToAnchors(IdxAnch);

    ParentOrigBounds   := Bounds(20,10, 400,300);
    ChildOrigBounds    := Bounds(40,20, 240,200);
    Setup(ParentOrigBounds, ChildOrigBounds, Anch);

    (* ***** Change Width only ***** *)
    BeforeChange;
    ATestPanel.Width := 420;
    BeforeCheckExp;
    ExpBnd := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after width', ATestLabel, ExpBnd, [0,1]); // SetBoundCount can be 0 or 1


    (* ***** Change Height only ***** *)
    BeforeChange;
    ATestPanel.Height := 330;
    BeforeCheckExp;
    ExpBnd := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after height', ATestLabel, ExpBnd, [0,1]);


    (* ***** Change Width and Height / SetBounds ***** *)
    BeforeChange;
    ATestPanel.SetBounds(20,10, 440, 280);
    BeforeCheckExp;
    ExpBnd := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after bounds', ATestLabel, ExpBnd, [0,1]);


    (* ***** Change Width and Height to zero and back ***** *)
    BeforeChange;
    ATestPanel.SetBounds(20,10, 0, 0);
    BeforeCheckExp;
    ExpBnd2 := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after bounds', ATestLabel, ExpBnd2, [0,1]);
    // restore values from previous
    BeforeChange;
    ATestPanel.SetBounds(20,10, 440, 280);
    BeforeCheckExp;
    AssertBounds('Label width after bounds', ATestLabel, ExpBnd, [0,1]);


    (* ***** Check AutoSize is ignored ***** *)
    if Anch = [akLeft, akRight, akTop, akBottom] then begin
      ResetSetBoundsCounts([ATestPanel, ATestLabel]);
      ATestLabel.AutoSize := True;
      AssertBounds('Label width after bounds', ATestLabel, ExpBnd, 0);
      ATestPanel.SetBounds(20,10, 1, 1);
      AssertBounds('Label width after bounds', ATestLabel, 40,20,0,0, 1);
      // restore values from previous
      ResetSetBoundsCounts([ATestPanel, ATestLabel]);
      ATestPanel.SetBounds(20,10, 440, 280);
      ATestLabel.AutoSize := False;
      AssertBounds('Label width after bounds', ATestLabel, ExpBnd, 1);
    end;


    (* ***** Change Position / SetBounds ***** *)
    BeforeChange;
    ATestPanel.Top := 30;
    ATestPanel.Left := 30;
    BeforeCheckExp;
    // ExpBnd should not be changed
    AssertBounds('Label width after pos', ATestLabel, ExpBnd, 0);


    (* ***** Change Width and Height / DisableAutoSizing ***** *)
    BeforeChange;
    ATestPanel.DisableAutoSizing;
    ATestPanel.Width  := 380;
    ATestPanel.Height := 340;
    ExpBnd := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    ATestPanel.EnableAutoSizing;
    BeforeCheckExp;
    AssertBounds('Label width after width/height', ATestLabel, ExpBnd, [0,1]);



    (* ***** Change Constraints / DisableAutoSizing ***** *)
    // keep above ExpBnd;
    BeforeChange;
    ATestPanel.DisableAutoSizing;
    ATestPanel.Constraints.MaxWidth  := 190;
    ATestPanel.Constraints.MaxHeight := 170;
    ATestPanel.EnableAutoSizing;
    ExpBnd2 := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    BeforeCheckExp;
    AssertBounds('Label width after width/height', ATestLabel, ExpBnd2, [0,1]);
    // remove constraints
    ResetSetBoundsCounts([ATestPanel, ATestLabel]);
    ATestPanel.Constraints.MaxWidth  := 0;
    ATestPanel.Constraints.MaxHeight := 0;
    AssertBounds('Label width after width/height', ATestLabel, ExpBnd2, 0);


    (* ***** Change Constraints ***** *)
    // re-use above ExpBnd;
    BeforeChange;
    ATestPanel.Constraints.MinWidth  := 380;
    ATestPanel.Constraints.MinHeight := 340;
    BeforeCheckExp;
    AssertBounds('Label width after width/height', ATestLabel, ExpBnd, [0,2]);
    // remove constraints
    ResetSetBoundsCounts([ATestPanel, ATestLabel]);
    ATestPanel.Constraints.MinWidth  := 0;
    ATestPanel.Constraints.MinHeight := 0;
    AssertBounds('Label width after width/height', ATestLabel, ExpBnd, 0);



    (* *****
       ***** Test with Align
       ***** *)

    // Current parent: 30,30, 380,340
    // Remember Current Child
    ExpBndStore := ExpBnd;

    for IdxReset := 0 to 3 do begin

      (* ***** Align, back to normal ***** *)

// TODO:
if IdxReset <> 0 then
      for IdxAlign := low(TAlign) to High(TAlign) do begin
        if IdxAlign = alNone then continue;
        MaybeReset(IdxReset, ExpBnd, ExpBndStore);

        BeforeChange;
        ATestLabel.Align := IdxAlign;
        BeforeCheckExp;
        if IdxAlign <> alCustom then
          AssertTrue('aligned '+dbgs(IdxAlign), (ATestLabel.Width >= ATestPanel.Width-2) or (ATestLabel.Height >= ATestPanel.Height-2)); // space for borders
        AssertSetBoundsCount('aligned '+dbgs(IdxAlign), ATestLabel, [0,1]);

        BeforeChange;
        ATestLabel.Align := alNone;
        BeforeCheckExp;
        AssertBounds('Label AlNone, after '+dbgs(IdxAlign), ATestLabel, ExpBnd, [0,1]);
      end;


      (* ***** Align - change panel size, back to normal ***** *)
// TODO:
if false then
      for IdxAlign := low(TAlign) to High(TAlign) do begin
        if IdxAlign = alNone then continue;
        MaybeReset(IdxReset, ExpBnd, ExpBndStore);

        BeforeChange;
        ATestLabel.Align := IdxAlign;
        ATestPanel.Width := 420;
        ATestPanel.Height := 360;
        BeforeCheckExp;
        if IdxAlign <> alCustom then
          AssertTrue('aligned', (ATestLabel.Width >= 418) or (ATestLabel.Height >= 328)); // space for borders
        AssertSetBoundsCount('aligned '+dbgs(IdxAlign), ATestLabel, [0,1]);

// XXXXXXXXXXXXXXXXXXXXXXXXXXXXX this updates ParentBaseClientSize
        BeforeChange;
        ATestLabel.Align := alNone;
        BeforeCheckExp;
        ExpBnd2 := GetExpRect(ChildOrigBounds, ParentOrigBounds, Bounds(30,30,420,360), Anch);
        AssertBounds('Label width after alNone', ATestLabel, ExpBnd2, [0,1]);

        // change size back / again while aligned
        ATestLabel.Align := IdxAlign;
        ATestPanel.SetBounds(30,30, 380,340);
        BeforeChange;
        ATestLabel.Align := alNone;
        BeforeCheckExp;
        AssertBounds('Label width after alNone', ATestLabel, ExpBnd, [0,1]);
      end;
    end; // IdxReset


    (* ***** Child exceeds parent ***** *)
    // reset test controls
    ParentOrigBounds   := Bounds(20,10, 300,200);
    ChildOrigBounds    := Bounds(-40,-30, 400,300);
    ATestLabel.Anchors := [akLeft,akRight];
    ATestPanel.BoundsRect := ParentOrigBounds;
    ATestLabel.BoundsRect := ChildOrigBounds;
    ATestLabel.Anchors := Anch;

    BeforeChange;
    ATestPanel.SetBounds(20,10, 400, 300);
    BeforeCheckExp;
    ExpBnd := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after alNone', ATestLabel, ExpBnd, [0,1]);

    BeforeChange;
    ATestPanel.SetBounds(0,0, 0, 0);
    BeforeCheckExp;
    ExpBnd2 := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after alNone', ATestLabel, ExpBnd2, [0,1]);

    BeforeChange;
    ATestPanel.SetBounds(20,10, 400, 300);
    BeforeCheckExp;
    AssertBounds('Label width after alNone', ATestLabel, ExpBnd, [0,1]);

    (* ***** Child starts at size 0x0 / negative pos ***** *)
    ParentOrigBounds   := Bounds(20,10, 300,200);
    ChildOrigBounds    := Bounds(-40,-30, 0,0);
    ATestLabel.Anchors := [akLeft,akRight];
    ATestPanel.BoundsRect := ParentOrigBounds;
    ATestLabel.BoundsRect := ChildOrigBounds;
    ATestLabel.Anchors := Anch;

    BeforeChange;
    ATestPanel.SetBounds(20,10, 600, 400);
    BeforeCheckExp;
    ExpBnd := GetExpRect(ChildOrigBounds, ParentOrigBounds, ATestPanel.BoundsRect, Anch);
    AssertBounds('Label width after alNone', ATestLabel, ExpBnd, [0,1]);

  end;


  CreateTestForm(10,10,510,510);
  CreateTestControl(ATestPanel, TTestPanel, 10,10,410,410);
  FTestForm.Show;


  (* ****
   *
   * AkLeft+AkRight: Original size is restored when constraint is removed.
   *
   * **** *)

  CreateTestControl(ATestLabel, TTestLabel, 10,10,250,200, ATestPanel, [cfNoAutoSize]);
  ATestLabel.Anchors := [akTop, akLeft, akRight];

  (* ****
   *
   * Label size below zero (by parent resize)
   *
   * **** *)

  ATestPanel.Width := 10;
  AssertBounds('Label width updated/constrained', ATestLabel, 10,10,0,200);
  ATestPanel.Width := 510;
  AssertBounds('Label width updated/constrained', ATestLabel, 10,10,350,200);

  ATestPanel.Width := 0;
  AssertBounds('Label width updated/constrained', ATestLabel, 10,10,0,200);
  ATestPanel.Width := 410;
  AssertBounds('Label width updated/constrained', ATestLabel, 10,10,250,200);

  (* ****
   *
   * Parent zero width
   *
   * **** *)

  ATestLabel.Anchors := [akTop, akLeft];
  ATestLabel.SetBounds(-10, 5, 100, 20);
  ATestPanel.SetBounds(10, 10, 0, 50);
  ATestPanel.DisableAutoSizing;
  ATestLabel.Visible := False;
  ATestLabel.Anchors := [akTop];
  ATestPanel.SetBounds(10, 10, 100, 50);
  ATestPanel.EnableAutoSizing;
  ATestLabel.Visible := True;
  AssertEquals('', [-51,49], ATestLabel.Left); // centered


end;

procedure TTestAnchors.AnchorsAndConstraint;
var
  ATestPanel: TTestPanel;
  ATestLabel: TTestLabel;
  IdxAnch, IdxVisible: Integer;
  Anch: TAnchors;
begin
  CreateTestForm(10,10,610,610);
  CreateTestControl(ATestPanel, TTestPanel, 10,10,410,410);
  FTestForm.Show;

  (* ****
   *
   * When a control is constraint on width/height it will move
   * - no anchor:      keep center
   * - akLeft:         keep Left
   * - akRight:        keep Right (move left)
   * - akLeft+akRight: keep Left
   *
   * When the constraint is lifted
   * - NO or ONE anchor: keep pos and size
   * -  akLeft+akRight: restore width (left is still where it should be)
   *
   *)

  CreateTestControl(ATestPanel, TTestPanel, 20,10, 400,300, [cfFreeOld]);
  CreateTestControl(ATestLabel, TTestLabel, 80,40, 200,120, ATestPanel, [cfNoAutoSize]);

  (*
   * Test Max-Constraints
   *)

  for IdxAnch := 0 to 15 do
  for IdxVisible := 0 to 5 do
  begin
    Anch := ToAnchors(IdxAnch);
    ATestLabel.Anchors := Anch;
    ATestLabel.SetBounds(80,40, 200,120);

    ResetSetBoundsCounts([ATestLabel]);
    if (IdxVisible and 1) <> 0 then ATestLabel.Visible := False;
    if (IdxVisible and 2) <> 0 then ATestPanel.Visible := False;
    ATestLabel.Constraints.MaxWidth := 20;
    ATestLabel.Constraints.MaxHeight := 10;
    if (IdxVisible and 1) <> 0 then ATestLabel.Visible := True;
    if (IdxVisible and 2) <> 0 then ATestPanel.Visible := True;
    AssertSetBoundsCount('', ATestLabel, [0,2]);

    AssertEquals('Constained Width', 20, ATestLabel.Width);
    AssertEquals('Constained Height', 10, ATestLabel.Height);

    if Anch * [akLeft, akRight] = [] then
      AssertEquals('Constained mid ',  80+90,  ATestLabel.Left)  // reduced width by 180 => move by 90
    else
    if Anch * [akLeft, akRight] = [akRight] then
      AssertEquals('Constained right', 80+200, ATestLabel.Left+ATestLabel.Width)
    else
      AssertEquals('Constained left',  80,     ATestLabel.Left);

    if Anch * [akTop, akBottom] = [] then
      AssertEquals('Constained mid ',   40+55,  ATestLabel.Top)  // reduced Height by 110 => move by 55
    else
    if Anch * [akTop, akBottom] = [akBottom] then
      AssertEquals('Constained bottom', 40+120, ATestLabel.Top+ATestLabel.Height)
    else
      AssertEquals('Constained top',    40,     ATestLabel.Top);



    ResetSetBoundsCounts([ATestLabel]);
    if IdxVisible = 4 then ATestLabel.Visible := False;
    ATestLabel.DisableAutoSizing;
    if (IdxVisible and 1) <> 0 then ATestPanel.Visible := False;
    if (IdxVisible and 2) <> 0 then ATestLabel.Visible := False;
    ATestLabel.Constraints.MaxWidth := 0;
    ATestLabel.Constraints.MaxHeight := 0;
    if (IdxVisible and 1) <> 0 then ATestPanel.Visible := True;
    if (IdxVisible and 2) <> 0 then ATestLabel.Visible := True;
    ATestLabel.EnableAutoSizing;
    if IdxVisible = 4 then ATestLabel.Visible := True;
    AssertSetBoundsCount('', ATestLabel, [0,1]);

    if Anch * [akLeft, akRight] = [] then
      AssertEquals('mid ',  80+90,  ATestLabel.Left)  // reduced width by 180 => move by 90
    else
    if Anch * [akLeft, akRight] = [akRight] then
      AssertEquals('right', 80+200, ATestLabel.Left+ATestLabel.Width)
    else
      AssertEquals('left',  80,     ATestLabel.Left);

    if Anch * [akLeft, akRight] = [akLeft, akRight] then
      AssertEquals('width',  200,     ATestLabel.Width)
    else
      AssertEquals('width',   20,     ATestLabel.Width);


    if Anch * [akTop, akBottom] = [] then
      AssertEquals('mid ',   40+55,  ATestLabel.Top)  // reduced Height by 110 => move by 55
    else
    if Anch * [akTop, akBottom] = [akBottom] then
      AssertEquals('bottom', 40+120, ATestLabel.Top+ATestLabel.Height)
    else
      AssertEquals('top',    40,     ATestLabel.Top);

    if Anch * [akTop, akBottom] = [akTop, akBottom] then
      AssertEquals('height', 120,     ATestLabel.Height)
    else
      AssertEquals('height',  10,     ATestLabel.Height);
  end;

  (*
   * Test Min-Constraints
   *)

  for IdxAnch := 0 to 15 do
  for IdxVisible := 0 to 5 do
  begin
    Anch := ToAnchors(IdxAnch);
    ATestLabel.Anchors := Anch;
    ATestLabel.SetBounds(80,40, 200,120);

    ResetSetBoundsCounts([ATestLabel]);
    if (IdxVisible and 1) <> 0 then ATestLabel.Visible := False;
    if (IdxVisible and 2) <> 0 then ATestPanel.Visible := False;
    ATestLabel.Constraints.MinWidth  := 500;
    ATestLabel.Constraints.MinHeight := 400;
    if (IdxVisible and 1) <> 0 then ATestLabel.Visible := True;
    if (IdxVisible and 2) <> 0 then ATestPanel.Visible := True;
    AssertSetBoundsCount('', ATestLabel, [0,2]);

    AssertEquals('Constained Width',  500, ATestLabel.Width);
    AssertEquals('Constained Height', 400, ATestLabel.Height);

    if Anch * [akLeft, akRight] = [] then
      AssertEquals('Constained mid ',  80-150,  ATestLabel.Left)  // increased width by 300 => move by -150
    else
    if Anch * [akLeft, akRight] = [akRight] then
      AssertEquals('Constained right', 80+200, ATestLabel.Left+ATestLabel.Width)
    else
      AssertEquals('Constained left',  80,     ATestLabel.Left);

    if Anch * [akTop, akBottom] = [] then
      AssertEquals('Constained mid ',   40-140,  ATestLabel.Top)  // increased Height by 280 => move by -140
    else
    if Anch * [akTop, akBottom] = [akBottom] then
      AssertEquals('Constained bottom', 40+120, ATestLabel.Top+ATestLabel.Height)
    else
      AssertEquals('Constained top',    40,     ATestLabel.Top);



    ResetSetBoundsCounts([ATestLabel]);
    if IdxVisible = 4 then ATestLabel.Visible := False;
    ATestLabel.DisableAutoSizing;
    if (IdxVisible and 1) <> 0 then ATestPanel.Visible := False;
    if (IdxVisible and 2) <> 0 then ATestLabel.Visible := False;
    ATestLabel.Constraints.MinWidth := 0;
    ATestLabel.Constraints.MinHeight := 0;
    if (IdxVisible and 1) <> 0 then ATestPanel.Visible := True;
    if (IdxVisible and 2) <> 0 then ATestLabel.Visible := True;
    ATestLabel.EnableAutoSizing;
    if IdxVisible = 4 then ATestLabel.Visible := True;
    AssertSetBoundsCount('', ATestLabel, [0,1]);

    if Anch * [akLeft, akRight] = [] then
      AssertEquals('mid ',  80-150, ATestLabel.Left)  // increased width by 300 => move by -150
    else
    if Anch * [akLeft, akRight] = [akRight] then
      AssertEquals('right', 80+200, ATestLabel.Left+ATestLabel.Width)
    else
      AssertEquals('left',  80,     ATestLabel.Left);

    if Anch * [akLeft, akRight] = [akLeft, akRight] then
      AssertEquals('width',  200,     ATestLabel.Width)
    else
      AssertEquals('width',  500,     ATestLabel.Width);


    if Anch * [akTop, akBottom] = [] then
      AssertEquals('mid ',   40-140, ATestLabel.Top)  // increased Height by 280 => move by -140
    else
    if Anch * [akTop, akBottom] = [akBottom] then
      AssertEquals('bottom', 40+120, ATestLabel.Top+ATestLabel.Height)
    else
      AssertEquals('top',    40,     ATestLabel.Top);

    if Anch * [akTop, akBottom] = [akTop, akBottom] then
      AssertEquals('height', 120,     ATestLabel.Height)
    else
      AssertEquals('height', 400,     ATestLabel.Height);
  end;


end;

initialization
  RegisterTest(TTestAnchors);

end.

