unit Test_ChildSizing;

{$mode objfpc}{$H+}
{off $DEFINE WITHOUT_AUTOSIZE_LOCK} // Don't call DisableAutoSizing / let Autosize compute every intermediate result / SLOW

interface

uses
  Classes, SysUtils, Math, fpcunit, testutils, testregistry, Controls;

type
  TIntegerArray = array of integer;

  { TTestWinControl }

  TTestWinControl = class(TWinControl)
  private
    FTestVisible: boolean;
    FPrefWidth, FPrefHeight: Integer;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    function IsVisible: Boolean; override;
    function IsControlVisible: Boolean; override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: Integer; Raw: Boolean = false;
      WithThemeSpace: Boolean = true); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetTestPrefSize(APrefWidth, APrefHeight: Integer);
    property TestVisible: boolean read FTestVisible write FTestVisible;
  end;

  { TTestChild }

  TTestChild = class(TTestWinControl)
  public
    constructor Create(TheOwner: TWinControl);
    constructor Create(TheOwner: TWinControl; APrefWidth, APrefHeight: Integer);
  end;

  { TTestContainer }

  TTestContainer = class(TTestWinControl)
  protected
    function AutoSizeDelayedHandle: Boolean; override;
  public
  end;

  TTestChildArray = array of TTestChild;

  { TTestChildSizing }

  TTestChildSizing = class(TTestCase)
  private
    FContainer: TTestContainer;
  protected
    class procedure AssertApprox(Expected, Actual: integer);
    class procedure AssertApprox(AName: String; Expected, Actual: integer);
    class procedure AssertNoDecrementInList(ANew,AOld: TIntegerArray);
    class procedure AssertMaxOneDecrementInList(ANew,AOld: TIntegerArray);
    procedure Init1(
      out P: TTestContainer; AContainerWidth: integer;
      AStyle: TChildControlResizeStyle; APerLine: Integer;
      out C: TTestChildArray; AWidths: array of integer;
      AInitContainerHeight: boolean = False);
    procedure AddPaddingAround(var C: TTestChildArray; APadding: integer; ALowIdx: Integer = -1; AHighIdx: integer = -1);
    function GetLefts(C: TTestChildArray; ALowIdx, AHighIdx: integer): TIntegerArray;
    function GetWidths(C: TTestChildArray; ALowIdx, AHighIdx: integer): TIntegerArray;
    function SumWidths(C: TTestChildArray; ALowIdx, AHighIdx: integer): integer;
    function GetSpaces(C: TTestChildArray; AStartX, ATotalWidth, ALowIdx, AHighIdx: integer): TIntegerArray;
    function SumSpaces(s: TIntegerArray): integer;
  public
    procedure TearDown; override;
  published
    procedure TestScaleChilds;
    procedure TestScaleChildsConstrained;
    procedure TestSameSize;
    procedure TestSameSizeConstrained;
    procedure TestHomogenousChildResize;
    procedure TestHomogenousChildResizeConstrained;
    procedure TestHomogenousSpaceResize;
    procedure TestCalculateCellConstraints;
  end;

implementation

{ TTestWinControl }

procedure TTestWinControl.CreateHandle;
begin
  Handle := 1;
end;

procedure TTestWinControl.DestroyHandle;
begin
  //
end;

function TTestWinControl.IsVisible: Boolean;
begin
  Result := FTestVisible;
end;

function TTestWinControl.IsControlVisible: Boolean;
begin
  Result := FTestVisible;
end;

procedure TTestWinControl.GetPreferredSize(var PreferredWidth, PreferredHeight: Integer;
  Raw: Boolean; WithThemeSpace: Boolean);
begin
  PreferredWidth := FPrefWidth;
  PreferredHeight := FPrefHeight;
end;

constructor TTestWinControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTestVisible := True;
end;

procedure TTestWinControl.SetTestPrefSize(APrefWidth, APrefHeight: Integer);
begin
  FPrefWidth := APrefWidth;
  FPrefHeight := APrefHeight;
end;

{ TTestChild }

constructor TTestChild.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);
  Parent := TheOwner;
end;

constructor TTestChild.Create(TheOwner: TWinControl; APrefWidth, APrefHeight: Integer);
begin
  Create(TheOwner);
  SetTestPrefSize(APrefWidth, APrefHeight);
end;

{ TTestContainer }

function TTestContainer.AutoSizeDelayedHandle: Boolean;
begin
  Result := False;
end;

{ TTestChildSizing }

class procedure TTestChildSizing.AssertApprox(Expected, Actual: integer);
begin
  if Actual = Expected + 1 then dec(Actual);
  AssertEquals(Expected, Actual);
end;

class procedure TTestChildSizing.AssertApprox(AName: String; Expected, Actual: integer);
begin
  if Actual = Expected + 1 then dec(Actual);
  AssertEquals(AName, Expected, Actual);
end;

class procedure TTestChildSizing.AssertNoDecrementInList(ANew, AOld: TIntegerArray);
var
  i: Integer;
begin
  AssertEquals('NO DECR', Length(ANew), Length(AOld));
  for i := 0 to Length(ANew) - 1 do
    AssertTrue('NO DECR', ANew[i] >= AOld[i]);
end;

class procedure TTestChildSizing.AssertMaxOneDecrementInList(ANew, AOld: TIntegerArray);
var
  i: Integer;
begin
  AssertEquals('MAX ONE DECR', Length(ANew), Length(AOld));
  for i := 0 to Length(ANew) - 1 do
    AssertTrue('MAX ONE DECR', ANew[i] >= AOld[i]-1);
end;

procedure TTestChildSizing.Init1(out P: TTestContainer; AContainerWidth: integer;
  AStyle: TChildControlResizeStyle; APerLine: Integer; out C: TTestChildArray;
  AWidths: array of integer; AInitContainerHeight: boolean);
var
  i: Integer;
begin
  P := TTestContainer.Create(nil);
  if AInitContainerHeight then begin
    P.SetBounds(0,0, 150, AContainerWidth);
    p.ChildSizing.ControlsPerLine := APerLine;
    p.ChildSizing.EnlargeVertical := AStyle;
    p.ChildSizing.ShrinkVertical  := AStyle;
    p.ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  end
  else begin
    P.SetBounds(0,0, AContainerWidth, 150);
    p.ChildSizing.ControlsPerLine := APerLine;
    p.ChildSizing.EnlargeHorizontal := AStyle;
    p.ChildSizing.ShrinkHorizontal  := AStyle;
    p.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  end;

  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} p.DisableAutoSizing; {$ENDIF}
  SetLength(C, Length(AWidths));
  for i := 0 to Length(AWidths) - 1 do
    if AInitContainerHeight then
      C[i] := TTestChild.Create(P, 10, AWidths[i])
    else
      C[i] := TTestChild.Create(P, AWidths[i], 10);
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} P.EnableAutoSizing; {$ENDIF}
end;

procedure TTestChildSizing.AddPaddingAround(var C: TTestChildArray; APadding: integer;
  ALowIdx: Integer; AHighIdx: integer);
var
  i: Integer;
begin
  if ALowIdx = -1 then ALowIdx := low(C);
  if AHighIdx = -1 then AHighIdx := High(C);
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
  for i := ALowIdx to AHighIdx do
    C[i].BorderSpacing.Around := APadding;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}
end;

function TTestChildSizing.GetLefts(C: TTestChildArray; ALowIdx, AHighIdx: integer): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, AHighIdx - ALowIdx + 1);
  for i := ALowIdx to AHighIdx do
    Result[i-ALowIdx] := C[i].Left;
end;

function TTestChildSizing.GetWidths(C: TTestChildArray; ALowIdx, AHighIdx: integer): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, AHighIdx - ALowIdx + 1);
  for i := ALowIdx to AHighIdx do
    Result[i-ALowIdx] := C[i].Width;
end;

function TTestChildSizing.SumWidths(C: TTestChildArray; ALowIdx, AHighIdx: integer): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := ALowIdx to AHighIdx do
    Result := Result + C[i].Width;
end;

function TTestChildSizing.GetSpaces(C: TTestChildArray; AStartX, ATotalWidth, ALowIdx,
  AHighIdx: integer): TIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, AHighIdx - ALowIdx + 2);
  Result[0] := C[ALowIdx].Left - AStartX;
  for i := ALowIdx to AHighIdx - 1 do
    Result[1+i-ALowIdx] := C[i+1].Left - (C[i].Left + C[i].Width);
  Result[1+AHighIdx-ALowIdx] := ATotalWidth - (C[AHighIdx].Left + C[AHighIdx].Width);
end;

function TTestChildSizing.SumSpaces(s: TIntegerArray): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := low(s) to High(s) do
    Result := Result + s[i];
end;

procedure TTestChildSizing.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FContainer);
end;

procedure TTestChildSizing.TestScaleChilds;
var
  C: TTestChildArray;
  i, MinVal, ALeftSpace, AMidSpace, ACtrlSpace, TotalSpace, j, k: Integer;
  WList, OldWList, LList, OldLList: TIntegerArray;
begin
  for ALeftSpace := 0 to 3 do
  for AMidSpace := 0 to 3 do
  for ACtrlSpace := 0 to 2 do
  begin
    TotalSpace := 2*Max(ALeftSpace, ACtrlSpace) + 2*Max(AMidSpace, ACtrlSpace);
    Init1(FContainer, 300 + TotalSpace,crsScaleChilds, 3, C,
          [20, 70, 30,
           20, 35  {-}]);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
    FContainer.ChildSizing.LeftRightSpacing := ALeftSpace;
    FContainer.ChildSizing.HorizontalSpacing := AMidSpace;
    AddPaddingAround(C, ACtrlSpace);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}

    AssertEquals(50,  C[0].Width);
    AssertEquals(50,  C[3].Width);
    AssertEquals(175, C[1].Width);
    AssertEquals(175, C[4].Width);
    AssertEquals(75,  C[2].Width);

    for j := 0 to 1000 do begin
      FContainer.Width := j;
      i := Max(0, j - TotalSpace);
      MinVal := 1;
      if i <= 2 then MinVal := 0;

      WList := GetWidths(C, 0,2);
      if j > 0 then
        AssertMaxOneDecrementInList(WList, OldWList);
      OldWList := WList;
      LList := GetLefts(C, 0,2);
      if j > 0 then
        AssertNoDecrementInList(LList, OldLList);
      OldLList := LList;

      k := 0;
      if i < 4 then k := -1; // column 2 may be restricted by others forced to 1
      AssertEquals('Total Width', i, SumWidths(C, 0, 2));
      AssertApprox(Max(MinVal, 20 * i div 120), C[0].Width);
      AssertApprox(Max(MinVal, 20 * i div 120), C[3].Width);
      AssertApprox(Max(MinVal, 70 * i div 120 + k), C[1].Width);
      AssertApprox(Max(MinVal, 70 * i div 120 + k), C[4].Width);
      AssertApprox(Max(MinVal, 30 * i div 120), C[2].Width);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + 0, C[0].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + 0, C[3].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) + C[0].Width, C[1].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) + C[0].Width, C[4].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) * 2 + C[0].Width+C[1].Width, C[2].Left);
    end;
    FreeAndNil(FContainer);
  end;
end;

procedure TTestChildSizing.TestScaleChildsConstrained;
var
  C: TTestChildArray;
  i, MinVal, ALeftSpace, AMidSpace, TotalSpace, j, k: Integer;
  WList, OldWList, LList, OldLList: TIntegerArray;
begin
  for ALeftSpace := 0 to 3 do
  for AMidSpace := 0 to 3 do
  begin
    TotalSpace := 2*ALeftSpace + 2*AMidSpace;
    Init1(FContainer, 170 + TotalSpace,crsScaleChilds, 3, C,
          [20, 40, 30,
           20, 35  {-}]);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
    FContainer.ChildSizing.LeftRightSpacing := ALeftSpace;
    FContainer.ChildSizing.HorizontalSpacing := AMidSpace;
    c[1].Constraints.MinWidth := 35;
    c[1].Constraints.MaxWidth := 45;
    c[4].Constraints.MinWidth := 35;
    c[4].Constraints.MaxWidth := 45;
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}

    AssertEquals(50,  C[0].Width);
    AssertEquals(50,  C[3].Width);
    AssertEquals(45,  C[1].Width);
    AssertEquals(45,  C[4].Width);
    AssertEquals(75,  C[2].Width);

    for j := 0 to 1000 do begin
      FContainer.Width := j;
      i := Max(0, j - TotalSpace);
      MinVal := 1;
      if i < 2+35 then MinVal := 0;

      WList := GetWidths(C, 0,2);
      if j > 0 then
        AssertMaxOneDecrementInList(WList, OldWList);
      OldWList := WList;
      LList := GetLefts(C, 0,2);
      if j > 0 then
        AssertNoDecrementInList(LList, OldLList);
      OldLList := LList;

      AssertEquals('Total Width', Max(35, i), SumWidths(C, 0, 2));

      if i <= 35 then begin
        AssertEquals(MinVal, C[0].Width);
        AssertEquals(MinVal, C[3].Width);
        AssertEquals(35,     C[1].Width);
        AssertEquals(35,     C[4].Width);
        AssertEquals(MinVal, C[2].Width);
      end
      else
      if i <= 80 then begin
        AssertApprox(Max(MinVal, 20 * (i-35) div 50), C[0].Width);
        AssertApprox(Max(MinVal, 20 * (i-35) div 50), C[3].Width);
        AssertEquals(35,     C[1].Width);
        AssertEquals(35,     C[4].Width);
        AssertApprox(Max(MinVal, 30 * (i-35) div 50), C[2].Width);
      end
      else
      if i <= 101 then begin
        AssertApprox(20 * i div 90, C[0].Width);
        AssertApprox(20 * i div 90, C[3].Width);
        AssertApprox(40 * i div 90, C[1].Width);
        AssertApprox(40 * i div 90, C[4].Width);
        AssertApprox(30 * i div 90, C[2].Width);
      end
      else
      begin
        AssertApprox(20 * (i-45) div 50, C[0].Width);
        AssertApprox(20 * (i-45) div 50, C[3].Width);
        AssertEquals(45,                 C[1].Width);
        AssertEquals(45,                 C[4].Width);
        AssertApprox(30 * (i-45) div 50, C[2].Width);
      end;

      AssertEquals(ALeftSpace + 0, C[0].Left);
      AssertEquals(ALeftSpace + 0, C[3].Left);
      AssertEquals(ALeftSpace + AMidSpace + C[0].Width, C[1].Left);
      AssertEquals(ALeftSpace + AMidSpace + C[0].Width, C[4].Left);
      AssertEquals(ALeftSpace + AMidSpace * 2 + C[0].Width+C[1].Width, C[2].Left);
    end;
    FreeAndNil(FContainer);
  end;
end;

procedure TTestChildSizing.TestSameSize;
var
  C: TTestChildArray;
  i, MinVal, ALeftSpace, AMidSpace, ACtrlSpace, TotalSpace, j: Integer;
  WList, OldWList, LList, OldLList: TIntegerArray;
begin
  for ALeftSpace := 0 to 3 do
  for AMidSpace := 0 to 3 do
  for ACtrlSpace := 0 to 2 do
  begin
    TotalSpace := 2*Max(ALeftSpace, ACtrlSpace) + 2*Max(AMidSpace, ACtrlSpace);
    Init1(FContainer, 150 + TotalSpace,crsSameSize, 3, C,
          [20, 40, 30,
           20, 35  {-}]);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
    FContainer.ChildSizing.LeftRightSpacing := ALeftSpace;
    FContainer.ChildSizing.HorizontalSpacing := AMidSpace;
    AddPaddingAround(C, ACtrlSpace);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}

    AssertEquals(50, C[0].Width);
    AssertEquals(50, C[3].Width);
    AssertEquals(50, C[1].Width);
    AssertEquals(50, C[4].Width);
    AssertEquals(50, C[2].Width);

    for j := 0 to 1000 do begin
      FContainer.Width := j;
      i := Max(0, j - TotalSpace);
      MinVal := 1;
      if i <= 2 then MinVal := 0;

      WList := GetWidths(C, 0,2);
      if j > 0 then
        AssertMaxOneDecrementInList(WList, OldWList);
      OldWList := WList;
      LList := GetLefts(C, 0,2);
      if j > 0 then
        AssertNoDecrementInList(LList, OldLList);
      OldLList := LList;

      AssertEquals('Total Width', i, SumWidths(C, 0, 2));
      AssertApprox(Max(MinVal, i div 3), C[0].Width);
      AssertApprox(Max(MinVal, i div 3), C[3].Width);
      AssertApprox(Max(MinVal, i div 3), C[1].Width);
      AssertApprox(Max(MinVal, i div 3), C[4].Width);
      AssertApprox(Max(MinVal, i div 3), C[2].Width);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + 0, C[0].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + 0, C[3].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) + C[0].Width, C[1].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) + C[0].Width, C[4].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) *2 + C[0].Width+C[1].Width, C[2].Left);
    end;
    FreeAndNil(FContainer);
  end;
end;

procedure TTestChildSizing.TestSameSizeConstrained;
var
  C: TTestChildArray;
  i, MinVal, ALeftSpace, AMidSpace, TotalSpace, j: Integer;
  WList, OldWList, LList, OldLList: TIntegerArray;
begin
  for ALeftSpace := 0 to 3 do
  for AMidSpace := 0 to 3 do
  begin
    TotalSpace := 2*ALeftSpace + 2*AMidSpace;
    Init1(FContainer, 145 + TotalSpace,crsSameSize, 3, C,
          [20, 40, 30,
           20, 35  {-}]);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
    FContainer.ChildSizing.LeftRightSpacing := ALeftSpace;
    FContainer.ChildSizing.HorizontalSpacing := AMidSpace;
    c[1].Constraints.MinWidth := 35;
    c[1].Constraints.MaxWidth := 45;
    c[4].Constraints.MinWidth := 35;
    c[4].Constraints.MaxWidth := 45;
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}

    AssertEquals(50, C[0].Width);
    AssertEquals(50, C[3].Width);
    AssertEquals(45, C[1].Width);
    AssertEquals(45, C[4].Width);
    AssertEquals(50, C[2].Width);

    for j := 0 to 1000 do begin
      FContainer.Width := j;
      i := Max(0, j - TotalSpace);
      MinVal := 1;
      if i < 2+35 then MinVal := 0;

      WList := GetWidths(C, 0,2);
      if j > 0 then
        AssertMaxOneDecrementInList(WList, OldWList);
      OldWList := WList;
      LList := GetLefts(C, 0,2);
      if j > 0 then
        AssertNoDecrementInList(LList, OldLList);
      OldLList := LList;

      AssertEquals('Total Width', Max(35, i), SumWidths(C, 0, 2));

      if i <= 35 then begin
        AssertEquals(MinVal, C[0].Width);
        AssertEquals(MinVal, C[3].Width);
        AssertEquals(35,     C[1].Width);
        AssertEquals(35,     C[4].Width);
        AssertEquals(MinVal, C[2].Width);
      end
      else
      if i <= 105 then begin
        AssertApprox(Max(MinVal, (i-35) div 2), C[0].Width);
        AssertApprox(Max(MinVal, (i-35) div 2), C[3].Width);
        AssertEquals(35,     C[1].Width);
        AssertEquals(35,     C[4].Width);
        AssertApprox(Max(MinVal, (i-35) div 2), C[2].Width);
      end
      else
      if i <= 135 then begin
        AssertApprox(Max(MinVal, i div 3), C[0].Width);
        AssertApprox(Max(MinVal, i div 3), C[3].Width);
        AssertApprox(Max(MinVal, i div 3), C[1].Width);
        AssertApprox(Max(MinVal, i div 3), C[4].Width);
        AssertApprox(Max(MinVal, i div 3), C[2].Width);
      end
      else
      begin
        AssertApprox(Max(MinVal, (i-45) div 2), C[0].Width);
        AssertApprox(Max(MinVal, (i-45) div 2), C[3].Width);
        AssertEquals(45,     C[1].Width);
        AssertEquals(45,     C[4].Width);
        AssertApprox(Max(MinVal, (i-45) div 2), C[2].Width);
      end;

      AssertEquals(ALeftSpace + 0, C[0].Left);
      AssertEquals(ALeftSpace + 0, C[3].Left);
      AssertEquals(ALeftSpace + AMidSpace + C[0].Width, C[1].Left);
      AssertEquals(ALeftSpace + AMidSpace + C[0].Width, C[4].Left);
      AssertEquals(ALeftSpace + AMidSpace *2 + C[0].Width+C[1].Width, C[2].Left);
    end;
    FreeAndNil(FContainer);
  end;
end;

procedure TTestChildSizing.TestHomogenousChildResize;
var
  C: TTestChildArray;
  i, ALeftSpace, AMidSpace, ACtrlSpace, TotalSpace, j: Integer;
  WList, OldWList, LList, OldLList: TIntegerArray;
begin
  for ALeftSpace := 0 to 3 do
  for AMidSpace := 0 to 3 do
  for ACtrlSpace := 0 to 2 do
  begin
    TotalSpace := 2*Max(ALeftSpace, ACtrlSpace) + 2*Max(AMidSpace, ACtrlSpace);
    Init1(FContainer, 120 + TotalSpace, crsHomogenousChildResize, 3, C,
          [20, 40, 30,
           20, 35  {-}]);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
    FContainer.ChildSizing.LeftRightSpacing := ALeftSpace;
    FContainer.ChildSizing.HorizontalSpacing := AMidSpace;
    AddPaddingAround(C, ACtrlSpace);
    {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}

    AssertEquals(30,  C[0].Width);
    AssertEquals(30,  C[3].Width);
    AssertEquals(50, C[1].Width);
    AssertEquals(50, C[4].Width);
    AssertEquals(40,  C[2].Width);

    for j := 0 to 1000 do begin
      FContainer.Width := j;
      i := Max(0, j - TotalSpace);

      WList := GetWidths(C, 0,2);
      if j > 0 then
        AssertMaxOneDecrementInList(WList, OldWList);
      OldWList := WList;
      LList := GetLefts(C, 0,2);
      if j > 0 then
        AssertNoDecrementInList(LList, OldLList);
      OldLList := LList;

      AssertEquals('Total Width', i, SumWidths(C, 0, 2));
      if i < 3 then begin  // All column are limited
        AssertApprox(0, C[0].Width);
        AssertApprox(0, C[3].Width);
        AssertApprox(0, C[1].Width);
        AssertApprox(0, C[4].Width);
        AssertApprox(0, C[2].Width);
      end
      else
      if i <= 11 then begin  // First and Last column is limited
        // 11 = 90 - 79 = 90 -  19 (1st) + 2*30
        AssertEquals(1, C[0].Width);
        AssertEquals(1, C[3].Width);
        // 40 + (i - (90 - (19+29))) div 1
        AssertEquals(Max(1, i-2), C[1].Width);
        AssertEquals(Max(1, i-2), C[4].Width);
        AssertEquals(1, C[2].Width);
      end
      else
      if i <= 30 then begin  // First column is limited
        // 30 = 90 - 60 = 90 - 3*20 to subtract => first column forced to 1
        AssertEquals(1, C[0].Width);
        AssertEquals(1, C[3].Width);
        AssertApprox(Max(1, -1 + 40 + (i-(90-19)) div 2), C[1].Width);
        AssertApprox(Max(1, -1 + 40 + (i-(90-19)) div 2), C[4].Width);
        AssertApprox(Max(1, -1 + 30 + (i-(90-19)) div 2), C[2].Width);
      end
      else
      if i <= 90 then begin  // shrink
        AssertApprox(Max(1, -1 + 20 + (i-90) div 3), C[0].Width);
        AssertApprox(Max(1, -1 + 20 + (i-90) div 3), C[3].Width);
        AssertApprox(Max(1, -1 + 40 + (i-90) div 3), C[1].Width);
        AssertApprox(Max(1, -1 + 40 + (i-90) div 3), C[4].Width);
        AssertApprox(Max(1, -1 + 30 + (i-90) div 3), C[2].Width);
      end
      else begin
        // enlarge
        AssertApprox(max(1, 20 + (i-90) div 3), C[0].Width);
        AssertApprox(Max(1, 20 + (i-90) div 3), C[3].Width);
        AssertApprox(Max(1, 40 + (i-90) div 3), C[1].Width);
        AssertApprox(Max(1, 40 + (i-90) div 3), C[4].Width);
        AssertApprox(Max(1, 30 + (i-90) div 3), C[2].Width);
      end;
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + 0, C[0].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + 0, C[3].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) + C[0].Width, C[1].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) + C[0].Width, C[4].Left);
      AssertEquals(Max(ALeftSpace, ACtrlSpace) + Max(AMidSpace, ACtrlSpace) * 2 + C[0].Width+C[1].Width, C[2].Left);
    end;
    FreeAndNil(FContainer);
  end;
end;

procedure TTestChildSizing.TestHomogenousChildResizeConstrained;
var
  C: TTestChildArray;
  i, MinVal, j: Integer;
  WList, OldWList, LList, OldLList: TIntegerArray;
begin
  Init1(FContainer, 115, crsHomogenousChildResize, 3, C,
        [20, 40, 30,
         20, 35  {-}]);
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
    c[1].Constraints.MinWidth := 35;
    c[1].Constraints.MaxWidth := 45;
    c[4].Constraints.MinWidth := 35;
    c[4].Constraints.MaxWidth := 45;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}

  //   // 90 + 25 (5 Constrained / +20 for the 2 other column)
  AssertEquals(30,  C[0].Width);
  AssertEquals(30,  C[3].Width);
  AssertEquals(45, C[1].Width);
  AssertEquals(45, C[4].Width);
  AssertEquals(40,  C[2].Width);

  FContainer.Width := 65;  // 90 - 25 (5 Constrained / -20 for the 2 other column)
  AssertEquals(10,  C[0].Width);
  AssertEquals(10,  C[3].Width);
  AssertEquals(35, C[1].Width);
  AssertEquals(35, C[4].Width);
  AssertEquals(20,  C[2].Width);

  FContainer.Width := 45; // 90 - 35 (5 Constrained / -30 for the 2 other column)
  AssertEquals( 1,  C[0].Width);
  AssertEquals( 1,  C[3].Width);
  AssertEquals(35, C[1].Width);
  AssertEquals(35, C[4].Width);
  AssertEquals( 9,  C[2].Width);

  FContainer.Width := 40; // 90 - 50 (5 Constrained / -45 for the 2 other column)
  AssertEquals( 1,  C[0].Width);
  AssertEquals( 1,  C[3].Width);
  AssertEquals(35, C[1].Width);
  AssertEquals(35, C[4].Width);
  AssertEquals( 4,  C[2].Width);

  FContainer.Width := 30;
  AssertEquals( 0,  C[0].Width);
  AssertEquals( 0,  C[3].Width);
  AssertEquals(35, C[1].Width);
  AssertEquals(35, C[4].Width);
  AssertEquals( 0,  C[2].Width);



  for j := 0 to 1000 do begin
    FContainer.Width := j;
    i := Max(0, j);

    WList := GetWidths(C, 0,2);
    if j > 0 then
      AssertMaxOneDecrementInList(WList, OldWList);
    OldWList := WList;
    LList := GetLefts(C, 0,2);
    if j > 0 then
      AssertNoDecrementInList(LList, OldLList);
    OldLList := LList;

    AssertEquals('Total Width', Max(35, i), SumWidths(C, 0, 2));

    AssertEquals(0, C[0].Left);
    AssertEquals(0, C[3].Left);
    AssertEquals(C[0].Width, C[1].Left);
    AssertEquals(C[0].Width, C[4].Left);
    AssertEquals(C[0].Width+C[1].Width, C[2].Left);
  end;

end;

procedure TTestChildSizing.TestHomogenousSpaceResize;
var
  C: TTestChildArray;
  i, d, j: Integer;
  gaps, OldGaps, LList, OldLList: TIntegerArray;
begin
  Init1(FContainer, 120, crsHomogenousSpaceResize, 3, C,
        [20, 40, 30,
         20, 35  {-}]);

  for i := 0 to 1000 do begin
    FContainer.Width := i;
    AssertEquals('Total Width', 90, SumWidths(C, 0, 2));
    AssertEquals(20,  C[0].Width);
    AssertEquals(20,  C[3].Width);
    AssertEquals(40, C[1].Width);
    //AssertEquals(35, C[4].Width); // Even though it's "space resize", the cell size gets applied to all children
    AssertEquals(30,  C[2].Width);

    gaps := GetSpaces(C, 0, Max(90,i), 0,2);
    AssertEquals('Spaces', Max(0, i-90), SumSpaces(gaps));
    d := Max(0, i-90) div 4;
    for j := 0 to Length(gaps) - 1 do
      AssertApprox(d, gaps[j]);
  end;
  FreeAndNil(FContainer);

  /////////////////
  // With Spacing

  Init1(FContainer, 120, crsHomogenousSpaceResize, 3, C,
        [20, 40, 30,
         20, 35  {-}]);
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
  FContainer.ChildSizing.LeftRightSpacing  := 9;
  FContainer.ChildSizing.HorizontalSpacing := 4;
  C[2].BorderSpacing.Left := 11;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}
  // Spacing   9  C0  4  C1  11  C2  9


  for i := 0 to 1000 do begin
    FContainer.Width := i;
    AssertEquals('Total Width', 90, SumWidths(C, 0, 2));
    AssertEquals(20,  C[0].Width);
    AssertEquals(20,  C[3].Width);
    AssertEquals(40, C[1].Width);
    //AssertEquals(35, C[4].Width); // Even though it's "space resize", the cell size gets applied to all children
    AssertEquals(30,  C[2].Width);

    gaps := GetSpaces(C, 0, Max(90,i), 0,2);
    if i > 0 then
      AssertMaxOneDecrementInList(gaps, OldGaps);
    OldGaps := gaps;
    LList := GetLefts(C, 0,2);
    if i > 0 then
      AssertNoDecrementInList(LList, OldLList);
    OldLList := LList;
    AssertEquals('Spaces', Max(0, i-90), SumSpaces(gaps));

    if i <= 90 then begin
      AssertEquals( 0, gaps[0]);
      AssertEquals( 0, gaps[1]);
      AssertEquals( 0, gaps[2]);
      AssertEquals( 0, gaps[3]);
    end
    else
    if i <= 92 then begin
      d := i-90;
      AssertEquals( 0, gaps[0]);
      AssertEquals( 0, gaps[1]);
      AssertEquals( 0 + d, gaps[2]); // 91 = 1 .... 92 = 2
      AssertEquals( 0, gaps[3]);
    end
    else
    if i <= 107 then begin
      d := Max(0, i-92) div 3;   // 93-95=1 ..  105..107=5
      AssertApprox( 0 + d, gaps[0]);
      AssertEquals( 0, gaps[1]);
      AssertApprox( 2 + d, gaps[2]);
      AssertApprox( 0 + d, gaps[3]);
    end
    else
    if i <= 123 then begin
      d := Max(0, i-107) div 4;
      AssertApprox( 5 + d, gaps[0]);
      AssertApprox( 0 + d, gaps[1]);
      AssertApprox( 7 + d, gaps[2]);
      AssertApprox( 5 + d, gaps[3]);
    end;



  end;
end;

procedure TTestChildSizing.TestCalculateCellConstraints;
var
  C: TTestChildArray;
  w1, w2, w3: Integer;
begin
  Init1(FContainer, 1000,crsScaleChilds, 3, C,
        [20, 90, 30,
         25, 85, 30,
         20, 95, 30]);

  // preferred witd
  w1 := 25 * 1000 div (25+95+30);
  w2 := 95 * 1000 div (25+95+30);
  w3 := 30 * 1000 div (25+95+30);
  AssertApprox(w1, C[0].Width); AssertApprox(w2, C[1].Width); AssertApprox(w3, C[2].Width);
  AssertApprox(w1, C[3].Width); AssertApprox(w2, C[4].Width); AssertApprox(w3, C[5].Width);
  AssertApprox(w1, C[6].Width); AssertApprox(w2, C[7].Width); AssertApprox(w3, C[8].Width);

  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
  c[4].Constraints.MaxWidth := 70; // lowest MaxWidth
  c[7].Constraints.MaxWidth := 75;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}
  w1 := 25 * (1000-70) div (25+30);
  w2 := 70;
  w3 := 30 * (1000-70) div (25+30);
  AssertApprox(w1, C[0].Width); AssertEquals(w2, C[1].Width); AssertApprox(w3, C[2].Width);
  AssertApprox(w1, C[3].Width); AssertEquals(w2, C[4].Width); AssertApprox(w3, C[5].Width);
  AssertApprox(w1, C[6].Width); AssertEquals(w2, C[7].Width); AssertApprox(w3, C[8].Width);

  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
  c[4].Constraints.MaxWidth := 900;
  c[7].Constraints.MaxWidth := 900;
  c[0].Constraints.MinWidth := 135; // highest MinWidth
  c[3].Constraints.MinWidth := 115;
  FContainer.Width := 200;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}
  w1 := 135;
  w2 := 95 * (200-135) div (95+30);
  w3 := 30 * (200-135) div (95+30);
  AssertEquals(w1, C[0].Width); AssertApprox(w2, C[1].Width); AssertApprox(w3, C[2].Width);
  AssertEquals(w1, C[3].Width); AssertApprox(w2, C[4].Width); AssertApprox(w3, C[5].Width);
  AssertEquals(w1, C[6].Width); AssertApprox(w2, C[7].Width); AssertApprox(w3, C[8].Width);

  FreeAndNil(FContainer);

  // check Height
  Init1(FContainer, 1000,crsScaleChilds, 3, C,
        [20, 90, 30,
         25, 85, 30,
         20, 95, 30],
        True);

  // preferred witd
  w1 := 25 * 1000 div (25+95+30);
  w2 := 95 * 1000 div (25+95+30);
  w3 := 30 * 1000 div (25+95+30);
  AssertApprox(w1, C[0].Height); AssertApprox(w2, C[1].Height); AssertApprox(w3, C[2].Height);
  AssertApprox(w1, C[3].Height); AssertApprox(w2, C[4].Height); AssertApprox(w3, C[5].Height);
  AssertApprox(w1, C[6].Height); AssertApprox(w2, C[7].Height); AssertApprox(w3, C[8].Height);

  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
  c[4].Constraints.MaxHeight := 70; // lowest MaxHeight
  c[7].Constraints.MaxHeight := 75;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}
  w1 := 25 * (1000-70) div (25+30);
  w2 := 70;
  w3 := 30 * (1000-70) div (25+30);
  AssertApprox(w1, C[0].Height); AssertEquals(w2, C[1].Height); AssertApprox(w3, C[2].Height);
  AssertApprox(w1, C[3].Height); AssertEquals(w2, C[4].Height); AssertApprox(w3, C[5].Height);
  AssertApprox(w1, C[6].Height); AssertEquals(w2, C[7].Height); AssertApprox(w3, C[8].Height);

  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.DisableAutoSizing; {$ENDIF}
  c[4].Constraints.MaxHeight := 900;
  c[7].Constraints.MaxHeight := 900;
  c[0].Constraints.MinHeight := 135; // highest MinHeight
  c[3].Constraints.MinHeight := 115;
  FContainer.Height := 200;
  {$IFnDEF WITHOUT_AUTOSIZE_LOCK} FContainer.EnableAutoSizing; {$ENDIF}
  w1 := 135;
  w2 := 95 * (200-135) div (95+30);
  w3 := 30 * (200-135) div (95+30);
  AssertEquals(w1, C[0].Height); AssertApprox(w2, C[1].Height); AssertApprox(w3, C[2].Height);
  AssertEquals(w1, C[3].Height); AssertApprox(w2, C[4].Height); AssertApprox(w3, C[5].Height);
  AssertEquals(w1, C[6].Height); AssertApprox(w2, C[7].Height); AssertApprox(w3, C[8].Height);

  FreeAndNil(FContainer);

end;


initialization

  RegisterTest(TTestChildSizing);
end.

