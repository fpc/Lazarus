unit TestPaintColorMerging;

{$mode objfpc}{$H+}

interface

uses
  testregistry, TestBase, SynEditHighlighter, SynEditMiscClasses, SynEditTypes, Graphics,
  sysutils;

type

  { TTestPaintColorMerging }

  TTestPaintColorMerging = class(TTestBase)
  published
    procedure MergeAttrib;
  end;

implementation

{ TTestPaintColorMerging }

procedure TTestPaintColorMerging.MergeAttrib;

  procedure SetAttrib(AnAttrib: TSynHighlighterAttributes;
    AFore, ABack, AFrame: TColor;
    AStyle: TFontStyles = []; AStyleMask: TFontStyles = [];
    AFrameSides: TSynFrameEdges = sfeAround; AFrameStyle: TSynLineStyle = slsSolid;
    AStartX: Integer = -1; AEndX: Integer = -1);
  begin
    AnAttrib.Foreground := AFore;
    AnAttrib.Background := ABack;
    AnAttrib.FrameColor := AFrame;
    AnAttrib.Style      := AStyle;
    AnAttrib.StyleMask  := AStyleMask;
    AnAttrib.FrameEdges := AFrameSides;
    AnAttrib.FrameStyle := AFrameStyle;
    if AnAttrib is TSynSelectedColor then begin
      TSynSelectedColor(AnAttrib).SetFrameBoundsPhys(AStartX, AEndX);
    end;
  end;

  procedure CheckAttrib(AName: String; AnAttrib: TSynSelectedColorMergeResult;
    AFore, ABack, AFrameL, AFrameR, AFrameT, AFrameB: TColor;
    AStyle: TFontStyles = []);
  begin
    AssertEquals(AName + ' Fore',   AFore,   AnAttrib.Foreground);
    AssertEquals(AName + ' Back',   ABack,   AnAttrib.Background);
    AssertEquals(AName + ' FrameL', AFrameL, AnAttrib.FrameSideColors[bsLeft]);
    AssertEquals(AName + ' FrameR', AFrameR, AnAttrib.FrameSideColors[bsRight]);
    AssertEquals(AName + ' FrameT', AFrameT, AnAttrib.FrameSideColors[bsTop]);
    AssertEquals(AName + ' FrameB', AFrameB, AnAttrib.FrameSideColors[bsBottom]);
    AssertTrue(AName + ' Style',  AStyle = AnAttrib.Style);
  end;

var
  Merger: TSynSelectedColorMergeResult;
  Base: TSynHighlighterAttributes;
  Modifier: TSynSelectedColor;
  b1, b2: TLazSynDisplayTokenBound;
begin
  b1.Logical := -1;
  b1.Physical := 3;
  b2.Logical := -1;
  b2.Physical := 7;

  Merger := TSynSelectedColorMergeResult.Create;
  Base := TSynHighlighterAttributes.Create;
  Modifier := TSynSelectedColor.Create;

  SetAttrib(Base, clRed, clYellow, clNone);
  Merger.Assign(Base);
  Merger.CurrentStartX := b1;
  Merger.CurrentEndX := b2;
  CheckAttrib('', Merger, clRed, clYellow, clNone, clNone, clNone, clNone);


  SetAttrib(Base, clRed, clYellow, clNone);
  Merger.Assign(Base);
  Merger.CurrentStartX := b1;
  Merger.CurrentEndX := b2;
  Merger.ProcessMergeInfo;
  CheckAttrib('', Merger, clRed, clYellow, clNone, clNone, clNone, clNone);


  SetAttrib(Base, clRed, clYellow, clNone);
  Merger.Assign(Base);
  Merger.CurrentStartX := b1;
  Merger.CurrentEndX := b2;
  Merger.InitMergeInfo;
  Merger.ProcessMergeInfo;
  CheckAttrib('', Merger, clRed, clYellow, clNone, clNone, clNone, clNone);


  SetAttrib(Base, clRed, clYellow, clNone);
  SetAttrib(Modifier, clNone, clNone, clNone);
  Merger.Assign(Base);
  Merger.Merge(Modifier);
  Merger.ProcessMergeInfo;
  CheckAttrib('', Merger, clRed, clYellow, clNone, clNone, clNone, clNone);

  SetAttrib(Base, clRed, clYellow, clNone);
  SetAttrib(Modifier, clNone, clNone, clNone, [], [], sfeAround, slsSolid, 3, 7);
  Merger.Assign(Base);
  Merger.Merge(Modifier);
  Merger.ProcessMergeInfo;
  CheckAttrib('', Merger, clRed, clYellow, clNone, clNone, clNone, clNone);


  SetAttrib(Base, clRed, clYellow, clNone);
  SetAttrib(Modifier, clNone, clGray, clGreen, [], [], sfeAround, slsSolid, 3, 7);
  Merger.Assign(Base);
  Merger.Merge(Modifier, b1, b2);
  Merger.ProcessMergeInfo;
  CheckAttrib('', Merger, clRed, clGray, clGreen, clGreen, clGreen, clGreen);




  FreeAndNil(Merger);
  FreeAndNil(Base);
  FreeAndNil(Modifier);
end;

initialization
  RegisterTest(TTestPaintColorMerging);

end.

