{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Backend writing a textual operation log, used by the golden tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.trace;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, fpsvg.types, fpsvg.backend;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpsvg.types, fpsvg.backend;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSVGTraceNesting = (tnClip, tnLayer);
  TSVGTraceNestingStack = array of TSVGTraceNesting;

  { Records every backend call as one line of text. Nested calls are
    indented. }
  TSVGTraceBackend = class(TSVGRenderBackend)
  private
    FLog: TStringList;
    FNesting: TSVGTraceNestingStack;
    FNestingCount: Integer;
    FIndent: Integer;
    FInFrame: Boolean;
    procedure Emit(const aLine: String);
    procedure EmitPath(aPath: TSVGPath);
    procedure Push(aKind: TSVGTraceNesting);
    procedure Pop(aKind: TSVGTraceNesting; const aOperation: String);
    function GetLog: TStrings;
    function GetText: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function BackendName: String; override;
    class function Capabilities: TSVGBackendCapabilities; override;

    // Discards the log and any frame that is open.
    procedure Clear;
    // Writes the log to a file, ending with a line break.
    procedure SaveToFile(const aFileName: String);

    procedure BeginFrame(aWidth, aHeight: Integer); override;
    procedure EndFrame; override;
    procedure FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double); override;
    procedure StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; const aPen: TSVGPen; aOpacity: Double); override;
    procedure PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      aRule: TSVGFillRule); override;
    procedure PushStrokeClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPen: TSVGPen); override;
    procedure PopClip; override;
    procedure SetColorInterpolation(
      aSpace: TSVGColorInterpolation); override;
    procedure PushLayer(const aBounds: TSVGRect; aOpacity: Double;
      aIsolate: Boolean); override;
    procedure PopLayer; override;
    procedure PopLayerAsMask(aMode: TSVGMaskMode); override;
    procedure PopLayerAsFilter(const aChain: TSVGFilterChain); override;
    procedure DrawGlyphRun(aFont: TSVGFontHandle; const aGlyphs: TSVGGlyphArray;
      const aCTM: TSVGMatrix; const aPaint: TSVGPaint; aOpacity: Double); override;
    procedure DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
      const aCTM: TSVGMatrix; aOpacity: Double); override;

    // The log recorded so far, one operation per line.
    property Log: TStrings read GetLog;
    // The log as a single string with platform line endings.
    property Text: String read GetText;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  FillRuleNames: array[TSVGFillRule] of String = ('nonzero', 'evenodd');
  MaskModeNames: array[TSVGMaskMode] of String = ('luminance', 'alpha');
  BoolNames: array[Boolean] of String = ('false', 'true');

{ TSVGTraceBackend }

constructor TSVGTraceBackend.Create;

begin
  inherited Create;
  FLog := TStringList.Create;
end;


destructor TSVGTraceBackend.Destroy;

begin
  FreeAndNil(FLog);
  inherited Destroy;
end;


class function TSVGTraceBackend.BackendName: String;

begin
  Result := 'trace';
end;


class function TSVGTraceBackend.Capabilities: TSVGBackendCapabilities;

begin
  Result := [bcClipPath, bcMask, bcGroupOpacity, bcPattern, bcDashes,
             bcNativeText, bcFilter];
end;


function TSVGTraceBackend.GetLog: TStrings;

begin
  Result := FLog;
end;


function TSVGTraceBackend.GetText: String;

begin
  Result := FLog.Text;
end;


procedure TSVGTraceBackend.Emit(const aLine: String);

begin
  FLog.Add(StringOfChar(' ', FIndent * 2) + aLine);
end;


procedure TSVGTraceBackend.EmitPath(aPath: TSVGPath);

var
  I: Integer;

begin
  Inc(FIndent);
  try
    if aPath = nil then
      begin
      Emit('nil-path');
      Exit;
      end;
    for I := 0 to aPath.SegmentCount - 1 do
      Emit(aPath[I].ToString);
  finally
    Dec(FIndent);
  end;
end;


procedure TSVGTraceBackend.Push(aKind: TSVGTraceNesting);

begin
  if FNestingCount = Length(FNesting) then
    SetLength(FNesting, FNestingCount + 8);
  FNesting[FNestingCount] := aKind;
  Inc(FNestingCount);
  Inc(FIndent);
end;


procedure TSVGTraceBackend.Pop(aKind: TSVGTraceNesting; const aOperation: String);

begin
  if FNestingCount = 0 then
    raise ESVGBackend.CreateFmt(SErrPopWithoutPush, [aOperation]);
  if FNesting[FNestingCount - 1] <> aKind then
    raise ESVGBackend.CreateFmt(SErrMismatchedNesting, [aOperation]);
  Dec(FNestingCount);
  Dec(FIndent);
end;


procedure TSVGTraceBackend.Clear;

begin
  FLog.Clear;
  FNestingCount := 0;
  FIndent := 0;
  FInFrame := False;
end;


procedure TSVGTraceBackend.SaveToFile(const aFileName: String);

begin
  FLog.SaveToFile(aFileName);
end;


procedure TSVGTraceBackend.BeginFrame(aWidth, aHeight: Integer);

begin
  if FInFrame then
    raise ESVGBackend.Create(SErrBeginFrameInFrame);
  FInFrame := True;
  FNestingCount := 0;
  FIndent := 0;
  Emit(Format('begin-frame %d %d', [aWidth, aHeight]));
end;


procedure TSVGTraceBackend.EndFrame;

begin
  if not FInFrame then
    raise ESVGBackend.Create(SErrEndFrameWithoutBegin);
  if FNestingCount <> 0 then
    raise ESVGBackend.CreateFmt(SErrEndFrameLevelsOpen, [FNestingCount]);
  FInFrame := False;
  Emit('end-frame');
end;


procedure TSVGTraceBackend.FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double);

begin
  Emit('fill-path rule=' + FillRuleNames[aRule]
    + ' paint=' + aPaint.ToString
    + ' opacity=' + SVGFormatFloat(aOpacity)
    + ' ctm=' + aCTM.ToString);
  EmitPath(aPath);
end;


procedure TSVGTraceBackend.StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; const aPen: TSVGPen; aOpacity: Double);

begin
  Emit('stroke-path ' + aPen.ToString
    + ' paint=' + aPaint.ToString
    + ' opacity=' + SVGFormatFloat(aOpacity)
    + ' ctm=' + aCTM.ToString);
  EmitPath(aPath);
end;


procedure TSVGTraceBackend.PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
  aRule: TSVGFillRule);

begin
  Emit('push-clip rule=' + FillRuleNames[aRule] + ' ctm=' + aCTM.ToString);
  EmitPath(aPath);
  Push(tnClip);
end;


procedure TSVGTraceBackend.PushStrokeClip(aPath: TSVGPath;
  const aCTM: TSVGMatrix; const aPen: TSVGPen);

begin
  Emit('push-stroke-clip ' + aPen.ToString + ' ctm=' + aCTM.ToString);
  EmitPath(aPath);
  Push(tnClip);
end;


procedure TSVGTraceBackend.SetColorInterpolation(
  aSpace: TSVGColorInterpolation);

const
  SpaceNames: array[TSVGColorInterpolation] of String = ('sRGB', 'linearRGB');

begin
  Emit('color-interpolation ' + SpaceNames[aSpace]);
end;


procedure TSVGTraceBackend.PopClip;

begin
  Pop(tnClip, 'PopClip');
  Emit('pop-clip');
end;


procedure TSVGTraceBackend.PushLayer(const aBounds: TSVGRect; aOpacity: Double;
  aIsolate: Boolean);

begin
  Emit('push-layer bounds=' + aBounds.ToString
    + ' opacity=' + SVGFormatFloat(aOpacity)
    + ' isolate=' + BoolNames[aIsolate]);
  Push(tnLayer);
end;


procedure TSVGTraceBackend.PopLayer;

begin
  Pop(tnLayer, 'PopLayer');
  Emit('pop-layer');
end;


procedure TSVGTraceBackend.PopLayerAsMask(aMode: TSVGMaskMode);

begin
  Pop(tnLayer, 'PopLayerAsMask');
  Emit('pop-layer-as-mask mode=' + MaskModeNames[aMode]);
end;


procedure TSVGTraceBackend.PopLayerAsFilter(const aChain: TSVGFilterChain);

const
  KindNames: array[TSVGFilterKind] of String = ('flood', 'blur', 'offset',
    'merge', 'composite', 'colour-matrix', 'blend', 'tile', 'image',
    'morphology', 'component-transfer', 'turbulence', 'diffuse-lighting',
    'specular-lighting', 'convolve-matrix', 'displacement-map');

  // The input of a primitive: an earlier one of the chain by its number,
  // or one of the sources by name.
  function InputName(aInput: Integer): String;
  begin
    case aInput of
      SVGFilterSourceGraphic: Result := 'source';
      SVGFilterSourceAlpha: Result := 'source-alpha';
      SVGFilterBackgroundImage: Result := 'background';
      SVGFilterBackgroundAlpha: Result := 'background-alpha';
      SVGFilterFillPaint: Result := 'fill';
      SVGFilterStrokePaint: Result := 'stroke';
    else
      Result := IntToStr(aInput);
    end;
  end;

var
  I, J: Integer;
  lLine: String;

begin
  Pop(tnLayer, 'PopLayerAsFilter');
  Emit(Format('pop-layer-as-filter region=%s linear=%s count=%d',
    [aChain.Region.ToString, BoolToStr(aChain.Linear, True),
     Length(aChain.Primitives)]));
  Inc(FIndent);
  try
    for I := 0 to High(aChain.Primitives) do
      begin
      lLine := Format('%d %s in=', [I, KindNames[aChain.Primitives[I].Kind]]);
      for J := 0 to High(aChain.Primitives[I].Inputs) do
        begin
        if J > 0 then
          lLine := lLine + ',';
        lLine := lLine + InputName(aChain.Primitives[I].Inputs[J]);
        end;
      if Length(aChain.Primitives[I].Inputs) = 0 then
        lLine := lLine + 'none';
      for J := 0 to High(aChain.Primitives[I].Numbers) do
        lLine := lLine + ' ' + SVGFormatFloat(aChain.Primitives[I].Numbers[J]);
      if aChain.Primitives[I].HasRegion then
        lLine := lLine + ' at ' + aChain.Primitives[I].Region.ToString;
      Emit(lLine);
      end;
  finally
    Dec(FIndent);
  end;
end;


procedure TSVGTraceBackend.DrawGlyphRun(aFont: TSVGFontHandle;
  const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aOpacity: Double);

var
  I: Integer;
  lName: String;

begin
  if aFont = nil then
    lName := 'nil'
  else
    lName := aFont.GetFontName;
  Emit('draw-glyphs font="' + lName + '"'
    + ' paint=' + aPaint.ToString
    + ' opacity=' + SVGFormatFloat(aOpacity)
    + ' ctm=' + aCTM.ToString);
  Inc(FIndent);
  try
    for I := 0 to High(aGlyphs) do
      Emit(aGlyphs[I].ToString);
  finally
    Dec(FIndent);
  end;
end;


procedure TSVGTraceBackend.DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
  const aCTM: TSVGMatrix; aOpacity: Double);

var
  lSize: String;

begin
  if aImage = nil then
    lSize := 'nil'
  else
    lSize := Format('%dx%d', [aImage.GetWidth, aImage.GetHeight]);
  Emit('draw-image source=' + lSize
    + ' rect=' + aRect.ToString
    + ' opacity=' + SVGFormatFloat(aOpacity)
    + ' ctm=' + aCTM.ToString);
end;


initialization
  SVGBackends.RegisterBackend(TSVGTraceBackend);
end.
