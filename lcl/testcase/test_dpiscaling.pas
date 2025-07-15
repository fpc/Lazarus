unit Test_DpiScaling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  System.UITypes, fpcunit, testutils, testregistry,
  Forms, StdCtrls, LResources, ExtCtrls, Controls, LMessages,
  LazLoggerBase;

type

  { TTestForm1 }

  TTestForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    LabelA: TLabel;
    LabelB: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TTestDpiScaling }

  TTestDpiScaling = class(TTestCase)
  private
    procedure DoShrinkForm(Sender: TObject);
  protected
    FForceApprox: boolean;
    FForceApproxThreshold: integer;
    function CreateTestForm(AFormName: String; AFormCLass: TFormClass; ALfm: array of string): TForm;
    function BuildLfm(const AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      AnAutoSize: Boolean = False; AnAnchors: TAnchors = [akLeft, akTop]; AnAlign: TAlign = alNone;
      ACaption: String = ''
      ): TStringArray;
    function BuildLfm(const AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign;
      const AMore: array of string
      ): TStringArray;
    function BuildLfm(const AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign; ACaption: String;
      const AMore: array of string
      ): TStringArray;
    function BuildFormLfm(const AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      APPI: Integer;
      const AMore: array of string
      ): TStringArray;

    procedure AssertEquals(AMessage: string; const AMsgParam: array of const; Expected, Actual: integer; AnApprox: boolean = False; AnThreshold: integer = 1); inline; overload;
    procedure AssertPos (const AName: String; AControl: TControl; ExpLeft, ExpTop: Integer);
    procedure AssertSize(const AName: String; AControl: TControl; ExpWidth, ExpHeight: Integer);
    procedure AssertRightBottom(const AName: String; AControl: TControl; ExpRight, ExpBottom: Integer);
    procedure AssertBounds(const AName: String; AControl: TControl; ExpLeft, ExpTop, ExpWidth, ExpHeight: Integer);
    procedure SendNewDPI(APPI: integer; TheForm: TForm);
  published
    procedure ScaleLfm_Anchor;
    procedure ScaleLfm_Align;
  end;

implementation

var
  GlobalTestFormOnCreate: TNotifyEvent = nil;

const
  akAll = [low(TAnchorKind)..high(TAnchorKind)];
  AUTOSIZE_OFFS = 1; // Children in autosized control start at top/left = 1,1

  OFFS_RANGE     = 100000;  // any smaller value should be compared to the control's right or bottom
  OFFS_BTM_RIGHT =  2*OFFS_RANGE; // -300k .. -100k  // Compare Bottom or Right
  OFFS_CENTER    =  4*OFFS_RANGE; // -500k .. -300k  // Compare Center
  OFFS_APPROX    = 20*OFFS_RANGE; // -3000k .. -1000k // Approx (for parent autosize)

(* If a control is inside an autosizing parent, then the parent may move any child at its
   bound (x=0) to x=1.
   - Other children would be moved together with those.
     This ONE pixel is not scaled (happens after scaling).
   - This test does not aim to check how anchors behave in autosizing.
     Only how they behaved in scaling. For that we can ignore the 1 pixel move.
*)
function Approx(APos: integer): integer; inline;
begin
  if APos < -(OFFS_APPROX - 10 * OFFS_RANGE) then
    exit(APos);
  if APos = MaxInt then Result := APos
  else                  Result := APos - OFFS_APPROX;
end;

function Approx(APos: TRect): TRect; inline;
begin
  Result.Left   := Approx(APos.Left);
  Result.Top    := Approx(APos.Top);
  Result.Right  := Approx(APos.Right);
  Result.Bottom := Approx(APos.Bottom);
end;

function Rgt(APos: integer): integer;  inline;
begin
  if APos = MaxInt then Result := APos
  else                  Result := APos - OFFS_BTM_RIGHT;
end;

function Cnt(APos: integer): integer;  inline;
begin
  if APos = MaxInt then Result := APos
  else                  Result := APos - OFFS_CENTER;
end;

function DecodePos(APos: Integer; out IsRight, IsCenter, IsApprox, IsUnknown: boolean): integer; inline;
begin
  Result := APos;
  IsUnknown := Result = MaxInt;
  if IsUnknown then
    exit;
  IsApprox := Result < -(OFFS_APPROX - 10 * OFFS_RANGE);
  if IsApprox then Result := Result + OFFS_APPROX;

  IsCenter := Result < -(OFFS_CENTER - OFFS_RANGE);
  if IsCenter then Result := Result + OFFS_CENTER;

  IsRight := Result < -(OFFS_BTM_RIGHT - OFFS_RANGE);
  if IsRight then Result := Result + OFFS_BTM_RIGHT;
end;

function DecodePos(APos: Integer): integer; inline;
var IsRight, IsCenter, IsApprox, IsUnknown: boolean;
begin
  Result := DecodePos(APos, IsRight, IsCenter, IsApprox, IsUnknown);
end;

function ToAnchors(i: integer): TAnchors;
begin
  Result := [];
  if (i and 1) <> 0 then Result := Result + [akLeft];
  if (i and 2) <> 0 then Result := Result + [akTop];
  if (i and 4) <> 0 then Result := Result + [akRight];
  if (i and 8) <> 0 then Result := Result + [akBottom];
end;

function ToSameAnchors(i: integer): TAnchors;
begin
  Result := [];
  if (i and 1) <> 0 then Result := Result + [akLeft];
  if (i and 2) <> 0 then Result := Result + [akRight];
  if (i and 1) <> 0 then Result := Result + [akTop];
  if (i and 2) <> 0 then Result := Result + [akBottom];
end;

function jn(const L1, L2: array of string): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(L1) + Length(L2));
  for i := 0 to Length(L1)-1 do
    Result[i] := L1[i];
  for i := 0 to Length(L2)-1 do
    Result[i+Length(L1)] := '  '+L2[i];
end;

function jn(const L1, L2, L3: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2), L3);
end;

function jn(const L1, L2, L3, L4: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2), jn(L3, L4));
end;

function jn(const L1, L2, L3, L4, L5: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2, L3), jn(L4, L5));
end;

function jn(const L1, L2, L3, L4, L5, L6: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2, L3), jn(L4, L5, L6));
end;

operator + (const a, b: TStringArray): TStringArray;
begin
  Result := jn(a, b);
end;

{ TTestForm1 }

constructor TTestForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if GlobalTestFormOnCreate <> nil then
    GlobalTestFormOnCreate(Self);
end;

{ TTestForm1 }

procedure TTestDpiScaling.DoShrinkForm(Sender: TObject);
var
  f: TForm;
begin
  f := Sender as TForm;
  f.Width  := f.Width  - 40;
  f.Height := f.Height - 40;
end;

function TTestDpiScaling.CreateTestForm(AFormName: String; AFormCLass: TFormClass;
  ALfm: array of string): TForm;
var
  LfmTxt, s: String;
  LfmStream, LrsStream: TStringStream;
  R: TLResource;
begin
  LfmTxt := '';
  for s in ALfm do LfmTxt := LfmTxt + s + LineEnding;

  LfmStream := TStringStream.Create;
  LfmStream.WriteAnsiString(LfmTxt);
  LfmStream.Position := 0;

  LrsStream := TStringStream.Create;
  LRSObjectTextToBinary(LfmStream, LrsStream);

  LrsStream.Position := 0;
  R := LazarusResources.Find(AFormName, 'FORMDATA');
  if R <> nil then
    R.Value := LrsStream.DataString
  else
    LazarusResources.Add(AFormName, 'FORMDATA', LrsStream.DataString);

  Result := AFormClass.Create(nil);
end;

function TTestDpiScaling.BuildLfm(const AName, AClass: String; ALeft, ATop, AWidth,
  AHeight: Integer; AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign; ACaption: String): TStringArray;
begin
  Result := BuildLfm(AName, AClass, ALeft, ATop, AWidth, AHeight, AnAutoSize, AnAnchors,
      AnAlign, ACaption, []);
end;

function TTestDpiScaling.BuildLfm(const AName, AClass: String; ALeft, ATop, AWidth,
  AHeight: Integer; AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign;
  const AMore: array of string): TStringArray;
begin
  Result := BuildLfm(AName, AClass, ALeft, ATop, AWidth, AHeight, AnAutoSize, AnAnchors,
      AnAlign, '', AMore);
end;

function TTestDpiScaling.BuildLfm(const AName, AClass: String; ALeft, ATop, AWidth,
  AHeight: Integer; AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign;
  ACaption: String; const AMore: array of string): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, 10+Length(AMore));
  Result[ 0] := 'object '+AName+': '+AClass;
  Result[ 1] := '  Left = '+IntToStr(ALeft);
  Result[ 2] := '  Top  = '+IntToStr(ATop);
  Result[ 3] := '  Width  = '+IntToStr(AWidth);
  Result[ 4] := '  Height = '+IntToStr(AHeight);
  Result[ 5] := '  AutoSize = '+DbgS(AnAutoSize);
  Result[ 6] := '  Caption = '''+ACaption+'''';
  Result[ 7] := '  Anchors = '+dbgs(AnAnchors);
  Result[ 8] := '  Align = '+dbgs(AnAlign);
  for i := 0 to Length(AMore)-1 do
    Result[9+i] := '  '+AMore[i];
  Result[ 9+Length(AMore)] := 'end';
end;

function TTestDpiScaling.BuildFormLfm(const AName, AClass: String; ALeft, ATop, AWidth,
  AHeight: Integer; APPI: Integer; const AMore: array of string): TStringArray;
begin
  Result := BuildLfm(AName, AClass,
                     ALeft, ATop, AWidth, AHeight, False, [akLeft, akTop], alNone,'',
                     jn(['DesignTimePPI = '+IntToStr(APPI)],
                        AMore));
end;

procedure TTestDpiScaling.AssertEquals(AMessage: string; const AMsgParam: array of const;
  Expected, Actual: integer; AnApprox: boolean; AnThreshold: integer);
begin
  if AnApprox or FForceApprox then begin
    if abs(Expected - Actual) <= Max(AnThreshold, FForceApproxThreshold) then exit;
    AMessage := 'Approx: '+ AMessage;
  end
  else
    if Expected = Actual then exit;
  AssertEquals(Format(AMessage, AMsgParam), Expected, Actual);
end;

procedure TTestDpiScaling.AssertPos(const AName: String; AControl: TControl; ExpLeft,
  ExpTop: Integer);
var
  IsRight, IsCenter, IsApprox, IsUnknown: boolean;
begin
  ExpLeft := DecodePos(ExpLeft, IsRight, IsCenter, IsApprox, IsUnknown);
  if not IsUnknown then begin
    if IsRight then
      AssertEquals('%s Right (%s, %s)', [AName, AControl.ClassName, AControl.Name],   ExpLeft,   AControl.Left + AControl.Width, IsApprox)
    else
    if IsCenter then
      AssertEquals('%s X-Center (%s, %s)', [AName, AControl.ClassName, AControl.Name],   ExpLeft,   AControl.Left + AControl.Width div 2, IsApprox)
    else
      AssertEquals('%s Left (%s, %s)', [AName, AControl.ClassName, AControl.Name],   ExpLeft,   AControl.Left, IsApprox);
  end;

  ExpTop := DecodePos(ExpTop, IsRight, IsCenter, IsApprox, IsUnknown);
  if not IsUnknown then begin
    if IsRight then
      AssertEquals('%s Bottom (%s, %s)', [AName, AControl.ClassName, AControl.Name],    ExpTop,    AControl.Top + AControl.Height, IsApprox)
    else
    if IsCenter then
      AssertEquals('%s Y-Center (%s, %s)', [AName, AControl.ClassName, AControl.Name],    ExpTop,    AControl.Top + AControl.Height div 2, IsApprox)
    else
      AssertEquals('%s Top (%s, %s)', [AName, AControl.ClassName, AControl.Name],    ExpTop,    AControl.Top, IsApprox);
  end;
end;

procedure TTestDpiScaling.AssertSize(const AName: String; AControl: TControl; ExpWidth,
  ExpHeight: Integer);
var
  IsRight, IsCenter, IsApprox, IsUnknown: boolean;
begin
  ExpWidth := DecodePos(ExpWidth, IsRight, IsCenter, IsApprox, IsUnknown);
  if not IsUnknown then
    AssertEquals('%s Width (%s, %s)', [AName, AControl.ClassName, AControl.Name], ExpWidth,  AControl.Width, IsApprox);

  ExpHeight := DecodePos(ExpHeight, IsRight, IsCenter, IsApprox, IsUnknown);
  if not IsUnknown then
    AssertEquals('%s Height (%s, %s)', [AName, AControl.ClassName, AControl.Name], ExpHeight, AControl.Height, IsApprox);
end;

procedure TTestDpiScaling.AssertRightBottom(const AName: String; AControl: TControl; ExpRight,
  ExpBottom: Integer);
var
  IsRight, IsCenter, IsApprox, IsUnknown: boolean;
begin
  ExpRight := DecodePos(ExpRight, IsRight, IsCenter, IsApprox, IsUnknown);
  if not IsUnknown then
    AssertEquals('%s Right (%s, %s)', [AName, AControl.ClassName, AControl.Name],   ExpRight,   AControl.Left + AControl.Width, IsApprox);

  ExpBottom := DecodePos(ExpBottom, IsRight, IsCenter, IsApprox, IsUnknown);
  if not IsUnknown then
    AssertEquals('%s Bottom (%s, %s)', [AName, AControl.ClassName, AControl.Name],    ExpBottom,    AControl.Top + AControl.Height, IsApprox);
end;

procedure TTestDpiScaling.AssertBounds(const AName: String; AControl: TControl; ExpLeft, ExpTop,
  ExpWidth, ExpHeight: Integer);
begin
  AssertPos(AName, AControl, ExpLeft, ExpTop);
  AssertSize(AName, AControl, ExpWidth, ExpHeight);
end;

procedure TTestDpiScaling.SendNewDPI(APPI: integer; TheForm: TForm);
var
  m: TLMessage;
begin
  m.msg := LM_DPICHANGED;
  m.lParam := 0;
  m.wParam := Cardinal(APPI) + (Cardinal(APPI) << 16);
  TheForm.Dispatch(m);
end;

procedure TTestDpiScaling.ScaleLfm_Anchor;

  procedure GetChildBounds(ALeftTopIdx, ARightBottomIdx, AParentWidthHeight: Integer;
    out ALeftTop, ARightBottom: integer);
  begin
    case ALeftTopIdx of
      0: ALeftTop := -100;
      1: ALeftTop := 0;
      2: ALeftTop := 50;
      3: ALeftTop := AParentWidthHeight - 100;
      4: ALeftTop := AParentWidthHeight - 1;
      5: ALeftTop := AParentWidthHeight;
      6: ALeftTop := AParentWidthHeight + 50;
    end;
    case ARightBottomIdx of
      0: ARightBottom := -50;
      1: ARightBottom := 0;
      2: ARightBottom := 100;
      3: ARightBottom := AParentWidthHeight - 50;
      4: ARightBottom := AParentWidthHeight;
      5: ARightBottom := AParentWidthHeight + 1;
      6: ARightBottom := AParentWidthHeight + 100;
    end;
  end;

var
  Anch: TAnchors;
type
  TAutoSizeAjust = (aaNone, aaParentAutoSize, aaParentAutoSizeGrow);

  procedure TestLabel(AName: String; ALbl: TLabel; ALblBnd: TRect; APW, APH: integer;
    AScale: double;
    AnIsAutosize: boolean; AnAdjust: TAutoSizeAjust);
  var
    x, y, r, b, xo, yo: integer;
  begin
    xo := 0; yo := 0;
    case AnAdjust of
      aaParentAutoSize: begin
        // Parent.Width dous not grow. Only move akLeft
        if (akLeft in Anch) or (AnAdjust = aaParentAutoSizeGrow) then xo := AUTOSIZE_OFFS;
        if (akTop in Anch)  or (AnAdjust = aaParentAutoSizeGrow) then yo := AUTOSIZE_OFFS;
      end;
      aaParentAutoSizeGrow: begin
        // Parent.Width grows by 2
        xo := AUTOSIZE_OFFS;
        yo := AUTOSIZE_OFFS;
        if Anch * [akLeft, akRight] = [akRight]  then xo := 2 * AUTOSIZE_OFFS; // moves one more, as it follows the width
        if Anch * [akTop, akBottom] = [akBottom] then yo := 2 * AUTOSIZE_OFFS;
      end;
    end;

    x := Round(ALblBnd.Left * AScale) + xo;
    y := Round(ALblBnd.Top  * AScale) + yo;
    r := MaxInt;
    b := MaxInt;
    if AnIsAutosize then begin
      if Anch * [akLeft, akRight] = [akRight]  then x := Rgt(Round(ALblBnd.Right * AScale) + xo);
      if Anch * [akTop, akBottom] = [akBottom] then y := Rgt(Round(ALblBnd.Bottom* AScale) + yo);
    end
    else begin
      r := Round(ALblBnd.Right *AScale) + xo;
      b := Round(ALblBnd.Bottom*AScale) + yo;
      if AnAdjust <> aaNone then begin
        r := Approx(r);  // Depending on anchors that may have grown/shrunk
        b := Approx(b);
      end;
    end;

    if Anch * [akLeft, akRight] = []  then begin
      // some code after the scaling messes with this
      x := Approx(x);
      r := Approx(r);
      if AnIsAutosize then begin
        x := Approx(Cnt(Round((ALblBnd.Left+ALblBnd.Right) / 2 * AScale) + xo));
        r := MaxInt;
      end;
    end;
    if Anch * [akTop, akBottom] = []  then begin
      y := Approx(y);
      b := Approx(b);
      if AnIsAutosize then begin
        y := Approx(Cnt(Round((ALblBnd.Top+ALblBnd.Bottom) / 2 * AScale) + yo));
        b := MaxInt;
      end;
    end;

    AssertPos(AName, ALbl, x, y);
    AssertRightBottom(AName, ALbl, r, b);
  end;

var
  NormPpi: integer;
  AnchIdx, CntrIdx: Integer;
  Scale: Integer;
  F: double;
  LabelLfm, SizeLfm, TheLfm: TStringArray;
  TheForm: TTestForm1;
  BndIdxLeft, BndIdxTop, BndIdxRight, BndIdxBottom: Integer;
  PWidth, PHeight: Integer;
  LblBnd, LblBnd2: TRect;
  x, y, ShrinkIdx: Integer;
  SkipAutoSizeLbl: Boolean;
  F2: Extended;
begin
  FForceApprox := False;
  GlobalTestFormOnCreate := nil;
  NormPpi := Screen.PixelsPerInch;


  for Scale := 1 to 5 do
  for AnchIdx := 0 to 3 do
  for CntrIdx := 0 to 7 do
  for BndIdxLeft  := 0 to 6 do
  for BndIdxRight :=  BndIdxLeft to 6 do
  for BndIdxTop    := 0 to 6 do
  for BndIdxBottom := BndIdxTop to 6 do
  begin
    FForceApprox := False;
    //Anch := ToAnchors(AnchIdx); // 0 to 15
    Anch := ToSameAnchors(AnchIdx);
    F := Scale;
    if Scale = 4 then F := 0.5;
    if Scale = 5 then F := 1/3;

    //if (BndIdxTop <> BndIdxLeft) or (BndIdxBottom <> BndIdxRight)  // horiz and vert size are independent
    //then
    //  continue;  // Don't run for all combinations


    (* Test all anchors with coordinates:
       outside the parent, at the exact border, inside first/second half
    *)

    SkipAutoSizeLbl := False;
    FForceApproxThreshold := 0;
    if (CntrIdx in [4..7]) then begin // Autosizing parent
      if (BndIdxLeft in [0,6]) or     // outside parent / don't resize
         (BndIdxRight in [0,5,6]) or
         (BndIdxTop in [0,6]) or
         (BndIdxBottom in [0,5,6])
      then
        continue;
      SkipAutoSizeLbl :=  // would autosize to be outside parent client
        (not(CntrIdx in [4,5])) and  // constrained
        ( (BndIdxRight in [4]) or
          (BndIdxBottom in [4]) );
    end;
    if (CntrIdx = 6) and
       ( (Anch * [akLeft, akRight] = []) or
         (Anch * [akTop, akBottom] = []) )
    then
      FForceApproxThreshold := 2;  // TODO: Because it moves in the container, when the container grows...

    // TODO:
    if (Scale >= 4) and
       ( (BndIdxLeft = 4) or (BndIdxRight = 5) or  // +1 or -1 // Currently can be 1 off
         (BndIdxTop = 4) or (BndIdxBottom = 5) )
    then
      continue;

    case CntrIdx of
      0..1,
      4:     begin PWidth  := 700;  PHeight := 500; end;
      2..3,
      5..7:  begin PWidth  := 600;  PHeight := 400; end;
    end;

    GetChildBounds(BndIdxLeft, BndIdxRight, PWidth,  LblBnd.Left, LblBnd.Right);
    GetChildBounds(BndIdxTop, BndIdxBottom, PHeight, LblBnd.Top, LblBnd.Bottom);

    LabelLfm := BuildLfm('Label1', 'TLabel',  LblBnd.Left,  LblBnd.Top,   LblBnd.Right-LblBnd.Left,  LblBnd.Bottom-LblBnd.Top,  False, Anch, alNone, 'abc');
    if not SkipAutoSizeLbl then
      LabelLfm := LabelLfm
      +  BuildLfm('Label2', 'TLabel',  LblBnd.Left,  LblBnd.Top,   LblBnd.Right-LblBnd.Left,  LblBnd.Bottom-LblBnd.Top,  True, Anch, alNone, 'abc');

    SizeLfm :=
       BuildLfm('LabelA', 'TLabel',  0,  0,   PWidth,  PHeight,  False, [akLeft, akTop], alNone)+
       BuildLfm('LabelB', 'TLabel',  0,  0,   PWidth,  PHeight,  False, [akRight, akBottom], alNone);

    (* Different containers. Those may be scaled or left to autosize. *)
    case CntrIdx of
      0: TheLfm := LabelLfm;  // no container
      1: TheLfm := BuildLfm('Panel1', 'TPanel',    0,   0,   700, 500,  False, akAll, alClient, LabelLfm);
      2: TheLfm := BuildLfm('Panel1', 'TPanel',   50,  50,   600, 400,  False, [akBottom, akRight], alNone, LabelLfm);
      3: TheLfm := BuildLfm('Panel1', 'TPanel',   50,  50,   600, 400,  False, akAll, alNone, LabelLfm);

      4: TheLfm := BuildLfm('Panel1', 'TPanel',    0,   0,   700, 500,  True, akAll, alClient, LabelLfm+SizeLfm);
      5: TheLfm := BuildLfm('Panel1', 'TPanel',   50,  50,   600, 400,  True, [akTop, akLeft], alNone,
        jn(['Constraints.MinWidth = 600',    'Constraints.MaxWidth = 600',
            'Constraints.MinHeight = 400',   'Constraints.MaxHeight = 400'
           ], LabelLfm+SizeLfm));
      6: TheLfm := BuildLfm('Panel1', 'TPanel',   50,  50,   600, 400,  True, [akBottom, akRight], alNone, LabelLfm+SizeLfm);
      7: TheLfm := BuildLfm('Panel1', 'TPanel',   50,  50,   600, 400,  True, akAll, alNone, LabelLfm+SizeLfm);
    end;



    TheForm := CreateTestForm('TTestForm1', TTestForm1,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0,  700, 500, Round(NormPpi/F), TheLfm)
    ) as TTestForm1;

    AssertSize('Created', TheForm, Round(700*F), Round(500*F));
    TheForm.Show;
    AssertSize('Created', TheForm, Round(700*F), Round(500*F));

    // Normally we should test Righ/Bottom, ranther than Width/Height. For the panels its ok, as there is no rounding
    case CntrIdx of
      0: ;
      1,4:  AssertBounds('Panel1', TheForm.Panel1, 0, 0, round(700*F), Round(500*F));
      2,3,
      5:    AssertBounds('Panel1', TheForm.Panel1, (Round(50*F)), (Round(50*F)), (round(600*F)), (Round(400*F)));
      // 6: Panel grows by 2 pixel due to autosize (and moves akRight)
      6:    AssertBounds('Panel1', TheForm.Panel1, Round(50*F)-2, Round(50*F)-2, round(600*F)+2, Round(400*F)+2);
      7:    AssertBounds('Panel1', TheForm.Panel1, (Round(50*F)), (Round(50*F)), (round(600*F)), (Round(400*F)));
    end;

    case CntrIdx of
      0..3: begin
        TestLabel('Label1', TheForm.Label1, LblBnd, PWidth, PHeight, F, False, aaNone);
        if not SkipAutoSizeLbl then
          TestLabel('Label2', TheForm.Label2, LblBnd, PWidth, PHeight, F, True, aaNone);
      end;
      6: begin
        TestLabel('Label1', TheForm.Label1, LblBnd, PWidth, PHeight, F, False, aaParentAutoSizeGrow);
        if not SkipAutoSizeLbl then
          TestLabel('Label2', TheForm.Label2, LblBnd, PWidth, PHeight, F, True, aaParentAutoSizeGrow);
      end;
      4..5,7: begin
        TestLabel('Label1', TheForm.Label1, LblBnd, PWidth, PHeight, F, False, aaParentAutoSize);
        if not SkipAutoSizeLbl then
          TestLabel('Label2', TheForm.Label2, LblBnd, PWidth, PHeight, F, True, aaParentAutoSize);
      end;
    end;

    if Scale > 4 then
      FForceApprox := True;
    if (CntrIdx < 4) and     // The movement by the autosize prevents restoring the orginal
       (Anch * [akLeft, akRight] <> []) and // something still moves them on each change
       (Anch * [akTop, akBottom] <> [])
    then begin
      if Scale = 1 then begin
        F2 := 2.5;

        SendNewDPI(Round(NormPpi*F2), TheForm);
        case CntrIdx of
          0: ;
          1:    AssertBounds('Panel1', TheForm.Panel1, 0, 0, round(700*F2), Round(500*F2));
          2,3:  AssertBounds('Panel1', TheForm.Panel1, (Round(50*F2)), (Round(50*F2)), (round(600*F2)), (Round(400*F2)));
        end;

        TestLabel('Label1', TheForm.Label1, LblBnd, PWidth, PHeight, F2, False, aaNone);
        if not SkipAutoSizeLbl then
          TestLabel('Label2', TheForm.Label2, LblBnd, PWidth, PHeight, F2, True, aaNone);
      end
      else begin

        SendNewDPI(Round(NormPpi/F), TheForm);
        case CntrIdx of
          0: ;
          1:    AssertBounds('Panel1', TheForm.Panel1, 0, 0, 700, 500);
          2,3:  AssertBounds('Panel1', TheForm.Panel1, 50, 50, 600, 400);
        end;
        TestLabel('Label1', TheForm.Label1, LblBnd, PWidth, PHeight, 1, False, aaNone);
        if not SkipAutoSizeLbl then
          TestLabel('Label2', TheForm.Label2, LblBnd, PWidth, PHeight, 1, True, aaNone);

        SendNewDPI(NormPpi, TheForm);
        case CntrIdx of
          0: ;
          1:    AssertBounds('Panel1', TheForm.Panel1, 0, 0, round(700*F), Round(500*F));
          2,3:  AssertBounds('Panel1', TheForm.Panel1, (Round(50*F)), (Round(50*F)), (round(600*F)), (Round(400*F)));
        end;

        TestLabel('Label1', TheForm.Label1, LblBnd, PWidth, PHeight, F, False, aaNone);
        if not SkipAutoSizeLbl then
          TestLabel('Label2', TheForm.Label2, LblBnd, PWidth, PHeight, F, True, aaNone);

      end;
    end;



    // reset dpi
    SendNewDPI(NormPpi, TheForm);
    TheForm.Free;
    FForceApprox := False;



    if Anch * [akLeft, akRight] = [] then Continue;
    if Anch * [akTop, akBottom] = [] then Continue;

    (* ************************************** *
     * Test with size changes in TForm.Create *
     * ************************************** *)
    if (Scale in [1..4]) then // Scale = 5 may get rounding errors
    for ShrinkIdx := 0 to 1 do
    begin
      if ShrinkIdx = 0 then
        GlobalTestFormOnCreate := @DoShrinkForm;
      TheForm := CreateTestForm('TTestForm1', TTestForm1,
        BuildFormLfm('TestForm1', 'TTestForm1', 0,0,  700, 500, Round(NormPpi/F), TheLfm)
      ) as TTestForm1;
      GlobalTestFormOnCreate := nil;
      if ShrinkIdx = 1 then begin
        // after scale / before show
        TheForm.Width  := TheForm.Width  - Round(40*F);
        TheForm.Height := TheForm.Height - Round(40*F);
      end;

      AssertSize('Created', TheForm, Round(660*F), Round(460*F));
      TheForm.Show;
      AssertSize('Created', TheForm, Round(660*F), Round(460*F));

      case CntrIdx of
        0: ;
        1,4:  AssertBounds('Panel1', TheForm.Panel1, 0, 0, round(660*F), Round(460*F));
        2:    AssertBounds('Panel1', TheForm.Panel1, (Round(10*F)), (Round(10*F)), (round(600*F)), (Round(400*F)));
        3:    AssertBounds('Panel1', TheForm.Panel1, (Round(50*F)), (Round(50*F)), (round(560*F)), (Round(360*F)));
        5:    AssertBounds('Panel1', TheForm.Panel1, (Round(50*F)), (Round(50*F)), (round(600*F)), (Round(400*F)));
        // 6: Panel grows by 2 pixel due to autosize (and moves akRight)
        6:    AssertBounds('Panel1', TheForm.Panel1, Approx(Round(10*F)-2), Approx(Round(10*F)-2), Approx(round(600*F)+2), Approx(Round(400*F)+2));
        7:    AssertBounds('Panel1', TheForm.Panel1, Approx(Round(50*F)), Approx(Round(50*F)), Approx(round(560*F)), Approx(Round(360*F)));
      end;

      LblBnd2 := LblBnd;
      if CntrIdx in [0, 1, 3, 4, 7] then begin
        if Anch * [akLeft, akRight] = [akRight]  then
          LblBnd2.Left := LblBnd2.Left - 40;
        if Anch * [akRight] = [akRight]  then
          LblBnd2.Right := Max(LblBnd2.Left, LblBnd2.Right - 40);

        if Anch * [akTop, akBottom] = [akBottom] then
          LblBnd2.Top  := LblBnd2.Top  - 40;
        if Anch * [akBottom] = [akBottom] then
          LblBnd2.Bottom  := Max(LblBnd2.Top, LblBnd2.Bottom  - 40);
      end;

      if CntrIdx < 4 then begin
        case CntrIdx of
          0..3: begin
            TestLabel('Label1', TheForm.Label1, LblBnd2, PWidth, PHeight, F, False, aaNone);
            if not SkipAutoSizeLbl then
              TestLabel('Label2', TheForm.Label2, LblBnd2, PWidth, PHeight, F, True, aaNone);
          end;
          6: begin
            TestLabel('Label1', TheForm.Label1, LblBnd2, PWidth, PHeight, F, False, aaParentAutoSizeGrow);
            if not SkipAutoSizeLbl then
              TestLabel('Label2', TheForm.Label2, LblBnd2, PWidth, PHeight, F, True, aaParentAutoSizeGrow);
          end;
          4..5,7: begin
            TestLabel('Label1', TheForm.Label1, LblBnd2, PWidth, PHeight, F, False, aaParentAutoSize);
            if not SkipAutoSizeLbl then
              TestLabel('Label2', TheForm.Label2, LblBnd2, PWidth, PHeight, F, True, aaParentAutoSize);
          end;
        end;
      end;

      TheForm.Free;
    end;

  end;

end;

procedure TTestDpiScaling.ScaleLfm_Align;
var
  NormPpi, HalfPpi, AnchIdx1, AnchIdx2, PnlIdx, Scale: Integer;
  Anch1, Anch2: TAnchors;
  Algn: TAlign;
  Pnl1Lfm, Pnl2Lfm, Pnl3Lfm, Pnl4Lfm, p: TStringArray;
  TheForm: TTestForm1;
  F: double;

  procedure CheckPanels(T: double);
  begin
    case Algn of
      alTop:    AssertBounds('Top',    TheForm.Panel1,   0,            0,   Round(700*T), Round(200*T));
      alBottom: AssertBounds('Bottom', TheForm.Panel1,   0, Round(300*T),   Round(700*T), Round(200*T));
      alLeft:   AssertBounds('Left',   TheForm.Panel1,   0,            0,   Round(300*T), Round(500*T));
      alRight:  AssertBounds('Right',  TheForm.Panel1, Round(400*T),   0,   Round(300*T), Round(500*T));
      alClient: AssertBounds('Client', TheForm.Panel1,   0,            0,   Round(700*T), Round(500*T));
    end;
    if PnlIdx in [2,3] then begin
      // Panel2: aligned to same side
      case Algn of
        alTop:    AssertBounds('P2.Top',    TheForm.Panel2,   0, Round(200*T),   Round(700*T), Round(100*T));
        alBottom: AssertBounds('P2.Bottom', TheForm.Panel2,   0, Round(200*T),   Round(700*T), Round(100*T));
        alLeft:   AssertBounds('P2.Left',   TheForm.Panel2, Round(300*T),   0,   Round(100*T), Round(500*T));
        alRight:  AssertBounds('P2.Right',  TheForm.Panel2, Round(300*T),   0,   Round(100*T), Round(500*T));
      end;
    end;
    if PnlIdx = 1 then begin
      // Panel3: alClient
      case Algn of
        alTop:    AssertBounds('P3.Top',    TheForm.Panel3,   0, Round(200*T),   Round(700*T), Round(300*T));
        alBottom: AssertBounds('P3.Bottom', TheForm.Panel3,   0,            0,   Round(700*T), Round(300*T));
        alLeft:   AssertBounds('P3.Left',   TheForm.Panel3, Round(300*T),   0,   Round(400*T), Round(500*T));
        alRight:  AssertBounds('P3.Right',  TheForm.Panel3,   0,            0,   Round(400*T), Round(500*T));
      end;
    end;
    if PnlIdx = 3 then begin
      // Panel3: alClient
      case Algn of
        alTop:    AssertBounds('P3.Top',    TheForm.Panel3,   0, Round(300*T),   Round(700*T), Round(200*T));
        alBottom: AssertBounds('P3.Bottom', TheForm.Panel3,   0,            0,   Round(700*T), Round(200*T));
        alLeft:   AssertBounds('P3.Left',   TheForm.Panel3, Round(400*T),   0,   Round(300*T), Round(500*T));
        alRight:  AssertBounds('P3.Right',  TheForm.Panel3,   0,            0,   Round(300*T), Round(500*T));
      end;
    end;
  end;

begin
  FForceApprox := False;
  FForceApproxThreshold := 0;
  GlobalTestFormOnCreate := nil;
  NormPpi := Screen.PixelsPerInch;
  HalfPpi := NormPpi div 2;

  for Scale := 1 to 4 do
  for AnchIdx1 := 0 to 15 do
  for AnchIdx2 := 0 to 15 do
  for Algn := low(TAlign) to high(TAlign) do
  for PnlIdx := 0 to 3 do // P1, P1+P3,  P1+P2, P1+P2+P4
  if Algn in [alTop, alBottom, alLeft, alRight {, alClient}] then
  begin
    if (PnlIdx < 2) and (AnchIdx2 > 0) then continue; // AnchIdx2 is not used, if the 2nd panel isn't created
    (* Anchors change align behaviour during form resizes.
       However during form scaling they do not take effect,
       since the proportions must be kept, and the control's
       size is dictated by the proportion.
       The position is given by the align.
       (So any extra anchors must be ignored in scaling)
       Issue #41007
    *)
    Anch1 := ToAnchors(AnchIdx1);
    Anch2 := ToAnchors(AnchIdx2);
    if (Scale > 2) and  // test non-effective anchors only with scale 1 and 2.
       ((Anch1 * AnchorAlign[Algn]) <> AnchorAlign[Algn]) then continue; // only add/remove the 4th anchor;
    if ((Anch2 * AnchorAlign[Algn]) <> AnchorAlign[Algn]) then continue; // 2nd panel, only add/remove the 4th anchor;

    // LFM loading of ALIGN resets anchors if the are loaded as [akLeft, akTop]
    if (Anch1 = [akLeft, akTop]) or (Anch2 = [akLeft, akTop]) then continue;

    case Algn of
      alTop:    begin Pnl1Lfm := BuildLfm('Panel1', 'TPanel',   0,  0,  700, 200, False, Anch1, Algn, []);
                      Pnl2Lfm := BuildLfm('Panel2', 'TPanel',   0,200,  700, 100, False, Anch2, Algn, []);
                      Pnl3Lfm := BuildLfm('Panel3', 'TPanel',   0,200,  700, 300, False, akAll, alClient, []);
                      Pnl4Lfm := BuildLfm('Panel3', 'TPanel',   0,300,  700, 200, False, akAll, alClient, []);
                end;
      alBottom: begin Pnl1Lfm := BuildLfm('Panel1', 'TPanel',   0,300,  700, 200, False, Anch1, Algn, []);
                      Pnl2Lfm := BuildLfm('Panel2', 'TPanel',   0,200,  700, 100, False, Anch2, Algn, []);
                      Pnl3Lfm := BuildLfm('Panel3', 'TPanel',   0,  0,  700, 300, False, akAll, alClient, []);
                      Pnl4Lfm := BuildLfm('Panel3', 'TPanel',   0,  0,  700, 200, False, akAll, alClient, []);
                end;
      alLeft:   begin Pnl1Lfm := BuildLfm('Panel1', 'TPanel',   0,  0,  300, 500, False, Anch1, Algn, []);
                      Pnl2Lfm := BuildLfm('Panel2', 'TPanel', 300,  0,  100, 500, False, Anch2, Algn, []);
                      Pnl3Lfm := BuildLfm('Panel3', 'TPanel', 300,  0,  400, 500, False, akAll, alClient, []);
                      Pnl4Lfm := BuildLfm('Panel3', 'TPanel', 400,  0,  300, 500, False, akAll, alClient, []);
                end;
      alRight:  begin Pnl1Lfm := BuildLfm('Panel1', 'TPanel', 400,  0,  300, 500, False, Anch1, Algn, []);
                      Pnl2Lfm := BuildLfm('Panel2', 'TPanel', 300,  0,  100, 500, False, Anch2, Algn, []);
                      Pnl3Lfm := BuildLfm('Panel3', 'TPanel',   0,  0,  400, 500, False, akAll, alClient, []);
                      Pnl4Lfm := BuildLfm('Panel3', 'TPanel',   0,  0,  300, 500, False, akAll, alClient, []);
                end;
      alClient: begin Pnl1Lfm := BuildLfm('Panel1', 'TPanel',   0,  0,  700, 500, False, Anch1, Algn, []);
      //                Pnl2Lfm := BuildLfm('Panel2', 'TPanel',   0,  0,  700, 500, False, Anch2, Algn, []);
      //                Pnl3Lfm := BuildLfm('Panel3', 'TPanel',   0,  0,  700, 500, False, akAll, alClient, []);
      //                Pnl4Lfm := BuildLfm('Panel3', 'TPanel',   0,  0,  700, 500, False, akAll, alClient, []);
                end;
    end;

    case PnlIdx of
      0: p := Pnl1Lfm;
      1: p := Pnl1Lfm + Pnl3Lfm;
      2: p := Pnl1Lfm + Pnl2Lfm;
      3: p := Pnl1Lfm + Pnl2Lfm + Pnl4Lfm;
    end;

    F := Scale;
    if Scale = 4 then F := 0.5;

    TheForm := CreateTestForm('TTestForm1', TTestForm1,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0, 700,500, Round(NormPpi/F), p)
    ) as TTestForm1;

    AssertSize('Created', TheForm, Round(700*F), Round(500*F));
    TheForm.Show;
    AssertSize('Created', TheForm, Round(700*F), Round(500*F));

    CheckPanels(F);


    SendNewDPI(Round(NormPpi/F), TheForm);
    CheckPanels(1);

    SendNewDPI(NormPpi, TheForm);
    CheckPanels(F);


    (* Test AUTO-SIZE: anchors affect aligned controls.
       This checks that anchors (on the free site) have the expected effect for
       the "resize in Create" test below
    *)

    if Scale = 2 then begin
      //TheForm.DisableAutoSizing;
      TheForm.Width  := TheForm.Width  - 80; // substract 80 / the form is already scaled
      TheForm.Height := TheForm.Height - 80;
      //TheForm.EnableAutoSizing;
      case Algn of
        alTop:
          if akBottom in Anch1 then  AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  320)
          else                      AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  400);
        alBottom:
          if akTop in Anch1 then     AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1320,  320)
          else                      AssertBounds('Bottom', TheForm.Panel1,   0, 520,  1320,  400);
        alLeft:
          if akRight in Anch1 then AssertBounds('Left',   TheForm.Panel1,   0,   0,   520,  920)
          else                    AssertBounds('Left',   TheForm.Panel1,   0,   0,   600,  920);
        alRight:
          if akLeft in Anch1 then  AssertBounds('Right',  TheForm.Panel1, 800,   0,   520,  920)
          else                    AssertBounds('Right',  TheForm.Panel1, 720,   0,   600,  920);
        alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  1320,  920);
      end;
    end;

    TheForm.Free;

    (* Anchors must take effect, if the form was resized *BEFORE* it got scaled
       E.g., if an alTop control has akBottom and the form resizes in "Create"
       then the height of the control must de/increase before scaling (and the new
       height must be scaled)
    *)
    GlobalTestFormOnCreate := @DoShrinkForm;
    TheForm := CreateTestForm('TTestForm1', TTestForm1,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0, 700,500, HalfPpi, Pnl1Lfm)
    ) as TTestForm1;
    GlobalTestFormOnCreate := nil;

    AssertSize('Created', TheForm, 1320, 920);
    TheForm.Show;
    AssertSize('Created', TheForm, 1320, 920);

    case Algn of
      alTop:
        if akBottom in Anch1 then  AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  320)   // unscaled T=0 H=160
        else                      AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  400);
      alBottom:
        if akTop in Anch1 then     //AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1320,  320)   // unscaled T=300 H=160
        else                      AssertBounds('Bottom', TheForm.Panel1,   0, 520,  1320,  400);
      alLeft:
        if akRight in Anch1 then AssertBounds('Left',   TheForm.Panel1,   0,   0,   520,  920)  // unscaled L=0 W=260
        else                    AssertBounds('Left',   TheForm.Panel1,   0,   0,   600,  920);
      alRight:
        if akLeft in Anch1 then  //AssertBounds('Right',  TheForm.Panel1, 800,   0,   520,  920)  // unscaled L=400 W=260
        else                    AssertBounds('Right',  TheForm.Panel1, 720,   0,   600,  920);
      alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  1320,  920);
    end;
    TheForm.Free;


    (* Change size after scale, but before show
       !!! This does not work on any Autosize control, since they aren't scaled yet.
    *)
    TheForm := CreateTestForm('TTestForm1', TTestForm1,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0, 700,500, HalfPpi, Pnl1Lfm)
    ) as TTestForm1;
    TheForm.Width  := TheForm.Width  - 80; // substract 80 / the form is already scaled
    TheForm.Height := TheForm.Height - 80;

    AssertSize('Created', TheForm, 1320, 920);
    TheForm.Show;
    AssertSize('Created', TheForm, 1320, 920);

    case Algn of
      alTop:
        if akBottom in Anch1 then  AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  320)   // unscaled T=0 H=160
        else                      AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  400);
      alBottom:
        if akTop in Anch1 then     //AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1320,  320)   // unscaled T=300 H=160
        else                      AssertBounds('Bottom', TheForm.Panel1,   0, 520,  1320,  400);
      alLeft:
        if akRight in Anch1 then AssertBounds('Left',   TheForm.Panel1,   0,   0,   520,  920)  // unscaled L=0 W=260
        else                    AssertBounds('Left',   TheForm.Panel1,   0,   0,   600,  920);
      alRight:
        if akLeft in Anch1 then  //AssertBounds('Right',  TheForm.Panel1, 800,   0,   520,  920)  // unscaled L=400 W=260
        else                    AssertBounds('Right',  TheForm.Panel1, 720,   0,   600,  920);
      alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  1320,  920);
    end;
    TheForm.Free;



  end;
end;



initialization

  {$IFnDEF LCLNOGUI}
  RegisterTest(TTestDpiScaling);
  {$ENDIF}
end.

