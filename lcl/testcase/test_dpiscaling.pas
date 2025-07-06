unit Test_DpiScaling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, LResources, ExtCtrls, Controls, LMessages, LazLoggerBase,
  System.UITypes, fpcunit, testutils, testregistry;

type

  { TTestForm1 }

  TTestForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
  end;

  { TTestForm1a }

  TTestForm1a = class(TTestForm1)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TTestDpiScaling }

  TTestDpiScaling = class(TTestCase)
  protected
    function CreateTestForm(AFormName: String; AFormCLass: TFormClass; ALfm: array of string): TForm;
    function jn(const L1, L2: array of string): TStringArray;
    function jn(const L1, L2, L3: array of string): TStringArray;
    function jn(const L1, L2, L3, L4: array of string): TStringArray;
    function jn(const L1, L2, L3, L4, L5: array of string): TStringArray;
    function jn(const L1, L2, L3, L4, L5, L6: array of string): TStringArray;
    function BuildLfm(AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      AnAutoSize: Boolean = False; AnAnchors: TAnchors = [akLeft, akTop]; AnAlign: TAlign = alNone
      ): TStringArray;
    function BuildLfm(AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign;
      const AMore: array of string
      ): TStringArray;
    function BuildFormLfm(AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
      APPI: Integer;
      const AMore: array of string
      ): TStringArray;

    procedure AssertSize(AName: String; AControl: TControl; ExpWidth, ExpHeight: Integer);
    procedure AssertPos (AName: String; AControl: TControl; ExpLeft, ExpTop: Integer);
    procedure AssertBounds(AName: String; AControl: TControl; ExpLeft, ExpTop, ExpWidth, ExpHeight: Integer);
    procedure SendNewDPI(APPI: integer; TheForm: TForm);
  published
    procedure ScaleLfm_Anchor;
    procedure ScaleLfm_Align;
  end;

implementation

const
  AUTOSIZE_OFFS = 1; // Children in autosized control start at top/left = 1,1

  RNG_BTM_RIGHT =  -100000;  // any smaller value should be compared to the control's right or bottom
  OFFS_BTM_RIGHT = 2*RNG_BTM_RIGHT;

function r(APos: integer): integer;
begin
  Result := APos + OFFS_BTM_RIGHT;
end;

{ TTestForm1a }

constructor TTestForm1a.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width  := Width  - 40;
  Height := Height - 40;
end;

{ TTestForm1 }

function TTestDpiScaling.CreateTestForm(AFormName: String; AFormCLass: TFormClass;
  ALfm: array of string): TForm;
var
  LfmTxt, s: String;
  LfmStream, LrsStream: TStringStream;
  r: TLResource;
begin
  LfmTxt := '';
  for s in ALfm do LfmTxt := LfmTxt + s + LineEnding;

  LfmStream := TStringStream.Create;
  LfmStream.WriteAnsiString(LfmTxt);
  LfmStream.Position := 0;

  LrsStream := TStringStream.Create;
  LRSObjectTextToBinary(LfmStream, LrsStream);

  LrsStream.Position := 0;
  r := LazarusResources.Find(AFormName, 'FORMDATA');
  if r <> nil then
    r.Value := LrsStream.DataString
  else
    LazarusResources.Add(AFormName, 'FORMDATA', LrsStream.DataString);

  Result := AFormClass.Create(nil);
end;

function TTestDpiScaling.jn(const L1, L2: array of string): TStringArray;
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

function TTestDpiScaling.jn(const L1, L2, L3: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2), L3);
end;

function TTestDpiScaling.jn(const L1, L2, L3, L4: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2), jn(L3, L4));
end;

function TTestDpiScaling.jn(const L1, L2, L3, L4, L5: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2, L3), jn(L4, L5));
end;

function TTestDpiScaling.jn(const L1, L2, L3, L4, L5, L6: array of string): TStringArray;
begin
  Result := jn(jn(L1, L2, L3), jn(L4, L5, L6));
end;

function TTestDpiScaling.BuildLfm(AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
  AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign): TStringArray;
begin
  Result := BuildLfm(AName, AClass, ALeft, ATop, AWidth, AHeight, AnAutoSize, AnAnchors,
      AnAlign, []);
end;

function TTestDpiScaling.BuildLfm(AName, AClass: String; ALeft, ATop, AWidth, AHeight: Integer;
  AnAutoSize: Boolean; AnAnchors: TAnchors; AnAlign: TAlign; const AMore: array of string
  ): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, 9+Length(AMore));
  Result[ 0] := 'object '+AName+': '+AClass;
  Result[ 1] := '  Left = '+IntToStr(ALeft);
  Result[ 2] := '  Top  = '+IntToStr(ATop);
  Result[ 3] := '  Width  = '+IntToStr(AWidth);
  Result[ 4] := '  Height = '+IntToStr(AHeight);
  Result[ 5] := '  AutoSize = '+DbgS(AnAutoSize);
  Result[ 6] := '  Anchors = '+dbgs(AnAnchors);
  Result[ 7] := '  Align = '+dbgs(AnAlign);
  for i := 0 to Length(AMore)-1 do
    Result[8+i] := '  '+AMore[i];
  Result[ 8+Length(AMore)] := 'end';
end;

function TTestDpiScaling.BuildFormLfm(AName, AClass: String; ALeft, ATop, AWidth,
  AHeight: Integer; APPI: Integer; const AMore: array of string): TStringArray;
begin
  Result := BuildLfm(AName, AClass,
                     ALeft, ATop, AWidth, AHeight, False, [akLeft, akTop], alNone,
                     jn(['DesignTimePPI = '+IntToStr(APPI)],
                        AMore));
end;

procedure TTestDpiScaling.AssertSize(AName: String; AControl: TControl; ExpWidth,
  ExpHeight: Integer);
begin
  AName := Format('%s (%s, %s)', [AName, AControl.ClassName, AControl.Name]);
  if ExpWidth<>MaxInt  then AssertEquals(AName+' Width',  ExpWidth,  AControl.Width);
  if ExpHeight<>MaxInt then AssertEquals(AName+' Height', ExpHeight, AControl.Height);
end;

procedure TTestDpiScaling.AssertPos(AName: String; AControl: TControl; ExpLeft, ExpTop: Integer);
begin
  AName := Format('%s (%s, %s)', [AName, AControl.ClassName, AControl.Name]);
  if ExpLeft < RNG_BTM_RIGHT then
    AssertEquals(AName+' Right',   ExpLeft-OFFS_BTM_RIGHT,   AControl.Left + AControl.Width)
  else
  if ExpLeft<>MaxInt then
    AssertEquals(AName+' Left',   ExpLeft,   AControl.Left);

  if ExpTop < RNG_BTM_RIGHT then
    AssertEquals(AName+' Bottom',    ExpTop-OFFS_BTM_RIGHT,    AControl.Top + AControl.Height)
  else
  if ExpTop<>MaxInt then
    AssertEquals(AName+' Top',    ExpTop,    AControl.Top);
end;

procedure TTestDpiScaling.AssertBounds(AName: String; AControl: TControl; ExpLeft, ExpTop,
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
var
  NormPpi, HalfPpi, AnchIdx: Integer;
  Anch: TAnchors;
  TheLfm: TStringArray;
  TheForm: TTestForm1;
  x, y, w, h: Integer;
begin
  NormPpi := Screen.PixelsPerInch;
  HalfPpi := NormPpi div 2;


  for AnchIdx := 0 to 15 do
  begin
    Anch := [];
    if (AnchIdx and 1) <> 0 then Anch := Anch + [akLeft];
    if (AnchIdx and 2) <> 0 then Anch := Anch + [akTop];
    if (AnchIdx and 4) <> 0 then Anch := Anch + [akRight];
    if (AnchIdx and 8) <> 0 then Anch := Anch + [akBottom];
if Anch * [akLeft, akRight] = [] then Continue;
if Anch * [akTop, akBottom] = [] then Continue;

    TheLfm :=
      jn(
        BuildLfm('Label1', 'TLabel',  100,  20,   200,  40,  False, Anch, alNone),
        BuildLfm('Label2', 'TLabel',  300,  28,    10,   2,  True,  Anch, alNone), // Autosize
        // Parent akRight
        BuildLfm('Panel1', 'TPanel',   20, 100,   600,  60,  False, [akBottom, akRight], alNone,
        jn(
          BuildLfm('Label3', 'TLabel',   10,  10,   60, 20,  False, Anch, alNone),
          BuildLfm('Label4', 'TLabel',  250,  28,   10,  2,  True,  Anch, alNone)
        )),
        // Parent Autosize + akRight
        BuildLfm('Panel2', 'TPanel',   20, 200,   600,  60,  True, [akBottom, akRight], alNone,
        jn(
          ['Constraints.MinHeight = 40',
           'Constraints.MinWidth = 400'  ],
          BuildLfm('Label5', 'TLabel',   10,  10,   60,  20, False, Anch, alNone),
          BuildLfm('Label6', 'TLabel',  250,  28,   10,   2, True,  Anch, alNone)
        )),
        // Parent akTop, akLeft, akBottom, akRight
        BuildLfm('Panel3', 'TPanel',   20, 300,   600,  60,  False, [akTop, akLeft, akBottom, akRight], alNone,
        jn(
          BuildLfm('Label7', 'TLabel',   10,  10,   60, 20,  False, Anch, alNone),
          BuildLfm('Label8', 'TLabel',  250,  28,   10,  2,  True,  Anch, alNone)
        ))
      );
    TheForm := CreateTestForm('TTestForm1', TTestForm1,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0,  700, 500, HalfPpi, TheLfm)
    ) as TTestForm1;

    AssertSize('Created', TheForm, 1400, 1000);
    TheForm.Show;
    AssertSize('Created', TheForm, 1400, 1000);

    AssertBounds('Panel1', TheForm.Panel1, 40, 200,  1200, 120);
    AssertBounds('Panel2', TheForm.Panel2, r(1240), r(520),   800,  80); // autosized
    AssertBounds('Panel3', TheForm.Panel3, 40, 600,  1200, 120);


    AssertBounds('Label1', TheForm.Label1, 200, 40,  400, 80);
    x := 600; if Anch * [akLeft, akRight] = [akRight]  then x := r(620);
    y :=  56; if Anch * [akTop, akBottom] = [akBottom] then y := r(60);
    AssertPos   ('Label2', TheForm.Label2, x, y);


    AssertBounds('Label3', TheForm.Label3, 20, 20,  120, 40);
    x := 500; if Anch * [akLeft, akRight] = [akRight]  then x := r(520);
    y :=  56; if Anch * [akTop, akBottom] = [akBottom] then y := r(60);
    AssertPos   ('Label4', TheForm.Label4, x, y);


(*
    // scaled RIGHT  L5=140  L6=520  Diff 380
    // scaled BOTTOM L5= 60  L6= 60
    x := AUTOSIZE_OFFS; if Anch * [akLeft, akRight] = [akRight]  then x := r(799-380+AUTOSIZE_OFFS);
    y := AUTOSIZE_OFFS; if Anch * [akTop, akBottom] = [akBottom] then y := r(79-AUTOSIZE_OFFS);
    AssertBounds('Label5', TheForm.Label5, x, y,  120, 40);
    // "20" the position of Label5 before it moved  // Caption='' -> size=1x1
    x := 500-20+AUTOSIZE_OFFS; if Anch * [akLeft, akRight] = [akRight]  then x := r(799+AUTOSIZE_OFFS);
    y :=  56-20+AUTOSIZE_OFFS; if Anch * [akTop, akBottom] = [akBottom] then y := r(79-AUTOSIZE_OFFS);
    AssertPos   ('Label6', TheForm.Label6, x, y);
//*)


    AssertBounds('Label7', TheForm.Label7, 20, 20,  120, 40);
    x := 500; if Anch * [akLeft, akRight] = [akRight]  then x := r(520);
    y :=  56; if Anch * [akTop, akBottom] = [akBottom] then y := r(60);
    AssertPos   ('Label8', TheForm.Label8, x, y);


    (* Re-scale while visible
    *)
    SendNewDPI(NormPpi * 2, TheForm);
    AssertSize('Created', TheForm, 2800, 2000);

    AssertBounds('Panel1', TheForm.Panel1, 80, 400,  2400, 240);
    AssertBounds('Panel2', TheForm.Panel2, r(2480), r(1040),  1600, 160); // autosized
    AssertBounds('Panel3', TheForm.Panel3, 80,1200,  2400, 240);


    AssertBounds('Label1', TheForm.Label1, 400, 80,  800,160);
    x :=1200; if Anch * [akLeft, akRight] = [akRight]  then x := r(1240);
    y := 112; if Anch * [akTop, akBottom] = [akBottom] then y := r(120);
    AssertPos   ('Label2', TheForm.Label2, x, y);


    AssertBounds('Label3', TheForm.Label3, 40, 40,  240, 80);
    x :=1000; if Anch * [akLeft, akRight] = [akRight]  then x := r(1040);
    y := 112; if Anch * [akTop, akBottom] = [akBottom] then y := r(120);
    AssertPos   ('Label4', TheForm.Label4, x, y);


(*
    // scaled RIGHT  L5=280  L6=1040  Diff 760
    // scaled BOTTOM L5= 120  L6= 120
    x := AUTOSIZE_OFFS; if Anch * [akLeft, akRight] = [akRight]  then x := r(1399-760+AUTOSIZE_OFFS);
    y := AUTOSIZE_OFFS; if Anch * [akTop, akBottom] = [akBottom] then y := r(159-AUTOSIZE_OFFS);
    AssertBounds('Label5', TheForm.Label5, x, y,  240, 80);
    // "40" the position of Label5 before it moved  // Caption='' -> size=1x1
    x :=1000-40+AUTOSIZE_OFFS; if Anch * [akLeft, akRight] = [akRight]  then x := r(1399+AUTOSIZE_OFFS);
    y := 112-40+AUTOSIZE_OFFS; if Anch * [akTop, akBottom] = [akBottom] then y := r(159-AUTOSIZE_OFFS);
    AssertPos   ('Label6', TheForm.Label6, x, y);
//*)


    AssertBounds('Label7', TheForm.Label7, 40, 40,  240, 80);
    x :=1000; if Anch * [akLeft, akRight] = [akRight]  then x := r(1040);
    y := 112; if Anch * [akTop, akBottom] = [akBottom] then y := r(120);
    AssertPos   ('Label8', TheForm.Label8, x, y);


    // reset dpi
    SendNewDPI(NormPpi, TheForm);
    TheForm.Free;

    ////
    TheForm := CreateTestForm('TTestForm1', TTestForm1a,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0,  700, 500, HalfPpi, TheLfm)
    ) as TTestForm1;

    AssertSize('Created', TheForm, 1320, 920);
    TheForm.Show;
    AssertSize('Created', TheForm, 1320, 920);

    AssertBounds('Panel1', TheForm.Panel1, 40, 200,  1200, 120);
    //AssertBounds('Panel2', TheForm.Panel2, r(1160), r(440),   800,  80); // autosized
    //AssertBounds('Panel3', TheForm.Panel3, 40, 600,  1120, 80);

    TheForm.Free;
  end;

end;

procedure TTestDpiScaling.ScaleLfm_Align;
var
  NormPpi, HalfPpi, AnchIdx: Integer;
  Anch: TAnchors;
  TheForm: TTestForm1;
  Algn: TAlign;
  PnlLfm: TStringArray;
begin
  NormPpi := Screen.PixelsPerInch;
  HalfPpi := NormPpi div 2;

  for AnchIdx := 0 to 15 do
  for Algn := low(TAlign) to high(TAlign) do
  if Algn in [alTop, alBottom, alLeft, alRight {, alClient}] then
  begin
    (* Anchors change align behaviour during form resizes.
       However during form scaling they do not take effect,
       since the proportions must be kept, and the control's
       size is dictated by the proportion.
       The position is given by the align.
       (So any extra anchors must be ignored in scaling)
       Issue #41007
    *)
    Anch := [];
    if (AnchIdx and 1) <> 0 then Anch := Anch + [akLeft];
    if (AnchIdx and 2) <> 0 then Anch := Anch + [akTop];
    //if (AnchIdx and 4) <> 0 then Anch := Anch + [akRight];
    //if (AnchIdx and 8) <> 0 then Anch := Anch + [akBottom];

    if Anch = [akLeft, akTop] then continue;// Broken in lfm loading

    case Algn of
      alTop:    PnlLfm := BuildLfm('Panel1', 'TPanel',   0,  0,  700, 200, False, Anch, Algn, []);
      alBottom: PnlLfm := BuildLfm('Panel1', 'TPanel',   0,300,  700, 200, False, Anch, Algn, []);
      alLeft:   PnlLfm := BuildLfm('Panel1', 'TPanel',   0,  0,  300, 500, False, Anch, Algn, []);
      alRight:  PnlLfm := BuildLfm('Panel1', 'TPanel', 400,  0,  300, 500, False, Anch, Algn, []);
      alClient: PnlLfm := BuildLfm('Panel1', 'TPanel',   0,  0,  700, 500, False, Anch, Algn, []);
    end;

    TheForm := CreateTestForm('TTestForm1', TTestForm1,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0, 700,500, HalfPpi, PnlLfm)
    ) as TTestForm1;

    AssertSize('Created', TheForm, 1400, 1000);
    TheForm.Show;
    AssertSize('Created', TheForm, 1400, 1000);

    case Algn of
      alTop:    AssertBounds('Top',    TheForm.Panel1,   0,   0,  1400,  400);
      alBottom: AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1400,  400);
      alLeft:   AssertBounds('Left',   TheForm.Panel1,   0,   0,   600, 1000);
      alRight:  AssertBounds('Right',  TheForm.Panel1, 800,   0,   600, 1000);
      alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  1400, 1000);
    end;

    SendNewDPI(NormPpi * 2, TheForm);
    case Algn of
      alTop:    AssertBounds('Top',    TheForm.Panel1,   0,   0,  2800,  800);
      alBottom: AssertBounds('Bottom', TheForm.Panel1,   0,1200,  2800,  800);
      alLeft:   AssertBounds('Left',   TheForm.Panel1,   0,   0,  1200, 2000);
      alRight:  AssertBounds('Right',  TheForm.Panel1,1600,   0,  1200, 2000);
      alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  2800, 2000);
    end;


    SendNewDPI(NormPpi, TheForm);
    case Algn of
      alTop:    AssertBounds('Top',    TheForm.Panel1,   0,   0,  1400,  400);
      alBottom: AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1400,  400);
      alLeft:   AssertBounds('Left',   TheForm.Panel1,   0,   0,   600, 1000);
      alRight:  AssertBounds('Right',  TheForm.Panel1, 800,   0,   600, 1000);
      alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  1400, 1000);
    end;

    (* Test AUTO-SIZE: anchors affect aligned controls.
       This checks that anchors (on the free site) have the expected effect for
       the "resize in Create" test below
    *)


    //TheForm.DisableAutoSizing;
    TheForm.Width  := TheForm.Width  - 80; // substract 80 / the form is already scaled
    TheForm.Height := TheForm.Height - 80;
    //TheForm.EnableAutoSizing;
    case Algn of
      alTop:
        if akBottom in Anch then  AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  320)
        else                      AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  400);
      alBottom:
        if akTop in Anch then     AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1320,  320)
        else                      AssertBounds('Bottom', TheForm.Panel1,   0, 520,  1320,  400);
      alLeft:
        if akRight in Anch then AssertBounds('Left',   TheForm.Panel1,   0,   0,   520,  920)
        else                    AssertBounds('Left',   TheForm.Panel1,   0,   0,   600,  920);
      alRight:
        if akLeft in Anch then  AssertBounds('Right',  TheForm.Panel1, 800,   0,   520,  920)
        else                    AssertBounds('Right',  TheForm.Panel1, 720,   0,   600,  920);
      alClient: AssertBounds('Client', TheForm.Panel1,   0,   0,  1320,  920);
    end;

    TheForm.Free;

    (* Anchors must take effect, if the form was resized *BEFORE* it got scaled
       E.g., if an alTop control has akBottom and the form resizes in "Create"
       then the height of the control must de/increase before scaling (and the new
       height must be scaled)
    *)
    TheForm := CreateTestForm('TTestForm1', TTestForm1a,
      BuildFormLfm('TestForm1', 'TTestForm1', 0,0, 700,500, HalfPpi, PnlLfm)
    ) as TTestForm1;

    AssertSize('Created', TheForm, 1320, 920);
    TheForm.Show;
    AssertSize('Created', TheForm, 1320, 920);

    case Algn of
      alTop:
        if akBottom in Anch then  AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  320)   // unscaled T=0 H=160
        else                      AssertBounds('Top',    TheForm.Panel1,   0,   0,  1320,  400);
      alBottom:
        if akTop in Anch then     //AssertBounds('Bottom', TheForm.Panel1,   0, 600,  1320,  320)   // unscaled T=300 H=160
        else                      AssertBounds('Bottom', TheForm.Panel1,   0, 520,  1320,  400);
      alLeft:
        if akRight in Anch then AssertBounds('Left',   TheForm.Panel1,   0,   0,   520,  920)  // unscaled L=0 W=260
        else                    AssertBounds('Left',   TheForm.Panel1,   0,   0,   600,  920);
      alRight:
        if akLeft in Anch then  //AssertBounds('Right',  TheForm.Panel1, 800,   0,   520,  920)  // unscaled L=400 W=260
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

