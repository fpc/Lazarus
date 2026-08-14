unit TestBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, Controls, fpcunit;

type

  (* Test versions of LCL components
     - keep track of calls to SetBounds: Allow to test how often this gets called in a single autosize.
  *)

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

  { TTestPanel }

  TTestPanel = class(TPanel)
  protected
    FDoSetBoundsCount: integer;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TTestLabel }

  TTestLabel = class(TLabel)
  protected
    FDoSetBoundsCount: integer;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  (* Common test helpers *)

  TExpectedInt = record
    vmin, vmax: integer;
  end;

  { TTestBase }

  TTestBase = class(TTestCase)
  protected type
    TCreateFlag = (cfFreeOld, cfNoAutoSize);
    TCreateFlags = set of TCreateFlag;
  protected
    FTestForm: TForm;

  public
    // New Asserts
    class procedure AssertTrue(const AMessage: string; ACondition: boolean; AnErrorAddress: Pointer = nil); overload;
    class procedure AssertTrue(const AMessage: string; const AFormatArgs: array of const; ACondition: boolean; AnErrorAddress: Pointer = nil); overload;
    class procedure AssertFalse(const AMessage: string; ACondition: boolean; AnErrorAddress: Pointer = nil); overload;
    class procedure AssertFalse(const AMessage: string; const AFormatArgs: array of const; ACondition: boolean; AnErrorAddress: Pointer = nil); overload;
    class procedure AssertEquals(const AMessage: string; Expected: TExpectedInt; Actual: integer); overload;
    class procedure AssertEquals(const AMessage: string; const AFormatArgs: array of const; Expected: TExpectedInt; Actual: integer); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: string); overload;
    class procedure AssertEquals(const AMessage: string; const AFormatArgs: array of const; Expected, Actual: string); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: Boolean); overload;
    class procedure AssertEquals(const AMessage: string; const AFormatArgs: array of const; Expected, Actual: Boolean); overload;
  public
    function CreateTestForm(x,y,w,h: integer): TForm;
    function CreateTestForm(AClass: TControlClass; x,y,w,h: integer): TForm;
    procedure CreateTestControl(var AVar; AClass: TControlClass; ABounds: TRect; ACreateFLags: TCreateFlags = []);
    procedure CreateTestControl(var AVar; AClass: TControlClass; ABounds: TRect; AParent: TWinControl; ACreateFLags: TCreateFlags = []);
    procedure CreateTestControl(var AVar; AClass: TControlClass; x,y,w,h: integer; ACreateFLags: TCreateFlags = []);
    procedure CreateTestControl(var AVar; AClass: TControlClass; x,y,w,h: integer; AParent: TWinControl; ACreateFLags: TCreateFlags = []);
  public
    procedure ResetSetBoundsCounts(ACtrl: TControl);
    procedure ResetSetBoundsCounts(ACtrl: array of TControl);
    procedure AssertSetBoundsCount(AName: String; ACtrl: TControl; AnExpSetBoundCount: TExpectedInt);
    procedure AssertBounds(AName: String; ACtrl: TControl; const b: TRect);
    procedure AssertBounds(AName: String; ACtrl: TControl; const b: TRect; AnExpSetBoundCount: TExpectedInt);
    procedure AssertBounds(AName: String; ACtrl: TControl; x,y,w,h: integer);
    procedure AssertBounds(AName: String; ACtrl: TControl; x,y,w,h: integer; AnExpSetBoundCount: TExpectedInt);
  end;

const
  SKIP_TST = low(integer);

function ToAnchors(i: integer): TAnchors;

operator := (i: integer): TExpectedInt;
operator := (i: array of integer): TExpectedInt;
operator := (e: TExpectedInt): String;
operator =  (i: integer; e: TExpectedInt): Boolean;
operator <> (i: integer; e: TExpectedInt): Boolean;

implementation

function ToAnchors(i: integer): TAnchors;
begin
  Result := [];
  if (i and 1) <> 0 then Result := Result + [akLeft];
  if (i and 2) <> 0 then Result := Result + [akTop];
  if (i and 4) <> 0 then Result := Result + [akRight];
  if (i and 8) <> 0 then Result := Result + [akBottom];
end;

operator := (i: integer): TExpectedInt;
begin
  Result.vmin := i;
  Result.vmax := i;
end;

operator := (i: array of integer): TExpectedInt;
begin
  assert(Length(i)=2, ':=: Length(i)=2');
  Result.vmin := i[0];
  Result.vmax := i[1];
end;

operator := (e: TExpectedInt): String;
begin
  Result := IntToStr(e.vmin);
  if e.vmax <> e.vmin then
    Result := Result + '..' + IntToStr(e.vmax);
end;

operator<>(i: integer; e: TExpectedInt): Boolean;
begin
  Result := (i < e.vmin) or (i > e.vmin);
end;

operator = (i: integer; e: TExpectedInt): Boolean;
begin
  Result := (i >= e.vmin) and (i <= e.vmax);
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

{ TTestPanel }

procedure TTestPanel.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inc(FDoSetBoundsCount);
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
end;

{ TTestLabel }

procedure TTestLabel.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inc(FDoSetBoundsCount);
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
end;

class procedure TTestBase.AssertTrue(const AMessage: string; ACondition: boolean;
  AnErrorAddress: Pointer);
begin
  if ACondition then exit;
  inherited AssertTrue(AMessage, ACondition, AnErrorAddress);
end;

class procedure TTestBase.AssertTrue(const AMessage: string; const AFormatArgs: array of const;
  ACondition: boolean; AnErrorAddress: Pointer);
begin
  if ACondition then exit;
  inherited AssertTrue(Format(AMessage, AFormatArgs), ACondition, AnErrorAddress);
end;

class procedure TTestBase.AssertFalse(const AMessage: string; ACondition: boolean;
  AnErrorAddress: Pointer);
begin
  if not ACondition then exit;
  inherited AssertFalse(AMessage, ACondition, AnErrorAddress);
end;

class procedure TTestBase.AssertFalse(const AMessage: string; const AFormatArgs: array of const;
  ACondition: boolean; AnErrorAddress: Pointer);
begin
  if not ACondition then exit;
  inherited AssertFalse(Format(AMessage, AFormatArgs), ACondition, AnErrorAddress);
end;

class procedure TTestBase.AssertEquals(const AMessage: string; Expected: TExpectedInt;
  Actual: integer);
begin
  if Actual = Expected then exit;
  inherited AssertEquals(Expected, IntToStr(Actual));
end;

class procedure TTestBase.AssertEquals(const AMessage: string; const AFormatArgs: array of const;
  Expected: TExpectedInt; Actual: integer);
begin
  if Actual = Expected then exit;
  inherited AssertEquals(Format(AMessage, AFormatArgs), Expected, IntToStr(Actual));
end;

class procedure TTestBase.AssertEquals(const AMessage: string; Expected, Actual: string);
begin
  if Actual = Expected then exit;
  inherited AssertEquals(AMessage, Expected, Actual);
end;

class procedure TTestBase.AssertEquals(const AMessage: string; const AFormatArgs: array of const;
  Expected, Actual: string);
begin
  if Actual = Expected then exit;
  inherited AssertEquals(Format(AMessage, AFormatArgs), Expected, Actual);
end;

class procedure TTestBase.AssertEquals(const AMessage: string; Expected, Actual: Boolean);
begin
  if Actual = Expected then exit;
  inherited AssertEquals(AMessage, Expected, Actual);
end;

class procedure TTestBase.AssertEquals(const AMessage: string; const AFormatArgs: array of const;
  Expected, Actual: Boolean);
begin
  if Actual = Expected then exit;
  inherited AssertEquals(Format(AMessage, AFormatArgs), Expected, Actual);
end;

function TTestBase.CreateTestForm(x, y, w, h: integer): TForm;
begin
  Result := CreateTestForm(TForm, x, y, w, h);
end;

function TTestBase.CreateTestForm(AClass: TControlClass; x, y, w, h: integer): TForm;
begin
  FTestForm.Free;
  FTestForm := TForm.CreateNew(nil);
  FTestForm.SetBounds(x,y, w,h);
  Result := FTestForm;
end;

procedure TTestBase.CreateTestControl(var AVar; AClass: TControlClass; ABounds: TRect;
  ACreateFLags: TCreateFlags);
begin
  CreateTestControl(AVar, AClass, ABounds.Left, ABounds.Top, ABounds.Width, ABounds.Height, ACreateFLags);
end;

procedure TTestBase.CreateTestControl(var AVar; AClass: TControlClass; ABounds: TRect;
  AParent: TWinControl; ACreateFLags: TCreateFlags);
begin
  CreateTestControl(AVar, AClass, ABounds.Left, ABounds.Top, ABounds.Width, ABounds.Height, AParent, ACreateFLags);
end;

procedure TTestBase.CreateTestControl(var AVar; AClass: TControlClass; x, y, w, h: integer;
  ACreateFLags: TCreateFlags);
begin
  CreateTestControl(AVar, AClass, x, y, w, h, FTestForm, ACreateFLags);
end;

procedure TTestBase.CreateTestControl(var AVar; AClass: TControlClass; x, y, w, h: integer;
  AParent: TWinControl; ACreateFLags: TCreateFlags);
var
  C: TControl;
begin
  if cfFreeOld in ACreateFLags then begin
    TControl(AVar).Free;
    Application.ProcessMessages;
  end;

  C := AClass.Create(FTestForm);
  if cfNoAutoSize in ACreateFLags then
    C.AutoSize := False;
  C.SetBounds(x,y, w,h);
  C.Parent := AParent;
  Pointer(AVar) := Pointer(C);
end;

procedure TTestBase.ResetSetBoundsCounts(ACtrl: TControl);
begin
  ResetSetBoundsCounts([ACtrl]);
end;

procedure TTestBase.ResetSetBoundsCounts(ACtrl: array of TControl);
var
  c: TControl;
begin
  for c in ACtrl do begin
         if c is TTestLabel      then TTestLabel(c).FDoSetBoundsCount := 0
    else if c is TTestPanel      then TTestPanel(c).FDoSetBoundsCount := 0
    else if c is TTestTabControl then TTestTabControl(c).FDoSetBoundsCount := 0
    else if c is TTestScrollBox  then TTestScrollBox(c).FDoSetBoundsCount := 0
    ;
  end;
end;

procedure TTestBase.AssertSetBoundsCount(AName: String; ACtrl: TControl;
  AnExpSetBoundCount: TExpectedInt);
begin
  if SKIP_TST <> AnExpSetBoundCount then begin
         if ACtrl is TTestLabel      then AssertEquals('%s: SetBndCount', [AName], AnExpSetBoundCount, TTestLabel(ACtrl).FDoSetBoundsCount)
    else if ACtrl is TTestPanel      then AssertEquals('%s: SetBndCount', [AName], AnExpSetBoundCount, TTestPanel(ACtrl).FDoSetBoundsCount)
    else if ACtrl is TTestTabControl then AssertEquals('%s: SetBndCount', [AName], AnExpSetBoundCount, TTestTabControl(ACtrl).FDoSetBoundsCount)
    else if ACtrl is TTestScrollBox  then AssertEquals('%s: SetBndCount', [AName], AnExpSetBoundCount, TTestScrollBox(ACtrl).FDoSetBoundsCount)
    else
    AssertFalse('SetBoundCount unknown class',true);
  end;
end;

procedure TTestBase.AssertBounds(AName: String; ACtrl: TControl; const b: TRect);
begin
  AssertBounds(AName, ACtrl, b, -1);
end;

procedure TTestBase.AssertBounds(AName: String; ACtrl: TControl; const b: TRect;
  AnExpSetBoundCount: TExpectedInt);
begin
  AssertBounds(AName, ACtrl, b.Left, b.Top, b.Width, b.Height, AnExpSetBoundCount);
end;

procedure TTestBase.AssertBounds(AName: String; ACtrl: TControl; x, y, w, h: integer);
begin
  AssertBounds(AName, ACtrl, x, y, w, h, -1);
end;

procedure TTestBase.AssertBounds(AName: String; ACtrl: TControl; x, y, w, h: integer;
  AnExpSetBoundCount: TExpectedInt);
begin
  if x <> SKIP_TST then AssertEquals('%s: Left',   [AName], x, ACtrl.Left);
  if y <> SKIP_TST then AssertEquals('%s: Top',    [AName], y, ACtrl.Top);
  if w <> SKIP_TST then AssertEquals('%s: Width',  [AName], w, ACtrl.Width);
  if h <> SKIP_TST then AssertEquals('%s: Height', [AName], h, ACtrl.Height);

  if -1 <> AnExpSetBoundCount then
    AssertSetBoundsCount(AName, ACtrl, AnExpSetBoundCount);
end;


end.

