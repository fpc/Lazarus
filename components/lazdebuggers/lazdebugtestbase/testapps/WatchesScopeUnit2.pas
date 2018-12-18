unit WatchesScopeUnit2;
{$H-}

interface

uses sysutils, Classes;

type

  { TClassMainBaseBase }

  TClassMainBaseBase = class
  private
    Int_TClassMainBaseBase_Priv: Integer;
    Int_HideTest_Class: Integer;
  protected
    Int_TClassMainBaseBase_Prot: Integer;
  public
    Int_TClassMainBaseBase: Integer;
    procedure MethodMainBaseBase;
    procedure MethodMainBase; virtual;
  end;

procedure Unit2Init;

var
  Int_GlobalUnit2: Integer;
  Int_HideTest_Class: Integer;
  Int_HideTest_Unit: Integer;
  BreakDummy2: Integer;

implementation

procedure Unit2Init;
begin
  Int_HideTest_Class := 2000;
  Int_HideTest_Unit := 2010;
end;

{ TClassMainBaseBase }

procedure TClassMainBaseBase.MethodMainBaseBase;
begin
  Int_TClassMainBaseBase      := 270;
  Int_TClassMainBaseBase_Prot := 271;
  Int_TClassMainBaseBase_Priv := 272;

  Int_HideTest_Class := 2001;

  MethodMainBase; // call inherited class
  BreakDummy2 := 1; // TEST_BREAKPOINT=MethodMainBaseBase
end;

procedure TClassMainBaseBase.MethodMainBase;
begin
  //
end;

end.

