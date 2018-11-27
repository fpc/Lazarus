unit WatchesScopeUnit1;
{$H-}

interface

uses sysutils, Classes, WatchesScopeUnit2;

type

  { TClassMainBase }

  TClassMainBase = class(TClassMainBaseBase)
  private
    Int_TClassMainBase_Priv: Integer;
    Int_HideTest_Class: Integer;
  protected
    Int_TClassMainBase_Prot: Integer;
  public
    Int_TClassMainBase: Integer;
    procedure MethodMainBase; override;
    procedure MethodMain; virtual;
  end;

procedure Unit1Init;

var
  Int_GlobalUnit1: Integer;
  Int_HideTest_Class: Integer;
  Int_HideTest_Unit: Integer;
  BreakDummy1: Integer;

implementation

procedure Unit1Init;
begin
  Int_HideTest_Class := 1000;
  Int_HideTest_Unit := 1010;
end;

{ TClassMainBase }

procedure TClassMainBase.MethodMainBase;
begin
  Int_TClassMainBase      := 170;
  Int_TClassMainBase_Prot := 171;
  Int_TClassMainBase_Priv := 172;

  Int_HideTest_Class := 1001;

  MethodMain; // call inherited class
  BreakDummy1 := 1; // TEST_BREAKPOINT=MethodMainBase
end;

procedure TClassMainBase.MethodMain;
begin
  //
end;

end.

