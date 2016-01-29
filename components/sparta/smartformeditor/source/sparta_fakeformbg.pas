{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_FakeFormBG;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, sparta_FakeForm, sparta_FakeFormBackground, sparta_DesignedForm,
  sparta_InterfacesMDI;

type

  { TFakeFormBG }

  TFakeFormBG = class(TFakeForm, IDesignedFormBackground)
  private
    FBackground: IDesignedFormBackground;
  public
    property Background: IDesignedFormBackground read FBackground implements IDesignedFormBackground;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  end;

implementation

{ TFakeFormBG }

constructor TFakeFormBG.CreateNew(AOwner: TComponent; Num: Integer);
begin
  FBackground := TfrFakeFormBackground.Create(DesignedForm, Self);
  FBackground._AddRef;

  inherited CreateNew(AOwner, Num);
end;

destructor TFakeFormBG.Destroy;
var
  I: IInterfaceComponentReference;
begin
  inherited;
  FBackground.QueryInterface(IInterfaceComponentReference, I); // only way to omit SIGSEGV
  I.GetComponent.Free;
  Pointer(I) := nil; // omit _Release (Free is above)
  Pointer(FBackground) := nil; // omit _Release (Free is above)
end;

end.

