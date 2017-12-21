unit opkman_showhintbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TfrShowHint }

  TfrShowHint = class(TFrame)
    lbDescription: TLabel;
    lbLicense: TLabel;
    mDescription: TMemo;
    mLicense: TMemo;
    pnPackageName: TPanel;
    pnBase: TPanel;
    pnDescription: TPanel;
    pnLicense: TPanel;
  private

  public
    procedure Init;
  end;

implementation

{$R *.lfm}

{ TfrShowHint }

procedure TfrShowHint.Init;
begin
  Self.DoubleBuffered := True;
  pnPackageName.Color := $00D9FFFF;
  pnDescription.Color := $00E6FFE6;
  mDescription.Color := $00E6FFE6;
  mDescription.DoubleBuffered := True;
  pnLicense.Color := $00FEEBD3;
  mLicense.Color := $00FEEBD3;
  mLicense.DoubleBuffered := True;
end;

end.

