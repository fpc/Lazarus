unit customdrawnprivate;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // LCL
  stdctrls, extctrls, comctrls, customdrawncontrols;

type
  { TCDIntfButton }

  TCDIntfButton = class(TCDButton)
  public
    LCLControl: TButton;
    constructor Create(AOwner: TComponent); override;
    procedure HandleOnClick(Sender: TObject);
  end;

  { TCDIntfGroupBox }

  TCDIntfGroupBox = class(TCDGroupBox)
  public
    LCLControl: TGroupBox;
    constructor Create(AOwner: TComponent); override;
    procedure HandleOnClick(Sender: TObject);
  end;

implementation

{ TCDIntfButton }

constructor TCDIntfButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnClick := @HandleOnClick;
end;

procedure TCDIntfButton.HandleOnClick(Sender: TObject);
begin
  LCLControl.OnClick(LCLControl);
end;

{ TCDIntfGroupBox }

constructor TCDIntfGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnClick := @HandleOnClick;
end;

procedure TCDIntfGroupBox.HandleOnClick(Sender: TObject);
begin
  LCLControl.OnClick(LCLControl);
end;

end.

