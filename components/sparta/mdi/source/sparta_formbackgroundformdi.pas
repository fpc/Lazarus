unit sparta_FormBackgroundForMDI;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  sparta_InterfacesMDI;

type

  { TfrFormBackgroundForMDI }

  TfrFormBackgroundForMDI = class(TFrame, IDesignedFormBackground)
    Panel1: TPanel;
    procedure Panel1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer
      );
    procedure Panel1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  private
    FDesignedForm: IDesignedForm;
    FResizeFrame: IResizeFrame;
    FDelta: TPoint;
    FDown: Boolean;

    function GetMargin(const AIndex: Integer): Integer;
  protected
    function GetParent: TWinControl; virtual;
    procedure SetParent(AParent: TWinControl); override;
    function GetResizeFrame: IResizeFrame;
    procedure SetResizeFrame(AValue: IResizeFrame);

    function GetDesignedForm: IDesignedForm;
  public
    { public declarations }
    constructor Create(const ADesignedForm: IDesignedForm); virtual; reintroduce;

    procedure RefreshValues;
  end;

implementation

{$R *.lfm}

{ TfrFormBackgroundForMDI }

procedure TfrFormBackgroundForMDI.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LCtrlPoint: TPoint;
begin
  LCtrlPoint := Panel1.ScreenToClient(Mouse.CursorPos);
  FDelta.x := -LCtrlPoint.X;
  FDelta.y := -LCtrlPoint.Y;
  FDown := True;
end;

procedure TfrFormBackgroundForMDI.Panel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  frmPoint: TPoint;
  LFrame: TCustomFrame;
begin
  if (not FDown) or (FResizeFrame = nil) then
    Exit;

  frmPoint := Self.ScreenToClient(Mouse.CursorPos);
  LFrame := FResizeFrame.Frame;
  LFrame.Left := LFrame.Left + (frmPoint.x + FDelta.x);
  LFrame.Top := LFrame.Top + (frmPoint.y + FDelta.y);
end;

procedure TfrFormBackgroundForMDI.Panel1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
end;

function TfrFormBackgroundForMDI.GetMargin(const AIndex: Integer): Integer;
begin
  case AIndex of
    0: // left
      Result := 5;
    1: // Top
      Result := 30;
    2: // Right
      Result := 5;
    3: // Bottom
      Result := 5;
  end;
end;

function TfrFormBackgroundForMDI.GetParent: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TfrFormBackgroundForMDI.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;

function TfrFormBackgroundForMDI.GetResizeFrame: IResizeFrame;
begin
  Result := FResizeFrame;
end;

procedure TfrFormBackgroundForMDI.SetResizeFrame(AValue: IResizeFrame);
begin
  FResizeFrame := AValue;
end;

function TfrFormBackgroundForMDI.GetDesignedForm: IDesignedForm;
begin
  Result := FDesignedForm as IDesignedForm;
end;

constructor TfrFormBackgroundForMDI.Create(const ADesignedForm: IDesignedForm);
begin
  inherited Create(nil);
  FDesignedForm := ADesignedForm;
  RefreshValues;
end;

procedure TfrFormBackgroundForMDI.RefreshValues;
begin

end;

end.

