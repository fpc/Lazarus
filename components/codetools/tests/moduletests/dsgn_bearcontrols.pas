{
  Simple components for testing the RTTI capabilities of codetools

}
unit Dsgn_BearControls;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBearComponent }

  TBearComponent = class(TComponent)

  end;

  TBearCaption = type string;

  { TBearControl }

  TBearControl = class(TBearComponent)
  private
    FCaption: TBearCaption;
    FControls: TFPList; // list of TBearControl
    FHeight: integer;
    FLeft: integer;
    FParent: TBearControl;
    FTop: integer;
    FVisible: boolean;
    FWidth: integer;
    function GetControlCount: integer;
    function GetControls(Index: integer): TBearControl;
    procedure SetParent(const AValue: TBearControl);
    procedure SetVisible(const AValue: boolean);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function IsParentOf(AControl: TBearControl): boolean;
    function HasParent: Boolean; override;
    property Caption: TBearCaption read FCaption write FCaption;
    property ControlCount: integer read GetControlCount;
    property Controls[Index: integer]: TBearControl read GetControls;
    property Height: integer read FHeight write FHeight;
    property Left: integer read FLeft write FLeft;
    property Parent: TBearControl read FParent write SetParent;
    property Top: integer read FTop write FTop;
    property Visible: boolean read FVisible write SetVisible;
    property Width: integer read FWidth write FWidth;
  end;

  { TBearCustomForm }

  TBearCustomForm = class(TBearControl)

  end;

  { TBearForm }

  TBearForm = class(TBearCustomForm)
  published
    property Caption;
    property Height;
    property Left;
    property Top;
    property Visible;
    property Width;
  end;

  { TBearCustomLabel }

  TBearCustomLabel = class(TBearControl)

  end;

  { TBearLabel }

  TBearLabel = class(TBearCustomLabel)
  published
    property Caption;
    property Height;
    property Left;
    property Top;
    property Visible;
    property Width;
  end;

  { TBearCustomPanel }

  TBearCustomPanel = class(TBearControl)
  private
    FBevelWidth: word;
  public
    property BevelWidth: word read FBevelWidth write FBevelWidth;
  end;

  { TBearPanel }

  TBearPanel = class(TBearCustomPanel)
  published
    property BevelWidth;
    property Height;
    property Left;
    property Top;
    property Visible;
    property Width;
  end;

implementation

{ TBearControl }

function TBearControl.GetControlCount: integer;
begin
  Result:=FControls.Count;
end;

function TBearControl.GetControls(Index: integer): TBearControl;
begin
  Result:=TBearControl(FControls[Index]);
end;

procedure TBearControl.SetParent(const AValue: TBearControl);
begin
  if FParent=AValue then Exit;
  if AValue=Self then
    raise Exception.Create('TBearControl.SetParent Self');
  if (AValue<>nil) and IsParentOf(AValue) then
    raise Exception.Create('TBearControl.SetParent cycle');
  if FParent<>nil then
    FParent.FControls.Remove(Self);
  FParent:=AValue;
  if FParent<>nil then begin
    FParent.FControls.Add(Self);
    FreeNotification(FParent);
  end;
end;

procedure TBearControl.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TBearControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TBearControl;
begin
  for I := 0 to ControlCount-1 do
  begin
    Control := Controls[i];
    if Control.Owner = Root then Proc(Control);
  end;
end;

procedure TBearControl.SetParentComponent(Value: TComponent);
begin
  if Value is TBearControl then
    Parent:=TBearControl(Value);
end;

procedure TBearControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if AComponent=FParent then
      FParent:=nil
    else
      FControls.Remove(AComponent);
  end;
end;

constructor TBearControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls:=TFPList.Create;
end;

destructor TBearControl.Destroy;
begin
  Parent:=nil;
  FreeAndNil(FControls);
  inherited Destroy;
end;

function TBearControl.GetParentComponent: TComponent;
begin
  Result:=Parent;
end;

function TBearControl.IsParentOf(AControl: TBearControl): boolean;
begin
  Result := False;
  while Assigned(AControl) do
  begin
    AControl := AControl.Parent;
    if Self = AControl then
      Exit(True);
  end;
end;

function TBearControl.HasParent: Boolean;
begin
  Result:=Parent<>nil;
end;

end.

