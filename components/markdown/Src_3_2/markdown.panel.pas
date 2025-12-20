unit markdown.panel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, markdown.canvasrender, markdown.parser;

Type

  { TMarkDownControl }

  TMarkDownControl = class(TCustomControl)
  private
    FMarkDown: TStrings;
    procedure SetMarkDown(AValue: TStrings);
  Public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure Paint; override;
    property MarkDown : TStrings Read FMarkDown Write SetMarkDown;
  end;

implementation

{ TMarkDownControl }

procedure TMarkDownControl.SetMarkDown(AValue: TStrings);
begin
  if FMarkDown=AValue then Exit;
  FMarkDown.Assign(AValue);
end;

constructor TMarkDownControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarkDown:=TStringList.Create;
end;

destructor TMarkDownControl.destroy;
begin
  FreeAndNil(FMarkDown);
  inherited destroy;
end;

procedure TMarkDownControl.Paint;
begin
  inherited Paint;
end;

end.

