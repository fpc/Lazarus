unit CheckLst;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, GraphType, Controls, VCLGlobals;
  

type
  { TCheckListBox }

  TCheckListBox = class(TCustomListBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property ExtendedSelect;
    property Items;
    property ItemHeight;
    property MultiSelect;
    property OnClick;
    property OnDblClick;
    property OnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional',[TCheckListBox]);
end;

{ TCheckListBox }

constructor TCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csCheckListBox;
end;

end.

