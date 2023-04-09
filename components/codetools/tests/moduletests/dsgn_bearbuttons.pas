unit Dsgn_BearButtons;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dsgn_BearControls;

type

  { TBearCustomButton }

  TBearCustomButton = class(TBearControl)
  private
    FOnClick: TNotifyEvent;
  public
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  { TBearButton }

  TBearButton = class(TBearCustomButton)
  published
    property Caption;
    property Height;
    property Left;
    property OnClick;
    property Top;
    property Visible;
    property Width;
  end;

implementation

end.

