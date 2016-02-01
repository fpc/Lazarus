unit sparta_InterfacesMDI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms;

type
  IDesignedRealForm = interface
  ['{AAEC32EE-4ABE-4691-A172-FC67B66118DD}']
    // bounds
    function GetRealBounds(AIndex: Integer): Integer;
    procedure SetRealBounds(AIndex: Integer; AValue: Integer);

    property RealLeft: Integer index 0 read GetRealBounds write SetRealBounds;
    property RealTop: Integer index 1 read GetRealBounds write SetRealBounds;
    property RealWidth: Integer index 2 read GetRealBounds write SetRealBounds;
    property RealHeight: Integer index 3 read GetRealBounds write SetRealBounds;

    // setters
    procedure SetRealBorderStyle(AVal: TFormBorderStyle);
    procedure SetRealBorderIcons(AVal: TBorderIcons);
    procedure SetRealFormStyle(AVal: TFormStyle);
    procedure SetRealPopupMode(AVal: TPopupMode);
    procedure SetRealPopupParent(AVal: TCustomForm);

    // getters
    function GetRealBorderStyle: TFormBorderStyle;
    function GetRealBorderIcons: TBorderIcons;
    function GetRealFormStyle: TFormStyle;
    function GetRealPopupMode: TPopupMode;
    function GetRealPopupParent: TCustomForm;

    // properties
    property RealBorderStyle: TFormBorderStyle read GetRealBorderStyle write SetRealBorderStyle;
    property RealBorderIcons: TBorderIcons read GetRealBorderIcons write SetRealBorderIcons;
    property RealFormStyle: TFormStyle read GetRealFormStyle write SetRealFormStyle;

    property RealPopupMode: TPopupMode read GetRealPopupMode write SetRealPopupMode;
    property RealPopupParent: TCustomForm read GetRealPopupParent write SetRealPopupParent;
  end;

  IDesignedForm = interface(IDesignedRealForm)
  ['{5D30C0DE-4D51-4FB5-99FC-88900FAE6B66}']
    procedure BeginUpdate;
    procedure EndUpdate(AModified: Boolean = False);

    function GetUpdate: Boolean;
    property Update: Boolean read GetUpdate;

    procedure ShowWindow;
    procedure HideWindow;

    // hacked values
    function GetPublishedBounds(AIndex: Integer): Integer;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer);
    property Left: Integer index 0 read GetPublishedBounds write SetPublishedBounds;
    property Top: Integer index 1 read GetPublishedBounds write SetPublishedBounds;
    property Width: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property Height: Integer index 3 read GetPublishedBounds write SetPublishedBounds;

    // design form scroll system
    procedure SetHorzScrollPosition(AValue: Integer);
    procedure SetVertScrollPosition(AValue: Integer);
    function GetHorzScrollPosition: Integer;
    function GetVertScrollPosition: Integer;
    property HorzScrollPosition: Integer read GetHorzScrollPosition write SetHorzScrollPosition;
    property VertScrollPosition: Integer read GetVertScrollPosition write SetVertScrollPosition;

    // on notify change
    procedure SetOnChangeHackedBounds(const AValue: TNotifyEvent);
    function GetOnChangeHackedBounds: TNotifyEvent;
    property OnChangeHackedBounds: TNotifyEvent read GetOnChangeHackedBounds write SetOnChangeHackedBounds;

    //
    function GetForm: TCustomForm;
    property Form: TCustomForm read GetForm;
  end;

  IDesignedRealFormHelper = interface(IDesignedRealForm)
    function GetLogicalClientRect(ALogicalClientRect: TRect): TRect;
  end;

  IDesignedFormBackground = interface
  ['{AC7F6594-1C2D-4424-977B-28053A79CE99}']
    function GetMargin(const AIndex: Integer): Integer;

    property LeftMargin: Integer index 0 read GetMargin;
    property TopMargin: Integer index 1 read GetMargin;
    property RightMargin: Integer index 2 read GetMargin;
    property BottomMargin: Integer index 3 read GetMargin;

    procedure SetParent(AValue: TWinControl);
    function GetParent: TWinControl;
    property Parent: TWinControl read GetParent write SetParent;

    function GetDesignedForm: IDesignedForm;
    property DesignedForm: IDesignedForm read GetDesignedForm;

    procedure RefreshValues;
  end;

  IResizer = interface
  ['{C3D1A2C0-8AED-493B-9809-1F5C3A54A8A8}']
    procedure TryBoundSizerToDesignedForm(Sender: TObject);
  end;

implementation

end.

