unit sparta_InterfacesMDI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls;

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
  ['{7EF20246-A8B4-4919-8C33-20E07C24F0E9}']
    function GetLogicalClientRect(ALogicalClientRect: TRect): TRect;
  end;

  IResizeFrame = interface
  ['{A674B2AF-4984-433D-8872-5B5825F345D7}']
    procedure HideSizeRects;
    procedure ShowSizeRects;
    procedure PositionNodes;
    function DesignedWidthToScroll: Integer;
    function DesignedHeightToScroll: Integer;
    procedure ClientChangeBounds;
    procedure DesignerSetFocus;
    procedure OnModified;

    function GetFrame: TCustomFrame;
    function GetVerticalScrollPos: Integer;
    procedure SetVerticalScrollPos(AValue: Integer);
    function GetHorizontalScrollPos: Integer;
    procedure SetHorizontalScrollPos(AValue: Integer);
    function GetBackgroundPanel: TPanel;
    function GetBackgroundMargin(const AIndex: Integer): Integer;
    function GetClientPanel: TPanel;
    function GetNodePositioning: Boolean;
    function GetDesignedForm: IDesignedForm;
    procedure SetDesignedForm(const AValue: IDesignedForm);

    function GetSizerRectSize: Integer;
    function GetSizerLineWidth: Integer;

    property Frame: TCustomFrame read GetFrame;
    property VerticalScrollPos: Integer read GetVerticalScrollPos write SetVerticalScrollPos;
    property HorizontalScrollPos: Integer read GetHorizontalScrollPos write SetHorizontalScrollPos;
    property BgPanel: TPanel read GetBackgroundPanel;

    property BgLeftMargin: Integer index 0 read GetBackgroundMargin;
    property BgTopMargin: Integer index 1 read GetBackgroundMargin;
    property BgRightMargin: Integer index 2 read GetBackgroundMargin;
    property BgBottomMargin: Integer index 3 read GetBackgroundMargin;

    property ClientPanel: TPanel read GetClientPanel;
    property NodePositioning: Boolean read GetNodePositioning;
    property DesignedForm: IDesignedForm read GetDesignedForm write SetDesignedForm;

    property SizerRectSize: Integer read GetSizerRectSize;
    property SizerLineWidth: Integer read GetSizerLineWidth;
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

    function GetResizeFrame: IResizeFrame;
    procedure SetResizeFrame(AValue: IResizeFrame);
    property ResizeFrame: IResizeFrame read GetResizeFrame write SetResizeFrame;

    procedure RefreshValues;
  end;

  IResizer = interface
  ['{C3D1A2C0-8AED-493B-9809-1F5C3A54A8A8}']
    procedure TryBoundSizerToDesignedForm(Sender: TObject);
    function GetActiveResizeFrame: IResizeFrame;
    property ActiveResizeFrame: IResizeFrame read GetActiveResizeFrame;
    function GetActiveDesignedForm: IDesignedForm;
    property ActiveDesignedForm: IDesignedForm read GetActiveDesignedForm;
  end;

implementation

end.

