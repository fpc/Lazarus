{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_DesignedForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, SrcEditorIntf;

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

  IDesignedRealFormHelper = interface(IDesignedRealForm)
    function GetLogicalClientRect(ALogicalClientRect: TRect): TRect;
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

    // for last active window
    function GetLastActiveSourceWindow: TSourceEditorWindowInterface;
    procedure SetLastActiveSourceWindow(AValue: TSourceEditorWindowInterface);
    property LastActiveSourceWindow: TSourceEditorWindowInterface read GetLastActiveSourceWindow write SetLastActiveSourceWindow;
  end;

  IDesignedFakeControl = interface
  ['{31708772-D9FF-42D8-88AD-D27663393177}']
  end;

  IDesignedFakeForm = interface
  ['{A887F50D-13A3-4048-AFFD-F07816FDD08A}']
    // other hacked values
    procedure SetFormBorderStyle(ANewStyle: TFormBorderStyle);
    procedure SetBorderIcons(AVal: TBorderIcons);
    procedure SetFormStyle(AValue : TFormStyle);
    procedure SetCaption(const AValue: string);
    function GetBorderStyle: TFormBorderStyle;
    function GetBorderIcons: TBorderIcons;
    function GetFormStyle: TFormStyle;
    function GetCaption: string;

    property BorderIcons: TBorderIcons read GetBorderIcons write SetBorderIcons;
    property BorderStyle: TFormBorderStyle read GetBorderStyle write SetFormBorderStyle;
    property FormStyle: TFormStyle read GetFormStyle write SetFormStyle;
    property Caption: string read GetCaption write SetCaption;
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

implementation

end.

