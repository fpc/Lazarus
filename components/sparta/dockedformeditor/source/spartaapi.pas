{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit SpartaAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  ISTADesignTimeUtil = interface
  ['{E135BF89-AFA9-402A-9663-4F1536C7717A}']
    function GetRoot: TPersistent;
    procedure SetRoot(ARoot: TPersistent);

    property Root: TPersistent read GetRoot write SetRoot;
  end;

  // Sparta Tools API
  ISTAMainDesignTimeUtil = interface(ISTADesignTimeUtil)
  ['{53491607-D285-4050-9064-C764EB8E59B9}']
    function GetShowNonVisualComponents: Boolean;
    property ShowNonVisualComponents: Boolean read GetShowNonVisualComponents;
  end;

  ISTANonVisualComponentsUtil = interface(ISTADesignTimeUtil)
  ['{A181688F-572E-4724-AAF1-575B979A1EC2}']
    function GetShowNonVisualComponents: Boolean;
    property ShowNonVisualComponents: Boolean read GetShowNonVisualComponents;
  end;

  ISTAExtendedDesignTimeUtil = interface(ISTADesignTimeUtil)
  ['{1F484121-2295-4847-BFD9-A77C643EA3A7}']
    // TODO OnShow
    // TODO OnHide
    // TODO UpdateRoot
    // TODO FreeOnStrongHide...? free mem for some utils
    procedure RefreshValues;

    procedure SetParent(AWinCtrl: TWinControl);
    function GetParent: TWinControl;
    procedure SetVisible(AValue: Boolean);
    function GetVisible: Boolean;

    property Visible: Boolean read GetVisible write SetVisible;
    property Parent: TWinControl read GetParent write SetParent;
  end;

  TSTADesignTimeUtil = class

  end;

  TSTADesignTimeUtilClass = class of TSTADesignTimeUtil;

  TEDTU = class
  public
    class function AvailableForRoot(ARoot: TPersistent): Boolean; virtual; abstract;
    class function CreateEDTUForRoot(TheOwner: TComponent; ARoot: TPersistent): ISTAExtendedDesignTimeUtil; virtual; abstract;
    class function GlyphName: string; virtual; abstract;
  end;

  TEDTUClass = class of TEDTU;

  { TSTADesignTimeUtilsManager }

  TSTADesignTimeUtilsManager = class
  protected
    function GetEDTUCount: Integer; virtual;
    function GetEDTU(Index: Integer): TEDTUClass; virtual; abstract;
  public
    function CreateMainDTU(AParent, AAddons: TWinControl): ISTAMainDesignTimeUtil; virtual;
    procedure RegisterEDTU(AEDTUClass: TEDTUClass); virtual;
    procedure UnregisterEDTU(AEDTUClass: TEDTUClass); virtual;
    property EDTUCount: Integer read GetEDTUCount;
    property EDTU[Index: Integer]: TEDTUClass read GetEDTU;
  end;

var
  DTUManager: TSTADesignTimeUtilsManager = nil;

implementation

{ TSTADesignTimeUtilsManager }

function TSTADesignTimeUtilsManager.GetEDTUCount: Integer;
begin
  Result := 0;
end;

function TSTADesignTimeUtilsManager.CreateMainDTU(AParent, AAddons: TWinControl
  ): ISTAMainDesignTimeUtil;
begin
  Result := nil;
end;

procedure TSTADesignTimeUtilsManager.RegisterEDTU(AEDTUClass: TEDTUClass);
begin
end;

procedure TSTADesignTimeUtilsManager.UnregisterEDTU(AEDTUClass: TEDTUClass);
begin
end;

end.

