{ $Id$
                  ----------------------------------------------
                  carbontabs.pp  -  Carbon tabs Control and tabs
                  ----------------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonTabs;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL Carbon
  CarbonUtils, CarbonDef, CarbonPrivate,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, ExtCtrls;
  
type
  TCarbonTabsControl = class;

  { TCarbonTab }

  TCarbonTab = class(TCarbonCustomControl)
  private
    FParent: TCarbonTabsControl;
    function GetIndex: Integer;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure Attach(AParent: TCarbonTabsControl);
    procedure UpdateTab;
    function SetText(const S: String): Boolean; override;
  end;

  { TCarbonTabsControl }

  TCarbonTabsControl = class(TCarbonControl)
  private
    FTabPositon: TTabPosition;
    FTabs: TObjectList; // of TCarbonTab
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    
    procedure UpdateTabs(AIndex: Integer; TilEnd: Boolean = False);
    procedure SetTabCaption(AIndex: Integer; const S: String);
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
    procedure Hit(AControlPart: ControlPartCode); override;
  public
    procedure Add(ATab: TCarbonTab; AIndex: Integer);
    procedure Remove(AIndex: Integer);
  public
    property TabPosition: TTabPosition read FTabPositon;
  end;


implementation

uses CarbonProc;

{ TCarbonTab }

function TCarbonTab.GetIndex: Integer;
begin
  if FParent <> nil then Result := FParent.FTabs.IndexOf(Self)
  else
  begin
    Result := -1;
    DebugLn('TCarbonTab.GetIndex Error - tab ' + LCLObject.Name +
      ' is not attached to parent!');
  end;
end;

procedure TCarbonTab.CreateWidget(const AParams: TCreateParams);
begin
  inherited CreateWidget(AParams);
  
  ShowHide(False);
end;

procedure TCarbonTab.Attach(AParent: TCarbonTabsControl);
begin
  FParent := AParent;
end;

procedure TCarbonTab.UpdateTab;
begin
  if FParent = nil then Exit;
  
  FParent.UpdateTabs(GetIndex);
end;

function TCarbonTab.SetText(const S: String): Boolean;
begin
  if FParent = nil then Exit;
  
  Result := False;
  FParent.SetTabCaption(GetIndex, S);
  Result := True;
end;

{ TCarbonTabsControl }

procedure TCarbonTabsControl.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Direction: ControlTabDirection;
  TabEntry: ControlTabEntry;
begin
  case (LCLObject as TCustomNotebook).TabPosition of
  tpTop:    Direction := kControlTabDirectionNorth;
  tpBottom: Direction := kControlTabDirectionSouth;
  tpLeft:   Direction := kControlTabDirectionWest;
  tpRight:  Direction := kControlTabDirectionEast;
  end;

  if CreateTabsControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    kControlTabSizeLarge, Direction, 0, TabEntry, Control) = noErr then
  begin
    Widget := Control;

    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);

  FTabPositon := (LCLObject as TCustomNotebook).TabPosition;
  
  FTabs := TObjectList.Create(False);
end;

procedure TCarbonTabsControl.DestroyWidget;
begin
  FTabs.Free;

  inherited DestroyWidget;
end;

procedure TCarbonTabsControl.UpdateTabs(AIndex: Integer; TilEnd: Boolean);
begin
  SetControl32BitMaximum(ControlRef(Widget), FTabs.Count);
  
  // TODO imageindex
  while AIndex < FTabs.Count do
  begin
    SetTabCaption(AIndex, TCarbonTab(FTabs[AIndex]).LCLObject.Caption);
    
    if not TilEnd then Exit;
    
    Inc(AIndex);
  end;
end;

procedure TCarbonTabsControl.SetTabCaption(AIndex: Integer; const S: String);
var
  Info: ControlTabInfoRecV1;
begin
  Info.version := kControlTabInfoVersionOne;
  Info.iconSuiteID := 0;

  CreateCFString(S, Info.name);
  try
    if OSError(SetControlData(ControlRef(Widget), AIndex + 1, kControlTabInfoTag,
        SizeOf(ControlTabInfoRecV1), @Info),
      Self, 'SetTabCaption', 'SetControlData') then Exit;
  finally
    FreeCFString(Info.name);
  end;
end;

class function TCarbonTabsControl.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceHit];
end;

procedure TCarbonTabsControl.ValueChanged;
var
  Msg: TLMNotify;
  NMHdr: TNMHDR;
begin
  // TODO changing

  FillChar(Msg, SizeOf(TLMNotify), 0);
  Msg.Msg := LM_NOTIFY;
  
  FillChar(NMHdr, SizeOf(TNMHdr), 0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndFrom := LCLObject.Handle;
  NMHdr.idFrom := GetControl32BitValue(ControlRef(Widget)) - 1;
  
  Msg.NMHdr := @NMHdr;
  
  DeliverMessage(LCLObject, Msg);
end;

procedure TCarbonTabsControl.Hit(AControlPart: ControlPartCode);
begin
  ValueChanged;
end;

procedure TCarbonTabsControl.Add(ATab: TCarbonTab; AIndex: Integer);
begin
  FTabs.Insert(AIndex, ATab);
  ATab.Attach(Self);
  
  UpdateTabs(AIndex, True);
end;

procedure TCarbonTabsControl.Remove(AIndex: Integer);
begin
  FTabs.Delete(AIndex);
  
  UpdateTabs(AIndex, True);
end;

end.

