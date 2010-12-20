{ This unit installs the EasyDockMgr as dock master for the Lazarus IDE.

  Copyright (C) 2010 Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RegisterEasyDockMgr;

{$mode objfpc}{$H+}

{$DEFINE DockMaster}  //must match IDE setting

interface

uses
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil,
  LazIDEIntf, IDEWindowIntf,
  uMakeSite;

const
  DefaultConfigFileName = 'easydocklayout.lyt';
type

  { TIDEEasyDockMaster }

{$IFDEF DockMaster}
{$ELSE}
  TIDEDockMaster = TIDELayout;
{$ENDIF}
  TIDEEasyDockMaster = class(TIDEDockMaster)
  private
    function DockMasterRestore(const CtrlName: string; ASite: TWinControl
      ): TControl;
    function DockMasterSave(ACtrl: TControl): string;
    procedure GetDefaultBounds(AForm: TCustomForm; out Creator: TIDEWindowCreator;
      out NewBounds: TRect; out DockSiblingName: string; out DockAlign: TAlign);
  public
  {$IFDEF DockMaster}
    constructor Create;
  {$ELSE}
    constructor Create; override;
    function AddableInWindowMenu(AForm: TCustomForm): boolean; override;
    procedure RestoreIDEWindows; override;
  {$ENDIF}
    destructor Destroy; override;
    procedure MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides = [alBottom]); override;
    procedure MakeIDEWindowDockable(AControl: TWinControl); override;
    function IsDockSite(AForm: TCustomForm): boolean;
    function IsDockable(AForm: TCustomForm): boolean;
    function GetDefaultLayoutFilename: string;
    procedure LoadDefaultLayout;
    procedure SaveDefaultLayout;
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); override;
    procedure CloseAll; override;
    procedure OnIDEClose(Sender: TObject);
    procedure OnIDERestoreWindows(Sender: TObject);
  end;

var
  IDEEasyDockMaster: TIDEEasyDockMaster = nil;

procedure Register;

implementation

procedure Register;
begin
//required?
{$IFDEF DockMaster}
  LazarusIDE.AddHandlerOnIDERestoreWindows(@IDEEasyDockMaster.OnIDERestoreWindows);
  LazarusIDE.AddHandlerOnIDEClose(@IDEEasyDockMaster.OnIDEClose);
{$ELSE}
  //should not be required
{$ENDIF}
end;

{ TIDEEasyDockMaster }

function TIDEEasyDockMaster.DockMasterRestore(const CtrlName: string;
  ASite: TWinControl): TControl;
begin
  debugln(['TIDEEasyDockMaster.DockMasterRestore CtrlName="',dbgstr(CtrlName),'"']);
  Result:=IDEWindowCreators.GetForm(CtrlName,true);
  debugln(['TIDEEasyDockMaster.DockMasterRestore Result=',DbgSName(Result)]);
end;

function TIDEEasyDockMaster.DockMasterSave(ACtrl: TControl): string;
begin
  Result:=ACtrl.Name;
end;

procedure TIDEEasyDockMaster.GetDefaultBounds(AForm: TCustomForm; out
  Creator: TIDEWindowCreator; out NewBounds: TRect; out DockSiblingName: string;
  out DockAlign: TAlign);
begin
  NewBounds:=Rect(0,0,0,0);
  DockSiblingName:='';
  DockAlign:=alNone;
  Creator:=IDEWindowCreators.FindWithName(AForm.Name);
  if Creator=nil then exit;
  if Creator.OnGetLayout<>nil then
    Creator.OnGetLayout(Self,AForm.Name,NewBounds,DockSiblingName,DockAlign)
  else begin
    Creator.GetDefaultBounds(AForm,NewBounds);
    DockSiblingName:=Creator.DockSibling;
    DockAlign:=Creator.DockAlign;
  end;
  NewBounds.Left:=Min(10000,Max(-10000,NewBounds.Left));
  NewBounds.Top:=Min(10000,Max(-10000,NewBounds.Top));
  NewBounds.Right:=Max(NewBounds.Left+100,NewBounds.Right);
  NewBounds.Bottom:=Max(NewBounds.Top+100,NewBounds.Bottom);
end;

constructor TIDEEasyDockMaster.Create;
begin
  IDEEasyDockMaster:=Self;
  DockMaster.OnRestore:=@DockMasterRestore;
  DockMaster.OnSave:=@DockMasterSave;
end;

{$IFDEF DockMaster}
{$ELSE}
function TIDEEasyDockMaster.AddableInWindowMenu(AForm: TCustomForm): boolean;
begin
  Result:=inherited AddableInWindowMenu(AForm);
end;

procedure TIDEEasyDockMaster.RestoreIDEWindows;
begin
  inherited RestoreIDEWindows; //required?
  LoadDefaultLayout;
end;
{$ENDIF}

destructor TIDEEasyDockMaster.Destroy;
begin
  IDEEasyDockMaster:=nil;
  inherited Destroy;
end;

procedure TIDEEasyDockMaster.MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides);
var
  Creator: TIDEWindowCreator;
  NewBounds: TRect;
  DockSiblingName: string;
  DockAlign: TAlign;
begin
  debugln(['TIDEEasyDockMaster.MakeIDEWindowDockSite BEFORE ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect)]);
  GetDefaultBounds(AForm,Creator,NewBounds,DockSiblingName,DockAlign);
  if Creator<>nil then
    AForm.BoundsRect:=NewBounds;
  DockMaster.AddElasticSites(AForm, ASides);
  debugln(['TIDEEasyDockMaster.MakeIDEWindowDockSite AFTER ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect)]);
end;

procedure TIDEEasyDockMaster.MakeIDEWindowDockable(AControl: TWinControl);
begin
  debugln(['TIDEEasyDockMaster.MakeIDEWindowDockable BEFORE ',DbgSName(AControl),' ',dbgs(AControl.BoundsRect)]);
  AControl.UndockWidth:=AControl.Width;
  AControl.UndockHeight:=AControl.Height;
  DockMaster.MakeDockable(AControl);
  debugln(['TIDEEasyDockMaster.MakeIDEWindowDockable AFTERE ',DbgSName(AControl),' ',dbgs(AControl.BoundsRect)]);
end;

function TIDEEasyDockMaster.IsDockSite(AForm: TCustomForm): boolean;
var
  i: Integer;
begin
  Result:=false;
  if AForm=nil then exit;
  if AForm.Parent<>nil then exit;
  for i:=0 to AForm.ControlCount-1 do
    if AForm.Controls[i] is TDockPanel then exit(true);
end;

function TIDEEasyDockMaster.IsDockable(AForm: TCustomForm): boolean;
begin
  Result:=false;
  if AForm=nil then exit;
  if AForm.Parent=nil then exit;
  Result:=true;
end;

function TIDEEasyDockMaster.GetDefaultLayoutFilename: string;
begin
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+DefaultConfigFileName;
end;

procedure TIDEEasyDockMaster.LoadDefaultLayout;
var
  Filename: String;
begin
  // load the users default layout
  Filename:=GetDefaultLayoutFilename;
  debugln(['TIDEEasyDockMaster.LoadDefaultLayout ',Filename,' exists=',FileExistsUTF8(Filename)]);
  if FileExistsUTF8(Filename) then
    DockMaster.LoadFromFile(Filename);
end;

procedure TIDEEasyDockMaster.SaveDefaultLayout;
begin
  debugln(['TIDEEasyDockMaster.SaveDefaultLayout ',GetDefaultLayoutFilename]);
  // load the users default layout
  DockMaster.SaveToFile(GetDefaultLayoutFilename);
end;

procedure TIDEEasyDockMaster.ShowForm(AForm: TCustomForm; BringToFront: boolean
  );
var
  Parent: TCustomForm;
  Creator: TIDEWindowCreator;
  NewBounds: TRect;
  DockSiblingName: string;
  DockAlign: TAlign;
  DockSibling: TCustomForm;
  NewDockSite: TWinControl;
  AControl: TControl;
begin
  debugln(['TIDEEasyDockMaster.ShowForm ',DbgSName(AForm),' BringToFront=',BringToFront,' IsDockSite=',IsDockSite(AForm),' IsDockable=',IsDockable(AForm)]);
  try
    AForm.DisableAlign;
    if AForm.HostDockSite<>nil then
    begin
      // already docked
    end else if not (IsDockSite(AForm) or IsDockable(AForm)) then
    begin
      // this form was not yet docked
      // place it at a default position and make it dockable
      GetDefaultBounds(AForm,Creator,NewBounds,DockSiblingName,DockAlign);
      if Creator<>nil then
      begin
        // this window should become dockable
        NewBounds.Left:=Min(10000,Max(-10000,NewBounds.Left));
        NewBounds.Top:=Min(10000,Max(-10000,NewBounds.Top));
        NewBounds.Right:=Max(NewBounds.Left+100,NewBounds.Right);
        NewBounds.Bottom:=Max(NewBounds.Top+100,NewBounds.Bottom);
        AForm.UndockWidth:=NewBounds.Right-NewBounds.Left;
        AForm.UndockHeight:=NewBounds.Bottom-NewBounds.Top;
        debugln(['TIDEEasyDockMaster.ShowForm creator for ',DbgSName(AForm),' found: Left=',Creator.Left,' Top=',Creator.Top,' Right=',Creator.Right,' Bottom=',Creator.Bottom,' DockSiblingName=',DockSiblingName,' DockAlign=',dbgs(DockAlign)]);
        if DockSiblingName<>'' then
        begin
          DockSibling:=Screen.FindForm(DockSiblingName);
          debugln(['TIDEEasyDockMaster.ShowForm DockSiblingName="',DockSiblingName,'" DockSibling=',DbgSName(DockSibling)]);
          if DockSibling<>nil then
          begin
            NewDockSite:=DockSibling.HostDockSite;
            debugln(['TIDEEasyDockMaster.ShowForm NewDockSite=',DbgSName(NewDockSite)]);
            if NewDockSite<>nil then
              AForm.ManualDock(NewDockSite,nil,DockAlign)
            else
              AForm.ManualDock(nil,DockSibling,DockAlign);
          end;
        end;
        if AForm.Parent=nil then begin
          debugln(['TIDEEasyDockMaster.ShowForm ',DbgSName(AForm),' make dockable NewBounds=',dbgs(NewBounds)]);
          AForm.BoundsRect:=NewBounds;
          // make form dockable
          MakeIDEWindowDockable(AForm);
        end;
      end;
    end;

  finally
    AControl:=AForm;
    while AControl<>nil do begin
      // ToDo: if this is a page switch pageindex of parent
    {$IFDEF old}
      if AControl is TCustomForm then
        TCustomForm(AControl).Show
      else
        AControl.Visible:=true;
    {$ELSE}
      AControl.Visible := True;
    {$ENDIF}
      AControl:=AControl.Parent;
    end;
    AForm.EnableAlign;

    if BringToFront then begin
      Parent:=GetParentForm(AForm);
      if Parent<>nil then
        Parent.ShowOnTop;
    end;
  end;
  debugln(['TIDEEasyDockMaster.ShowForm END ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect)]);
end;

procedure TIDEEasyDockMaster.CloseAll;
begin
  SaveDefaultLayout;
  inherited CloseAll;
end;

procedure TIDEEasyDockMaster.OnIDEClose(Sender: TObject);
begin
  SaveDefaultLayout;
end;

procedure TIDEEasyDockMaster.OnIDERestoreWindows(Sender: TObject);
begin
  LoadDefaultLayout;
end;

initialization
  // create the dockmaster in the initialization section, so that it is ready
  // when the Register procedures of the packages are called.
  TDockMaster.Create(nil);
{$IFDEF DockMaster}
  IDEDockMaster:=TIDEEasyDockMaster.Create;
{$ELSE}
  if IDELayout = nil then
    IDELayout := TIDEEasyDockMaster.Create;
{$ENDIF}

finalization
{$IFDEF DockMaster}
  FreeAndNil(IDEDockMaster);
{$ELSE}
  FreeAndNil(IDELayout);
{$ENDIF}
  FreeAndNil(DockMaster);

end.

