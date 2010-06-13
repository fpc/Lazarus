{ Installs anchor docking manager in the Lazarus IDE.

  Copyright (C) 2010 Mattias Gaertner mattias@freepascal.org

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
unit RegisterAnchorDocking;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil,
  BaseIDEIntf, LazConfigStorage, LazIDEIntf, IDEWindowIntf,
  AnchorDocking, AnchorDockOptionsDlg;

const
  DefaultConfigFileName = 'anchordocklayout.xml';

type

  { TIDEAnchorDockMaster }

  TIDEAnchorDockMaster = class(TIDEDockMaster)
  private
    procedure DockMasterCreateControl(Sender: TObject; aName: string;
      var AControl: TControl; DoDisableAutoSizing: boolean);
    procedure GetDefaultBounds(AForm: TCustomForm; out Creator: TIDEWindowCreator;
      out NewBounds: TRect; out DockSiblingName: string; out DockAlign: TAlign);
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeIDEWindowDockSite(AForm: TCustomForm); override;
    procedure MakeIDEWindowDockable(AControl: TWinControl); override;
    function GetDefaultLayoutFilename: string;
    procedure LoadDefaultLayout; override;
    procedure SaveDefaultLayout;
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); override;
    procedure CloseAll; override;
    procedure OnIDEClose(Sender: TObject);
  end;

var
  IDEAnchorDockMaster: TIDEAnchorDockMaster = nil;

procedure Register;

implementation

procedure Register;
begin

end;

{ TIDEAnchorDockMaster }

procedure TIDEAnchorDockMaster.DockMasterCreateControl(Sender: TObject;
  aName: string; var AControl: TControl; DoDisableAutoSizing: boolean);
begin
  debugln(['TIDEAnchorDockMaster.DockMasterCreateControl CtrlName="',dbgstr(AName),'"']);
  AControl:=IDEWindowCreators.GetForm(aName,true,DoDisableAutoSizing);
  debugln(['TIDEAnchorDockMaster.DockMasterCreateControl Result=',DbgSName(AControl)]);
end;

procedure TIDEAnchorDockMaster.GetDefaultBounds(AForm: TCustomForm; out
  Creator: TIDEWindowCreator; out NewBounds: TRect; out
  DockSiblingName: string; out DockAlign: TAlign);
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

constructor TIDEAnchorDockMaster.Create;
begin
  IDEAnchorDockMaster:=Self;
  DockMaster.OnCreateControl:=@DockMasterCreateControl;
  DockMaster.OnShowOptions:=@ShowAnchorDockOptions;
end;

destructor TIDEAnchorDockMaster.Destroy;
begin
  IDEAnchorDockMaster:=nil;
  inherited Destroy;
end;

procedure TIDEAnchorDockMaster.MakeIDEWindowDockSite(AForm: TCustomForm);
begin
  DockMaster.MakeDockSite(AForm,[akBottom],admrpChild);
end;

procedure TIDEAnchorDockMaster.MakeIDEWindowDockable(AControl: TWinControl);
begin
  DockMaster.MakeDockable(AControl,false);
end;

function TIDEAnchorDockMaster.GetDefaultLayoutFilename: string;
begin
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+DefaultConfigFileName;
end;

procedure TIDEAnchorDockMaster.LoadDefaultLayout;
var
  Filename: String;
  Config: TConfigStorage;
begin
  Filename:=DefaultConfigFileName;
  try
    debugln(['TIDEAnchorDockMaster.LoadDefaultLayout ',Filename]);
    Config:=GetIDEConfigStorage(Filename,true);
    try
      if not DockMaster.ConfigIsEmpty(Config) then
        DockMaster.LoadLayoutFromConfig(Config);
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn(['TIDEAnchorDockMaster.LoadDefaultLayout loading ',Filename,' failed: ',E.Message]);
    end;
  end;
end;

procedure TIDEAnchorDockMaster.SaveDefaultLayout;
var
  Filename: String;
  Config: TConfigStorage;
begin
  Filename:=DefaultConfigFileName;
  try
    debugln(['TIDEAnchorDockMaster.SaveDefaultLayout ',Filename]);
    Config:=GetIDEConfigStorage(Filename,false);
    try
      DockMaster.SaveLayoutToConfig(Config);
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn(['TIDEAnchorDockMaster.SaveDefaultLayout saving ',Filename,' failed: ',E.Message]);
    end;
  end;
end;

procedure TIDEAnchorDockMaster.ShowForm(AForm: TCustomForm;
  BringToFront: boolean);
var
  Parent: TCustomForm;
  Creator: TIDEWindowCreator;
  NewBounds: TRect;
  DockSiblingName: string;
  DockAlign: TAlign;
  DockSibling: TCustomForm;
  NewDockSite: TWinControl;
begin
  debugln(['TIDEAnchorDockMaster.ShowForm ',DbgSName(AForm),' BringToFront=',BringToFront]);
  try
    AForm.DisableAlign;
    if (AForm.HostDockSite<>nil) or (AForm is TAnchorDockHostSite) then
    begin
      // already docked
    end else begin
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
        debugln(['TIDEAnchorDockMaster.ShowForm creator for ',DbgSName(AForm),' found: Left=',Creator.Left,' Top=',Creator.Top,' Width=',Creator.Width,' Height=',Creator.Height,' DockSiblingName=',DockSiblingName,' DockAlign=',dbgs(DockAlign)]);
        if DockSiblingName<>'' then
        begin
          DockSibling:=Screen.FindForm(DockSiblingName);
          debugln(['TIDEAnchorDockMaster.ShowForm DockSiblingName="',DockSiblingName,'" DockSibling=',DbgSName(DockSibling)]);
          if DockSibling<>nil then
          begin
            NewDockSite:=DockSibling.HostDockSite;
            debugln(['TIDEAnchorDockMaster.ShowForm NewDockSite=',DbgSName(NewDockSite)]);
            {if NewDockSite<>nil then
              AForm.ManualDock(NewDockSite,nil,DockAlign)
            else
              AForm.ManualDock(nil,DockSibling,DockAlign);}
          end;
        end;
        if AForm.Parent=nil then begin
          debugln(['TIDEAnchorDockMaster.ShowForm ',DbgSName(AForm),' make dockable NewBounds=',dbgs(NewBounds)]);
          AForm.BoundsRect:=NewBounds;
          // make form dockable
          MakeIDEWindowDockable(AForm);
        end;
      end;
    end;

  finally
    if AForm=Application.MainForm then
      AForm.Show
    else
      DockMaster.MakeDockable(AForm,true,false);
    AForm.EnableAlign;

    if BringToFront then begin
      Parent:=GetParentForm(AForm);
      if Parent<>nil then
        Parent.ShowOnTop;
    end;
  end;
  debugln(['TIDEAnchorDockMaster.ShowForm END ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect)]);
end;

procedure TIDEAnchorDockMaster.CloseAll;
begin
  DockMaster.CloseAll;
end;

procedure TIDEAnchorDockMaster.OnIDEClose(Sender: TObject);
begin
  SaveDefaultLayout;
end;

initialization
  // create the dockmaster in the initialization section, so that it is ready
  // when the Register procedures of the packages are called.
  IDEDockMaster:=TIDEAnchorDockMaster.Create;

finalization
  FreeAndNil(IDEDockMaster);

end.

