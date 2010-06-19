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
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, Dialogs,
  LazConfigStorage, XMLCfg, XMLPropStorage,
  BaseIDEIntf, ProjectIntf, IDEDialogs, MenuIntf, LazIDEIntf, IDEWindowIntf,
  AnchorDockStr, AnchorDocking, AnchorDockOptionsDlg;

const
  DefaultConfigFileName = 'anchordocklayout.xml';
var
  mnuAnchorDockSection: TIDEMenuSection;
    mnuADSaveLayoutToFile: TIDEMenuCommand;
    mnuADLoadLayoutFromFile: TIDEMenuCommand;

type

  { TIDEAnchorDockMaster }

  TIDEAnchorDockMaster = class(TIDEDockMaster)
  private
    FDefaultLayoutLoaded: boolean;
    procedure DockMasterCreateControl(Sender: TObject; aName: string;
      var AControl: TControl; DoDisableAutoSizing: boolean);
    procedure GetDefaultBounds(AForm: TCustomForm; out Creator: TIDEWindowCreator;
      out NewBounds: TRect; out DockSiblingName: string; out DockAlign: TAlign);
    procedure SetDefaultLayoutLoaded(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeIDEWindowDockSite(AForm: TCustomForm); override;
    procedure MakeIDEWindowDockable(AControl: TWinControl); override;
    function GetDefaultLayoutFilename: string;
    procedure LoadDefaultLayout;
    procedure SaveDefaultLayout;
    procedure LoadLayoutFromFile(Filename: string);
    procedure SaveLayoutToFile(Filename: string);
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); override;
    procedure CloseAll; override;
    procedure OnIDERestoreWindows(Sender: TObject);
    function OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure LoadLayoutFromFileClicked(Sender: TObject);
    procedure SaveLayoutToFileClicked(Sender: TObject);
    property DefaultLayoutLoaded: boolean read FDefaultLayoutLoaded write SetDefaultLayoutLoaded;
  end;

var
  IDEAnchorDockMaster: TIDEAnchorDockMaster = nil;

procedure Register;

implementation

procedure Register;
begin
  if not (IDEDockMaster is TIDEAnchorDockMaster) then exit;

  LazarusIDE.AddHandlerOnIDERestoreWindows(@IDEAnchorDockMaster.OnIDERestoreWindows);
  LazarusIDE.AddHandlerOnProjectClose(@IDEAnchorDockMaster.OnProjectClose);

  // add menu section
  mnuAnchorDockSection:=RegisterIDEMenuSection(mnuEnvironment,'AnchorDocking');
  mnuADSaveLayoutToFile:=RegisterIDEMenuCommand(mnuAnchorDockSection,
    'ADSaveLayoutToFile', adrsSaveWindowLayoutToFile,
    @IDEAnchorDockMaster.SaveLayoutToFileClicked);
  mnuADLoadLayoutFromFile:=RegisterIDEMenuCommand(mnuAnchorDockSection,
    'ADLoadLayoutFromFile', adrsLoadWindowLayoutFromFile,
    @IDEAnchorDockMaster.LoadLayoutFromFileClicked);
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
var
  AControl: TControl;
begin
  NewBounds:=Rect(0,0,0,0);
  DockSiblingName:='';
  DockAlign:=alNone;

  // get the embedded control
  AControl:=DockMaster.GetControl(AForm);
  if not (AControl is TCustomForm) then exit;
  AForm:=TCustomForm(AControl);

  Creator:=IDEWindowCreators.FindWithName(AForm.Name);
  if Creator=nil then exit;
  debugln(['TIDEAnchorDockMaster.GetDefaultBounds AAA1 ',AForm.Name,' ',Creator.DockSibling,' ',dbgs(Creator.DockAlign)]);
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

procedure TIDEAnchorDockMaster.SetDefaultLayoutLoaded(const AValue: boolean);
begin
  if FDefaultLayoutLoaded=AValue then exit;
  FDefaultLayoutLoaded:=AValue;
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
  debugln(['TIDEAnchorDockMaster.MakeIDEWindowDockSite ',DbgSName(AForm)]);
  DockMaster.MakeDockSite(AForm,[akBottom],admrpChild);
end;

procedure TIDEAnchorDockMaster.MakeIDEWindowDockable(AControl: TWinControl);
begin
  debugln(['TIDEAnchorDockMaster.MakeIDEWindowDockable ',DbgSName(AControl)]);
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
      if not DockMaster.ConfigIsEmpty(Config) then begin
        debugln(['TIDEAnchorDockMaster.LoadDefaultLayout restoring ...']);
        DockMaster.LoadLayoutFromConfig(Config);
        DefaultLayoutLoaded:=true;
      end;
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

procedure TIDEAnchorDockMaster.LoadLayoutFromFile(Filename: string);
var
  XMLConfig: TXMLConfig;
  Config: TXMLConfigStorage;
begin
  XMLConfig:=TXMLConfig.Create(nil);
  try
    XMLConfig.Filename:=Filename;
    Config:=TXMLConfigStorage.Create(XMLConfig);
    try
      DockMaster.LoadLayoutFromConfig(Config);
    finally
      Config.Free;
    end;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

procedure TIDEAnchorDockMaster.SaveLayoutToFile(Filename: string);
var
  XMLConfig: TXMLConfig;
  Config: TXMLConfigStorage;
begin
  XMLConfig:=TXMLConfig.Create(nil);
  try
    XMLConfig.StartEmpty:=true;
    XMLConfig.Filename:=Filename;
    Config:=TXMLConfigStorage.Create(XMLConfig);
    try
      DockMaster.SaveLayoutToConfig(Config);
    finally
      Config.Free;
    end;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
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
  NewDockSite: TCustomForm;
  Site: TAnchorDockHostSite;
  AControl: TControl;
  NeedPlacing: Boolean;
  SiteForm: TCustomForm;
begin
  debugln(['TIDEAnchorDockMaster.ShowForm START ',DbgSName(AForm),' BringToFront=',BringToFront,' IsSite=',DockMaster.IsSite(AForm),' IsCustomSite=',DockMaster.IsCustomSite(AForm)]);
  try
    AForm.DisableAlign;

    NeedPlacing:=not AForm.IsVisible;
    if not DockMaster.IsSite(AForm) then begin
      // this form was not yet docked
      // => make it dockable
      DockMaster.MakeDockable(AForm,false);
      NeedPlacing:=true;
    end;
    AControl:=DockMaster.GetControl(AForm);

    if not Aform.IsVisible then debugln(['TIDEAnchorDockMaster.ShowForm AControl=',DbgSName(AControl),' NeedPlacing=',NeedPlacing,' Floating=',DockMaster.IsFloating(AForm)]);

    if (AControl<>nil) and NeedPlacing and DockMaster.IsFloating(AForm) then begin
      // this form is not yet on the screen and is not yet docked
      debugln(['TIDEAnchorDockMaster.ShowForm placing ',DbgSName(AControl),' ...']);

      // ToDo: use the restore layout

      // place it at a default position and/or dock it
      GetDefaultBounds(AForm,Creator,NewBounds,DockSiblingName,DockAlign);
      if Creator<>nil then begin
        SiteForm:=GetParentForm(AForm);
        SiteForm.BoundsRect:=NewBounds;
        SiteForm.UndockWidth:=NewBounds.Right-NewBounds.Left;
        SiteForm.UndockHeight:=NewBounds.Bottom-NewBounds.Top;
        debugln(['TIDEAnchorDockMaster.ShowForm creator for ',DbgSName(AControl),' found: Left=',Creator.Left,' Top=',Creator.Top,' Width=',Creator.Width,' Height=',Creator.Height,' DockSiblingName=',DockSiblingName,' DockAlign=',dbgs(DockAlign)]);
        Site:=DockMaster.GetAnchorSite(SiteForm);
        if (Site<>nil) and (DockSiblingName<>'') then begin
          DockSibling:=Screen.FindForm(DockSiblingName);
          debugln(['TIDEAnchorDockMaster.ShowForm DockSiblingName="',DockSiblingName,'" DockSibling=',DbgSName(DockSibling)]);
          if DockSibling<>nil then begin
            NewDockSite:=DockMaster.GetSite(DockSibling);
            if NewDockSite<>nil then begin
              debugln(['TIDEAnchorDockMaster.ShowForm NewDockSite=',DbgSName(NewDockSite),'="',NewDockSite.Caption,'"']);
              DockMaster.ManualDock(Site,NewDockSite,DockAlign);
              debugln(['TIDEAnchorDockMaster.ShowForm after docking: ',DbgSName(AControl),' Floating=',DockMaster.IsFloating(AControl)]);
            end;
          end;
        end;
      end;
    end;

  finally
    if not Aform.IsVisible then debugln(['TIDEAnchorDockMaster.ShowForm MakeVisible ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect),' Floating=',DockMaster.IsFloating(AForm)]);
    DockMaster.MakeVisible(AForm,BringToFront);
    AForm.EnableAlign;

    if BringToFront then begin
      Parent:=GetParentForm(AForm);
      if Parent<>nil then
        Parent.ShowOnTop;
    end;
  end;
  //debugln(['TIDEAnchorDockMaster.ShowForm END ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect)]);
end;

procedure TIDEAnchorDockMaster.CloseAll;
begin
  DockMaster.CloseAll;
end;

function TIDEAnchorDockMaster.OnProjectClose(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  Result:=mrOk;
  if AProject=nil then exit;
  SaveDefaultLayout;
end;

procedure TIDEAnchorDockMaster.OnIDERestoreWindows(Sender: TObject);
begin
  LoadDefaultLayout;
end;

procedure TIDEAnchorDockMaster.LoadLayoutFromFileClicked(Sender: TObject);
var
  Dlg: TSaveDialog;
  Filename: String;
begin
  Dlg:=TSaveDialog.Create(nil);
  try
    InitIDEFileDialog(Dlg);
    Dlg.Title:=adrsLoadWindowLayoutFromFileXml;
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    Dlg.Filter:=adrsAnchorDockingLayout+'|*.xml|'+adrsAllFiles+'|'+GetAllFilesMask;
    if Dlg.Execute then begin
      Filename:=CleanAndExpandFilename(Dlg.FileName);
      try
        LoadLayoutFromFile(Filename);
      except
        on E: Exception do begin
          IDEMessageDialog(adrsError,
            Format(adrsErrorLoadingWindowLayoutFromFile, [Filename, #13, E.Message]),
            mtError,[mbCancel]);
        end;
      end;
    end;
    StoreIDEFileDialog(Dlg);
  finally
    Dlg.Free;
  end;
end;

procedure TIDEAnchorDockMaster.SaveLayoutToFileClicked(Sender: TObject);
var
  Dlg: TSaveDialog;
  Filename: String;
begin
  Dlg:=TSaveDialog.Create(nil);
  try
    InitIDEFileDialog(Dlg);
    Dlg.Title:=adrsSaveWindowLayoutToFileXml;
    Dlg.Options:=Dlg.Options+[ofPathMustExist,ofNoReadOnlyReturn];
    Dlg.Filter:=adrsAnchorDockingLayout+'|*.xml|'+adrsAllFiles+'|'+GetAllFilesMask;
    if Dlg.Execute then begin
      Filename:=CleanAndExpandFilename(Dlg.FileName);
      if ExtractFileExt(Filename)='' then
        Filename:=Filename+'.xml';
      try
        SaveLayoutToFile(Filename);
      except
        on E: Exception do begin
          IDEMessageDialog(adrsError,
            Format(adrsErrorWritingWindowLayoutToFile, [Filename, #13, E.Message]),
            mtError,[mbCancel]);
        end;
      end;
    end;
    StoreIDEFileDialog(Dlg);
  finally
    Dlg.Free;
  end;
end;

initialization
  // create the dockmaster in the initialization section, so that it is ready
  // when the Register procedures of the packages are called.
  if IDEDockMaster<>nil then
    debugln('WARNING: there is already another IDEDOckMaster installed.')
  else
    IDEDockMaster:=TIDEAnchorDockMaster.Create;

finalization
  FreeAndNil(IDEDockMaster);

end.

