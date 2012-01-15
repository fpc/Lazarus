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

  ToDo:
    - save settings after options dialog
    - close source editor and show again
    - show anchor editor => too small
    - qt: focus on close page
    - gtk2: focus on cancel completion box
    - gtk2: focus on execute completion box
    - gtk2: restore fails for mainidebar
}
unit RegisterAnchorDocking;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, Dialogs,
  LazConfigStorage, XMLConf, XMLPropStorage, StdCtrls, LCLIntf,
  BaseIDEIntf, ProjectIntf, MacroIntf, IDEDialogs, MenuIntf, LazIDEIntf,
  IDEWindowIntf, IDEOptionsIntf,
  AnchorDockStr, AnchorDocking, AnchorDockOptionsDlg;

const
  DefaultConfigFileName = 'anchordocklayout.xml';
var
  mnuAnchorDockSection: TIDEMenuSection;
    mnuADSaveLayoutAsDefault: TIDEMenuCommand;
    mnuADSaveLayoutToFile: TIDEMenuCommand;
    mnuADLoadLayoutFromFile: TIDEMenuCommand;
    mnuADRestoreDefaultLayout: TIDEMenuCommand;

type

  { TIDEAnchorDockMaster }

  TIDEAnchorDockMaster = class(TIDEDockMaster)
  private
    FChangeStamp: int64;
    FEnabled: boolean;
    FModified: boolean;
    FUserLayoutLoaded: boolean;
    procedure DockMasterCreateControl(Sender: TObject; aName: string;
      var AControl: TControl; DoDisableAutoSizing: boolean);
    procedure GetDefaultBounds(AForm: TCustomForm; out Creator: TIDEWindowCreator;
      out NewBounds: TRect; out DockSiblingName: string; out DockAlign: TAlign);
    procedure SetEnabled(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetUserLayoutLoaded(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides = [alBottom]); override;
    procedure MakeIDEWindowDockable(AControl: TWinControl); override;
    function AddableInWindowMenu(AForm: TCustomForm): boolean; override;
    function GetDefaultLayoutFilename(Full: boolean): string;
    procedure LoadDefaultLayout;
    procedure LoadUserLayout;
    procedure SaveUserLayout;
    procedure LoadLayoutFromFile(Filename: string);
    procedure SaveLayoutToFile(Filename: string);
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); override;
    procedure CloseAll; override;
    procedure OnIDERestoreWindows(Sender: TObject);
    function OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure RestoreDefaultLayoutClicked(Sender: TObject);
    procedure LoadLayoutFromFileClicked(Sender: TObject);
    procedure SaveLayoutToFileClicked(Sender: TObject);
    procedure SaveLayoutAsDefaultClicked(Sender: TObject);
    property UserLayoutLoaded: boolean read FUserLayoutLoaded write SetUserLayoutLoaded;
    property Enabled: boolean read FEnabled write SetEnabled;
    procedure IncreaseChangeStamp;
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read FModified write SetModified;
  end;

  { TAnchorDockIDEFrame }

  TAnchorDockIDEFrame = class(TAbstractIDEOptionsEditor)
    EnableCheckBox: TCheckBox;
    NoteLabel: TLabel;
  private
    FSettings: TAnchorDockSettings;
  public
    OptionsFrame: TAnchorDockOptionsFrame;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  IDEAnchorDockMaster: TIDEAnchorDockMaster = nil;
  AnchorDockOptionsID: integer = 1000;

procedure Register;

implementation

procedure Register;
begin
  if not (IDEDockMaster is TIDEAnchorDockMaster) then exit;

  LazarusIDE.AddHandlerOnIDERestoreWindows(@IDEAnchorDockMaster.OnIDERestoreWindows);
  LazarusIDE.AddHandlerOnProjectClose(@IDEAnchorDockMaster.OnProjectClose);

  // add menu section
  // As this procedure seems to be called too early, menuitems names will be
  // not localized. So we will localize them in TIDEAnchorDockMaster.OnIDERestoreWindows for now
  mnuAnchorDockSection:=RegisterIDEMenuSection(itmSecondaryTools,'AnchorDocking');
  mnuADSaveLayoutAsDefault:=RegisterIDEMenuCommand(mnuAnchorDockSection,
    'ADSaveLayoutAsDefault', adrsSaveWindowLayoutAsDefault,
    @IDEAnchorDockMaster.SaveLayoutAsDefaultClicked);
  mnuADSaveLayoutToFile:=RegisterIDEMenuCommand(mnuAnchorDockSection,
    'ADSaveLayoutToFile', adrsSaveWindowLayoutToFile,
    @IDEAnchorDockMaster.SaveLayoutToFileClicked);
  mnuADLoadLayoutFromFile:=RegisterIDEMenuCommand(mnuAnchorDockSection,
    'ADLoadLayoutFromFile', adrsLoadWindowLayoutFromFile,
    @IDEAnchorDockMaster.LoadLayoutFromFileClicked);
  mnuADRestoreDefaultLayout:=RegisterIDEMenuCommand(mnuAnchorDockSection,
    'ADRestoreDefaultLayout', adrsRestoreDefaultLayout,
    @IDEAnchorDockMaster.RestoreDefaultLayoutClicked);

  // add options frame
  {$R *.lfm}
  AnchorDockOptionsID:=RegisterIDEOptionsEditor(GroupEnvironment,TAnchorDockIDEFrame,
                                                AnchorDockOptionsID)^.Index;
end;

{ TIDEAnchorDockMaster }

procedure TIDEAnchorDockMaster.DockMasterCreateControl(Sender: TObject;
  aName: string; var AControl: TControl; DoDisableAutoSizing: boolean);
begin
  //debugln(['TIDEAnchorDockMaster.DockMasterCreateControl CtrlName="',dbgstr(AName),'"']);
  AControl:=IDEWindowCreators.GetForm(aName,true,DoDisableAutoSizing);
  //debugln(['TIDEAnchorDockMaster.DockMasterCreateControl Result=',DbgSName(AControl)]);
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

procedure TIDEAnchorDockMaster.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  IncreaseChangeStamp;
end;

procedure TIDEAnchorDockMaster.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

procedure TIDEAnchorDockMaster.SetUserLayoutLoaded(const AValue: boolean);
begin
  if FUserLayoutLoaded=AValue then exit;
  FUserLayoutLoaded:=AValue;
end;

constructor TIDEAnchorDockMaster.Create;
begin
  inherited Create;
  fEnabled:=true;
  IDEAnchorDockMaster:=Self;
  DockMaster.OnCreateControl:=@DockMasterCreateControl;
  DockMaster.OnShowOptions:=@ShowAnchorDockOptions;
  FHideSimpleLayoutOptions:=true;
end;

destructor TIDEAnchorDockMaster.Destroy;
begin
  IDEAnchorDockMaster:=nil;
  if IDEDockMaster=Self then
    IDEDockMaster:=Self;
  inherited Destroy;
end;

procedure TIDEAnchorDockMaster.MakeIDEWindowDockSite(AForm: TCustomForm;
  ASides: TDockSides);
var
  aManager: TAnchorDockManager;
begin
  debugln(['TIDEAnchorDockMaster.MakeIDEWindowDockSite ',DbgSName(AForm)]);
  if ASides=[] then ;
  DockMaster.MakeDockSite(AForm,[akBottom],admrpChild);
  if AForm.DockManager is TAnchorDockManager then begin
    aManager:=TAnchorDockManager(AForm.DockManager);
    aManager.PreferredSiteSizeAsSiteMinimum:=false;
  end;
end;

procedure TIDEAnchorDockMaster.MakeIDEWindowDockable(AControl: TWinControl);
begin
  debugln(['TIDEAnchorDockMaster.MakeIDEWindowDockable ',DbgSName(AControl)]);
  DockMaster.MakeDockable(AControl,false);
end;

function TIDEAnchorDockMaster.AddableInWindowMenu(AForm: TCustomForm): boolean;
begin
  Result:=false;
  if AForm is TAnchorDockHostSite then exit;
  if (DockMaster.FindControl(AForm.Name)=nil) and (AForm.Parent<>nil) then exit;
  Result:=true;
end;

function TIDEAnchorDockMaster.GetDefaultLayoutFilename(Full: boolean): string;
begin
  Result:=DefaultConfigFileName;
  if Full then
    Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+Result;
end;

procedure TIDEAnchorDockMaster.LoadDefaultLayout;
var
  BaseDir: String;
  Filename: String;
begin
  BaseDir:='$PkgDir(AnchorDockingDsgn)';
  IDEMacros.SubstituteMacros(BaseDir);
  if (BaseDir<>'') and DirectoryExistsUTF8(BaseDir) then begin
    Filename:=AppendPathDelim(BaseDir)+'ADLayoutDefault.xml';
    if FileExistsUTF8(Filename) then
      LoadLayoutFromFile(Filename);
  end;
end;

procedure TIDEAnchorDockMaster.LoadUserLayout;
var
  Filename: String;
  Config: TConfigStorage;
begin
  Filename:=GetDefaultLayoutFilename(false);
  try
    debugln(['TIDEAnchorDockMaster.LoadUserLayout ',Filename]);
    Config:=GetIDEConfigStorage(Filename,true);
    try
      if not DockMaster.ConfigIsEmpty(Config) then begin
        // loading last layout
        debugln(['TIDEAnchorDockMaster.LoadUserLayout restoring ...']);
        DockMaster.LoadSettingsFromConfig(Config);
        DockMaster.LoadLayoutFromConfig(Config,true);
        UserLayoutLoaded:=true;
      end else begin
        debugln(['TIDEAnchorDockMaster.LoadUserLayout loading default layout ...']);
        LoadDefaultLayout;
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn(['TIDEAnchorDockMaster.LoadUserLayout loading ',Filename,' failed: ',E.Message]);
    end;
  end;
end;

procedure TIDEAnchorDockMaster.SaveUserLayout;
var
  Filename: String;
  Config: TConfigStorage;
begin
  Filename:=GetDefaultLayoutFilename(false);
  try
    debugln(['TIDEAnchorDockMaster.SaveDefaultLayout ',Filename]);
    Config:=GetIDEConfigStorage(Filename,false);
    try
      DockMaster.SaveSettingsToConfig(Config);
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
  //debugln(['TIDEAnchorDockMaster.LoadLayoutFromFile ',Filename]);
  XMLConfig:=TXMLConfig.Create(nil);
  try
    XMLConfig.Filename:=Filename;
    Config:=TXMLConfigStorage.Create(XMLConfig);
    try
      DockMaster.LoadLayoutFromConfig(Config,true);
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
  OldActiveControl: TWinControl;
begin
  //debugln(['TIDEAnchorDockMaster.ShowForm START ',DbgSName(AForm),' BringToFront=',BringToFront,' IsSite=',DockMaster.IsSite(AForm),' IsCustomSite=',DockMaster.IsCustomSite(AForm)]);
  try
    AForm.DisableAlign;

    NeedPlacing:=not AForm.IsVisible;
    if DockMaster.GetSite(AForm)=nil then begin
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
        debugln(['TIDEAnchorDockMaster.ShowForm creator for ',DbgSName(AControl),' found: Left=',Creator.Left,' Top=',Creator.Top,' Right=',Creator.Right,' Bottom=',Creator.Bottom,' DockSiblingName=',DockSiblingName,' DockAlign=',dbgs(DockAlign),' ',dbgs(SiteForm.BoundsRect)]);
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
    OldActiveControl:=AForm.ActiveControl;
    if not AForm.IsVisible then debugln(['TIDEAnchorDockMaster.ShowForm MakeVisible ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect),' Floating=',DockMaster.IsFloating(AForm)]);
    DockMaster.MakeVisible(AForm,BringToFront);
    AForm.EnableAlign;

    if BringToFront then begin
      if (OldActiveControl=nil)
      or (not OldActiveControl.HandleAllocated)
      or (FindControl(GetFocus)<>OldActiveControl) then begin
        Parent:=GetParentForm(AForm);
        Parent.ShowOnTop;
        if (OldActiveControl<>nil) and OldActiveControl.CanFocus then
        begin
          Parent.ActiveControl:=OldActiveControl;
          Parent.SetFocus;
        end;
        //debugln(['TIDEAnchorDockMaster.ShowForm AForm.ActiveControl=',dbgsname(AForm.ActiveControl),' ',DbgSName(Parent.ActiveControl),' ',DbgSName(FindControl(GetFocus))]);
      end;
    end;
  end;
  //debugln(['TIDEAnchorDockMaster.ShowForm END ',DbgSName(AForm),' ',dbgs(AForm.BoundsRect),' ',DbgSName(FindControl(GetFocus))]);
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
  // do not auto save user layout, the restore is not yet stable
  //SaveUserLayout;
end;

procedure TIDEAnchorDockMaster.RestoreDefaultLayoutClicked(Sender: TObject);
begin
  LoadDefaultLayout;
end;

procedure TIDEAnchorDockMaster.OnIDERestoreWindows(Sender: TObject);
begin
  // localize menu captions
  mnuADSaveLayoutAsDefault.Caption:=adrsSaveWindowLayoutAsDefault;
  mnuADSaveLayoutToFile.Caption:=adrsSaveWindowLayoutToFile;
  mnuADLoadLayoutFromFile.Caption:=adrsLoadWindowLayoutFromFile;
  mnuADRestoreDefaultLayout.Caption:=adrsRestoreDefaultLayout;
  LoadUserLayout;
end;

procedure TIDEAnchorDockMaster.LoadLayoutFromFileClicked(Sender: TObject);
var
  Dlg: TOpenDialog;
  Filename: String;
begin
  Dlg:=TOpenDialog.Create(nil);
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
    Dlg.Options:=Dlg.Options+[ofPathMustExist,ofNoReadOnlyReturn,ofOverwritePrompt];
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

procedure TIDEAnchorDockMaster.SaveLayoutAsDefaultClicked(Sender: TObject);
begin
  SaveUserLayout;
end;

procedure TIDEAnchorDockMaster.IncreaseChangeStamp;
begin
  if FChangeStamp<High(FChangeStamp) then
    inc(FChangeStamp)
  else
   FChangeStamp:=low(FChangeStamp);
end;

{ TAnchorDockIDEFrame }

constructor TAnchorDockIDEFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSettings:=TAnchorDockSettings.Create;
  OptionsFrame:=TAnchorDockOptionsFrame.Create(Self);
  with OptionsFrame do begin
    Name:='OptionsFrame';
    Flags:=[adofShow_ShowHeader];
  end;
end;

destructor TAnchorDockIDEFrame.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

function TAnchorDockIDEFrame.GetTitle: String;
begin
  Result:=adrsDockingAnchordocking;
end;

procedure TAnchorDockIDEFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  if ADialog=nil then ;
  if IDEDockMaster=IDEAnchorDockMaster then begin
    NoteLabel.Visible:=false;
    EnableCheckBox.AnchorParallel(akTop,6,Self);
    OptionsFrame.Align:=alBottom;
    OptionsFrame.AnchorToNeighbour(akTop,6,EnableCheckBox);
    OptionsFrame.Parent:=Self;
    EnableCheckBox.Caption:=adrsDockingEnabledRequiresARestartOfTheIDE;
  end else begin
    NoteLabel.Visible:=true;
    NoteLabel.Caption:=Format(adrsToUseAnchordockingYouMustFirstUninstall, [
      DbgSName(IDEDockMaster)]);
    NoteLabel.Hint:=Format(
      adrsThereIsAnotherDockMasterInstalledOnlyOneDockingPac, [DbgSName(
      IDEDockMaster)]);
    EnableCheckBox.Visible:=false;
    OptionsFrame.Parent:=nil;
  end;
end;

procedure TAnchorDockIDEFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is SupportedOptionsClass) then exit;
  EnableCheckBox.Checked:=IDEAnchorDockMaster.Enabled;
  DockMaster.SaveSettings(FSettings);
  OptionsFrame.LoadFromSettings(FSettings);
end;

procedure TAnchorDockIDEFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is SupportedOptionsClass) then exit;
  IDEAnchorDockMaster.Enabled:=EnableCheckBox.Checked;
  OptionsFrame.SaveToSettings(FSettings);
  if (not DockMaster.SettingsAreEqual(FSettings))
  or (not FileExistsUTF8(IDEAnchorDockMaster.GetDefaultLayoutFilename(true)))
  then begin
    DockMaster.LoadSettings(FSettings);
    IDEAnchorDockMaster.SaveUserLayout;
  end;
end;

class function TAnchorDockIDEFrame.
  SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

initialization
  // create the dockmaster in the initialization section, so that it is ready
  // when the Register procedures of the packages are called.
  if IDEDockMaster<>nil then begin
    debugln('WARNING: there is already another IDEDockMaster installed.');
    TIDEAnchorDockMaster.Create;
  end else
    IDEDockMaster:=TIDEAnchorDockMaster.Create;

finalization
  FreeAndNil(IDEAnchorDockMaster);

end.

