unit AnchorDesktopOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLProc, Forms, Controls,
  LazFileUtils, LazConfigStorage, Laz2_XMLCfg,
  IDEOptionsIntf, MacroIntf, LazIDEIntf,
  AnchorDocking, AnchorDockStorage;

type

  { TAnchorDesktopOpt }

  TAnchorDesktopOpt = class(TAbstractDesktopDockingOpt)
  private
    FTree: TAnchorDockLayoutTree;
    FRestoreLayouts: TAnchorDockRestoreLayouts;
    FRestoreLayoutsCreated: Boolean;
  public
    procedure LoadDefaultLayout;
    procedure LoadLayoutFromConfig(Path: string; aXMLCfg: TRttiXMLConfig);
    procedure LoadLayoutFromFile(FileName: string);

    procedure SaveMainLayoutToTree;
    procedure SaveLayoutToConfig(Path: string; aXMLCfg: TRttiXMLConfig);
  public
    constructor Create(aUseIDELayouts: Boolean); override;
    destructor Destroy; override;
    procedure Load(Path: String; aXMLCfg: TRttiXMLConfig); override;
    procedure Save(Path: String; aXMLCfg: TRttiXMLConfig); override;
    procedure Assign(Source: TAbstractDesktopDockingOpt); override;
    procedure StoreWindowPositions; override;
    function RestoreDesktop: Boolean; override;
  end;

implementation

{ TAnchorDesktopOpt }

procedure TAnchorDesktopOpt.Assign(Source: TAbstractDesktopDockingOpt);
var
  xSource: TAnchorDesktopOpt;
begin
  if Source is TAnchorDesktopOpt then
  begin
    xSource := TAnchorDesktopOpt(Source);
    FTree.Assign(xSource.FTree);
    FRestoreLayouts.Assign(xSource.FRestoreLayouts);
  end;
end;

constructor TAnchorDesktopOpt.Create(aUseIDELayouts: Boolean);
begin
  inherited Create(aUseIDELayouts);
  FTree := TAnchorDockLayoutTree.Create;
  if aUseIDELayouts then
    FRestoreLayouts := DockMaster.RestoreLayouts
  else begin
    FRestoreLayouts := TAnchorDockRestoreLayouts.Create;
    FRestoreLayoutsCreated := True;
  end;
end;

destructor TAnchorDesktopOpt.Destroy;
begin
  FTree.Free;
  if FRestoreLayoutsCreated then
    FRestoreLayouts.Free;
  inherited Destroy;
end;

procedure TAnchorDesktopOpt.Load(Path: String; aXMLCfg: TRttiXMLConfig);
begin
  //new version of old "TIDEAnchorDockMaster.LoadUserLayout"

  Path := Path + 'AnchorDocking/';
  try
    {$IFDEF VerboseAnchorDocking}
    debugln(['TIDEAnchorDockMaster.LoadUserLayout ',Filename]);
    {$ENDIF}
    if aXMLCfg.GetValue(Path+'MainConfig/Nodes/ChildCount',0) > 0 then//config is not empty
    begin
      // loading last layout
      {$IF defined(VerboseAnchorDocking) or defined(VerboseAnchorDockRestore)}
      debugln(['TIDEAnchorDockMaster.LoadUserLayout restoring ...']);
      {$ENDIF}
      LoadLayoutFromConfig(Path,aXMLCfg);
    end else begin
      // loading defaults
      {$IF defined(VerboseAnchorDocking) or defined(VerboseAnchorDockRestore)}
      debugln(['TIDEAnchorDockMaster.LoadUserLayout loading default layout ...']);
      {$ENDIF}
      LoadDefaultLayout;
    end;
  except
    on E: Exception do begin
      DebugLn(['TIDEAnchorDockMaster.LoadUserLayout loading ',aXMLCfg.GetValue(Path+'Name', ''),' failed: ',E.Message]);
      Raise;
    end;
  end;
end;

procedure TAnchorDesktopOpt.LoadDefaultLayout;
var
  BaseDir: String;
  Filename: String;
begin
  Filename := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'anchordocklayout.xml';
  if FileExistsUTF8(Filename) then//first load from anchordocklayout.xml -- backwards compatibility
    LoadLayoutFromFile(Filename)
  else
  begin
    BaseDir := '$PkgDir(AnchorDockingDsgn)';
    IDEMacros.SubstituteMacros(BaseDir);
    if (BaseDir<>'') and DirectoryExistsUTF8(BaseDir) then begin
      Filename:=AppendPathDelim(BaseDir)+'ADLayoutDefault.xml';
      if FileExistsUTF8(Filename) then
        LoadLayoutFromFile(Filename);
    end;
  end;
end;

procedure TAnchorDesktopOpt.LoadLayoutFromConfig(Path: string; aXMLCfg: TRttiXMLConfig);
begin
  FTree.LoadFromConfig(Path+'MainConfig/', aXMLCfg);
  FRestoreLayouts.LoadFromConfig(Path+'Restores/', aXMLCfg);
end;

procedure TAnchorDesktopOpt.LoadLayoutFromFile(FileName: string);
var
  Config: TRttiXMLConfig;
begin
  Config := TRttiXMLConfig.Create(FileName);
  try
    LoadLayoutFromConfig('',Config);
  finally
    Config.Free;
  end;
end;

procedure TAnchorDesktopOpt.Save(Path: String; aXMLCfg: TRttiXMLConfig);
begin
  Path := Path + 'AnchorDocking/';
  try
    {$IF defined(VerboseAnchorDocking) or defined(VerboseAnchorDockRestore)}
    debugln(['TIDEAnchorDockMaster.SaveDefaultLayout ',Filename]);
    {$ENDIF}
    SaveLayoutToConfig(Path, aXMLCfg);
  except
    on E: Exception do begin
      DebugLn(['TIDEAnchorDockMaster.SaveDefaultLayout saving ',aXMLCfg.GetValue(Path+'Name', ''),' failed: ',E.Message]);
      Raise;
    end;
  end;
end;

procedure TAnchorDesktopOpt.SaveLayoutToConfig(Path: string; aXMLCfg: TRttiXMLConfig);
begin
  FTree.SaveToConfig(Path+'MainConfig/', aXMLCfg);
  FRestoreLayouts.SaveToConfig(Path+'Restores/', aXMLCfg);
  WriteDebugLayout('TAnchorDesktopOpt.SaveLayoutToConfig ',FTree.Root);
end;

procedure TAnchorDesktopOpt.SaveMainLayoutToTree;
var
  i: Integer;
  AControl: TControl;
  Site: TAnchorDockHostSite;
  SavedSites: TFPList;
  LayoutNode: TAnchorDockLayoutTreeNode;
  AForm: TCustomForm;
  VisibleControls: TStringList;
begin
  FTree.Clear;
  SavedSites:=TFPList.Create;
  VisibleControls:=TStringList.Create;
  with DockMaster do
  try
    for i:=0 to ControlCount-1 do begin
      AControl:=Controls[i];
      if not DockedControlIsVisible(AControl) then continue;
      VisibleControls.Add(AControl.Name);
      AForm:=GetParentForm(AControl);
      if AForm=nil then continue;
      if SavedSites.IndexOf(AForm)>=0 then continue;
      SavedSites.Add(AForm);
      debugln(['TAnchorDesktopOpt.SaveMainLayoutToTree AForm=',DbgSName(AForm)]);
      DebugWriteChildAnchors(AForm,true,true);
      if (AForm is TAnchorDockHostSite) then begin
        Site:=TAnchorDockHostSite(AForm);
        LayoutNode:=FTree.NewNode(FTree.Root);
        Site.SaveLayout(FTree,LayoutNode);
      end else if IsCustomSite(AForm) then begin
        // custom dock site
        LayoutNode:=FTree.NewNode(FTree.Root);
        LayoutNode.NodeType:=adltnCustomSite;
        LayoutNode.Assign(AForm);
        // can have one normal dock site
        Site:=TAnchorDockManager(AForm.DockManager).GetChildSite;
        if Site<>nil then begin
          LayoutNode:=FTree.NewNode(LayoutNode);
          Site.SaveLayout(FTree,LayoutNode);
          {if Site.BoundSplitter<>nil then begin
            LayoutNode:=FTree.NewNode(LayoutNode);
            Site.BoundSplitter.SaveLayout(LayoutNode);
          end;}
        end;
      end else
        raise EAnchorDockLayoutError.Create('invalid root control for save: '+DbgSName(AControl));
    end;
    // remove invisible controls
    FTree.Root.Simplify(VisibleControls);
  finally
    VisibleControls.Free;
    SavedSites.Free;
  end;
end;

procedure TAnchorDesktopOpt.StoreWindowPositions;
begin
  SaveMainLayoutToTree;
end;

function TAnchorDesktopOpt.RestoreDesktop: Boolean;
begin
  Result := DockMaster.FullRestoreLayout(FTree,True);
end;

end.

