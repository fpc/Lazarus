unit uMakeSite;
(* Create elastic dock sites within a form, make forms dockable.

Owners:
The DockMaster can own all floating forms, for easy enumeration.
The owner of the dockable forms is responsible for creating or finding dockable forms?
  The auto-created forms are/shall be owned by DockMaster.Owner?

Problems:

Forms are not (easily) dockable on all platforms!
  We wrap them into a managed floating form, that always shows dockheaders.

  Eventually all forms/wincontrols should use a floating dockhost form,
  on all platforms that do not allow to dock forms.

Default floating sites are owned by Application,
  we have to create the floating sites in the form.OnEndDock event.
  Beware: client.OnEndDock handlers are replaced in MakeDockable!

  Owning panels is dangerous, they are not destroyed with their parent form!
*)

{$mode objfpc}{$H+}

{$DEFINE ownPanels} //elastic panels owned by DockMaster?
{.$DEFINE appDockMgr} //using special AppDockManager?
{.$DEFINE NeedHost}   //request float host?
{.$DEFINE TestForm}   //enable special debug form

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls, EasyDockSite, Graphics,
  fFloatingSite;

type
  sDockSides = TAlignSet;

  TDockPanel = class(TPanel)
  protected
    AutoExpand: boolean;  //do autoshrink?
    Splitter: TSplitter;    //associated
    procedure pnlDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure pnlDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pnlGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure pnlUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  public
  end;

{$IFDEF appDockMgr}
(* DockManager derived from EasyTree, connecting to DockMaster.
  Assume only forms are docked.
*)
  TAppDockManager = class(TEasyTree)
  protected
    function  ReloadDockedControl(const AName: string): TControl; override;
    //function  SaveDockedControl(Control: TControl; Site: TWinControl): string; override;
    function  SaveDockedControl(Control: TControl): string; override;
  end;
{$ELSE}
  TAppDockManager = TEasyTree;
{$ENDIF}

(* The owner of all docksites
*)
  //TDockMaster = class(TComponent)
  TDockMaster = class(TCustomDockMaster)
  protected //event handlers
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
  protected //utilities
    function  ReloadForm(const AName: string; fMultiInst: boolean; fVisible: boolean): TWinControl; virtual;
    function  WrapDockable(Client: TControl; fVisible: boolean): TFloatingSite;
  private
    LastPanel: TDockPanel;  //last elastic panel created
  {$IFDEF ownPanels}
    //elastic panels are in Components[]
  {$ELSE}
    ElasticSites: TFPList;
  {$ENDIF}
  public
    Factory: TWinControl; //generic owner
    ForIDE: boolean; //try some special workarounds
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddElasticSites(AForm: TCustomForm; Sides: sDockSides);
    function  CreateDockable(const AName: string;
        fMultiInst: boolean; fWrap: boolean = True; fVisible: boolean = False): TWinControl;
    function  MakeDockable(AForm: TWinControl; fWrap: boolean = True; fVisible: boolean = False): TForm; override;
    procedure DumpSites;
  //persistence
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function  ReloadDockedControl(const AName: string; Site: TWinControl): TControl; virtual;
    procedure LoadFromFile(const AName: string);
    procedure SaveToFile(const AName: string);
  end;

var
  DockMaster: TDockMaster; //for access by docksites on Reload...

implementation

uses
  Dialogs,
  LCLIntf, LCLProc;

type
  TWinControlAccess = class(TWinControl) end;
  TControlAccess = class(TControl) end;

{$IFDEF TestForm}
  { TExampleForm }

  TExampleForm = class(TForm)
  public
    procedure Paint; override;
  end;

{ TExampleForm }

procedure TExampleForm.Paint;
begin
  inherited Paint;
  with Canvas do begin
    Pen.Color:=clRed;
    MoveTo(0,0);
    LineTo(ClientWidth-1,0);
    LineTo(ClientWidth-1,ClientHeight-1);
    LineTo(0,ClientHeight-1);
    LineTo(0,0);
    LineTo(ClientWidth-1,ClientHeight-1);
  end;
end;
{$ELSE}
type
  TExampleForm = TForm;
{$ENDIF}

{ TDockMaster }

constructor TDockMaster.Create(AOwner: TComponent);
begin
  assert(DockMaster=nil, 'illegal recreate DockMaster');
  inherited Create(AOwner);
{$IFDEF appDockMgr}
  //DebugLn('dockmgr=%s', [DefaultDockManagerClass.ClassName]);
  DefaultDockManagerClass := TAppDockManager;
{$ELSE}
{$ENDIF}
  DockMaster := self;
  if assigned(DockLoader) then
    DockLoader.Free;
  DockLoader := self;
{$IFDEF ownPanels}
{$ELSE}
  ElasticSites := TFPList.Create;
{$ENDIF}
end;

destructor TDockMaster.Destroy;
begin
{$IFDEF ownPanels}
{$ELSE}
  ElasticSites.Free;
{$ENDIF}
  inherited Destroy;
  DockMaster := nil;
  DockLoader := nil;
end;

const //panel prefix for form name
  AlignString: array[TAlign] of string = (
  //alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom
    'n', 't', '_B_', '_L_', '_R_', 'n', 'n'
  );

procedure TDockMaster.AddElasticSites(AForm: TCustomForm; Sides: sDockSides);
var
  side: TAlign;
  pnl: TDockPanel;
  spl: TSplitter;
  dm: TEasyTree;
  po: TComponent; //panel owner
  pnlName: string;

const
  AllowedSides: sDockSides = [alLeft, alRight, alBottom];
begin
{$IFDEF ownPanels}
  po := Self;
{$ELSE}
  po := AForm; //owned by the form - proper destruction
{$ENDIF}
  AForm.DisableAlign;
  for side := low(side) to high(side) do begin
    if (side in AllowedSides) and (side in Sides) then begin
      pnlName := AlignString[side] + AForm.Name;
      TComponent(pnl) := po.FindComponent(pnlName);
      if pnl = nil then begin
      //create the components
        pnl := TDockPanel.Create(po); //owned by?
        TryRename(pnl, pnlName);
        pnl.Caption := '';
        pnl.Parent := AForm;
        pnl.Align := side;
        pnl.BorderWidth := 1;
        //pnl.BorderStyle := bsSingle; // does not properly handle the size
      (* Create and configure DockManager
      *)
        dm := TAppDockManager.Create(pnl);
        dm.SetStyle(hsForm);
        dm.HideSingleCaption:=False; //always show header, for docking forms
        pnl.DockSite := True;
        pnl.UseDockManager := True;
        pnl.Visible := True;
        spl := TSplitter.Create(AForm);
        spl.Parent := AForm;
        spl.Align := side;
        //spl.BorderStyle := bsSingle;
        spl.Beveled := True;
      //size components
        pnl.Splitter := spl;
        if side in [alLeft,alRight] then
          pnl.Width := 0
        else if ForIDE then
          pnl.Height := 1 //keep above StatusBar
        else
          pnl.Height := 0;
      //handlers required for elastic sites
        pnl.OnDockDrop := @pnl.pnlDockDrop;
        pnl.OnDockOver := @pnl.pnlDockOver;
        pnl.OnUnDock := @pnl.pnlUnDock;
        pnl.OnGetSiteInfo := @pnl.pnlGetSiteInfo;
      {$IFDEF ownPanels}
      {$ELSE}
      //register panel for load/save
        ElasticSites.Add(pnl);
      {$ENDIF}
      end;
      LastPanel := pnl; //for reload layout
    end;
  end;
  AForm.EnableAlign;
end;

function TDockMaster.CreateDockable(const AName: string;
  fMultiInst: boolean; fWrap: boolean; fVisible: boolean): TWinControl;
begin
(* Create a dockable form, based on its name.
  Used also to restore a layout.

Options (to come or to be removed)
  fMultiInst allows to auto-create new instances (if True),
  otherwise an already existing instance is returned. (really returned?)
*)
//get the form
  Result := ReloadForm(AName, fMultiInst, False);
  if Result = nil then
    exit;
  MakeDockable(Result, fWrap, fVisible);
//problem with first show?
  //Result.Invalidate; - doesn't help
end;

function TDockMaster.MakeDockable(AForm: TWinControl; fWrap: boolean;
  fVisible: boolean): TForm;
var
  Site: TFloatingSite absolute Result;
  Res: TWinControlAccess absolute AForm;
begin
  Result := nil;
  //Result  := inherited MakeDockable(AForm, fWrap, fVisible); - premature Visbile?
//check already dockable
  { TODO -cdocking : problems with IDE windows:
    wrapping results in exceptions - conflicts with OnEndDock? }
//make it dockable
  Res.DragKind := dkDock;
  Res.DragMode := dmAutomatic;
//always force FloatingDockSite
{$IFDEF NeedHost}
  Res.FloatingDockSiteClass := TFloatingSite;
{$ELSE}
  //if not ForIDE then //problems with the IDE?
    Res.OnEndDock := @FormEndDock; //float into default host site
{$ENDIF}
//wrap into floating site, if requested (not on restore Layout)
  if fWrap then begin
    Site := WrapDockable(AForm, fVisible);
  end;
//IDE?
  if ForIDE and fWrap and assigned(Site) and assigned(Site.DockManager) then begin
    Site.Invalidate;
  end;
end;

function TDockMaster.ReloadDockedControl(const AName: string;
  Site: TWinControl): TControl;
var
  i: integer;
  lst: TStringList;
  nb: TCustomDockSite absolute Result;
  s: string;
  ctl: TControl;
begin
(* Reload any docked forms, from Screen's collection
*)
//search existing forms
  Result := nil;
  if AName <> '' then begin
    Result := Screen.FindForm(AName);
    if Result <> nil then begin
    {$IFDEF ForceVisible}
      Result.Visible := True; //empty edit book?
    {$ENDIF}
      exit; //found it
    end;
  end;
//not found
  Result := inherited ReloadControl(AName, Site);
  if (Result = nil) and TryCreateControls then
    Result := ReloadForm(AName, False, True);
end;

procedure TDockMaster.FormEndDock(Sender, Target: TObject; X, Y: Integer);
var
  ctl: TControl;
  Site: TFloatingSite;
begin
(* Handler for Form.OnEndDock.
  When a form becomes floating, dock immediately into a new floating host docksite.

  Prevent wrapping controls in destruction?

  Since this event is raised during dragging, we should not try to wrap a control
  that still has its (old?) HostDockSite set. If Nil it seems to be acceptable
  to set a new dock site, but this would be easier if we simply could update the
  Target.

  Made WrapDockable check for detailed conditions.
*)
  if Target <> nil then
    exit; //docked, not floating
  ctl := Sender as TControl;
  //if not (csDestroying in ctl.ComponentState) and (ctl.HostDockSite = nil) then begin
  //if (ctl.HostDockSite = nil) then
    WrapDockable(ctl, True);
end;

procedure TDockMaster.LoadFromFile(const AName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TDockMaster.SaveToFile(const AName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AName, fmCreate or fmShareExclusive);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

type
  eSiteKind = (
  //top level sites
    skEnd,      //end marker
    skElastic,  //include host
    skFloating, //unnamed? don't dock
  //nested sites
    skManaged,  //create EasyTree by default, use DockManager for save/restore
    skUnManaged,//notebook..., use their own format
  //keep last
    skNone      //nostore
  );
  RSiteRec = packed record
    Bounds, Extent: TRect;  //form and site bounds
    Align:  TAlign;
    AutoExpand: boolean;
    Kind: eSiteKind;
    //NameLen: byte; //site name, 0 for floating sites
  end;
var
  SiteRec: RSiteRec;
  SiteName: string;

procedure TDockMaster.LoadFromStream(Stream: TStream);
var
  site: TWinControl;
  host: TForm;
  hcomp: TComponent absolute host;

  function ReadSite: boolean;
  begin
    Stream.Read(SiteRec, sizeof(SiteRec));
    Result := SiteRec.Kind <> skEnd;
    if Result then begin
      SiteName := Stream.ReadAnsiString;
      DebugLn('--- Reload site %s', [SiteName]);
      //DebugLn('--- Error SiteRec ---');
      //DebugLn('Name=' + SiteName);
      with SiteRec do begin
        DebugLn('Bounds=x(%d-%d) y(%d-%d)', [Bounds.Left, Bounds.Right, Bounds.Top, Bounds.Bottom]);
        DebugLn('Extent=x(%d-%d) y(%d-%d)', [Extent.Left, Extent.Right, Extent.Top, Extent.Bottom]);
      end;
    end else
      DebugLn('reload site done');
  end;

  function MakePanel(const FormName: string; aln: TAlign): TDockPanel;
  begin
  (* AName is <align><form>
  *)
    hcomp := self.ReloadForm(FormName, True, False); //try multi-instance first
    if host <> nil then begin
      AddElasticSites(host, [aln]);
      Result := LastPanel; //found or created
      host.BoundsRect := SiteRec.Bounds;
      Result.BoundsRect := SiteRec.Extent;
      Result.AutoExpand := SiteRec.AutoExpand;
      //Result.EnableAlign; ???
      Result.Visible := True;
      exit;
    end;
  //failed
    Result := nil;
  end;

begin
(* Restore a layout.
- Create all floating sites
- Create all ElasticSites - should exist???
- Reload all docked controls

Notebooks?
  In the simple case a notebook is created automatically, by docking a control
    with align=alCustom.
  In order to maintain proper docking we'll have to create and name the notebooks
    before, then create and dock all their clients.
  Ownership?
    When notebooks are dockable, they cannot be owned by the DockSite!
*)
//Allow for non-dockable forms information in front of this part!
  //Stream.Position := 0; //rewind! - must be done by the caller
{ TODO : add non-dockable forms, for full application layout }
//restore all top level sites
  while ReadSite do begin
    case SiteRec.Kind of
    skEnd,      //end marker
    skNone:     //not to be saved/restored
      break;  //should never occur
    skElastic:  //hosted panel - find parent form
      begin
        site := MakePanel(SiteName, SiteRec.Align);
        //continue;
      end;
    skFloating: //floating site
      begin
        site := TFloatingSite.Create(self);
        site.BoundsRect := SiteRec.Bounds;
      end;
    skManaged,
    skUnManaged:
      begin //we own all top level sites
        site := TCustomDockSite.ReloadSite(SiteName, self); // Owner);
        //if SiteRec.Kind = skManaged then
        continue; //unmanaged sites do everything themselves
      end
    else
      DebugLn('unhandled site kind: ', SiteName);
      ShowMessage('unhandled site kind: ' + SiteName);
      exit;
    end;
  //debug
    if site = nil then begin
      ShowMessage('could not reload ' + SiteName);
      exit; //stream error!
    end;
  //adjust host form
    if site.DockManager = nil then
      TAppDockManager.Create(site);
    site.DisableAutoSizing;
    site.DockManager.LoadFromStream(Stream);
    site.EnableAutoSizing;
    site.Show;
  end;
end;

procedure TDockMaster.SaveToStream(Stream: TStream);

  procedure SaveSite(Site: TWinControl);
  begin
  (* what if a site doesn't have an DockManager?
    need form bounds always, for elastic sites also the panel extent
  *)
  //don't store empty sites?
    if Site.DockClientCount = 0 then begin
      //destroy empty floating sites?
      exit;
    end;
    SiteRec.Kind := skNone;
  //handle elastic sites first!
    if Site is TDockPanel then begin
    //elastic panel
      if site.Parent = nil then begin
      //destroy orphaned panel?
        site.Free;
        exit;
      end;
      SiteRec.Kind := skElastic;
      SiteRec.Bounds := Site.Parent.BoundsRect;
      SiteRec.Extent := site.BoundsRect;
      SiteRec.AutoExpand := (site as TDockPanel).AutoExpand;
      SiteName := Site.Parent.Name; //parent site
    end else if Site.HostDockSite = nil then begin
    //floating site - must be managed!
      if Site.DockManager = nil then
        exit; //must be managed
      SiteRec.Bounds := Site.BoundsRect;
      SiteRec.Kind := skFloating;
      SiteName := ''; //default floating site
{this could become another public method, for storing custom dock sites
    end else if Site is TCustomDockSite then begin
    //nested site
      if Site.DockManager <> nil then
        SiteRec.Kind := skManaged
      else
        SiteRec.Kind := skUnManaged;
      SiteName := TCustomDockSite(Site).SaveSite;
}
{
    end else if Site.DockManager = nil then begin
      exit; //don't store
}
    end else begin
      exit; //unhandled, skNone - don't store
    end;
    SiteRec.Align := Site.Align;
    Stream.Write(SiteRec, sizeof(SiteRec));
    Stream.WriteAnsiString(SiteName);
    case SiteRec.Kind of
    skManaged, skUnManaged: //everything stored in SiteName
      ; //unexpected here
    skFloating, skElastic:
      Site.DockManager.SaveToStream(Stream);
    else
      ShowMessage('unhandled site ' + SiteName);
      //raise...
    end;
  end;

var
  i: integer;
  cmp: TComponent;
  wc: TWinControl absolute cmp;
begin
(* Save all floating sites and elastic panels.
  The sites are in our Components[].
  The panels are in ElasticSites (if ownPanels is undefined).
*)
//save floating sites
  for i := 0 to ComponentCount - 1 do begin
    cmp := Components[i];
    if (cmp is TWinControl) and wc.DockSite then begin
      SaveSite(wc) //save top level sites
    end;
  end;
//end marker
  SiteRec.Kind := skEnd;
  //SiteName:='';
  Stream.Write(SiteRec, sizeof(SiteRec));
end;

function TDockMaster.ReloadForm(const AName: string;
  fMultiInst: boolean; fVisible: boolean): TWinControl;
var
  //instname: string
  basename: string;
  fc: TWinControlClass;
  fo: TComponent; //form owner
  ctl: TControl;
  cmp: TComponent absolute Result;
const
  digits = ['0'..'9'];

  procedure SplitName;
  var
    i, l, instno: integer;
  begin
  //find the instance number, if present
    l := Length(AName);
    i := l;
    while AName[i] in digits do
      dec(i);
    //i now is the position of the last non-digit in the name
  (*extract the instance number
    TReader.ReadRootComponent appends "_nnn"
  *)
  {$IFDEF old}
    if AName[i] = '_' then //handle name_inst
      basename := Copy(AName, 1, i-1)
    else
      basename := Copy(AName, 1, i);
    instno := 0;
    while i < l do begin
      inc(i);
      instno := instno * 10 + ord(AName[i])-ord('0');
    end;
  //single/multi instance?
    if instno = 0 then
      instno := 1; //default instance number for forms
  //lookup existing instance
    instname := basename + IntToStr(instno);
  {$ELSE}
    if AName[i] = '_' then begin
    //assume this is a multi-instance name
      basename := 'T' + Copy(AName, 1, i-1);
      //instname := AName;
    end else
      basename := 'T' + AName;
  {$ENDIF}
  end;

begin
(* Get a form from the Factory, or search/create it.

  The name is split into basename and instance number.
  A component of T<basename> is created (and named AName - automatic!).

  The form is not made visible by default.

  Result type? (a TWinControl is sufficient as a DockSite)
*)
//search existing forms
  if AName <> '' then begin
    Result := Screen.FindForm(AName);
    if Result <> nil then begin
      //if DisableUpdate then Result.DisableAlign;
    {$IFDEF ForceVisible}
      Result.Visible := True; //empty edit book?
    {$ENDIF}
      exit; //found it
    end;
  end;
//check if Factory can provide the form
  if assigned(Factory) then begin
    TWinControlAccess(Factory).ReloadDockedControl(AName, ctl);
    if ctl is TWinControl then begin
      Result := TWinControl(ctl);
      //if DisableUpdate then Result.DisableAlign;
      exit;
    end; //else assume that we should do everything?
    FreeAndNil(ctl);
  end;
//search/create ourselves
  fo := Owner; //our owner also owns the forms
  if AName = '' then begin
    Result := TExampleForm.Create(fo); //named Form1, Form2... - not now???
  end else begin
  //create new instance
    //DebugLn('!!! create new: ', AName);
    SplitName;
    fc := TWinControlClass(GetClass(basename));
    if not assigned(fc) then begin
      DebugLn(basename , ' is not a registered class');
      exit(nil); //bad form name
    end;
    Result := TWinControl(fc.Create(fo));
    if (AName <> '') and (Result.Name <> AName) then
      TryRename(Result, AName);
  end;
  //if not DisableUpdate then Result.EnableAlign;
  if fVisible then
    Result.Visible := True;
end;

function TDockMaster.WrapDockable(Client: TControl; fVisible: boolean): TFloatingSite;
var
  Site: TFloatingSite absolute Result;
  ctl: TControlAccess absolute Client;
  wctl: TWinControlAccess absolute Client;
  r: TRect;
begin
(* Wrap a control into a floating site.
  Prevent wrapping under certain conditions:
  - client under destruction
  - client already docked
  - invisible client?
*)
  if (csDestroying in Client.ComponentState)
  or assigned(Client.HostDockSite) //already wrapped
  //or not Client.Visible //or force visible (below)?
  then
    exit(nil); //do nothing with client under destruction!


  Site := TFloatingSite.Create(self); //DockMaster owns the new site
  try
  //keep undocked extent
    r := Client.BoundsRect;
    r.Right := r.Left + Client.UndockWidth;
    r.Bottom := r.Top + Client.UndockHeight;
    Site.BoundsRect := r;
    //DebugLn('Before Wrap: ', DbgS(Site.BoundsRect));
    Client.Align := alNone;
  {//retry make client auto-dockable?
    ctl.DragKind := dkDock;
    ctl.DragMode := dmAutomatic;}
    if Client is TWinControl then
      wctl.DisableAlign;
    Client.ManualDock(Site);
    Client.Visible := True; //shown only when Site becomes visible
    if Client is TWinControl then
      wctl.EnableAlign;
    //DebugLn('After Wrap: ', DbgS(Site.BoundsRect));

    if ForIDE then begin
      //Site.Invalidate; //helps?
      //Client.Top := 0;
      //Client.Left := 0;
      Site.DockManager.ResetBounds(True);
    end;
    if fVisible then
      Site.Visible := True;
  except
    DebugLn('error WrapDockable: ' + Client.Name);
    if Client.HostDockSite <> Site then
      Site.Release;
    raise;
  end;
end;

procedure TDockMaster.DumpSites;
const
  OrientString: array[TDockOrientation] of char = (
    'N','H','V' {$IFDEF FPC} ,'P' {$ENDIF}
  );
  AlignChar: array[TAlign] of char = (
    //(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);
    'n', 't', 'B', 'L', 'R', 'C', 'c'
  );

  function SiteName(ph: TControl): string;
  begin
    if ph = nil then
      exit('<nil>');
    Result := ph.Name;
    if Result = '' then
      Result := '<' + ph.ClassName + '>';
  end;

  procedure DumpSite(ASite: TWinControl);
  var
    ctl: TControl;
    wc: TWinControl absolute ctl;
    n, s: string;
    hds: boolean;
    Site: TWinControl;
    j: integer;
    r: TRect;
  begin
    ctl := ASite;
    s := Format('Site=%s (%d,%d)[%d,%d]', [SiteName(ctl),
      ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
    while ctl <> nil do begin
      hds := ctl.HostDockSite <> nil;
      if hds then begin
        Site := ctl.HostDockSite;
        if Site <> nil then begin
          n := ' in ' + SiteName(Site) + '@' + OrientString[ctl.DockOrientation];
          if assigned(Site.DockManager) then begin
            Site.DockManager.GetControlBounds(ctl, r);
            n := n + Format(' in [%d,%d]', [r.Right, r.Bottom]);
          end;
        end;
      end else begin
        Site := ctl.Parent;
        if Site <> nil then
          n := ' at ' + SiteName(Site) + '@' + AlignChar[ctl.Align];
      end;
      if Site = nil then
        break;
      s := s + n;
      ctl := Site;
    end;
    DebugLn(s);
  //clients
    Site := ASite;
    for j := 0 to Site.DockClientCount - 1 do begin
      ctl := site.DockClients[j];
      s := OrientString[ctl.DockOrientation];
      DebugLn('  %s.Client=%s.%s@%s (%d,%d)[%d,%d]', [SiteName(ASite), ctl.Owner.Name, SiteName(ctl), s,
        ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
      if assigned(Site.DockManager) then begin
        Site.DockManager.GetControlBounds(ctl, r);
        //n := Format(' in [%d,%d]', [r.Right, r.Bottom]);
        //DebugLn(n);
        DebugLn(' in ', DbgS(r));
      end;
      //if ctl is TFloatingSite then
      if (ctl is TWinControl) and wc.DockSite then
        DumpSite(wc);
    end;
  end;

var
  i: integer;
  cmp: TComponent;
  wc: TWinControl absolute cmp;
  ctl: TControl absolute cmp;
  //zone: TEasyZone;
  r: TRect;
begin
(* Dump registered docking sites.
  Elastic panels have no name.
  Dump of docked clients by DockManager (structural info!)
  Notebooks are docked, i.e. HostDockSite<>nil.
    Pages are DockSites???
    EditPages contain Files -> include (full?) filename
--> dump-levels
  dock sites[] and clients[]
    contents[]
*)
  DebugLn('--- dump sites ---');
  for i := 0 to ComponentCount - 1 do begin
    cmp := Components[i];
    if (cmp is TWinControl) and wc.DockSite then
      DumpSite(wc)
    else if ctl is TControl then begin
      wc.DockManager.GetControlBounds(ctl, r);
      DebugLn('Client=%s in %s (%d,%d)[%d,%d] in [%d,%d]', [SiteName(ctl), SiteName(ctl.HostDockSite),
        ctl.Left, ctl.Top, ctl.Width, ctl.Height,
        r.Right, r.Bottom]);
    end;
  end;
  DebugLn('--- dump forms ---');
  for i := 0 to Application.ComponentCount - 1 do begin
    cmp := Application.Components[i];
    DebugLn('%s: %s', [cmp.Name, cmp.ClassName]);
  end;
  DebugLn('--- end dump ---');
end;


{ TDockPanel }

procedure TDockPanel.pnlDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
  w: integer;
  r: TRect;
begin
(* Adjust docksite extent, if required.
  H/V depending on align LR/TB.
  Take 1/3 of the form's extent for the dock site.
  When changed, ensure that the form layout is updated.
*)
  if (DockClientCount > 1)
  or ((Width > 1) and (Height > 1)) //NoteBook!
  then
    exit; //no adjustments of the dock site required

//this is the first drop - handle AutoExpand
  with Source do begin
    if Align in [alLeft, alRight] then begin
      w := Parent.Width div 3;
      if Width < w then begin
      //enlarge docksite
        Parent.DisableAlign; //form(?)
        //w := Source.Control.Width;
        Width := w;
        if Align = alRight then begin
          if AutoExpand then begin
            r := Parent.BoundsRect;
            inc(r.Right, w);
            Parent.BoundsRect := r;
          end else begin
            Left := Left-w;
            Splitter.Left := Splitter.Left-w;
          end;
        end else if AutoExpand then begin
        //enlarge left
          r := Parent.BoundsRect;
          dec(r.Left, w);
          Parent.BoundsRect := r;
        end;
        Parent.EnableAlign;
      end;
    end else begin //alBottom
      w := Parent.Height div 3;
      if Height < w then begin
      //enlarge docksite
        Parent.DisableAlign; //form(?)
        Height := w;
        if Align = alBottom then begin
          if AutoExpand then begin
            //w := Source.Control.Height;
            w := Source.DockRect.Bottom - Source.DockRect.Top;
            Height := w;
            r := Parent.BoundsRect;
            inc(r.Bottom, w);
            Parent.BoundsRect := r;
          end else begin
            Splitter.Top := Splitter.Top-w;
            Top := Top-w;
          end;
        end;
        Parent.EnableAlign;
      end;
    end;
  end;
end;

procedure TDockPanel.pnlDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  r: TRect;

  procedure Adjust(dw, dh: integer);
  begin
  (* r.TopLeft in screen coords, r.BottomRight is W/H(?)
    negative values mean expansion towards screen origin
  *)
    if dw <> 0 then begin
      r.Right := r.Left;
      inc(r.Bottom, r.Top);
      if dw > 0 then
        inc(r.Right, dw)
      else
        inc(r.Left, dw);
    end else begin
      r.Bottom := r.Top;
      inc(r.Right, r.Left);
      if dh > 0 then
        inc(r.Bottom, dh)
      else
        inc(r.Top, dh);
    end;
  end;

var
  dw, dh: integer;
  ctl: TControl;
const
  d = 2; //shift mousepos slightly outside container
begin
(* This handler has to determine the intended DockRect,
  and the alignment within this rectangle.
*)
  if Source.DragTarget = nil then begin
  //DragManager signals deny!
    exit;
  end;
  if State = dsDragMove then begin
    if DockClientCount > 0 then
      exit; //everything should be okay
  //make DockRect reflect the docking area
    r := BoundsRect; //XYWH
    r.TopLeft := Parent.ClientToScreen(r.TopLeft);
    dw := Parent.Width div 3;  //r.Right := r.Left + dw;
    dh := Parent.Height div 3; //r.Bottom := r.Top + dh;
  //dock inside/outside depending on mouse position
    case Align of
    alLeft:
      begin
        AutoExpand := Source.DragPos.x + d < r.Left;
        if AutoExpand then Adjust(-dw, 0) else Adjust(dw, 0);
      end;
    alRight:
      begin
        AutoExpand := Source.DragPos.x + d >= r.Left;
        if AutoExpand then Adjust(dw, 0) else Adjust(-dw, 0);
      end;
    alBottom:
      begin
        AutoExpand := Source.DragPos.y + d > r.Top;
      { TODO : for all modes... }
        //if AutoExpand then Adjust(0, dh) else Adjust(0, -dh);
        if AutoExpand then Adjust(0, Source.Control.TBDockHeight) else Adjust(0, -dh);
      end
    else
      exit;
    end;
    Source.DockRect := r;
    Accept := True;
  end;
end;

procedure TDockPanel.pnlGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
(* Signal acceptance.
  Inflate InfluenceRect, for easier docking into a shrinked site.
*)
  CanDock := True;
  InflateRect(InfluenceRect, 20, 20);
end;

procedure TDockPanel.pnlUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
var
  wh: integer;
  r: TRect;
begin
(* When the last client is undocked, shrink the dock site to zero extent.
  Called *before* the dock client is removed.
*)
  if DockClientCount <= 1 then begin
  //become empty, hide the dock site
    Parent.DisableAlign;
    case Align of
    alLeft:
      begin
        wh := Width;
        Width := 0; //behaves as expected
        if AutoExpand then begin
          r := Parent.BoundsRect;
          inc(r.Left, wh);
          Parent.BoundsRect := r;
        end;
      end;
    alRight:
      begin
        wh := Width;
        Width := 0;
        if AutoExpand then begin
          r := Parent.BoundsRect;
          dec(r.Right, wh);
          Parent.BoundsRect := r;
        end else begin
          Left := Left+wh;
          Splitter.Left := Splitter.Left+wh;
        end;
      end;
    alBottom:
      begin
        wh := Height;
        Height := 0;
        if AutoExpand then begin
          r := Parent.BoundsRect;
          dec(r.Bottom, wh);
          Parent.BoundsRect := r;
          Splitter.Top := Splitter.Top-wh;
        end else begin
          Top := Top+wh;
          Splitter.Top := Top - Splitter.Height - 10;
        end;
      end;
    end;
    Parent.EnableAlign;
  end;
end;

{$IFDEF appDockMgr}
{ TAppDockManager }

function TAppDockManager.ReloadDockedControl(const AName: string): TControl;
begin
(* Special connect to DockMaster.
  Assume docking forms ONLY:
  - try get existing form from Screen
  - if none exists, defer to DockMaster
*)
  if False then Result:=inherited ReloadDockedControl(AName); //asking DockSite (very bad idea)
  Result := Screen.FindForm(AName);
  if (Result = nil) and assigned(DockMaster) then
    Result := DockMaster.ReloadDockedControl(AName, FDockSite);
  if Result <> nil then
    DebugLn('Reloaded %s.%s', [Result.Owner.Name, Result.Name])
  else
    DebugLn('NOT reloaded: ', AName, ' ------------------');
end;

function TAppDockManager.SaveDockedControl(Control: TControl): string;
begin
  if assigned(DockMaster) then
    Result := DockMaster.SaveDockedControl(Control, FDockSite)
  else
    Result:=inherited SaveDockedControl(Control);
  DebugLn('Saved as ', Result);
end;
{$ELSE}
{$ENDIF}

initialization
  RegisterClass(TFloatingSite);
{$IFDEF appDockMgr}
  DefaultDockManagerClass := TAppDockManager;
{$ELSE}
  DefaultDockManagerClass := TEasyTree; //required?
{$ENDIF}
end.

