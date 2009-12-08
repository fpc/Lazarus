unit uMakeSite;
(* Create elastic dock sites within a form, make forms dockable.

Owners:
The DockMaster can own all floating forms, for easy enumeration.
The DockMaster can own all dock grips, for easy detection of the dockable forms.
  Handle destruction how?
The owner of the dockable forms is responsible for creating or finding dockable forms?
  The auto-created forms are/shall be owned by DockMaster.Owner?

Problems:

Forms are not (easily) dockable on all platforms,
  we add a grabber icon to each dockable form,
  and wrap them in a managed floating form.

Default floating sites are owned by Application,
  we have to create the floating sites in the form.OnEndDock event.

  Owning panels is dangerous, they are not destroyed with their parent form!
*)

{$mode objfpc}{$H+}

{$DEFINE ownPanels} //elastic panels owned by DockMaster?

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls, EasyDockSite,
  fFloatingSite;

type
//events triggered from load/save docked controls
  TOnReloadControl = function(const CtrlName: string; ASite: TWinControl): TControl of object;
  TOnSaveControl = function(ACtrl: TControl): string of object;

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

  TAppDockManager = class(TEasyTree)
  protected
    function  ReloadDockedControl(const AName: string): TControl; override;
  end;

(* The owner of all docksites
*)
  TDockMaster = class(TComponent)
  protected //event handlers
    procedure DockHandleMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
  protected //utilities
    function  ReloadForm(const AName: string; fMultiInst: boolean): TWinControl; virtual;
    function  WrapDockable(Client: TControl): TFloatingSite;
  private
    LastPanel: TDockPanel;  //last elastic panel created
    FOnSave: TOnSaveControl;
    FOnRestore: TOnReloadControl;
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
    //function  CreateDockable(const AName: string; site: TWinControl; fMultiInst: boolean; fWrap: boolean = True): TWinControl;
    function  CreateDockable(const AName: string; fMultiInst: boolean; fWrap: boolean = True): TWinControl;
    function  MakeDockable(AForm: TWinControl; fWrap: boolean = True): TForm;
    procedure DumpSites;
  //persistence
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function  ReloadDockedControl(const AName: string; Site: TWinControl): TControl;
    property OnSave: TOnSaveControl read FOnSave write FOnSave;
    property OnRestore: TOnReloadControl read FOnRestore write FOnRestore;
  end;

function  TryRename(AComp: TComponent; const NewName: string): boolean;

var
  DockMaster: TDockMaster; //for access by docksites on Reload...

implementation

uses
  //SynEdit,  //try editor notebooks
  LCLIntf, LCLProc;

type
  TWinControlAccess = class(TWinControl)
  end;
  TControlAccess = class(TControl)
  end;

const //what characters are acceptable, for unique names?
  PanelNames: array[TAlign] of string = (
    '', '', //alNone, alTop,
    '_Elastic_Bottom_', '_Elastic_Left_', '_Elastic_Right_',
    '', ''  //alClient, alCustom
  );

function  TryRename(AComp: TComponent; const NewName: string): boolean;
begin
  Result := True; //assume done
  try
    AComp.Name := NewName;
  except
    Result := False;
  end;
end;

{ TDockMaster }

constructor TDockMaster.Create(AOwner: TComponent);
begin
  assert(DockMaster=nil, 'illegal recreate DockMaster');
  inherited Create(AOwner);
  //DebugLn('dockmgr=%s', [DefaultDockManagerClass.ClassName]);
  DefaultDockManagerClass := TAppDockManager;
  DockMaster := self;
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
  for side := low(side) to high(side) do begin
    if (side in AllowedSides) and (side in Sides) then begin
      pnlName := AlignString[side] + AForm.Name;
      TComponent(pnl) := po.FindComponent(pnlName);
      if pnl = nil then begin
      //create the components
        pnl := TDockPanel.Create(po); //owned by?
        LastPanel := pnl; //for reload layout
        TryRename(pnl, pnlName);
        pnl.Caption := '';
        pnl.Parent := AForm;
        pnl.Align := side;
        pnl.BorderWidth := 1;
        //pnl.BorderStyle := bsSingle; // does not properly handle the size
        dm := TAppDockManager.Create(pnl);
        dm.SetStyle(hsForm);
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
    end;
  end;
end;

//function TDockMaster.CreateDockable(const AName: string; site: TWinControl;
function TDockMaster.CreateDockable(const AName: string;
  fMultiInst: boolean; fWrap: boolean): TWinControl;
var
  nb: TEasyBook;
begin
(* Create a dockable form, based on its name.
  Used also to restore a layout.

Options (to come or to be removed)
  fMultiInst allows to auto-create new instances (if True),
  otherwise an already existing instance is returned. (really returned?)
*)
//get the form
  Result := ReloadForm(AName, fMultiInst);
  if Result = nil then
    exit;
  MakeDockable(Result, fWrap);
end;

function TDockMaster.MakeDockable(AForm: TWinControl; fWrap: boolean): TForm;
var
  img: TImage;
  r: TRect;
  Site: TFloatingSite absolute Result;
  Res: TWinControlAccess absolute AForm;
begin
  AForm.DisableAlign;
  try
  //check make dockable
    { TODO -cdocking : problems with IDE windows:
      wrapping results in exceptions - conflicts with OnEndDock? }
    if Res.DragKind <> dkDock then begin
    //make it dockable
      Res.DragKind := dkDock;
      //if not ForIDE then //problems with the IDE?
        Res.OnEndDock := @FormEndDock; //float into default host site
    end;
    Res.DragMode := dmAutomatic;
  //wrap into floating site, if requested (not on restore Layout)
    if fWrap then begin
    //wrap into dock site
      Site := WrapDockable(AForm);
    end;
  //create a docking handle - should become a component?
    img := TImage.Create(AForm); //we could own the img, and be notified when its parent becomes nil
    img.Align := alNone;
    //if not ForIDE then
    begin  //prevent problems with the following code!
      img.AnchorParallel(akRight,0,Result);
      img.AnchorParallel(akTop,0,Result);
    end;
    img.Anchors:=[akRight,akTop];
    img.Cursor := crHandPoint;
    img.Parent := AForm;
    r := AForm.ClientRect;
    r.bottom := 16;
    r.Left := r.Right - 16;
    img.BoundsRect := r;
    if DockGrip <> nil then  //problem: find grabber picture!?
      try
        img.Picture := DockGrip;
      except
        on E: Exception do begin
          DebugLn('exception loading picture ',E.Message);
        end;
      end;
    //else???
    img.OnMouseMove := @DockHandleMouseMove;
    img.Visible := True;
  //make visible, so that it can be docked without problems
    AForm.Visible := True;
  finally
    AForm.EnableAlign;
  end;
  img.BringToFront;
end;

function TDockMaster.ReloadDockedControl(const AName: string;
  Site: TWinControl): TControl;
var
  i: integer;
  lst: TStringList;
  nb: TEasyBook absolute Result;
  s: string;
  ctl: TControl;
begin
(* Reload docked controls - forms or NoteBook
  Called from AppDockManager on ReloadDockedControl.

  NoteBook identified by comma separated names in AName.
    FileEditor identified by dot in name? (not here)
*)
  if Pos(',', AName) > 0 then begin
  //restore NoteBook
    nb := NoteBookCreate(Site);
    lst := TStringList.Create;
    try
      lst.CommaText := AName;
      for i := 0 to lst.Count - 1 do begin
        s := lst[i];
      //try handler
        ctl := nil;
        if assigned(FOnRestore) then
          ctl := FOnRestore(AName, nb);
        if ctl = nil then
          ctl := ReloadForm(s, True); //try both multi and single instance
        DebugLn(['TDockMaster.ReloadDockedControl ',DbgSName(ctl)]);
        ctl.ManualDock(nb);
      end;
    finally
      lst.Free;
    end;
  end else begin
  //restore control (form?)
    Result := nil;
    if assigned(FOnRestore) then
      Result := FOnRestore(AName, Site);
    if Result = nil then
      Result := ReloadForm(AName, True);
  end;
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
  WrapDockable(ctl);
end;

type
  RSiteRec = packed record
    Bounds, Extent: TRect;  //form and site bounds
    Align:  TAlign;
    AutoExpand: boolean;
    NameLen: byte; //site name, 0 for floating sites
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
    Result := SiteRec.Bounds.Right > 0;
    SetLength(SiteName, SiteRec.NameLen);
    if Result and (SiteRec.NameLen > 0) then
      Stream.Read(SiteName[1], SiteRec.NameLen);
    if Result then begin
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
    hcomp := self.ReloadForm(FormName, True); //try multi-instance first
    if host <> nil then begin
      AddElasticSites(host, [aln]);
      Result := LastPanel; //found or created
      host.BoundsRect := SiteRec.Bounds;
      Result.BoundsRect := SiteRec.Extent;
      Result.AutoExpand := SiteRec.AutoExpand;
      exit;
    end;
  //failed
    Result := nil;
  end;

begin
(* Restore a layout.
- Create all floating sites
- Create all ElasticSites
- Reload all docked controls

Notebooks?
  In the simple case a notebook is created automatically, by docking a control
    with align=alCustom.
  In order to maintain proper docking we'll have to create and name the notebooks
    before, then create and dock all their clients.
  Ownership?
    When notebooks are dockable, they cannot be owned by the DockSite!
*)
  Stream.Position := 0; //rewind!
//restore all floating sites
  while ReadSite do begin
    if SiteRec.NameLen = 0 then begin
    //floating site
      site := TFloatingSite.Create(self);
      site.BoundsRect := SiteRec.Bounds;
    end else begin
    //hosted panel - find parent form
    {$IFDEF old}
      if Factory = nil then
        hcomp := Screen.FindForm(SiteName)
      else begin
        hcomp := Factory.FindComponent(SiteName);
        //hcomp := Factory.ReloadDockedControl(SiteName); - reload form!?
      end;
      if (hcomp = nil) or not (hcomp is TWinControl) then
        host := TForm.Create(Application);
      AddElasticSites(host, [SiteRec.Align]);
      site := LastPanel;
    {$ELSE}
      site := MakePanel(SiteName, SiteRec.Align);
    {$ENDIF}
    end;
  //debug
    if site = nil then begin
      exit; //stream error!
    end;
  //adjust host form
    if site.DockManager = nil then
      TAppDockManager.Create(site);
    site.DockManager.LoadFromStream(Stream);
  end;
end;

procedure TDockMaster.SaveToStream(Stream: TStream);

  procedure SaveSite(Site: TWinControl; const AName: string);
  begin
  (* what if a site doesn't have an DockManager?
    need form bounds always, for elastic sites also the panel extent
  *)
    if Site.DockManager = nil then
      exit;
    if Site is TDockPanel then begin
      if site.Parent = nil then begin
      //destroy orphaned panel?
        site.Free;
        exit;
      end;
      SiteRec.Bounds := Site.Parent.BoundsRect;
      SiteRec.Extent := site.BoundsRect;
      SiteRec.AutoExpand := (site as TDockPanel).AutoExpand;
    end else {if Site.Parent = nil then} begin
      SiteRec.Bounds := Site.BoundsRect
    end;
    SiteName := AName;
    SiteRec.Align := Site.Align;
    SiteRec.NameLen := Length(SiteName);
    Stream.Write(SiteRec, sizeof(SiteRec));
    if AName <> '' then
      Stream.Write(SiteName[1], Length(SiteName));
    Site.DockManager.SaveToStream(Stream);
  end;

var
  i: integer;
  cmp: TComponent;
  wc: TWinControl absolute cmp;
begin
(* Save all floating sites and elastic panels.
  The sites are in Components[].
  The panels are in ElasticSites (if ownPanels is undefined).
*)
//save floating sites
  for i := 0 to ComponentCount - 1 do begin
    cmp := Components[i];
    if (cmp is TWinControl) and wc.DockSite then begin
      if wc.Parent = nil then
        SaveSite(wc, '') //save top level (floating) sites
      else if cmp is TDockPanel then
        SaveSite(wc, wc.Parent.Name); //elastic site
    end;
  end;
//end marker
  SiteRec.Bounds.Right := -1;
  SiteRec.NameLen := 0;
  Stream.Write(SiteRec, sizeof(SiteRec));
end;

function TDockMaster.ReloadForm(const AName: string; fMultiInst: boolean): TWinControl;
var
  basename, instname: string;
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
    instno := 0;
    l := Length(AName);
    i := l;
    while AName[i] in digits do
      dec(i);
    //i now is the position of the last non-digit in the name
  //extract the instance number
    basename := Copy(AName, 1, i);
    while i < l do begin
      inc(i);
      instno := instno * 10 + ord(AName[i])-ord('0');
    end;
  //single/multi instance?
    if instno = 0 then
      instno := 1; //default instance number for forms
  //lookup existing instance
    instname := basename + IntToStr(instno);
  end;

begin
(* Get a form from the Factory, or search/create it.

  The name is split into basename and instance number.
  A component of T<basename> is created (and named AName - automatic!).

  Result type? (a TWinControl is sufficient as a DockSite)
*)
//search existing forms
  if AName <> '' then begin
    Result := Screen.FindForm(AName);
    if Result <> nil then
      exit; //found it
  end;
//check if Factory can provide the form
  if assigned(Factory) then begin
    TWinControlAccess(Factory).ReloadDockedControl(AName, ctl);
    if ctl is TWinControl then begin
      Result := TWinControl(ctl);
      exit;
    end; //else assume that we should do everything?
    FreeAndNil(ctl);
  end;
//search/create ourselves
  fo := Owner; //our owner also owns the forms
  if AName = '' then begin
    Result := TForm.Create(fo); //named Form1, Form2... - not now???
  end else begin
  //create new instance
    if fMultiInst then
      SplitName
    else
      basename := AName;
    basename := 'T' + basename;
    fc := nil;
    fc := TWinControlClass(GetClass(basename)); //must be registered class name!
    //assert(assigned(fc), 'class not registered: ' + basename);
    if fMultiInst and not assigned(fc) then
      fc := TWinControlClass(GetClass('T' + AName)); //retry with non-splitted name
    if not assigned(fc) then
      exit(nil); //bad form name
    Result := fc.Create(fo);
    if Result.Name <> AName then
      TryRename(Result, AName);
  end;
  Result.Visible := True; //required for docking
end;

function TDockMaster.WrapDockable(Client: TControl): TFloatingSite;
var
  Site: TFloatingSite absolute Result;
  ctl: TControlAccess absolute Client;
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
  or not Client.Visible //or force visible (below)?
  then
    exit(nil); //do nothing with client under destruction!

  Site := TFloatingSite.Create(Self); //we own the new site
  try
  {$IFDEF old}
    Site.BoundsRect := Client.BoundsRect; //the new position and extension
  {$ELSE}
  //keep undocked extent
    r := Client.BoundsRect;
    r.Right := r.Left + Client.UndockWidth;
    r.Bottom := r.Top + Client.UndockHeight;
    //site.ClientRect := r;
    Site.BoundsRect := r;
  {$ENDIF}
    Client.Align := alClient;
    //Client.Visible := True; //otherwise docking may be rejected
    Client.ManualDock(Site);
  except
    DebugLn('error WrapDockable: ' + Client.Name);
    if Client.HostDockSite <> Site then
      Site.Release;
  end;
//retry make client auto-dockable
  ctl.DragKind := dkDock;
  ctl.DragMode := dmAutomatic;
end;

procedure TDockMaster.DockHandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ctl: TControl;  // absolute Sender;
begin
(* Handler for DockHandle.OnMouseMove.
  When the left button is pressed, start dragging (for docking).
*)
  if ssLeft in Shift then begin
    ctl := Sender as TControl;
    if ForIDE then
      TWinControlAccess(ctl.Parent).DragKind := dkDock;
    ctl.Parent.BeginDrag(ForIDE); //start immediately?
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
  begin
    ctl := ASite;
    s := Format('Site=%s (%d,%d)[%d,%d]', [SiteName(ctl),
      ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
    while ctl <> nil do begin
      hds := ctl.HostDockSite <> nil;
      if hds then begin
        Site := ctl.HostDockSite;
        if Site <> nil then
          n := ' in ' + SiteName(Site) + '@' + OrientString[ctl.DockOrientation];
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
      DebugLn('Client=%s in %s (%d,%d)[%d,%d]', [SiteName(ctl), SiteName(ctl.HostDockSite),
        ctl.Left, ctl.Top, ctl.Width, ctl.Height]);
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
const
  d = 10; //shift mousepos with InfluenceRect
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
        if AutoExpand then Adjust(0, dh) else Adjust(0, -dh);
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

{ TAppDockManager }

function TAppDockManager.ReloadDockedControl(const AName: string): TControl;
begin
(* Special connect to DockManager, and restore NoteBooks.
*)
  if False then
    Result:=inherited ReloadDockedControl(AName); //asking DockSite (very bad idea)
  if assigned(DockMaster) then begin
    //Result := DockMaster.CreateDockable(AName, FDockSite, True, False);
    //Result := DockMaster.CreateDockable(AName, True, False);
    Result := DockMaster.ReloadDockedControl(AName, FDockSite);
  end else begin  //application default - search Application or Screen?
    //Owner.FindComponent(AControlName) as TControl;
    //Result := Application.FindComponent(AName) as TControl;
    Result := Screen.FindForm(AName);
    //if Result = nil then Result := 'T' + AName
    { TODO -cdocking : load form by name, or create from typename }
  end;
  if Result <> nil then
    DebugLn('Reloaded %s.%s', [Result.Owner.Name, Result.Name]);
end;

initialization
  RegisterClass(TFloatingSite);
  DefaultDockManagerClass := TAppDockManager;
end.

