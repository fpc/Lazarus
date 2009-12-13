unit fEditBook;
(* Take2: embed a DockBook, add elastic sites.
  Maintain a chain of active (editor) pages.
  Whenever a page is activated, move it in front of the chain.
  Destroy (and dequeue) all pages when the form is destroyed.

  The queue head is stored in the global variable MRUEdit.

  SynEdit seems not to be properly dockable - use intermediate form instead.
*)

{$mode objfpc}{$H+}

{ TODO : figure out what's wrong with the mru list - with multiple windows }
{$DEFINE mru} //problems with MRU list???

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SynEdit, EasyDockSite,
  fDockBook;

type
  //TEditPage = class(TSynEdit)
  TEditPage = class(TCustomForm)
  protected
    FEdit: TSynEdit;
    NRUEdit: TEditPage; //Next Recently Used EditPage
    procedure DoFloatMsg(ADockSource: TDragDockObject); override;//CM_FLOAT
    function  GetDefaultDockCaption: string; override;
    function  GetPrev: TEditPage;
    procedure SetFocus; override;
  public
    FileName: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFile(const AName: string);
  end;

(* EditPages is a notebook with special notification of its parent.
*)
  TEditPages = class(TEasyDockBook)
  protected
    procedure AfterUndock(tabidx: integer); override;
  end;

(* An EditBook wraps a DockBook.
  AsString is implemented for save/load the entire form.
  Reloading files has to be done separately, not as part of a layout?
*)
{$IFDEF new}
  TEditBook = class(TCustomDockSite)
  protected
    FEdit: TEditPages;
    procedure LoadNames(const str: string); override;
    function  SaveNames: string; override;
    //procedure OpenFiles(TheFiles: string);
  public
    constructor Create(TheOwner: TComponent); override;
    function  OpenFile(const AName: string): boolean; //virtual;
    //property  Files: string read SaveNames write LoadNames;
    property  AsString;
  end; 
{$ELSE}
  TEditBook = class(TForm)
  protected
    FEdit: TEditPages;
  public
    constructor Create(TheOwner: TComponent); override;
    function  OpenFile(const AName: string): boolean; //virtual;
  end;
{$ENDIF}

var
  MRUEdit: TEditPage; //Most Recently Used EditPage

implementation

uses
  uMakeSite;

{ TEditBook }

constructor TEditBook.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEdit := TEditPages.Create(self);
  FEdit.BorderStyle := bsNone;
  FEdit.Parent := self;
  FEdit.Align := alClient;
  FEdit.Visible := True;
  FEdit.DragMode := dmManual; //disallow undocking
  FEdit.StayDocked := True;
  DockMaster.AddElasticSites(self, [alLeft, alRight, alBottom]);
end;

function TEditBook.OpenFile(const AName: string): boolean;
var
  se: TEditPage;
begin
  se := TEditPage.Create(Owner);
  se.LoadFile(AName);
  se.ManualDock(FEdit);
end;

{ TEditPage }

constructor TEditPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FloatingDockSiteClass := TEditBook; //also created if NOT floating?
  DragKind := dkDock;
  DragMode := dmAutomatic; //most probably doesn't work
  FEdit := TSynEdit.Create(self);
  FEdit.Parent := self;
  FEdit.Align := alClient;
//update chain
  NRUEdit := MRUEdit;
  MRUEdit := self;
  //SetFocus;
end;

destructor TEditPage.Destroy;
var
  prev: TEditPage;
begin
//update chain
  if MRUEdit = self then
    MRUEdit := NRUEdit
  else begin
    prev := GetPrev;
    if prev <> nil then
      prev.NRUEdit := NRUEdit; //okay
    //else not in chain???
  end;
  inherited Destroy;
end;

procedure TEditPage.DoFloatMsg(ADockSource: TDragDockObject);
var
  FloatHost: TEditBook;
begin
//wrap into TEditBook
  if False then inherited DoFloatMsg(ADockSource); //for reference purpose only
//parent still is the OLD parent!
  //if Parent <> nil then  exit; //really do nothing? (if normal child)
  FloatHost := TEditBook.Create(Application); // CreateFloatingDockSite(ADockSource.DockRect);
  FloatHost.BoundsRect := ADockSource.DockRect;
  FloatHost.Visible := True;
  FloatHost.Caption := FloatHost.GetDockCaption(Self);
  ADockSource.DragTarget := FloatHost.FEdit;  // FloatHost;
  ADockSource.DropOnControl := nil; // FloatHost.FEdit;
  ADockSource.DropAlign := alCustom;
end;

function TEditPage.GetDefaultDockCaption: string;
begin
  Result := ExtractFileName(FileName);
  if Result = '' then
    Result := inherited GetDefaultDockCaption;
end;

function TEditPage.GetPrev: TEditPage;
begin
//get preceding edit page in MRU chain
  if MRUEdit = self then
    exit(nil);
//not head
  Result := MRUEdit;
  while (Result <> nil) and (Result.NRUEdit <> self) do
    Result := Result.NRUEdit;
//now result can be nil if we are not chained at all!
end;

procedure TEditPage.LoadFile(const AName: string);
begin
  FileName := AName;
  FEdit.Lines.LoadFromFile(AName);
  Caption := ExtractFileName(AName);
end;

procedure TEditPage.SetFocus;
var
  prev: TEditPage;
begin
//repair
  //FloatingDockSiteClass := TEditBook; //not used???
//update chain
  if MRUEdit <> self then begin
    prev := GetPrev;
    prev.NRUEdit := NRUEdit; //deQ
    NRUEdit := MRUEdit;
    MRUEdit := self;
  end;
  inherited SetFocus;
end;

{ TEditPages }

procedure TEditPages.AfterUndock(tabidx: integer);
var
  frm: TCustomForm;
begin
//if last client undocked - close
  if Tabs.ButtonCount = 0 then begin
    frm := GetParentForm(self);
    if frm is TEditBook then begin
      frm.Close;
      exit;
    end;
  end;
  inherited AfterUndock(tabidx);
end;

initialization
  {$I feditbook.lrs}
  RegisterClass(TEditBook);
  RegisterClass(TEditPage);
end.

