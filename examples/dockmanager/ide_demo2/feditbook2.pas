unit fEditBook2;
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
{$DEFINE DockSite}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, SynEdit, EasyDockSite, fDockBook;

type
(* SynEdits seem not to dock properly, so we make them part of a dockable form.
  Docked SynEdits have 2 sets of scrollbars!?
*)
  //TEditPage = class(TSynEdit)
  TEditPage = class(TCustomForm)
  protected
    FEdit: TSynEdit;
    NRUEdit: TEditPage; //Next Recently Used EditPage
    procedure DoFloatMsg(ADockSource: TDragDockObject); override;//CM_FLOAT
    function  GetDefaultDockCaption: string; override;
    function  GetPrev: TEditPage;
  public
    FileName: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFile(const AName: string);
    procedure SetFocus; override;
  end;

(* EditPages is a notebook with special notification of its parent.
*)
  TEditPages = class(TEasyDockBook)
  protected
    procedure AfterUndock(tabidx: integer); override;
  end;

(* An EditBook wraps a DockBook (TEditPages).
  It can have more components, here: a StatusBar.
  Special layout save/restore methods, to handle opened files.
*)

  { TEditBook }

  //TEditBook = class(TForm)
  TEditBook = class(TCustomDockSite)
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  protected
    FEdit: TEditPages;
    {$IFDEF new}
    function GetFloatingDockSiteClass: TWinControlClass; override;
    {$ENDIF}
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoStartDock(var DragObject: TDragObject); override;
  public
    constructor Create(TheOwner: TComponent); override;
    function  OpenFile(const AName: string): boolean; //virtual;
    function  AddFile(const AName: string): boolean; //virtual;
    procedure LoadFromStream(strm: TStream); override;
    procedure SaveToStream(strm: TStream); override;
    property Pages: TEditPages read FEdit;
  end;

function  GetEditWindow(ctl: TControl): TEditBook;

const
  EditBookID = CustomDockSiteID + 1;

var
  MRUEdit: TEditPage; //Most Recently Used EditPage

implementation

uses
  uMakeSite, fFloatingSite;

function  GetEditWindow(ctl: TControl): TEditBook;
begin
  while assigned(ctl) and not (ctl is TEditBook) do
    ctl := ctl.Parent; //floathost?
  Result := TEditBook(ctl);
  assert(Result <> nil, 'not in EditBook');
end;

{ TEditBook }

{$IFDEF new}
function TEditBook.GetFloatingDockSiteClass: TWinControlClass;
begin
  //Result:=inherited GetFloatingDockSiteClass;
  Result := TFloatingSite;
end;
{$ENDIF}

procedure TEditBook.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TEditBook.DoStartDrag(var DragObject: TDragObject);
begin
  inherited DoStartDrag(DragObject);
end;

procedure TEditBook.DoStartDock(var DragObject: TDragObject);
begin
  inherited DoStartDock(DragObject);
  UndockWidth:=Width;
  UndockHeight:=Height;
end;

constructor TEditBook.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEdit := TEditPages.Create(self);
  FEdit.BorderStyle := bsNone;
  FEdit.Parent := self;
  FEdit.Align := alClient;
  FEdit.Visible := True;
  FEdit.DragMode := dmManual; //disallow undocking
  //FEdit.StayDocked := True;
end;

function TEditBook.AddFile(const AName: string): boolean;
var
  i: integer;
  ctl: TControl;
  ep: TEditPage absolute ctl;
begin
  for i := 0 to FEdit.DockClientCount - 1 do begin
    ctl := FEdit.DockClients[i];
    Result := ep.FileName = AName;
    if Result then
      exit; //activate this page???
  end;
//file not found
  Result := OpenFile(AName);
end;

function TEditBook.OpenFile(const AName: string): boolean;
var
  se: TEditPage;
begin
  se := TEditPage.Create(Owner);
  se.LoadFile(AName);
  se.ManualDock(FEdit);
  Result:=true;
end;

procedure TEditBook.LoadFromStream(strm: TStream);
var
  i, n: byte;
  fn: string;
begin
(* Merge list of files with already open files.
*)
  //inherited LoadFromStream(strm);
  i := strm.ReadByte;
  assert(i = EditBookID, 'bad stream');
  n := strm.ReadByte;
  for i := 0 to n-1 do begin
    fn := strm.ReadAnsiString;
    AddFile(fn);
  end;
end;

procedure TEditBook.SaveToStream(strm: TStream);
var
  i, n: byte;
  ctl: TControl;
  ep: TEditPage absolute ctl;
begin
(* Stream list of open files.
*)
  //inherited SaveToStream(strm);
  strm.WriteByte(EditBookID);
  n := FEdit.DockClientCount;
  strm.WriteByte(n);
  for i := 0 to n - 1 do begin
    ctl := FEdit.DockClients[i];
    strm.WriteAnsiString(ep.FileName);
  end;
end;

{ TEditPage }

constructor TEditPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FloatingDockSiteClass := TEditBook; //also created if NOT floating?
  DragKind := dkDock;
  //DragMode := dmAutomatic; //most probably doesn't work
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
    //frm := GetParentForm(self);
    frm := GetEditWindow(self);
    if frm is TEditBook then begin
      //frm.Close;
      frm.Release;
      exit;
    end;
  end;
  inherited AfterUndock(tabidx);
end;

{$R *.lfm}

initialization
  RegisterClass(TEditBook);
  RegisterClass(TEditPage);
end.

