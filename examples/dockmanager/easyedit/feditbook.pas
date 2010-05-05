unit fEditBook;
(* Maintain a chain of active (editor) windows.
  Move form to front whenever activated.
  Dequeue form when destroyed.

  The queue head is stored in the global variable MRUEdit.

  The EditBook should become a frame, embeddable without docking.
*)

{$mode objfpc}{$H+}

{ TODO : figure out what's wrong with the mru list - with multiple windows }
{.$DEFINE mru} //problems with MRU list???

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  SynEdit,
  fDockBook;

type
  TEditPage = class(TSynEdit)
  protected
    function  GetFloatingDockSiteClass: TWinControlClass; override;
  public
    FileName: string;
    procedure LoadFile(const AName: string);
  end;

  TEditBook = class(TEasyDockBook)
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  protected
    NRUEdit: TEditBook; //Next Recently Used EditBook
  public
    constructor Create(TheOwner: TComponent); override;
    function OpenFile(const AName: string): boolean;
  end; 

var
  MRUEdit: TEditBook; //Most Recently Used EditBook

implementation

uses
  uMakeSite;

{ TEditBook }

constructor TEditBook.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //DockMaster.AddElasticSites(self, [alLeft, alRight, alBottom]);
end;

procedure TEditBook.FormActivate(Sender: TObject);
var
  prev: TEditBook;
begin
//enQ self as first
  if MRUEdit = Self then
    exit; //is alread head
  prev := MRUEdit;
{$IFDEF mru}
  while (prev <> nil) and (prev.NRUEdit <> self) do
    prev := prev.NRUEdit;
  if prev <> nil then
    prev.NRUEdit := self.NRUEdit; //was already in Q
  NRUEdit := MRUEdit; //old head
  MRUEdit := self;  //become head
{$ELSE}
{$ENDIF}
end;

procedure TEditBook.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  prev: TEditBook;
begin
//deQ self
  prev := MRUEdit;
{$IFDEF mru}
  if prev = self then
    MRUEdit := NRUEdit
  else begin
    while (prev <> nil) and (prev.NRUEdit <> self) do
      prev := prev.NRUEdit;
    if prev.NRUEdit = self then
      prev.NRUEdit := NRUEdit;
    //else not in chain?
  end;
  NRUEdit := nil;
{$ELSE}
{$ENDIF}
end;

function TEditBook.OpenFile(const AName: string): boolean;
var
  se: TEditPage;
begin
  se := TEditPage.Create(Owner);
  se.ManualDock(self);
  se.LoadFile(AName);
  Result := True;
end;

{ TEditPage }

function TEditPage.GetFloatingDockSiteClass: TWinControlClass;
begin
  //Result:=inherited GetFloatingDockSiteClass;
  Result := TEditBook; //try auto-wrap
end;

procedure TEditPage.LoadFile(const AName: string);
begin
  FileName := AName;
  Lines.LoadFromFile(AName);
end;

initialization
  {$I feditbook.lrs}

end.

