unit fEditBook;
(* Maintain a chain of active (editor) windows.
  Move form to front whenever activated.
  Dequeue form when destroyed.

  The queue head is stored in the global variable MRUEdit;
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  fDockBook;

type
  TEditBook = class(TEasyDockBook)
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  protected
    NRUEdit: TEditBook; //Next Recently Used EditBook
  public
    { public declarations }
  end; 

var
  MRUEdit: TEditBook; //Most Rectently Used EditBook

implementation

{ TEditBook }

procedure TEditBook.FormActivate(Sender: TObject);
var
  prev: TEditBook;
begin
//enQ self as first
  if MRUEdit = Self then
    exit; //is alread head
  prev := MRUEdit;
  while (prev <> nil) and (prev.NRUEdit <> self) do
    prev := prev.NRUEdit;
  if prev <> nil then
    prev.NRUEdit := self.NRUEdit; //was already in Q
  NRUEdit := MRUEdit; //old head
  MRUEdit := self;  //become head
end;

procedure TEditBook.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  prev: TEditBook;
begin
//deQ self
  prev := MRUEdit;
  if prev = self then
    MRUEdit := NRUEdit
  else begin
    while (prev <> nil) and (prev.NRUEdit <> self) do
      prev := prev.NRUEdit;
    if prev.NRUEdit = self then
      prev.NRUEdit := NRUEdit;
    //else not in chain?
  end;
end;

initialization
  {$I feditbook.lrs}

end.

