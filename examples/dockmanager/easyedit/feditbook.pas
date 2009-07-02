unit fEditBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  fDockBook;

type
  TEditBook = class(TEasyDockBook)
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  protected
    NRUEdit: TEditBook;
  public
    { public declarations }
  end; 

var
  EditBook: TEditBook;
  MRUEdit: TEditBook;

implementation

{ TEditBook }

procedure TEditBook.FormActivate(Sender: TObject);
var
  prev: TEditBook;
begin
  if MRUEdit = Self then
    exit;
  prev := MRUEdit;
  while (prev <> nil) and (prev.NRUEdit <> self) do
    prev := prev.NRUEdit;
  if prev <> nil then
    prev.NRUEdit := self.NRUEdit;
  NRUEdit := MRUEdit;
  MRUEdit := self;
end;

initialization
  {$I feditbook.lrs}

end.

