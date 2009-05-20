unit fdockbook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls;

type
  TDockBook = class(TForm)
  private
    Pages: TPageControl;
    procedure FormCreate(Sender: TObject);
  protected
  {$IFDEF new}
    procedure DoRemoveDockClient(Client: TControl); override;
    function GetDefaultDockCaption: string; override;
  {$ELSE}
  {$ENDIF}
  public
    { public declarations }
  end; 

implementation

{ TDockBook }

procedure TDockBook.FormCreate(Sender: TObject);
begin
  //nop?
end;

{$IFDEF new}
procedure TDockBook.DoRemoveDockClient(Client: TControl);
begin
(* Destroy notebook when it becomes empty.
  Notebook clients are organized in pages, not in dock clients.
  Hence we have to test for PageCount, instead of DockClientCount.
*)
  inherited;
  //DebugLn('TEasyBook.DoRemoveDockClient: remaining ' + IntToStr(PageCount));
  if PageCount = 0 then
    Application.ReleaseComponent(self);
end;

function TDockBook.GetDefaultDockCaption: string;
var
  i: integer;
  pg: TTabSheet;
begin
  Result := '';
  for i := 0 to PageCount - 1 do begin
    pg := Pages[i];
    if Result = '' then
      Result := pg.Caption
    else
      Result := Result + ' ' + pg.Caption;
  end;
end;
{$ELSE}
{$ENDIF}

initialization
  {$I fdockbook.lrs}

end.

