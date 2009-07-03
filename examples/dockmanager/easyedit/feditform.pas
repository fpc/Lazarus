unit fEditForm;
(* Workaround: Wrap SynEdit in a form, for proper docking.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, SynEdit;

type
  TEditPage = class(TForm)
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
  protected
    function GetDefaultDockCaption: string; override;
  public
    FileName: string;
    constructor Create(AOwner: TComponent); override;
    procedure LoadFile(const AName: string);
    property DragMode;
    property DragKind;
  end;

implementation

uses
  EditMain;

{ TEditPage }

constructor TEditPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragKind := dkDock;
  DragMode := dmAutomatic;
  //FloatingDockSiteClass := TEasyDockBook;
  //FloatingDockSiteClass := TEditBook;
  FloatingDockSiteClass := TEditForm;
end;

function TEditPage.GetDefaultDockCaption: string;
begin
  Result := ExtractFileName(FileName);
  if Result = '' then
    Result := inherited GetDefaultDockCaption;
end;

procedure TEditPage.LoadFile(const AName: string);
begin
  SynEdit1.Lines.LoadFromFile(AName);
  FileName := AName;
end;

initialization
  {$I feditform.lrs}

end.

