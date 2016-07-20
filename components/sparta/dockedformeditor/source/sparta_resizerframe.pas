{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_ResizerFrame;

{$mode delphi}{$H+}

interface

uses
  Classes, contnrs, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  Graphics, LCLType, lclintf, Menus, LMessages, sparta_DesignedForm, Math,
  Types, FormEditingIntf, PropEdits, sparta_BasicResizeFrame;

type

  { TResizerFrame }

  TResizerFrame = class(TBasicResizeFrame)
  private
    FActivePropertyGridItemIndex: Integer;
  protected
    procedure TryBoundDesignedForm; override;
    procedure BeginFormSizeUpdate(Sender: TObject); override;
    procedure EndFormSizeUpdate(Sender: TObject); override;
  end;

implementation

{$R *.lfm}

{ TResizerFrame }

procedure TResizerFrame.TryBoundDesignedForm;
begin
  if DesignedForm = nil then
    Exit;

  // special for frames
  {DesignedForm.BeginUpdate;
  DesignedForm.RealWidth := DesignedForm.RealWidth + 1;
  DesignedForm.RealWidth := DesignedForm.RealWidth - 1;
  DesignedForm.EndUpdate;}

  inherited TryBoundDesignedForm;
end;

procedure TResizerFrame.BeginFormSizeUpdate(Sender: TObject);
begin
  inherited;

  // when was active ActivePropertyGrid.ItemIndex for height or width during scaling
  // there was problem with values :<
  if ((Sender = pR) or (Sender = pB) or (FNodes.IndexOf(Sender) in [3,4,5])) and (FormEditingHook.GetCurrentObjectInspector <> nil) then
  begin
    FActivePropertyGridItemIndex := FormEditingHook.GetCurrentObjectInspector.GetActivePropertyGrid.ItemIndex;
    FormEditingHook.GetCurrentObjectInspector.GetActivePropertyGrid.ItemIndex := -1;
  end
  else
    FActivePropertyGridItemIndex := -1;
end;

procedure TResizerFrame.EndFormSizeUpdate(Sender: TObject);
begin
  inherited;

  // restore last selected item in OI.
  if FActivePropertyGridItemIndex <> -1 then
  begin
    if FormEditingHook.GetCurrentObjectInspector <> nil then
      FormEditingHook.GetCurrentObjectInspector.GetActivePropertyGrid.ItemIndex := FActivePropertyGridItemIndex;
    FActivePropertyGridItemIndex := -1;
  end;

  GlobalDesignHook.RefreshPropertyValues;
end;

end.

