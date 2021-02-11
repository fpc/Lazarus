{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Maciej Izak
          Michael W. Vogel

 The PageControl shown in source editor window.
 Every unit got a own pagecontrol

}

unit DockedModulePageControl;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // LCL
  Forms, ComCtrls, Controls,
  // IDEIntf
  SrcEditorIntf, FormEditingIntf,
  // DockedFormEditor
  DockedDesignForm, DockedResizer, DockedOptionsIDE, DockedAnchorDesigner,
  DockedStrConsts;

type

  { TModulePageControl }

  TModulePageControl = class(TPageControl)
  private
    FResizer: TResizer;
    FDesignForm: TDesignForm;
    procedure OnAdjustPage(Sender: TObject);
  protected
    procedure SetDesignForm(const AValue: TDesignForm); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustPage;
    procedure CreateResizer;
    procedure DesignerSetFocus;
    procedure HideDesignPages;
    procedure InitPage;
    procedure RefreshResizer;
    procedure ShowAnchorPage;
    procedure ShowDesignPage;
  public
    property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
    property Resizer: TResizer read FResizer;
  end;

implementation

{ TModulePageControl }

procedure TModulePageControl.OnAdjustPage(Sender: TObject);
begin
  AdjustPage;
end;

procedure TModulePageControl.SetDesignForm(const AValue: TDesignForm);
begin
  if (AValue = FDesignForm) then
    // for show lfm code, if we want after editing lfm go back to form without any error
    // (when we restart IDE some error can be raised )
    if (FResizer = nil)
    or ((AValue <> nil) and (FResizer.DesignForm = AValue)) then
      Exit;

  FDesignForm := AValue;
  if AValue = nil then
  begin
    //find
    if Assigned(FResizer) then
      FResizer.DesignForm := nil;
  end else begin
    FDesignForm.OnAdjustPageNeeded := @OnAdjustPage;
    AValue.LastActiveSourceWindow := Owner as TSourceEditorWindowInterface;
    if Assigned(FResizer) then
      FResizer.DesignForm := AValue;
    AdjustPage;
  end;
end;

constructor TModulePageControl.Create(TheOwner: TComponent);
var
  LNewTabSheet: TTabSheet;
begin
  inherited Create(TheOwner);
  FResizer := nil;

  TabPosition := DockedOptions.TabPosition;
  Align := alClient;

  LNewTabSheet := TTabSheet.Create(Self);
  LNewTabSheet.PageControl := Self;
  LNewTabSheet.Caption := SCode;

  LNewTabSheet := TTabSheet.Create(Self);
  LNewTabSheet.PageControl := Self;
  LNewTabSheet.Caption := SDesigner;
  Pages[1].TabVisible := False;

  LNewTabSheet := TTabSheet.Create(Self);
  LNewTabSheet.PageControl := Self;
  LNewTabSheet.Caption := SAnchors;
  Pages[2].TabVisible := False;
end;

destructor TModulePageControl.Destroy;
begin
  DesignForm := nil;
  inherited Destroy;
end;

procedure TModulePageControl.AdjustPage;
begin
  if not (ActivePageIndex in [1,2]) then Exit;
  if Assigned(FResizer) then
    FResizer.AdjustResizer(nil);
end;

procedure TModulePageControl.CreateResizer;
begin
  if Assigned(FResizer) then
    raise Exception.Create('TModulePageControl.CreateResizer: Resizer already created');
  FResizer := TResizer.Create(Self);
  FResizer.Parent := Pages[1];
end;

procedure TModulePageControl.DesignerSetFocus;
begin
  Resizer.DesignerSetFocus;
end;

procedure TModulePageControl.HideDesignPages;
begin
  Pages[1].TabVisible := False;
  Pages[2].TabVisible := False;
end;

procedure TModulePageControl.InitPage;
begin
  case PageIndex of
    1:
      begin
        Resizer.Parent := Pages[1];
        Resizer.ResizeFrame.PanelFormClient.Visible := True;
        Resizer.ResizeFrame.PanelAnchorContainer.Visible := False;
      end;
    2:
      begin
        Resizer.Parent := Pages[2];
        Resizer.ResizeFrame.PanelFormClient.Visible := False;
        Resizer.ResizeFrame.PanelAnchorContainer.Visible := True;
        if not Assigned(DesignForm.AnchorDesigner) then
        begin
          DesignForm.AnchorDesigner := TAnchorDesigner.Create(DesignForm, Resizer.ResizeFrame.PanelAnchorContainer);
          DesignForm.AnchorDesigner.IsFocusedFunc := @Resizer.ResizeFrame.IsFocused;
          DesignForm.AnchorDesigner.OnDesignerSetFocus := @DesignerSetFocus;
        end;
        DesignForm.AnchorDesigner.Refresh;
      end;
    else;
  end;
end;

procedure TModulePageControl.RefreshResizer;
begin
  if not Assigned(FResizer) then Exit;
  FreeAndNil(FResizer);
  CreateResizer;
end;

procedure TModulePageControl.ShowAnchorPage;
begin
  Pages[2].TabVisible := DockedOptions.AnchorTabVisible;
end;

procedure TModulePageControl.ShowDesignPage;
begin
  Pages[1].TabVisible := True;
end;

end.

