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
    FTabSheetAnchors: TTabSheet;
    FTabSheetCode: TTabSheet;
    FTabSheetDesigner: TTabSheet;
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
    procedure CreateTabSheetAnchors;
    procedure CreateTabSheetDesigner;
    procedure DesignerSetFocus;
    procedure HideDesignPages;
    procedure InitPage;
    procedure RefreshResizer;
    procedure ShowCode;
    procedure ShowDesigner(AIndex: Integer = 0);
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
begin
  inherited Create(TheOwner);
  FResizer := nil;

  TabPosition := DockedOptions.TabPosition;
  Align := alClient;
  ShowTabs := False;

  FTabSheetCode := TTabSheet.Create(Self);
  FTabSheetCode.PageControl := Self;
  FTabSheetCode.Caption := SCode;
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
  if not Assigned(FTabSheetDesigner) then
    CreateTabSheetDesigner;
  FResizer.Parent := FTabSheetDesigner;
end;

procedure TModulePageControl.CreateTabSheetAnchors;
begin
  if not DockedOptions.AnchorTabVisible then Exit;
  if Assigned(FTabSheetAnchors) then Exit;
  FTabSheetAnchors := TTabSheet.Create(Self);
  FTabSheetAnchors.PageControl := Self;
  FTabSheetAnchors.Caption := SAnchors;
end;

procedure TModulePageControl.CreateTabSheetDesigner;
begin
  if Assigned(FTabSheetDesigner) then Exit;
  FTabSheetDesigner := TTabSheet.Create(Self);
  FTabSheetDesigner.PageControl := Self;
  FTabSheetDesigner.Caption := SDesigner;
end;

procedure TModulePageControl.DesignerSetFocus;
begin
  Resizer.DesignerSetFocus;
end;

procedure TModulePageControl.HideDesignPages;
begin
  FreeAndNil(FTabSheetAnchors);
  FreeAndNil(FTabSheetDesigner);
end;

procedure TModulePageControl.InitPage;
begin
  ShowTabs := PageCount > 1;
  if ActivePage = FTabSheetDesigner then
  begin
    Resizer.Parent := FTabSheetDesigner;
    Resizer.ResizeFrame.PanelFormClient.Visible := True;
    Resizer.ResizeFrame.PanelAnchorContainer.Visible := False;
  end
  else if ActivePage = FTabSheetAnchors then
  begin
    Resizer.Parent := FTabSheetAnchors;
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
end;

procedure TModulePageControl.RefreshResizer;
begin
  if not Assigned(FResizer) then Exit;
  FreeAndNil(FResizer);
  CreateResizer;
end;

procedure TModulePageControl.ShowCode;
begin
  PageIndex := 0;
  InitPage;
end;

procedure TModulePageControl.ShowDesigner(AIndex: Integer);
begin
  if (AIndex = 0) or not (Pages[AIndex].TabVisible) then
    AIndex := 1;
  if PageCount <= AIndex then Exit;
  if not Pages[AIndex].TabVisible then Exit;
  PageIndex := AIndex;
  InitPage;
  OnChange(Self);
end;

end.

