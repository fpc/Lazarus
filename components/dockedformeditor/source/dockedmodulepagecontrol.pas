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
{ $define DEBUGDOCKEDFORMEDITOR}

interface

uses
  // RTL
  Classes, SysUtils,
  // LCL
  Forms, ComCtrls, Controls, LCLProc,
  // IDEIntf
  SrcEditorIntf, FormEditingIntf,
  // DockedFormEditor
  DockedDesignForm, DockedResizer, DockedOptionsIDE, DockedAnchorDesigner,
  {%H-}DockedTools, DockedStrConsts;

type

  { TModulePageControl }

  TModulePageControl = class(TPageControl)
  private
    FDesignerSetFocusAsyncCount: Integer;
    FDesignForm: TDesignForm;
    FResizer: TResizer;
    FTabSheetAnchors: TTabSheet;
    FTabSheetCode: TTabSheet;
    FTabSheetDesigner: TTabSheet;
    procedure AsyncDesignerSetFocus({%H-}Data: PtrInt);
    procedure ModulePageControlMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure OnAdjustPage(Sender: TObject);
  protected
    procedure SetDesignForm(const AValue: TDesignForm); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustPage;
    function  AnchorPageActive: Boolean;
    procedure CreateResizer;
    procedure CreateTabSheetAnchors;
    procedure CreateTabSheetDesigner;
    procedure DesignerSetFocus;
    procedure DesignerSetFocusAsync;
    function  DesignerPageActive: Boolean;
    function  FormPageActive: Boolean;
    procedure RemoveDesignPages;
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

procedure TModulePageControl.ModulePageControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DesignerPageActive then
    DesignerSetFocus;
end;

procedure TModulePageControl.AsyncDesignerSetFocus(Data: PtrInt);
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.AsyncDesignerSetFocus'); {$ENDIF}
  DesignerSetFocus;
  FDesignerSetFocusAsyncCount := 0;
end;

procedure TModulePageControl.SetDesignForm(const AValue: TDesignForm);
begin
  if (AValue = FDesignForm) then
    // for show lfm code, if we want after editing lfm go back to form without any error
    // (when we restart IDE some error can be raised )
    if (FResizer = nil)
    or ((AValue <> nil) and (FResizer.DesignForm = AValue)) then
      Exit;

  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.SetDesignForm: ', DbgSName(AValue)); {$ENDIF}

  FDesignForm := AValue;
  if AValue = nil then
  begin
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
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.Create'); {$ENDIF}

  inherited Create(TheOwner);
  FDesignerSetFocusAsyncCount := 0;
  FResizer := nil;

  TabPosition := DockedOptions.TabPosition;
  Align := alClient;
  ShowTabs := False;
  OnMouseUp := @ModulePageControlMouseUp;

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
  if not DesignerPageActive then Exit;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.AdjustPage'); {$ENDIF}
  if Assigned(FResizer) then
    FResizer.AdjustResizer(nil);
end;

function TModulePageControl.AnchorPageActive: Boolean;
begin
  Result := ActivePage = FTabSheetAnchors;
end;

procedure TModulePageControl.CreateResizer;
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.CreateResizer'); {$ENDIF}
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
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.CreateTabSheetAnchors'); {$ENDIF}
  FTabSheetAnchors := TTabSheet.Create(Self);
  FTabSheetAnchors.PageControl := Self;
  FTabSheetAnchors.Caption := SAnchors;
end;

procedure TModulePageControl.CreateTabSheetDesigner;
begin
  if Assigned(FTabSheetDesigner) then Exit;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.CreateTabSheetDesigner'); {$ENDIF}
  FTabSheetDesigner := TTabSheet.Create(Self);
  FTabSheetDesigner.PageControl := Self;
  FTabSheetDesigner.Caption := SDesigner;
end;

procedure TModulePageControl.DesignerSetFocus;
begin
  if not Assigned(Resizer) then Exit;
  if not Assigned(DesignForm) then Exit;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.DesignerSetFocus'); {$ENDIF}
  Resizer.DesignerSetFocus;
end;

procedure TModulePageControl.DesignerSetFocusAsync;
begin
  if FDesignerSetFocusAsyncCount = 0 then
    Application.QueueAsyncCall(@AsyncDesignerSetFocus, 0);
  Inc(FDesignerSetFocusAsyncCount);
end;

function TModulePageControl.DesignerPageActive: Boolean;
begin
  Result := (ActivePage = FTabSheetDesigner) or
            (ActivePage = FTabSheetAnchors);
end;

function TModulePageControl.FormPageActive: Boolean;
begin
  Result := ActivePage = FTabSheetDesigner;
end;

procedure TModulePageControl.RemoveDesignPages;
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.RemoveDesignPages'); {$ENDIF}
  FreeAndNil(FTabSheetAnchors);
  FreeAndNil(FTabSheetDesigner);
  ShowTabs := False;
end;

procedure TModulePageControl.InitPage;
begin
  ShowTabs := PageCount > 1;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.InitPage: ShowTabs[' + ShowTabs.ToString(TUseBoolStrs.True) + ']'); {$ENDIF}
  if ActivePage = FTabSheetDesigner then
  begin
    Resizer.Parent := FTabSheetDesigner;
    Resizer.ResizeControl.FormClient.Visible := True;
    Resizer.ResizeControl.AnchorContainer.Visible := False;
  end
  else if ActivePage = FTabSheetAnchors then
  begin
    Resizer.Parent := FTabSheetAnchors;
    Resizer.ResizeControl.FormClient.Visible := False;
    Resizer.ResizeControl.AnchorContainer.Visible := True;
    if not Assigned(DesignForm.AnchorDesigner) then
    begin
      DesignForm.AnchorDesigner := TAnchorDesigner.Create(DesignForm, Resizer.ResizeControl.AnchorContainer);
      DesignForm.AnchorDesigner.OnDesignerSetFocus := @DesignerSetFocus;
    end;
    DesignForm.AnchorDesigner.Refresh;
  end;
end;

procedure TModulePageControl.RefreshResizer;
begin
  if not Assigned(FResizer) then Exit;
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.RefreshResizer'); {$ENDIF}
  FreeAndNil(FResizer);
  CreateResizer;
end;

procedure TModulePageControl.ShowCode;
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.ShowCode'); {$ENDIF}
  PageIndex := 0;
  InitPage;
end;

procedure TModulePageControl.ShowDesigner(AIndex: Integer);
begin
  {$IFDEF DEBUGDOCKEDFORMEDITOR} DebugLn('TModulePageControl.ShowDesigner'); {$ENDIF}
  if (AIndex = 0) or not (Pages[AIndex].TabVisible) then
    AIndex := 1;
  if PageCount <= AIndex then Exit;
  if not Pages[AIndex].TabVisible then Exit;
  PageIndex := AIndex;
  InitPage;
  OnChange(Self);
end;

end.

