{ /***************************************************************************
                   designer.pp  -  Lazarus IDE unit
                   --------------------------------

                 Initial Revision  : Sat May 10 23:15:32 CST 1999


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit Designer;

{$mode objfpc}{$H+}

interface

{$DEFINE VerboseDesigner}
{ $DEFINE VerboseDesignerDraw}

uses
  Classes, LCLType, LCLLinux, Forms, Controls, LMessages, GraphType, Graphics,
  Dialogs, ControlSelection, CustomFormEditor, UnitEditor, CompReg, Menus,
  AlignCompsDlg, SizeCompsDlg, ScaleCompsDlg, ExtCtrls, EnvironmentOpts,
  DesignerProcs, PropEdits, ComponentEditors, KeyMapping;

type
  TDesigner = class;

  TOnGetSelectedComponentClass = procedure(Sender: TObject; 
    var RegisteredComponent: TRegisteredComponent) of object;
  TOnSetDesigning = procedure(Sender: TObject; Component: TComponent;
    Value: boolean) of object;
  TOnComponentAdded = procedure(Sender: TObject; Component: TComponent;
    ComponentClass: TRegisteredComponent) of object;
  TOnRemoveComponent = procedure(Sender: TObject; Component: TComponent)
    of object;
  TOnGetNonVisualCompIconCanvas = procedure(Sender: TObject;
    AComponent: TComponent; var IconCanvas: TCanvas;
    var IconWidth, IconHeight: integer) of object;
  TOnRenameComponent = procedure(Designer: TDesigner; AComponent: TComponent;
    const NewName: string) of object;
  TOnProcessCommand = procedure(Sender: TObject; Command: word;
    var Handled: boolean) of object;
    
  TDesignerFlag = (dfHasSized, dfDuringPaintControl,dfShowHints);
  TDesignerFlags = set of TDesignerFlag;

  TDesigner = class(TComponentEditorDesigner)
  private
    FCustomForm: TCustomForm;
    FTheFormEditor: TCustomFormEditor;
    FSourceEditor: TSourceEditor;
    FFlags: TDesignerFlags;
    FGridColor: TColor;
    FOnComponentAdded: TOnComponentAdded;
    FOnComponentListChanged: TNotifyEvent;
    FOnGetSelectedComponentClass: TOnGetSelectedComponentClass;
    FOnGetNonVisualCompIconCanvas: TOnGetNonVisualCompIconCanvas;
    FOnModified: TNotifyEvent;
    FOnProcessCommand: TOnProcessCommand;
    FOnPropertiesChanged: TNotifyEvent;
    FOnRemoveComponent: TOnRemoveComponent;
    FOnSetDesigning: TOnSetDesigning;
    FOnUnselectComponentClass: TNotifyEvent;
    FOnActivated: TNotifyEvent;
    FOnRenameComponent: TOnRenameComponent;
    FPopupMenu: TPopupMenu;
    FShiftState: TShiftState;
    FAlignMenuItem: TMenuItem;
    FMirrorHorizontalMenuItem: TMenuItem;
    FMirrorVerticalMenuItem: TMenuItem;
    FScaleMenuItem: TMenuItem;
    FSizeMenuItem: TMenuItem;
    FBringToFrontMenuItem: TMenuItem;
    FSendToBackMenuItem: TMenuItem;
    FDeleteSelectionMenuItem: TMenuItem;

    //hint stuff
    FHintTimer : TTimer;
    FHintWIndow : THintWindow;
    
    function GetGridColor: TColor;
    function GetShowGrid: boolean;
    function GetGridSizeX: integer;
    function GetGridSizeY: integer;
    function GetIsControl: Boolean;
    function GetShowHints: boolean;
    function GetSnapToGrid: boolean;
    Procedure HintTimer(sender : TObject);
    procedure InvalidateWithParent(AComponent: TComponent);
    procedure SetGridColor(const AValue: TColor);
    procedure SetShowGrid(const AValue: boolean);
    procedure SetGridSizeX(const AValue: integer);
    procedure SetGridSizeY(const AValue: integer);
    procedure SetIsControl(Value: Boolean);
    procedure SetShowHints(const AValue: boolean);
    procedure SetSnapToGrid(const AValue: boolean);
  protected
    MouseDownComponent: TComponent;
    MouseDownSender: TComponent;
    MouseDownPos: TPoint;
    MouseDownClickCount: integer;
    MouseUpPos: TPoint;
    LastMouseMovePos: TPoint;
    PopupMenuComponentEditor: TBaseComponentEditor;
    LastFormCursor: TCursor;

    function PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
    function SizeControl(Sender: TControl; TheMessage: TLMSize):boolean;
    function MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
    Procedure MouseDownOnControl(Sender: TControl; TheMessage : TLMMouse);
    Procedure MouseMoveOnControl(Sender: TControl; var TheMessage: TLMMouse);
    Procedure MouseUpOnControl(Sender: TControl; TheMessage:TLMMouse);
    Procedure KeyDown(Sender: TControl; TheMessage:TLMKEY);
    Procedure KeyUp(Sender: TControl; TheMessage:TLMKEY);

    procedure DoDeleteSelectedComponents;
    //procedure DoCreateComponent(AComponent);
    function GetSelectedComponentClass: TRegisteredComponent;
    Procedure NudgeControl(DiffX, DiffY: Integer);
    Procedure NudgeSize(DiffX, DiffY: Integer);

    procedure BuildPopupMenu;
    procedure OnAlignPopupMenuClick(Sender: TObject);
    procedure OnMirrorHorizontalPopupMenuClick(Sender: TObject);
    procedure OnMirrorVerticalPopupMenuClick(Sender: TObject);
    procedure OnScalePopupMenuClick(Sender: TObject);
    procedure OnSizePopupMenuClick(Sender: TObject);
    procedure OnBringToFrontMenuClick(Sender: TObject);
    procedure OnSendToBackMenuClick(Sender: TObject);
    procedure OnDeleteSelectionMenuClick(Sender: TObject);
    Procedure OnFormActivated;
    procedure OnComponentEditorVerbMenuItemClick(Sender: TObject);

    function GetPropertyEditorHook: TPropertyEditorHook; override;
  public
    ControlSelection : TControlSelection;
    DDC: TDesignerDeviceContext;
    
    constructor Create(Customform : TCustomform;
       AControlSelection: TControlSelection);
    destructor Destroy; override;

    procedure Modified; override;
    Procedure SelectOnlyThisComponent(AComponent:TComponent); override;
    function InvokeComponentEditor(AComponent: TComponent;
      MenuIndex: integer): boolean;
    Procedure RemoveComponent(AComponent: TComponent);

    function NonVisualComponentLeftTop(AComponent: TComponent): TPoint;
    function NonVisualComponentAtPos(x,y: integer): TComponent;
    function GetDesignedComponent(AComponent: TComponent): TComponent;
    function GetComponentEditorForSelection: TBaseComponentEditor;
    procedure AddComponentEditorMenuItems(
      AComponentEditor: TBaseComponentEditor; AParentMenuItem: TMenuItem);

    function IsDesignMsg(Sender: TControl;
       var TheMessage: TLMessage): Boolean; override;
    procedure Notification(AComponent: TComponent;
       Operation: TOperation); override;
    procedure ValidateRename(AComponent: TComponent;
       const CurName, NewName: string); override;
    function GetShiftState: TShiftState; override;
    function CreateUniqueComponentName(const AClassName: string): string; override;

    procedure PaintGrid; override;
    procedure PaintClientGrid(AWinControl: TWinControl;
       aDDC: TDesignerDeviceContext);
    procedure DrawNonVisualComponents(aDDC: TDesignerDeviceContext);

    property ShowGrid: boolean read GetShowGrid write SetShowGrid;
    property Form: TCustomForm read FCustomForm write FCustomForm;
    property TheFormEditor: TCustomFormEditor read FTheFormEditor write FTheFormEditor;
    property GridSizeX: integer read GetGridSizeX write SetGridSizeX;
    property GridSizeY: integer read GetGridSizeY write SetGridSizeY;
    property GridColor: TColor read GetGridColor write SetGridColor;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property OnActivated: TNotifyEvent
       read FOnActivated write FOnActivated;
    property OnComponentAdded: TOnComponentAdded
       read FOnComponentAdded write FOnComponentAdded;
    property OnComponentListChanged: TNotifyEvent
       read FOnComponentListChanged write FOnComponentListChanged;
    property OnGetSelectedComponentClass: TOnGetSelectedComponentClass
       read FOnGetSelectedComponentClass write FOnGetSelectedComponentClass;
    property OnProcessCommand: TOnProcessCommand
       read FOnProcessCommand write FOnProcessCommand;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnPropertiesChanged: TNotifyEvent
       read FOnPropertiesChanged write FOnPropertiesChanged;
    property OnRemoveComponent: TOnRemoveComponent
       read FOnRemoveComponent write FOnRemoveComponent;
    property OnRenameComponent: TOnRenameComponent
       read FOnRenameComponent write FOnRenameComponent;
    property OnSetDesigning: TOnSetDesigning
       read FOnSetDesigning write FOnSetDesigning;
    property OnUnselectComponentClass: TNotifyEvent
       read FOnUnselectComponentClass write FOnUnselectComponentClass;
    property OnGetNonVisualCompIconCanvas: TOnGetNonVisualCompIconCanvas
       read FOnGetNonVisualCompIconCanvas write FOnGetNonVisualCompIconCanvas;
    property ShowHints: boolean read GetShowHints write SetShowHints;
    property SnapToGrid: boolean read GetSnapToGrid write SetSnapToGrid;
    property SourceEditor : TSourceEditor read FSourceEditor write FSourceEditor;
  end;


implementation


uses
  SysUtils, Math;

const
  mk_lbutton =   1;
  mk_rbutton =   2;
  mk_shift   =   4;
  mk_control =   8;
  mk_mbutton = $10;


constructor TDesigner.Create(CustomForm : TCustomForm; 
  AControlSelection: TControlSelection);
begin
  inherited Create;
  FCustomForm := CustomForm;
  ControlSelection:=AControlSelection;
  FFlags:=[];
  FGridColor:=clGray;

  FHintTimer := TTimer.Create(nil);
  FHintTimer.Interval := 500;
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := @HintTimer;
  
  FHintWindow := THintWindow.Create(nil);

  FHIntWindow.Visible := False;
  FHintWindow.HideInterval := 4000;
  FHintWindow.AutoHide := True;
  
  DDC:=TDesignerDeviceContext.Create;
  LastFormCursor:=crDefault;
end;

destructor TDesigner.Destroy;
Begin
  if FPopupMenu<>nil then
    FPopupMenu.Free;
    
  FHintWIndow.Free;
  FHintTimer.Free;
  DDC.Free;
  Inherited Destroy;
end;

Procedure TDesigner.RemoveComponent(AComponent :TComponent);
var
  i: integer;
  AWinControl: TWinControl;
  ChildControl: TControl;
Begin
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.RemoveControl] ',AComponent.Name,':',AComponent.ClassName);
  {$ENDIF}
  if AComponent=Form then exit;
  // remove all child controls owned by the form
  if (AComponent is TWinControl) then begin
    AWinControl:=TWinControl(AComponent);
    i:=AWinControl.ControlCount-1;
    while (i>=0) do begin
      ChildControl:=AWinControl.Controls[i];
      if ChildControl.Owner=Form then begin
        RemoveComponent(ChildControl);
        // the component list of the form has changed
        // -> restart the search
        i:=AWinControl.ControlCount-1;
        continue;
      end;
    end;
    dec(i);
  end;
  // remove component
  if Assigned(FOnRemoveComponent) then
    FOnRemoveComponent(Self,AComponent);
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.RemoveControl] C ',AComponent.Name,':',AComponent.ClassName);
  {$ENDIF}
  Form.Invalidate;
  // this sends a message to notification and removes it from the controlselection
  TheFormEditor.DeleteControl(AComponent);
end;

Procedure TDesigner.NudgeControl(DiffX, DiffY : Integer);
Begin
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.NudgeControl]');
  {$ENDIF}
  ControlSelection.MoveSelection(DiffX, DiffY);
  if ControlSelection.OnlyNonVisualComponentsSelected then
    FCustomForm.Invalidate;
end;

Procedure TDesigner.NudgeSize(DiffX, DiffY: Integer);
Begin
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.NudgeSize]');
  {$ENDIF}
  ControlSelection.SizeSelection(DiffX, DiffY);
end;

procedure TDesigner.SelectOnlyThisComponent(AComponent:TComponent);
begin
  ControlSelection.AssignComponent(AComponent);
end;

function TDesigner.InvokeComponentEditor(AComponent: TComponent;
  MenuIndex: integer): boolean;
var
  CompEditor: TBaseComponentEditor;
begin
  Result:=false;
  writeln('TDesigner.InvokeComponentEditor A ',AComponent.Name,':',AComponent.ClassName);
  CompEditor:=TheFormEditor.GetComponentEditor(AComponent);
  if CompEditor=nil then begin
    writeln('TDesigner.InvokeComponentEditor',
      ' WARNING: no component editor found for ',
        AComponent.Name,':',AComponent.ClassName);
    exit;
  end;
  writeln('TDesigner.InvokeComponentEditor B ',CompEditor.ClassName);
  try
    CompEditor.Edit;
    Result:=true;
  except
    on E: Exception do begin
      writeln('TDesigner.InvokeComponentEditor ERROR: ',E.Message);
      MessageDlg('Error in '+CompEditor.ClassName,
        'The component editor of class "'+CompEditor.ClassName+'"'
        +'has created the error:'#13
        +'"'+E.Message+'"',
        mtError,[mbOk],0);
    end;
  end;
end;

function TDesigner.NonVisualComponentLeftTop(AComponent: TComponent): TPoint;
begin
  Result.X:=Min(LongRec(AComponent.DesignInfo).Lo,
                Form.ClientWidth-NonVisualCompWidth);
  Result.Y:=Min(LongRec(AComponent.DesignInfo).Hi,
                Form.ClientHeight-NonVisualCompWidth);
end;

procedure TDesigner.InvalidateWithParent(AComponent: TComponent);
begin
  {$IFDEF VerboseDesigner}
  writeln('TDesigner.INVALIDATEWITHPARENT ',AComponent.Name,':',AComponent.ClassName);
  {$ENDIF}
  if AComponent is TControl then begin
    if TControl(AComponent).Parent<>nil then
      TControl(AComponent).Parent.Invalidate
    else
      TControl(AComponent).Invalidate;
  end else begin
    FCustomForm.Invalidate;
  end;
end;

procedure TDesigner.SetGridColor(const AValue: TColor);
begin
  if GridColor=AValue then exit;
  EnvironmentOptions.GridColor:=AValue;
  Form.Invalidate;
end;

function TDesigner.PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
var OldDuringPaintControl: boolean;
begin
  Result:=true;
  
//writeln('TDesigner.PaintControl A ',Sender.Name);
  //writeln('***  LM_PAINT A ',Sender.Name,':',Sender.ClassName,' DC=',HexStr(Message.DC,8));
  // Set flag
  OldDuringPaintControl:=dfDuringPaintControl in FFlags;
  Include(FFlags,dfDuringPaintControl);
  
  // send the Paint message to the control, so that it paints itself
//writeln('TDesigner.PaintControl B ',Sender.Name);
  Sender.Dispatch(TheMessage);
  //writeln('TDesigner.PaintControl C ',Sender.Name,' DC=',HexStr(Cardinal(TheMessage.DC),8));

  // paint the Designer stuff
  if TheMessage.DC<>0 then begin
    DDC.SetDC(Form,TheMessage.DC);
    {$IFDEF VerboseDesignerDraw}
    writeln('TDesigner.PaintControl D ',Sender.Name,':',Sender.ClassName,
      ' DC=',HexStr(DDC.DC,8),
      ' FormOrigin=',DDC.FormOrigin.X,',',DDC.FormOrigin.Y,
      ' DCOrigin=',DDC.DCOrigin.X,',',DDC.DCOrigin.Y,
      ' FormClientOrigin=',DDC.FormClientOrigin.X,',',DDC.FormClientOrigin.Y
      );
    {$ENDIF}
    if (Sender is TWinControl)
    and (csAcceptsControls in Sender.ControlStyle) then begin
      PaintClientGrid(TWinControl(Sender),DDC);
    end;
    if (ControlSelection.IsSelected(Sender)) then begin
      // writeln('***  LM_PAINT ',Sender.Name,':',Sender.ClassName,' DC=',HexStr(Message.DC,8));
      ControlSelection.DrawMarker(Sender,DDC);
    end;
    DrawNonVisualComponents(DDC);
    ControlSelection.DrawGuideLines(DDC);
    ControlSelection.DrawGrabbers(DDC);
    if ControlSelection.RubberBandActive then
      ControlSelection.DrawRubberBand(DDC);
    DDC.Clear;
  end;
//writeln('TDesigner.PaintControl END ',Sender.Name);

  if not OldDuringPaintControl then
    Exclude(FFlags,dfDuringPaintControl);
end;

function TDesigner.SizeControl(Sender: TControl; TheMessage: TLMSize):boolean;
begin
  Result:=true;
  Sender.Dispatch(TheMessage);
  if (ControlSelection.IsSelected(Sender)) then begin
    {writeln('###  TDesigner.SizeControl ',Sender.Name,':',Sender.ClassName,
      ' ',Sender.Width,',',Sender.Height,
      ' Type=',TheMessage.SizeType
      ,' ',TheMessage.Width,',',TheMessage.Height,' Pos=',Sender.Left,',',Sender.Top);}
    if not ControlSelection.IsResizing then begin
      ControlSelection.UpdateBounds;
      if Assigned(FOnPropertiesChanged) then
        FOnPropertiesChanged(Self);
    end;
  end;
end;

function TDesigner.MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
begin
  Result:=true;
  Sender.Dispatch(TheMessage);
  if (ControlSelection.IsSelected(Sender)) then begin
    //    writeln('***  LM_Move ',Sender.Name,':',Sender.ClassName);
    ControlSelection.UpdateBounds;
    if Assigned(FOnPropertiesChanged) then
      FOnPropertiesChanged(Self);
  end;
end;

procedure TDesigner.MouseDownOnControl(Sender: TControl; TheMessage: TLMMouse);
var i,
  CompIndex:integer;
  SelectedCompClass: TRegisteredComponent;
  NonVisualComp: TComponent;
Begin
  FHintTimer.Enabled := False;
  Exclude(FFLags,dfHasSized);
  SetCaptureControl(nil);
  if (getParentForm(Sender)=nil) then exit;

  if MouseDownComponent=nil then begin
    MouseDownComponent:=GetDesignedComponent(Sender);
    if MouseDownComponent=nil then exit;
    MouseDownSender:=Sender;
  end;
  case TheMessage.Msg of
  LM_LBUTTONDOWN,LM_MBUTTONDOWN,LM_RBUTTONDOWN:
    MouseDownClickCount:=1;
    
  LM_LBUTTONDBLCLK,LM_MBUTTONDBLCLK,LM_RBUTTONDBLCLK:
    MouseDownClickCount:=2;
    
  LM_LBUTTONTRIPLECLK,LM_MBUTTONTRIPLECLK,LM_RBUTTONTRIPLECLK:
    MouseDownClickCount:=3;
    
  LM_LBUTTONQUADCLK,LM_MBUTTONQUADCLK,LM_RBUTTONQUADCLK:
    MouseDownClickCount:=4;
  else
    MouseDownClickCount:=1;
  end;

  MouseDownPos:=GetFormRelativeMousePosition(Form);
  LastMouseMovePos:=MouseDownPos;

  {$IFDEF VerboseDesigner}
  writeln('************************************************************');
  write('MouseDownOnControl');
  write(' ',Sender.Name,':',Sender.ClassName);
  write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
  write(' Mouse=',MouseDownPos.X,',',MouseDownPos.Y);
  writeln('');

  if (TheMessage.Keys and MK_Shift) = MK_Shift then
    Write(' Shift down')
  else
    Write(' No Shift down');

  if (TheMessage.Keys and MK_Control) = MK_Control then
    Writeln(', CTRL down')
  else
    Writeln(', No CTRL down');
  {$ENDIF}

  SelectedCompClass:=nil;
  if Assigned(FOnGetSelectedComponentClass) then
    FOnGetSelectedComponentClass(Self,SelectedCompClass);

  if (TheMessage.Keys and MK_LButton) > 0 then begin
    // left button
    // -> check if a grabber was activated
    ControlSelection.ActiveGrabber:=
      ControlSelection.GrabberAtPos(MouseDownPos.X,MouseDownPos.Y);
      
    if SelectedCompClass = nil then begin
      // selection mode
      if ControlSelection.ActiveGrabber=nil then begin
        NonVisualComp:=NonVisualComponentAtPos(MouseDownPos.X,MouseDownPos.Y);
        if NonVisualComp<>nil then MouseDownComponent:=NonVisualComp;
        
        CompIndex:=ControlSelection.IndexOf(MouseDownComponent);
        if (TheMessage.Keys and MK_SHIFT)>0 then begin
        
          // shift key pressed (multiselection)
          if CompIndex<0 then begin
            // not selected
            // add component to selection
            if (ControlSelection.Count=0)
            or (not (Sender is TCustomForm)) then begin
              ControlSelection.Add(MouseDownComponent);
              InvalidateWithParent(MouseDownComponent);
            end;
          end else begin
            // remove from multiselection
            ControlSelection.Delete(CompIndex);
            InvalidateWithParent(MouseDownComponent);
          end;
        end else begin
        
          // no shift key (single selection)
          if (CompIndex<0) then begin
            // select only this component
            
            // invalidate old components
            for i:=0 to ControlSelection.Count-1 do
              if ControlSelection[i].Component is TControl then
                InvalidateWithParent(TControl(ControlSelection[i].Component));
                
            // clear old selection and select new component
            ControlSelection.BeginUpdate;
            ControlSelection.Clear;
            ControlSelection.Add(MouseDownComponent);
            ControlSelection.EndUpdate;
            InvalidateWithParent(MouseDownComponent);
          end;
        end;
      end else begin
        // mouse down on grabber -> begin sizing
        // grabber is already activated
        // the sizing is handled in mousemove
      end;
    end else begin
      // add component mode  -> handled in mousemove and mouseup
    end;
  end else begin
    // not left button
    ControlSelection.ActiveGrabber:=nil;
  end;

  {$IFDEF VerboseDesigner}
  writeln('[TDesigner.MouseDownOnControl] END');
  {$ENDIF}
End;

procedure TDesigner.MouseUpOnControl(Sender : TControl;
  TheMessage:TLMMouse);
var
  ParentCI, NewCI: TComponentInterface;
  NewLeft, NewTop, NewWidth, NewHeight, MoveX, MoveY: Integer;
  Shift: TShiftState;
  SenderParentForm: TCustomForm;
  RubberBandWasActive: boolean;
  ParentClientOrigin: TPoint;
  SelectedCompClass: TRegisteredComponent;
  NewParent: TWinControl;
  SelectionChanged, NewRubberbandSelection: boolean;
  
  procedure GetShift;
  begin
    Shift := [];
    if (TheMessage.keys and MK_Shift) = MK_Shift then
      Shift := [ssShift];
    if (TheMessage.keys and MK_Control) = MK_Control then
      Include(Shift,ssCtrl);
      
    case TheMessage.Msg of
    LM_LBUTTONUP: Include(Shift,ssLeft);
    LM_MBUTTONUP: Include(Shift,ssMiddle);
    LM_RBUTTONUP: Include(Shift,ssRight);
    end;
    
    if MouseDownClickCount=2 then
      Include(Shift,ssDouble);
    if MouseDownClickCount=3 then
      Include(Shift,ssTriple);
    if MouseDownClickCount=4 then
      Include(Shift,ssQuad);
  end;
  
  procedure AddComponent;
  begin
    // add a new component
    ControlSelection.RubberbandActive:=false;
    ControlSelection.BeginUpdate;

    // find a parent for the new component
    NewParent:=TWinControl(MouseDownComponent);
    while (NewParent<>nil)
    and ((not (csAcceptsControls in NewParent.ControlStyle))
      or ((NewParent.Owner<>Form) and (NewParent<>Form)))
    do begin
      NewParent:=NewParent.Parent;
    end;
    ParentCI:=TComponentInterface(TheFormEditor.FindComponent(NewParent));
    if Assigned(ParentCI) then begin
      ParentClientOrigin:=GetParentFormRelativeClientOrigin(NewParent);
      NewLeft:=Min(MouseDownPos.X,MouseUpPos.X)-ParentClientOrigin.X;
      NewWidth:=Abs(MouseUpPos.X-MouseDownPos.X);
      NewTop:=Min(MouseDownPos.Y,MouseUpPos.Y)-ParentClientOrigin.Y;
      NewHeight:=Abs(MouseUpPos.Y-MouseDownPos.Y);
      if Abs(NewWidth+NewHeight)<7 then begin
        // this very small component is probably only a wag, take default size
        NewWidth:=0;
        NewHeight:=0;
      end;

      NewCI := TComponentInterface(TheFormEditor.CreateComponent(
         ParentCI,SelectedCompClass.ComponentClass
        ,NewLeft,NewTop,NewWidth,NewHeight));
      if NewCI.Component is TControl then
        TControl(NewCI.Component).Visible:=true;
      if Assigned(FOnSetDesigning) then
        FOnSetDesigning(Self,NewCI.Component,True);
      if Assigned(FOnComponentAdded) then
        FOnComponentAdded(Self,NewCI.Component,SelectedCompClass);

      SelectOnlyThisComponent(TComponent(NewCI.Component));
      if not (ssShift in Shift) then
        if Assigned(FOnUnselectComponentClass) then
          // this resets the component palette to the selection tool
          FOnUnselectComponentClass(Self);
      Form.Invalidate;
      {$IFDEF VerboseDesigner}
      writeln('NEW COMPONENT ADDED: Form.ComponentCount=',Form.ComponentCount,
         '  NewCI.Control.Owner.Name=',NewCI.Component.Owner.Name);
      {$ENDIF}
    end;
    ControlSelection.EndUpdate;
  end;
  
  procedure RubberbandSelect;
  begin
    ControlSelection.BeginUpdate;
    NewRubberbandSelection:=(not (ssShift in Shift))
                            and ControlSelection.IsOnlySelected(Form);
    SelectionChanged:=false;
    ControlSelection.SelectWithRubberBand(
      Form,NewRubberbandSelection,ssShift in Shift,SelectionChanged);
    if ControlSelection.Count=0 then begin
      ControlSelection.Add(Form);
      SelectionChanged:=true;
    end;
    ControlSelection.RubberbandActive:=false;
    ControlSelection.EndUpdate;
    with ControlSelection.Grabbers[0] do
      writeln('AAA1 ',Left,',',Top,',',Width,',',Height);
    Form.Invalidate;
  end;
  
  procedure PointSelect;
  begin
    if (not (ssShift in Shift)) then begin
      // select only the mouse down component
      if ControlSelection.AssignComponent(MouseDownComponent) then
        Form.Invalidate;
      if MouseDownClickCount=2 then begin
        // Double Click -> invoke 'Edit' of the component editor
        FShiftState:=Shift;
        InvokeComponentEditor(MouseDownComponent,-1);
        FShiftState:=[];
      end;
    end;
  end;
  
  procedure DisableRubberBand;
  begin
    if ControlSelection.RubberbandActive then begin
      ControlSelection.RubberbandActive:=false;
      Form.Invalidate;
    end;
  end;
  
Begin
  FHintTimer.Enabled := False;
  SetCaptureControl(nil);
  
  // check if the message is for the designed form
  SenderParentForm:=GetParentForm(Sender);
  if (MouseDownComponent=nil) or (SenderParentForm=nil)
  or (SenderParentForm<>Form) then begin
    MouseDownComponent:=nil;
    MouseDownSender:=nil;
    exit;
  end;
  
  ControlSelection.ActiveGrabber:=nil;
  RubberBandWasActive:=ControlSelection.RubberBandActive;
  
  GetShift;

  MouseUpPos:=GetFormRelativeMousePosition(Form);
  MoveX:=MouseUpPos.X-MouseDownPos.X;
  MoveY:=MouseUpPos.Y-MouseDownPos.Y;

  SelectedCompClass:=GetSelectedComponentClass;

  {$IFDEF VerboseDesigner}
  writeln('************************************************************');
  write('MouseUpOnControl');
  write(' ',Sender.Name,':',Sender.ClassName);
  write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
  write(' Move=',MoveX,',',MoveY);
  writeln('');
  {$ENDIF}

  if TheMessage.Msg=LM_LBUTTONUP then begin
    if SelectedCompClass = nil then begin
      // layout mode (selection, moving and resizing)
      if not (dfHasSized in FFlags) then begin
        // new selection
        if RubberBandWasActive then begin
          // rubberband selection
          RubberbandSelect;
        end else begin
          // point selection
          PointSelect;
        end;
      end;
    end else begin
      // create new a component on the form
      AddComponent;
    end;
  end else if TheMessage.Msg=LM_RBUTTONUP then begin
    // right click -> popup menu
    DisableRubberBand;
    if not ControlSelection.IsSelected(MouseDownComponent) then
      PointSelect;
    PopupMenuComponentEditor:=GetComponentEditorForSelection;
    BuildPopupMenu;
    FPopupMenu.Popup(MouseUpPos.X,MouseUpPos.Y);
  end;
  
  DisableRubberBand;

  LastMouseMovePos.X:=-1;
  Exclude(FFlags,dfHasSized);

  MouseDownComponent:=nil;
  MouseDownSender:=nil;
  {$IFDEF VerboseDesigner}
  writeln('[TDesigner.MouseLeftUpOnControl] END');
  {$ENDIF}
end;

procedure TDesigner.MouseMoveOnControl(Sender: TControl;
  var TheMessage: TLMMouse);
var
  Shift : TShiftState;
  SenderParentForm:TCustomForm;
  OldMouseMovePos: TPoint;
  Grabber: TGrabber;
  ACursor: TCursor;
begin
  SetCaptureControl(nil);
  if ShowHints then begin
    FHintTimer.Enabled := False;

    { don't want it enabled when a mouse button is pressed. }
    FHintTimer.Enabled :=
          (TheMessage.keys or (MK_LButton and MK_RButton and MK_MButton) = 0);
    if FHintWindow.Visible then
      FHintWindow.Visible := False;
  end;

  SenderParentForm:= GetParentForm(Sender);
  if (SenderParentForm = nil) or (SenderParentForm <> Form) then exit;

  OldMouseMovePos:= LastMouseMovePos;
  LastMouseMovePos:= GetFormRelativeMousePosition(Form);
  if (OldMouseMovePos.X=LastMouseMovePos.X)
  and (OldMouseMovePos.Y=LastMouseMovePos.Y) then exit;

  Grabber:= ControlSelection.GrabberAtPos(LastMouseMovePos.X, LastMouseMovePos.Y);
  if Grabber = nil then
    ACursor:= crDefault
  else begin
    ACursor:= Grabber.Cursor;
  end;
  if ACursor<>LastFormCursor then begin
    LastFormCursor:=ACursor;
    CNSendMessage(LM_SETCURSOR, Form, Pointer(Integer(ACursor)));
  end;

  if MouseDownComponent=nil then exit;

  Shift := [];
  if (TheMessage.keys and MK_Shift) = MK_Shift then
    Shift := [ssShift];
  if (TheMessage.keys and MK_Control) = MK_Control then
    Shift := Shift + [ssCTRL];

  if (TheMessage.keys and MK_LButton) = MK_LButton then begin
    // left button pressed
    if ControlSelection.ActiveGrabber<>nil then begin
      // grabber moving -> size selection
      if not (dfHasSized in FFlags) then begin
        ControlSelection.SaveBounds;
        Include(FFlags,dfHasSized);
      end;
      ControlSelection.SizeSelection(
        LastMouseMovePos.X-OldMouseMovePos.X,
        LastMouseMovePos.Y-OldMouseMovePos.Y);
      FCustomForm.Invalidate;
      if Assigned(OnModified) then OnModified(Self);
    end else begin
      if (not ComponentIsTopLvl(MouseDownComponent))
      and (ControlSelection.Count>=1)
      and not (ControlSelection[0].Component is TCustomForm) then
      begin
        // move selection
        if not (dfHasSized in FFlags) then begin
          ControlSelection.SaveBounds;
          Include(FFlags,dfHasSized);
        end;
        if ControlSelection.MoveSelectionWithSnapping(
          LastMouseMovePos.X-MouseDownPos.X,LastMouseMovePos.Y-MouseDownPos.Y)
        then begin
          if Assigned(OnModified) then OnModified(Self);
          FCustomForm.Invalidate;
        end;
      end
      else
      begin
        // rubberband sizing
        ControlSelection.RubberBandBounds:=Rect(MouseDownPos.X,MouseDownPos.Y,
                                                LastMouseMovePos.X,
                                                LastMouseMovePos.Y);
        ControlSelection.RubberBandActive:=true;
        SenderParentForm.Invalidate;
      end;
    end;
  end else begin
    ControlSelection.ActiveGrabber:=nil;
  end;
end;


{
-----------------------------K E Y D O W N -------------------------------
}
{
 Handles the keydown messages.  DEL deletes the selected controls, CTRL-ARROR
 moves the selection up one, SHIFT-ARROW resizes, etc.
}
Procedure TDesigner.KeyDown(Sender : TControl; TheMessage:TLMKEY);
var
  Shift : TShiftState;
  Command: word;
  Handled: boolean;
Begin
  {$IFDEF VerboseDesigner}
  Writeln('TDesigner.KEYDOWN ',TheMessage.CharCode,' ',TheMessage.KeyData);
  {$ENDIF}

  Shift := KeyDataToShiftState(TheMessage.KeyData);

  Handled:=false;
  Command:=FTheFormEditor.TranslateKeyToDesignerCommand(
                                                 TheMessage.CharCode,Shift);
  if Assigned(OnProcessCommand) and (Command<>ecNone) then begin
    OnProcessCommand(Self,Command,Handled);
    if Handled or (Command=ecNone) then exit;
  end;

  case TheMessage.CharCode of
  VK_DELETE:
    DoDeleteSelectedComponents;

  VK_UP:
    if (ssCtrl in Shift) then
      NudgeControl(0,-1)
    else if (ssShift in Shift) then
      NudgeSize(0,-1);

  VK_DOWN:
    if (ssCtrl in Shift) then
      NudgeControl(0,1)
    else if (ssShift in Shift) then
      NudgeSize(0,1);

  VK_RIGHT:
    if (ssCtrl in Shift) then
      NudgeControl(1,0)
    else if (ssShift in Shift) then
      NudgeSize(1,0);

  VK_LEFT:
    if (ssCtrl in Shift) then
      NudgeControl(-1,0)
    else if (ssShift in Shift) then
      NudgeSize(-1,0);

  end;
end;


{-----------------------------------------K E Y U P --------------------------------}
Procedure TDesigner.KeyUp(Sender : TControl; TheMessage:TLMKEY);
Begin
  {$IFDEF VerboseDesigner}
  Writeln('TDesigner.KEYUP ',TheMessage.CharCode,' ',TheMessage.KeyData);
  {$ENDIF}
end;

procedure TDesigner.DoDeleteSelectedComponents;
var
  AComponent: TComponent;
begin
  if (ControlSelection.IsSelected(FCustomForm)) then begin
    if ControlSelection.Count>1 then
      MessageDlg('Invalid delete',
       'Selections can not be deleted, if the form is selected.',mtInformation,
       [mbOk],0);
    exit;
  end;
  ControlSelection.BeginUpdate;
  while ControlSelection.Count>0 do Begin
    AComponent:=ControlSelection[ControlSelection.Count-1].Component;
    {$IFDEF VerboseDesigner}
    Writeln('TDesigner: Removing control: ',
      AComponent.Name,':',AComponent.ClassName);
    {$ENDIF}
    RemoveComponent(AComponent);
  End;
  SelectOnlythisComponent(FCustomForm);
  ControlSelection.EndUpdate;
end;

function TDesigner.GetSelectedComponentClass: TRegisteredComponent;
begin
  Result:=nil;
  if Assigned(FOnGetSelectedComponentClass) then
    FOnGetSelectedComponentClass(Self,Result);
end;

function TDesigner.IsDesignMsg(Sender: TControl; var TheMessage: TLMessage): Boolean;
Begin
  Result := false;
  if csDesigning in Sender.ComponentState then begin
    Result:=true;
    case TheMessage.Msg of
      LM_PAINT:   Result:=PaintControl(Sender,TLMPaint(TheMessage));
      LM_KEYDOWN: KeyDown(Sender,TLMKey(TheMessage));
      LM_KEYUP:   KeyUP(Sender,TLMKey(TheMessage));
      LM_LBUTTONDOWN,
      LM_RBUTTONDOWN,
      LM_LBUTTONDBLCLK: MouseDownOnControl(Sender,TLMMouse(TheMessage));
      LM_LBUTTONUP,
      LM_RBUTTONUP:   MouseUpOnControl(Sender,TLMMouse(TheMessage));
      LM_MOUSEMOVE:   MouseMoveOnControl(Sender, TLMMouse(TheMessage));
      LM_SIZE:    Result:=SizeControl(Sender,TLMSize(TheMessage));
      LM_MOVE:    Result:=MoveControl(Sender,TLMMove(TheMessage));
      LM_ACTIVATE : OnFormActivated;
//      CM_MOUSELEAVE:  Writeln('MOUSELEAVE!!!!!!!!!!!!');//Result:=MoveControl(Sender,TLMMove(Message));
    else
      Result:=false;
    end;
  end;
end;

procedure TDesigner.Modified;
Begin
  ControlSelection.SaveBounds;
  Form.Invalidate;
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TDesigner.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  if Operation = opInsert then begin
    {$IFDEF VerboseDesigner}
    Writeln('opInsert');
    {$ENDIF}
  end
  else
  if Operation = opRemove then begin
    {$IFDEF VerboseDesigner}
    writeln('[TDesigner.Notification] opRemove ',
            AComponent.Name,':',AComponent.ClassName);
    {$ENDIF}
    PopupMenuComponentEditor:=nil;
    ControlSelection.Remove(AComponent);
    TheFormEditor.DeleteControl(AComponent);
  end;
end;

procedure TDesigner.PaintGrid;
begin
  // This is done in PaintControls
end;

procedure TDesigner.PaintClientGrid(AWinControl: TWinControl;
  aDDC: TDesignerDeviceContext);
var
  Clip: integer;
  Count: integer;
  x,y, StepX, StepY, MaxX, MaxY: integer;
  i: integer;
  SavedDC: hDC;
  LogPen: TLogPen;
  Pen, OldPen: HPen;
  OldPoint: TPoint;
begin
  if (AWinControl=nil)
  or (not (csAcceptsControls in AWinControl.ControlStyle))
  or (not ShowGrid) then exit;
  
  SavedDC:=SaveDC(aDDC.DC);
  try
    // exclude all child control areas
    Count:=AWinControl.ControlCount;
    for I := 0 to Count - 1 do begin
      with AWinControl.Controls[I] do begin
        if (Visible or (csDesigning in ComponentState)
        and not (csNoDesignVisible in ControlStyle))
        and (csOpaque in ControlStyle) then
        begin
          Clip := ExcludeClipRect(aDDC.DC, Left, Top, Left + Width, Top + Height);
          if Clip = NullRegion then exit;
        end;
      end;
    end;

    // select color
    with LogPen do
    begin
      lopnStyle := PS_SOLID;
      lopnWidth.X := 1;
      lopnColor := GridColor;
    end;
    Pen := CreatePenIndirect(LogPen);
    OldPen:=SelectObject(aDDC.DC, Pen);

    // paint points
    StepX:=GridSizeX;
    StepY:=GridSizeY;
    MaxX:=AWinControl.ClientWidth;
    MaxY:=AWinControl.ClientHeight;
    x := 0;
    while x <= MaxX do begin
      y := 0;
      while y <= MaxY do begin
        MoveToEx(aDDC.DC,x,y,@OldPoint);
        LineTo(aDDC.DC,x+1,y);
        Inc(y, StepY);
      end;
      Inc(x, StepX);
    end;
    
    // restore pen
    SelectObject(aDDC.DC,OldPen);
    DeleteObject(Pen);

  finally
    RestoreDC(aDDC.DC,SavedDC);
  end;
end;

procedure TDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
Begin
  // check if contol is initialized
  if (CurName='') or (NewName='')
  or ((AComponent<>nil) and (csDestroying in AComponent.ComponentState)) then
    exit;
  // check if control is the form
  if AComponent=nil then AComponent:=FCustomForm;
  // consistency check
  if CurName<>AComponent.Name then
    writeln('WARNING: TDesigner.ValidateRename: OldComponentName="',CurName,'"');
  if Assigned(OnRenameComponent) then
    OnRenameComponent(Self,AComponent,NewName);
end;

function TDesigner.GetShiftState: TShiftState;
begin
  Result:=FShiftState;
end;

function TDesigner.CreateUniqueComponentName(const AClassName: string): string;
begin
  Result:=TheFormEditor.CreateUniqueComponentName(AClassName,Form);
end;

procedure TDesigner.OnComponentEditorVerbMenuItemClick(Sender: TObject);
var
  Verb: integer;
  VerbCaption: string;
  AMenuItem: TMenuItem;
begin
  if (PopupMenuComponentEditor=nil) or (Sender=nil) then exit;
  if not (Sender is TMenuItem) then exit;
  AMenuItem:=TMenuItem(Sender);
  Verb:=AMenuItem.MenuIndex;
  VerbCaption:=AMenuItem.Caption;
  try
    PopupMenuComponentEditor.ExecuteVerb(Verb);
  except
    on E: Exception do begin
      writeln('TDesigner.OnComponentEditorVerbMenuItemClick ERROR: ',E.Message);
      MessageDlg('Error in '+PopupMenuComponentEditor.ClassName,
        'The component editor of class "'+PopupMenuComponentEditor.ClassName+'"'#13
        +'invoked with verb #'+IntToStr(Verb)+' "'+VerbCaption+'"'#13
        +'has created the error:'#13
        +'"'+E.Message+'"',
        mtError,[mbOk],0);
    end;
  end;
end;

procedure TDesigner.OnDeleteSelectionMenuClick(Sender: TObject);
begin
  DoDeleteSelectedComponents;
end;

function TDesigner.GetGridColor: TColor;
begin
  Result:=EnvironmentOptions.GridColor;
end;

function TDesigner.GetShowGrid: boolean;
begin
  Result:=EnvironmentOptions.ShowGrid;
end;

function TDesigner.GetGridSizeX: integer;
begin
  Result:=EnvironmentOptions.GridSizeX;
  if Result<2 then Result:=2;
end;

function TDesigner.GetGridSizeY: integer;
begin
  Result:=EnvironmentOptions.GridSizeY;
  if Result<2 then Result:=2;
end;

function TDesigner.GetIsControl: Boolean;
Begin
  Result := True;
end;

function TDesigner.GetShowHints: boolean;
begin
  Result:=dfShowHints in FFlags;
end;

function TDesigner.GetSnapToGrid: boolean;
begin
  Result:=EnvironmentOptions.SnapToGrid;
end;

procedure TDesigner.SetShowGrid(const AValue: boolean);
begin
  if ShowGrid=AValue then exit;
  EnvironmentOptions.ShowGrid:=AValue;
  Form.Invalidate;
end;

procedure TDesigner.SetGridSizeX(const AValue: integer);
begin
  if GridSizeX=AValue then exit;
  EnvironmentOptions.GridSizeX:=AValue;
end;

procedure TDesigner.SetGridSizeY(const AValue: integer);
begin
  if GridSizeY=AValue then exit;
  EnvironmentOptions.GridSizeY:=AValue;
end;

procedure TDesigner.SetIsControl(Value: Boolean);
Begin

end;

procedure TDesigner.SetShowHints(const AValue: boolean);
begin
  if AValue=ShowHints then exit;
  Include(FFlags,dfShowHints);
end;

procedure TDesigner.DrawNonVisualComponents(aDDC: TDesignerDeviceContext);
var
  i, j, ItemLeft, ItemTop, ItemRight, ItemBottom,
  IconWidth, IconHeight: integer;
  Diff, ItemLeftTop: TPoint;
  IconRect: TRect;
  IconCanvas: TCanvas;
begin
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    if not (FCustomForm.Components[i] is TControl) then begin
      Diff:=aDDC.FormOrigin;
      aDDC.Save;
      // non-visual component
      ItemLeftTop:=NonVisualComponentLeftTop(FCustomForm.Components[i]);
      ItemLeft:=ItemLeftTop.X-Diff.X;
      ItemTop:=ItemLeftTop.Y-Diff.Y;
      ItemRight:=ItemLeft+NonVisualCompWidth;
      ItemBottom:=ItemTop+NonVisualCompWidth;
      with aDDC.Canvas do begin
        Brush.Color:=clWhite;
        for j:=0 to NonVisualCompBorder-1 do begin
          MoveTo(ItemLeft+j,ItemBottom-j);
          LineTo(ItemLeft+j,ItemTop+j);
          LineTo(ItemRight-j,ItemTop+j);
        end;
        Brush.Color:=clBlack;
        for j:=0 to NonVisualCompBorder-1 do begin
          MoveTo(ItemLeft+j,ItemBottom-j);
          LineTo(ItemRight-j,ItemBottom-j);
          MoveTo(ItemRight-j,ItemTop+j);
          LineTo(ItemRight-j,ItemBottom-j+1);
        end;
        IconRect:=Rect(ItemLeft+NonVisualCompBorder,ItemTop+NonVisualCompBorder,
             ItemRight-NonVisualCompBorder,ItemBottom-NonVisualCompBorder);
        Brush.Color:=clBtnFace;
        FillRect(Rect(IconRect.Left,IconRect.Top,
           IconRect.Right+1,IconRect.Bottom+1));
      end;
      if Assigned(FOnGetNonVisualCompIconCanvas) then begin
        IconCanvas:=nil;
        FOnGetNonVisualCompIconCanvas(Self,FCustomForm.Components[i]
             ,IconCanvas,IconWidth,IconHeight);
        if IconCanvas<>nil then begin
          inc(IconRect.Left,(NonVisualCompIconWidth-IconWidth) div 2);
          inc(IconRect.Top,(NonVisualCompIconWidth-IconHeight) div 2);
          IconRect.Right:=IconRect.Left+IconWidth;
          IconRect.Bottom:=IconRect.Top+IconHeight;
          aDDC.Canvas.CopyRect(IconRect, IconCanvas,
             Rect(0,0,IconWidth,IconHeight));
        end;
      end;
      if (ControlSelection.Count>1)
      and (ControlSelection.IsSelected(FCustomForm.Components[i])) then
        ControlSelection.DrawMarkerAt(aDDC.Canvas,
          ItemLeft,ItemTop,NonVisualCompWidth,NonVisualCompWidth);
    end;
  end;
end;

function TDesigner.GetDesignedComponent(AComponent: TComponent): TComponent;
begin
  Result:=AComponent;
  while (Result<>nil)
  and (Result<>Form)
  and (Result.Owner<>Form)
  and (Result is TControl) do
    Result:=TControl(Result).Parent;
end;

function TDesigner.GetComponentEditorForSelection: TBaseComponentEditor;
begin
  Result:=nil;
  if ControlSelection.Count<>1 then exit;
  Result:=TheFormEditor.GetComponentEditor(ControlSelection[0].Component);
end;

procedure TDesigner.AddComponentEditorMenuItems(
  AComponentEditor: TBaseComponentEditor; AParentMenuItem: TMenuItem);
var
  VerbCount, i: integer;
  NewMenuItem: TMenuItem;
begin
  if (AComponentEditor=nil) or (AParentMenuItem=nil) then exit;
  VerbCount:=AComponentEditor.GetVerbCount;
  for i:=0 to VerbCount-1 do begin
    NewMenuItem:=TMenuItem.Create(AParentMenuItem);
    NewMenuItem.Name:='ComponentEditorVerMenuItem'+IntToStr(i);
    NewMenuItem.Caption:=AComponentEditor.GetVerb(i);
    NewMenuItem.OnClick:=@OnComponentEditorVerbMenuItemClick;
    AParentMenuItem.Add(NewMenuItem);
    AComponentEditor.PrepareItem(i,NewMenuItem);
  end;
  if VerbCount>0 then begin
    // Add seperator
    NewMenuItem:=TMenuItem.Create(AParentMenuItem);
    NewMenuItem.Caption:='-';
    AParentMenuItem.Add(NewMenuItem);
  end;
end;

function TDesigner.NonVisualComponentAtPos(x,y: integer): TComponent;
var i: integer;
  LeftTop: TPoint;
begin
  for i:=FCustomForm.ComponentCount-1 downto 0 do begin
    Result:=FCustomForm.Components[i];
    if not (Result is TControl) then begin
      with Result do begin
        LeftTop:=NonVisualComponentLeftTop(Result);
        if (LeftTop.x<=x) and (LeftTop.y<=y)
        and (LeftTop.x+NonVisualCompWidth>x)
        and (LeftTop.y+NonVisualCompWidth>y) then
          exit;
      end;
    end;
  end;
  Result:=nil;
end;

procedure TDesigner.BuildPopupMenu;

  procedure AddSeparator;
  var
    NewMenuItem: TMenuItem;
  begin
    NewMenuItem:=TMenuItem.Create(FPopupMenu);
    with NewMenuItem do begin
      Caption:='-';
    end;
    FPopupMenu.Items.Add(NewMenuItem);
  end;

var
  ControlSelIsNotEmpty,
  FormIsSelected,
  OnlyNonVisualCompsAreSelected,
  CompsAreSelected: boolean;
begin
  if FPopupMenu<>nil then FPopupMenu.Free;

  ControlSelIsNotEmpty:=ControlSelection.Count>0;
  FormIsSelected:=ControlSelection.IsSelected(Form);
  OnlyNonVisualCompsAreSelected:=
    ControlSelection.OnlyNonVisualComponentsSelected;
  CompsAreSelected:=ControlSelIsNotEmpty and not FormIsSelected;

  FPopupMenu:=TPopupMenu.Create(nil);

  AddComponentEditorMenuItems(PopupMenuComponentEditor,FPopupMenu.Items);

  FAlignMenuItem := TMenuItem.Create(FPopupMenu);
  with FAlignMenuItem do begin
    Caption := 'Align';
    OnClick := @OnAlignPopupMenuClick;
    Enabled := CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FAlignMenuItem);

  FMirrorHorizontalMenuItem := TMenuItem.Create(FPopupMenu);
  with FMirrorHorizontalMenuItem do begin
    Caption := 'Mirror horizontal';
    OnClick := @OnMirrorHorizontalPopupMenuClick;
    Enabled := CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FMirrorHorizontalMenuItem);

  FMirrorVerticalMenuItem := TMenuItem.Create(FPopupMenu);
  with FMirrorVerticalMenuItem do begin
    Caption := 'Mirror vertical';
    OnClick := @OnMirrorVerticalPopupMenuClick;
    Enabled := CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FMirrorVerticalMenuItem);

  FScaleMenuItem := TMenuItem.Create(FPopupMenu);
  with FScaleMenuItem do begin
    Caption := 'Scale';
    OnClick := @OnScalePopupMenuClick;
    Enabled := CompsAreSelected and not OnlyNonVisualCompsAreSelected;
  end;
  FPopupMenu.Items.Add(FScaleMenuItem);

  FSizeMenuItem := TMenuItem.Create(FPopupMenu);
  with FSizeMenuItem do begin
    Caption := 'Size';
    OnClick := @OnSizePopupMenuClick;
    Enabled := CompsAreSelected and not OnlyNonVisualCompsAreSelected;
  end;
  FPopupMenu.Items.Add(FSizeMenuItem);
  
  AddSeparator;
  
  FBringToFrontMenuItem := TMenuItem.Create(FPopupMenu);
  with FBringToFrontMenuItem do begin
    Caption:= 'Bring to front';
    OnClick:= @OnBringToFrontMenuClick;
    Enabled:= CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FBringToFrontMenuItem);
  
  FSendToBackMenuItem:= TMenuItem.Create(FPopupMenu);
  with FSendToBackMenuItem do begin
    Caption:= 'Send to back';
    OnClick:= @OnSendToBackMenuClick;
    Enabled:= CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FSendToBackMenuItem);
  
  AddSeparator;
  
  FDeleteSelectionMenuItem:=TMenuItem.Create(FPopupMenu);
  with FDeleteSelectionMenuItem do begin
    Caption:= 'Delete selection';
    OnClick:=@OnDeleteSelectionMenuClick;
    Enabled:= ControlSelIsNotEmpty;
  end;
  FPopupMenu.Items.Add(FDeleteSelectionMenuItem);
end;

procedure TDesigner.OnAlignPopupMenuClick(Sender: TObject);
var HorizAlignment, VertAlignment: TComponentAlignment;
begin
  if ShowAlignComponentsDialog=mrOk then begin
    case AlignComponentsDialog.HorizontalRadioGroup.ItemIndex of
     0: HorizAlignment:=csaNone;
     1: HorizAlignment:=csaSides1;
     2: HorizAlignment:=csaCenters;
     3: HorizAlignment:=csaSides2;
     4: HorizAlignment:=csaCenterInWindow;
     5: HorizAlignment:=csaSpaceEqually;
     6: HorizAlignment:=csaSide1SpaceEqually;
     7: HorizAlignment:=csaSide2SpaceEqually;
    end;
    case AlignComponentsDialog.VerticalRadioGroup.ItemIndex of
     0: VertAlignment:=csaNone;
     1: VertAlignment:=csaSides1;
     2: VertAlignment:=csaCenters;
     3: VertAlignment:=csaSides2;
     4: VertAlignment:=csaCenterInWindow;
     5: VertAlignment:=csaSpaceEqually;
     6: VertAlignment:=csaSide1SpaceEqually;
     7: VertAlignment:=csaSide2SpaceEqually;
    end;
    ControlSelection.AlignComponents(HorizAlignment,VertAlignment);
  end;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnMirrorHorizontalPopupMenuClick(Sender: TObject);
begin
  ControlSelection.MirrorHorizontal;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnMirrorVerticalPopupMenuClick(Sender: TObject);
begin
  ControlSelection.MirrorVertical;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnScalePopupMenuClick(Sender: TObject);
begin
  if ShowScaleComponentsDialog=mrOk then begin
    ControlSelection.ScaleComponents(
      StrToIntDef(ScaleComponentsDialog.PercentEdit.Text,100));
  end;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnSizePopupMenuClick(Sender: TObject);
var HorizSizing, VertSizing: TComponentSizing;
  AWidth, AHeight: integer;
begin
  if ShowSizeComponentsDialog=mrOk then begin
    case SizeComponentsDialog.WidthRadioGroup.ItemIndex of
     0: HorizSizing:=cssNone;
     1: HorizSizing:=cssShrinkToSmallest;
     2: HorizSizing:=cssGrowToLargest;
     3: HorizSizing:=cssFixed;
    end;
    case SizeComponentsDialog.HeightRadioGroup.ItemIndex of
     0: VertSizing:=cssNone;
     1: VertSizing:=cssShrinkToSmallest;
     2: VertSizing:=cssGrowToLargest;
     3: VertSizing:=cssFixed;
    end;
    if HorizSizing=cssFixed then
      AWidth:=StrToIntDef(SizeComponentsDialog.WidthEdit.Text,0)
    else
      AWidth:=0;
    if VertSizing=cssFixed then
      AHeight:=StrToIntDef(SizeComponentsDialog.HeightEdit.Text,0)
    else
      AHeight:=0;
    ControlSelection.SizeComponents(HorizSizing,AWidth,VertSizing,AHeight);
  end;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnBringToFrontMenuClick(Sender: TObject);
var AComponent : TComponent;
begin
  if ControlSelection.Count = 1 then begin
    AComponent:= ControlSelection.Items[0].Component;
    if AComponent is TControl then
      TControl(AComponent).BringToFront;
  end;    
end;

procedure TDesigner.OnSendToBackMenuClick(Sender: TObject);
var AComponent : TComponent;
begin
  if ControlSelection.Count = 1 then begin
    AComponent:= ControlSelection.Items[0].Component;
    if AComponent is TControl then
      TControl(AComponent).SendToBack;
  end;
end;

Procedure TDesigner.HintTimer(sender : TObject);
var
  Rect : TRect;
  AHint : String;
  Control : TControl;
  Position : TPoint;
  BW       : Integer;
  Window : TWInControl;
begin
  FHintTimer.Enabled := False;
  if not ShowHints then exit;

  Position := Mouse.CursorPos;
  Window := FindLCLWindow(Position);
  if not(Assigned(window)) then Exit;

  //get the parent until parent is nil
  While Window.Parent <> nil do
  Window := Window.Parent;

  if (window <> FCustomForm) then Exit;

  BW := 0;
//  if (FCustomForm is TForm) then
//     BW := TForm(FCustomForm).BorderWidth;

  if ((Position.X < (FCustomForm.Left + BW)) or (Position.X > (FCustomForm.Left+FCustomForm.Width - BW)) or (Position.Y < FCustomForm.Top+22) or (Position.Y > (FCustomForm.Top+FCustomForm.Height - BW))) then Exit;

  Position := FCustomForm.ScreenToClient(Position);

  Control := FCustomForm.ControlAtPos(Position,True);
  if not Assigned(Control) then
     Control := FCustomForm;
  AHint := Control.Name + ' : '+Control.ClassName;
  AHint := AHint + #10+'Left : '+Inttostr(Control.Left)+ '  Top : '+Inttostr(Control.Top)+
                   #10+'Width : '+Inttostr(Control.Width)+ '  Height : '+Inttostr(Control.Height);

  Rect := FHintWindow.CalcHintRect(0,AHint,nil);  //no maxwidth
  Rect.Left := Mouse.CursorPos.X+10;
  Rect.Top := Mouse.CursorPos.Y+5;
  Rect.Right := Rect.Left + Rect.Right;
  Rect.Bottom := Rect.Top + Rect.Bottom;

  FHintWindow.ActivateHint(Rect,AHint);
end;

procedure TDesigner.SetSnapToGrid(const AValue: boolean);
begin
  if SnapToGrid=AValue then exit;
  EnvironmentOptions.SnapToGrid:=AValue;
end;

Procedure TDesigner.OnFormActivated;
begin
  //the form was activated.
  if Assigned(FOnActivated) then
    FOnActivated(Self);
end;

function TDesigner.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result:=TheFormEditor.PropertyEditorHook;
end;


end.

