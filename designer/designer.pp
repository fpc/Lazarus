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
{$DEFINE NewMousePos}

uses
  Classes, LCLType, LCLLinux, Forms, Controls, LMessages, GraphType, Graphics,
  ControlSelection, CustomFormEditor, FormEditor, UnitEditor, CompReg, Menus,
  AlignCompsDlg, SizeCompsDlg, ScaleCompsDlg, ExtCtrls, EnvironmentOpts;

type
  TDesigner = class;

  TOnGetSelectedComponentClass = procedure(Sender: TObject; 
    var RegisteredComponent: TRegisteredComponent) of object;
  TOnSetDesigning = procedure(Sender: TObject; Component: TComponent;
    Value: boolean) of object;
  TOnAddComponent = procedure(Sender: TObject; Component: TComponent;
    ComponentClass: TRegisteredComponent) of object;
  TOnRemoveComponent = procedure(Sender: TObject; Component: TComponent)
    of object;
  TOnGetNonVisualCompIconCanvas = procedure(Sender: TObject;
    AComponent: TComponent; var IconCanvas: TCanvas;
    var IconWidth, IconHeight: integer) of object;
  TOnRenameComponent = procedure(Designer: TDesigner; AComponent: TComponent;
    const NewName: string) of object;

  TDesigner = class(TIDesigner)
  private
    FCustomForm: TCustomForm;
    FFormEditor : TFormEditor;
    FSourceEditor : TSourceEditor;
    FHasSized: boolean;
    FGridColor: TColor;
    FDuringPaintControl: boolean;
    FOnAddComponent: TOnAddComponent;
    FOnComponentListChanged: TNotifyEvent;
    FOnGetSelectedComponentClass: TOnGetSelectedComponentClass;
    FOnGetNonVisualCompIconCanvas: TOnGetNonVisualCompIconCanvas;
    FOnModified: TNotifyEvent;
    FOnPropertiesChanged: TNotifyEvent;
    FOnRemoveComponent: TOnRemoveComponent;
    FOnSetDesigning: TOnSetDesigning;
    FOnUnselectComponentClass: TNotifyEvent;
    FOnActivated: TNotifyEvent;
    FOnRenameComponent: TOnRenameComponent;
    FPopupMenu: TPopupMenu;
    FAlignMenuItem: TMenuItem;
    FMirrorHorizontalMenuItem: TMenuItem;
    FMirrorVerticalMenuItem: TMenuItem;
    FScaleMenuItem: TMenuItem;
    FSizeMenuItem: TMenuItem;
    FBringToFrontMenuItem: TMenuItem;
    FSendToBackMenuItem: TMenuItem;
    FShowHints: boolean;

    //hint stuff
    FHintTimer : TTimer;
    FHintWIndow : THintWindow;
    
    function GetShowGrid: boolean;
    function GetGridSizeX: integer;
    function GetGridSizeY: integer;
    function GetIsControl: Boolean;
    function GetSnapToGrid: boolean;
    Procedure HintTimer(sender : TObject);
    procedure InvalidateWithParent(AComponent: TComponent);
    procedure SetShowGrid(const AValue: boolean);
    procedure SetGridSizeX(const AValue: integer);
    procedure SetGridSizeY(const AValue: integer);
    procedure SetIsControl(Value: Boolean);
    procedure SetSnapToGrid(const AValue: boolean);
  protected
    MouseDownComponent: TComponent;
    MouseDownSender: TComponent;
    MouseDownPos: TPoint;
    MouseUpPos: TPoint;
    LastMouseMovePos: TPoint;

    function PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
    function SizeControl(Sender: TControl; TheMessage: TLMSize):boolean;
    function MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
    Procedure MouseDownOnControl(Sender: TControl; TheMessage : TLMMouse);
    Procedure MouseMoveOnControl(Sender: TControl; var TheMessage: TLMMouse);
    Procedure MouseLeftUpOnControl(Sender: TControl; TheMessage:TLMMouse);
    Procedure MouseRightUpOnControl(Sender: TControl; TheMessage:TLMMouse);
    Procedure KeyDown(Sender: TControl; TheMessage:TLMKEY);
    Procedure KeyUp(Sender: TControl; TheMessage:TLMKEY);

    Procedure RemoveControl(Control: TComponent);
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
    Procedure OnFormActivated;
  public
    ControlSelection : TControlSelection;
    constructor Create(Customform : TCustomform; AControlSelection: TControlSelection);
    destructor Destroy; override;

    function IsDesignMsg(Sender: TControl; var TheMessage: TLMessage): Boolean; override;
    procedure Modified; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintGrid; override;
    procedure ValidateRename(AComponent: TComponent;
       const CurName, NewName: string); override;
    Procedure SelectOnlyThisComponent(AComponent:TComponent);
    function NonVisualComponentLeftTop(AComponent: TComponent): TPoint;
    function NonVisualComponentAtPos(x,y: integer): TComponent;
    procedure DrawNonVisualComponents(DC: HDC);

    property ShowGrid: boolean read GetShowGrid write SetShowGrid;
    property Form: TCustomForm read FCustomForm write FCustomForm;
    property FormEditor: TFormEditor read FFormEditor write FFormEditor;
    property GridSizeX: integer read GetGridSizeX write SetGridSizeX;
    property GridSizeY: integer read GetGridSizeY write SetGridSizeY;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property OnActivated: TNotifyEvent
       read FOnActivated write FOnActivated;
    property OnAddComponent: TOnAddComponent read FOnAddComponent write FOnAddComponent;
    property OnComponentListChanged: TNotifyEvent
       read FOnComponentListChanged write FOnComponentListChanged;
    property OnGetSelectedComponentClass: TOnGetSelectedComponentClass
       read FOnGetSelectedComponentClass write FOnGetSelectedComponentClass;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnPropertiesChanged: TNotifyEvent
       read FOnPropertiesChanged write FOnPropertiesChanged;
    property OnRemoveComponent: TOnRemoveComponent
       read FOnRemoveComponent write FOnRemoveComponent;
    property OnRenameComponent: TOnRenameComponent
       read FOnRenameComponent write FOnRenameComponent;
    property OnSetDesigning: TOnSetDesigning read FOnSetDesigning write FOnSetDesigning;
    property OnUnselectComponentClass: TNotifyEvent
       read FOnUnselectComponentClass write FOnUnselectComponentClass;
    property OnGetNonVisualCompIconCanvas: TOnGetNonVisualCompIconCanvas
       read FOnGetNonVisualCompIconCanvas write FOnGetNonVisualCompIconCanvas;
    property ShowHints: boolean read FShowHints write FShowHints;
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
  FHasSized:=false;
  FGridColor:=clGray;
  FDuringPaintControl:=false;

  FHintTimer := TTimer.Create(nil);
  FHintTimer.Interval := 500;
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := @HintTimer;
  
  FHintWindow := THintWindow.Create(nil);

  FHIntWindow.Visible := False;
  FHintWindow.Caption := 'This is a hint window'#13#10'Neat huh?';
  FHintWindow.HideInterval := 4000;
  FHintWindow.AutoHide := True;
end;

destructor TDesigner.Destroy;
Begin
  if FPopupMenu<>nil then
    FPopupMenu.Free;
    
  FHintWIndow.Free;
  FHintTimer.Free;
  Inherited Destroy;
end;

Procedure TDesigner.RemoveControl(Control : TComponent);
Begin
  Writeln('[TDesigner.RemoveControl] ',Control.Name,':',Control.ClassName);
  if Assigned(FOnRemoveComponent) then
    FOnRemoveComponent(Self,Control);
  FCustomForm.RemoveControl(TCOntrol(Control));
  //this send a message to notification and removes it from the controlselection
  FFormEditor.DeleteControl(Control);
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
  ControlSelection.BeginUpdate;
  ControlSelection.Clear;
  ControlSelection.Add(TControl(AComponent));
  ControlSelection.EndUpdate;
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

function TDesigner.PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
var OldDuringPaintControl: boolean;
begin
  Result:=true;

  //writeln('***  LM_PAINT A ',Sender.Name,':',Sender.ClassName,' DC=',HexStr(Message.DC,8));
  OldDuringPaintControl:=FDuringPaintControl;
  FDuringPaintControl:=true;
  Sender.Dispatch(TheMessage);
  

  //writeln('***  LM_PAINT B ',Sender.Name,':',Sender.ClassName,' DC=',HexStr(Message.DC,8));
  if (ControlSelection.IsSelected(Sender)) then begin
    // writeln('***  LM_PAINT ',Sender.Name,':',Sender.ClassName,' DC=',HexStr(Message.DC,8));
    ControlSelection.DrawMarker(Sender,TheMessage.DC);
  end;
  //if OldDuringPaintControl=false then begin
    DrawNonVisualComponents(TheMessage.DC);
    ControlSelection.DrawGrabbers(TheMessage.DC);
    ControlSelection.DrawGuideLines(TheMessage.DC);
    if ControlSelection.RubberBandActive then
      ControlSelection.DrawRubberBand(TheMessage.DC);
  //  end;
  FDuringPaintControl:=OldDuringPaintControl;
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
  {$IfNDef NewMousePos}
  SenderClientOrigin:TPoint;
  {$EndIf}
  SelectedCompClass: TRegisteredComponent;
  NonVisualComp: TComponent;
Begin
  FHintTimer.Enabled := False;
  FHasSized:=false;
  if (getParentForm(Sender)=nil) then exit;

  if MouseDownComponent=nil then begin
    MouseDownComponent:=Sender;
    MouseDownSender:=Sender;
  end;

  {$IfDef NewMousePos}
  MouseDownPos:=GetFormRelativeMousePosition(Form);
  {$Else}
  SenderClientOrigin:=GetParentFormRelativeClientOrigin(Sender);
  MouseDownPos := Point(TheMessage.Pos.X+SenderClientOrigin.X,
                        TheMessage.Pos.Y+SenderClientOrigin.Y);
  {$EndIf}
  LastMouseMovePos:=MouseDownPos;

  {$IFDEF VerboseDesigner}
  writeln('************************************************************');
  write('MouseDownOnControl');
  write(' ',Sender.Name,':',Sender.ClassName);
  {$IfNDef NewMousePos}
  write(' ClientOrg=',SenderClientOrigin.X,',',SenderClientOrigin.Y);
  {$EndIf}
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

procedure TDesigner.MouseLeftUpOnControl(Sender : TControl;
  TheMessage:TLMMouse);
var
  ParentCI, NewCI: TComponentInterface;
  NewLeft, NewTop, NewWidth, NewHeight, MoveX, MoveY: Integer;
  Shift: TShiftState;
  SenderParentForm: TCustomForm;
  RubberBandWasActive: boolean;
  {$IfNDef NewMousePos}
  SenderClientOrigin: TPoint;
  {$EndIf}
  ParentClientOrigin: TPoint;
  SelectedCompClass: TRegisteredComponent;
  NewParent: TWinControl;
Begin
  FHintTimer.Enabled := False;

  SenderParentForm:=GetParentForm(Sender);
  if (MouseDownComponent=nil) or (SenderParentForm=nil) then exit;

  ControlSelection.ActiveGrabber:=nil;
  RubberBandWasActive:=ControlSelection.RubberBandActive;

  Shift := [];
  if (TheMessage.keys and MK_Shift) = MK_Shift then
    Shift := [ssShift];
  if (TheMessage.keys and MK_Control) = MK_Control then
    Shift := Shift +[ssCTRL];

  {$IfDef NewMousePos}
  MouseUpPos:=GetFormRelativeMousePosition(Form);
  {$Else}
  SenderClientOrigin:=GetParentFormRelativeClientOrigin(Sender);
  MouseUpPos := Point(TheMessage.Pos.X+SenderClientOrigin.X,
                      TheMessage.Pos.Y+SenderClientOrigin.Y);
  {$EndIf}
  MoveX:=MouseUpPos.X-MouseDownPos.X;
  MoveY:=MouseUpPos.Y-MouseDownPos.Y;

  {$IFDEF VerboseDesigner}
  writeln('************************************************************');
  write('MouseLeftUpOnControl');
  write(' ',Sender.Name,':',Sender.ClassName);
  {$IfNDef NewMousePos}
  write(' ClientOrigin=',SenderClientOrigin.X,',',SenderClientOrigin.Y);
  {$EndIf}
  write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
  write(' Move=',MoveX,',',MoveY);
  writeln('');
  {$ENDIF}

  SelectedCompClass:=nil;
  if Assigned(FOnGetSelectedComponentClass) then
    FOnGetSelectedComponentClass(Self,SelectedCompClass);

  if (TheMessage.Keys and MK_LButton) > 0 then begin
    // left mouse button up
    if SelectedCompClass = nil then begin
      // selection mode (+ moving and resizing)
      ControlSelection.BeginUpdate;
      if not FHasSized then begin
        if RubberBandWasActive then begin
          if (not (ssShift in Shift)) 
          or ((ControlSelection.Count=1) 
           and (ControlSelection[0].Component is TCustomForm)) then
            ControlSelection.Clear;
          ControlSelection.SelectWithRubberBand(
            SenderParentForm,ssShift in Shift);
          if ControlSelection.Count=0 then
            ControlSelection.Add(SenderParentForm);
          ControlSelection.RubberbandActive:=false;
        end else begin
          if (not (ssShift in Shift)) then begin
            ControlSelection.Clear;
            ControlSelection.Add(Sender);
          end;
        end;
        SenderParentForm.Invalidate;
      end;
      ControlSelection.EndUpdate;
    end else begin
      // add a new component
      ControlSelection.RubberbandActive:=false;
      ControlSelection.BeginUpdate;

      // find a parent for the new component
      NewParent:=TWinControl(Sender);
      while (NewParent<>nil)
      and ((not (csAcceptsControls in NewParent.ControlStyle))
        or ((NewParent.Owner<>Form) and (NewParent<>Form)))
      do begin
        NewParent:=NewParent.Parent;
      end;
      ParentCI:=TComponentInterface(FFormEditor.FindComponent(NewParent));
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
        
        NewCI := TComponentInterface(FFormEditor.CreateComponent(
           ParentCI,SelectedCompClass.ComponentClass
          ,NewLeft,NewTop,NewWidth,NewHeight));
        if NewCI.Control is TControl then
          TControl(NewCI.Control).Visible:=true;
        if Assigned(FOnSetDesigning) then
          FOnSetDesigning(Self,NewCI.Control,True);
        if Assigned(FOnAddComponent) then
          FOnAddComponent(Self,NewCI.Control,SelectedCompClass);

        SelectOnlyThisComponent(TComponent(NewCI.Control));
        if not (ssShift in Shift) then
          if Assigned(FOnUnselectComponentClass) then
            // this resets the component toolbar to the mouse. (= selection tool)
            FOnUnselectComponentClass(Self);
        Form.Invalidate;
        {$IFDEF VerboseDesigner}
        writeln('NEW COMPONENT ADDED: Form.ComponentCount=',Form.ComponentCount,
           '  NewCI.Control.Owner.Name=',NewCI.Control.Owner.Name);
        {$ENDIF}
      end;
      ControlSelection.EndUpdate;
    end;
  end;
  LastMouseMovePos.X:=-1;
  FHasSized:=false;

  MouseDownComponent:=nil;
  MouseDownSender:=nil;
  {$IFDEF VerboseDesigner}
  writeln('[TDesigner.MouseLeftUpOnControl] END');
  {$ENDIF}
end;

Procedure TDesigner.MouseMoveOnControl(Sender: TControl;
  var TheMessage: TLMMouse);
var
  Shift : TShiftState;
  {$IfNDef NewMousePos}
  SenderClientOrigin:TPoint;
  {$EndIf}
  SenderParentForm:TCustomForm;
  OldMouseMovePos: TPoint;
begin
  if FShowHints then begin
    FHintTimer.Enabled := False;

    { don't want it enabled when a mouse button is pressed. }
    FHintTimer.Enabled :=
          (TheMessage.keys or (MK_LButton and MK_RButton and MK_MButton) = 0);
    if FHintWindow.Visible then
      FHintWindow.Visible := False;
  end;

  if MouseDownComponent=nil then exit;
  SenderParentForm:=GetParentForm(Sender);
  if SenderParentForm=nil then exit;
  
  OldMouseMovePos:=LastMouseMovePos;
  {$IfDef NewMousePos}
  LastMouseMovePos:=GetFormRelativeMousePosition(Form);
  {$Else}
  SenderClientOrigin:=GetParentFormRelativeClientOrigin(Sender);
  LastMouseMovePos:=Point(TheMessage.Pos.X+SenderClientOrigin.X,
                          TheMessage.Pos.Y+SenderClientOrigin.Y);
  {$EndIf}


  //debugging commented out
{  if (Message.keys and MK_LButton) = MK_LButton then begin
    Write('MouseMoveOnControl'
          ,' ',Sender.ClassName
          ,' ',GetCaptureControl<>nil
          ,' ',Sender.Left,',',Sender.Top
          ,' Origin=',SenderOrigin.X,',',SenderOrigin.Y
          ,' Msg=',Message.Pos.x,',',Message.Pos.Y
          ,' Mouse=',MouseX,',',MouseY
    );
    write(' ',MouseDownComponent is TWinControl);
    if (MouseDownComponent is TControl) then begin
      write(' ',csCaptureMouse in TWinControl(MouseDownComponent).ControlStyle);
    end;
    writeln();
  end;
}
  Shift := [];
  if (TheMessage.keys and MK_Shift) = MK_Shift then
    Shift := [ssShift];
  if (TheMessage.keys and MK_Control) = MK_Control then
    Shift := Shift + [ssCTRL];

  if (TheMessage.keys and MK_LButton) = MK_LButton then begin
    // left button pressed
    if ControlSelection.ActiveGrabber<>nil then begin
      // grabber moving -> size selection
      if not FHasSized then begin
        ControlSelection.SaveBounds;
        FHasSized:=true;
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
        if not FHasSized then begin
          ControlSelection.SaveBounds;
          FHasSized:=true;
        end;
        ControlSelection.MoveSelectionWithSnapping(
          LastMouseMovePos.X-MouseDownPos.X,LastMouseMovePos.Y-MouseDownPos.Y);
        if Assigned(OnModified) then OnModified(Self);
        FCustomForm.Invalidate;
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

procedure TDesigner.MouseRightUpOnControl(Sender : TControl; TheMessage:TLMMouse);
{$IfNDef NewMousePos}
var
  SenderOrigin: TPoint;
{$EndIf}
begin
  FHintTimer.Enabled := False;

  {$IfDef NewMousePos}
  MouseUpPos:=GetFormRelativeMousePosition(Form);
  {$Else}
  SenderOrigin:=GetParentFormRelativeTopLeft(Sender);
  MouseUpPos.X:=TheMessage.Pos.X+SenderOrigin.X;
  MouseUpPos.Y:=TheMessage.Pos.Y+SenderOrigin.Y;
  {$EndIf}
  BuildPopupMenu;
  FPopupMenu.Popup(MouseUpPos.X,MouseUpPos.Y);
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
  I : Integer;
  Shift : TShiftState;
Begin
  {$IFDEF VerboseDesigner}
  Writeln('TDesigner.KEYDOWN');
  with TheMessage do
  Begin
    Writeln('CHARCODE = '+inttostr(charcode));
    Writeln('KEYDATA = '+inttostr(KeyData));
  end;
  {$ENDIF}

  Shift := KeyDataToShiftState(TheMessage.KeyData);

  if (TheMessage.CharCode = 46) then //DEL KEY
  begin
    if (ControlSelection.Count = 1)
    and (ControlSelection.Items[0].Component = FCustomForm) then
       Exit;
    ControlSelection.BeginUpdate;
    for  I := ControlSelection.Count-1 downto 0 do Begin
      Writeln('I = '+inttostr(i));
      RemoveControl(ControlSelection.Items[I].Component);
    End;
    SelectOnlythisComponent(FCustomForm);
    ControlSelection.EndUpdate;
  end
  else
  if TheMessage.CharCode = 38 then //UP ARROW
  Begin
    if (ssCtrl in Shift) then
      NudgeControl(0,-1)
    else if (ssShift in Shift) then
      NudgeSize(0,-1);
    end
  else if TheMessage.CharCode = 40 then //DOWN ARROW
  Begin
    if (ssCtrl in Shift) then
      NudgeControl(0,1)
    else if (ssShift in Shift) then
      NudgeSize(0,1);
    end
  else
  if TheMessage.CharCode = 39 then //RIGHT ARROW
  Begin
    if (ssCtrl in Shift) then
      NudgeControl(1,0)
    else if (ssShift in Shift) then
      NudgeSize(1,0);
    end
  else
  if TheMessage.CharCode = 37 then //LEFT ARROW
  Begin
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
  Writeln('KEYUp');
  with TheMessage do
  Begin
    Writeln('CHARCODE = '+inttostr(charcode));
    Writeln('KEYDATA = '+inttostr(KeyData));
  end;
  {$ENDIF}
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
      LM_RBUTTONDOWN: MouseDownOnControl(Sender,TLMMouse(TheMessage));
      LM_LBUTTONUP:   MouseLeftUpOnControl(Sender,TLMMouse(TheMessage));
      LM_RBUTTONUP:   MouseRightUpOnControl(sender,TLMMouse(TheMessage));
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
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TDesigner.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  if Operation = opInsert then
    begin
      Writeln('opInsert');
    end
  else
  if Operation = opRemove then
    begin
      writeln('[TDesigner.Notification] opRemove '+
        ''''+AComponent.ClassName+'.'+AComponent.Name+'''');
      if (AComponent is TControl) then
        if ControlSelection.IsSelected(AComponent) then
          ControlSelection.Remove(AComponent);
    end;
end;

procedure TDesigner.PaintGrid;
var
  x,y, StepX, StepY : integer;
begin
  if not ShowGrid then exit;
  StepX:=GridSizeX;
  StepY:=GridSizeY;
  with FCustomForm.Canvas do begin
    Pen.Color := FGridColor;
    x := StepX-1;
    while x <= FCustomForm.Width do begin
      y := StepY-1;
      while y <= FCustomForm.Height do begin
         MoveTo(x,y);
         LineTo(x+1,y);
//         Pixels[X,Y]:=FGridColor;
         Inc(y, StepY);
      end;
      Inc(x, StepX);
    end;
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

procedure TDesigner.DrawNonVisualComponents(DC: HDC);
var
  i, j, ItemLeft, ItemTop, ItemRight, ItemBottom,
  IconWidth, IconHeight: integer;
  FormOrigin, DCOrigin, Diff, ItemLeftTop: TPoint;
  SaveIndex: HDC;
  IconRect: TRect;
  IconCanvas: TCanvas;
begin
  GetWindowOrgEx(DC, DCOrigin);
  FormOrigin:=FCustomForm.ClientOrigin;
  Diff.X:=FormOrigin.X-DCOrigin.X;
  Diff.Y:=FormOrigin.Y-DCOrigin.Y;
  SaveIndex:=SaveDC(DC);
  FCustomForm.Canvas.Handle:=DC;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    if not (FCustomForm.Components[i] is TControl) then begin
      // non-visual component
      ItemLeftTop:=NonVisualComponentLeftTop(FCustomForm.Components[i]);
      ItemLeft:=ItemLeftTop.X+Diff.X;
      ItemTop:=ItemLeftTop.Y+Diff.Y;
      ItemRight:=ItemLeft+NonVisualCompWidth;
      ItemBottom:=ItemTop+NonVisualCompWidth;
      with FCustomForm.Canvas do begin
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
          inc(IconRect.Left,((IconRect.Right-IconRect.Left)-IconWidth) div 2);
          inc(IconRect.Top,((IconRect.Bottom-IconRect.Top)-IconHeight) div 2);
          FCustomForm.Canvas.CopyRect(IconRect, IconCanvas,
             Rect(0,0,IconWidth,IconHeight));
        end;
      end;
      if (ControlSelection.Count>1)
      and (ControlSelection.IsSelected(FCustomForm.Components[i])) then
        ControlSelection.DrawMarkerAt(FCustomForm.Canvas,
          ItemLeft,ItemTop,NonVisualCompWidth,NonVisualCompWidth);
    end;
  end;
  FCustomForm.Canvas.Handle:=0;
  RestoreDC(DC,SaveIndex);
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
var
  ControlSelIsNotEmpty, FormIsSelected, OnlyNonVisualCompsAreSelected,
  CompsAreSelected: boolean;
begin
  if FPopupMenu<>nil then FPopupMenu.Free;

  ControlSelIsNotEmpty:=ControlSelection.Count>0;
  FormIsSelected:=ControlSelIsNotEmpty 
    and (ControlSelection[0].Component is TCustomForm);
  OnlyNonVisualCompsAreSelected:=
    ControlSelection.OnlyNonVisualComponentsSelected;
  CompsAreSelected:=ControlSelIsNotEmpty and not FormIsSelected;

  FPopupMenu:=TPopupMenu.Create(nil);

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
    Enabled := CompsAreSelected and OnlyNonVisualCompsAreSelected;
  end;
  FPopupMenu.Items.Add(FScaleMenuItem);

  FSizeMenuItem := TMenuItem.Create(FPopupMenu);
  with FSizeMenuItem do begin
    Caption := 'Size';
    OnClick := @OnSizePopupMenuClick;
    Enabled := CompsAreSelected and OnlyNonVisualCompsAreSelected;
  end;
  FPopupMenu.Items.Add(FSizeMenuItem);
  
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
  if not FShowHints then exit;

  Position := Mouse.CursorPos;
  Window := FindLCLWindow(Position);
  if not(Assigned(window)) then Exit;

  //get the parent until parent is nil
  While Window.Parent <> nil do
  Window := Window.Parent;

  if (window <> FCustomForm) then Exit;

  BW := 0;
  if (FCustomForm is TForm) then
     BW := TForm(FCustomForm).BorderWidth;

  if ((Position.X < (FCustomForm.LEft +BW)) or (Position.X > (FCustomForm.Left+FCustomForm.Width - BW)) or (Position.Y < FCustomForm.Top+22) or (Position.Y > (FCustomForm.Top+FCustomForm.Height - BW))) then Exit;

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
     FOnActivated(Form);
end;


end.

