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
  Dialogs, ExtCtrls, Menus, ClipBrd, IDEProcs,
  LazarusIDEStrConsts, EnvironmentOpts, KeyMapping, ComponentReg,
  NonControlForms, AlignCompsDlg, SizeCompsDlg, ScaleCompsDlg,
  DesignerProcs, PropEdits, ComponentEditors, CustomFormEditor,
  ControlSelection;

type
  TDesigner = class;

  TOnGetSelectedComponentClass = procedure(Sender: TObject; 
    var RegisteredComponent: TRegisteredComponent) of object;
  TOnSetDesigning = procedure(Sender: TObject; Component: TComponent;
    Value: boolean) of object;
  TOnComponentAdded = procedure(Sender: TObject; Component: TComponent;
    ComponentClass: TRegisteredComponent) of object;
  TOnPasteComponent = procedure(Sender: TObject; LookupRoot: TComponent;
    TxtCompStream: TStream; ParentControl: TWinControl;
    var NewComponent: TComponent) of object;
  TOnRemoveComponent = procedure(Sender: TObject; Component: TComponent)
    of object;
  TOnComponentDeleted = procedure(Sender: TObject; Component: TComponent)
    of object;
  TOnGetNonVisualCompIconCanvas = procedure(Sender: TObject;
    AComponent: TComponent; var IconCanvas: TCanvas;
    var IconWidth, IconHeight: integer) of object;
  TOnRenameComponent = procedure(Designer: TDesigner; AComponent: TComponent;
    const NewName: string) of object;
  TOnProcessCommand = procedure(Sender: TObject; Command: word;
    var Handled: boolean) of object;
    
  TDesignerFlag = (
    dfHasSized,
    dfDuringPaintControl,
    dfShowEditorHints,
    dfShowComponentCaptionHints,
    dfDestroyingForm
    );
  TDesignerFlags = set of TDesignerFlag;

  TDesigner = class(TComponentEditorDesigner)
  private
    FAlignMenuItem: TMenuItem;
    FBringToFrontMenuItem: TMenuItem;
    FForm: TCustomForm;
    FDeleteSelectionMenuItem: TMenuItem;
    FFlags: TDesignerFlags;
    FGridColor: TColor;
    FLookupRoot: TComponent;
    FMirrorHorizontalMenuItem: TMenuItem;
    FMirrorVerticalMenuItem: TMenuItem;
    FOnActivated: TNotifyEvent;
    FOnCloseQuery: TNotifyEvent;
    FOnComponentAdded: TOnComponentAdded;
    FOnComponentDeleted: TOnComponentDeleted;
    FOnGetNonVisualCompIconCanvas: TOnGetNonVisualCompIconCanvas;
    FOnGetSelectedComponentClass: TOnGetSelectedComponentClass;
    FOnModified: TNotifyEvent;
    FOnPasteComponent: TOnPasteComponent;
    FOnProcessCommand: TOnProcessCommand;
    FOnPropertiesChanged: TNotifyEvent;
    FOnRemoveComponent: TOnRemoveComponent;
    FOnRenameComponent: TOnRenameComponent;
    FOnSetDesigning: TOnSetDesigning;
    FOnShowOptions: TNotifyEvent;
    FOnUnselectComponentClass: TNotifyEvent;
    FPopupMenu: TPopupMenu;
    FScaleMenuItem: TMenuItem;
    FSendToBackMenuItem: TMenuItem;
    FShiftState: TShiftState;
    FShowOptionsMenuItem: TMenuItem;
    FSizeMenuItem: TMenuItem;
    FSnapToGridOptionMenuItem: TMenuItem;
    FSnapToGuideLinesOptionMenuItem: TMenuItem;
    FTheFormEditor: TCustomFormEditor;

    //hint stuff
    FHintTimer : TTimer;
    FHintWIndow : THintWindow;
    
    function GetGridColor: TColor;
    function GetShowComponentCaptionHints: boolean;
    function GetShowGrid: boolean;
    function GetGridSizeX: integer;
    function GetGridSizeY: integer;
    function GetIsControl: Boolean;
    function GetShowEditorHints: boolean;
    function GetSnapToGrid: boolean;
    Procedure HintTimer(sender : TObject);
    procedure InvalidateWithParent(AComponent: TComponent);
    procedure SetGridColor(const AValue: TColor);
    procedure SetShowComponentCaptionHints(const AValue: boolean);
    procedure SetShowGrid(const AValue: boolean);
    procedure SetGridSizeX(const AValue: integer);
    procedure SetGridSizeY(const AValue: integer);
    procedure SetIsControl(Value: Boolean);
    procedure SetShowEditorHints(const AValue: boolean);
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
    DeletingComponents: TList;
    
    LastPaintSender: TControl;

    // event handlers for designed components
    function PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
    function SizeControl(Sender: TControl; TheMessage: TLMSize):boolean;
    function MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
    Procedure MouseDownOnControl(Sender: TControl; var TheMessage : TLMMouse);
    Procedure MouseMoveOnControl(Sender: TControl; var TheMessage: TLMMouse);
    Procedure MouseUpOnControl(Sender: TControl; var TheMessage:TLMMouse);
    Procedure KeyDown(Sender: TControl; var TheMessage:TLMKEY);
    Procedure KeyUp(Sender: TControl; var TheMessage:TLMKEY);

    // procedures for working with components
    procedure DoDeleteSelectedComponents;
    procedure DoDeleteComponent(AComponent: TComponent; FreeComponent: boolean);
    procedure MarkComponentForDeletion(AComponent: TComponent);
    function ComponentIsMarkedForDeletion(AComponent: TComponent): boolean;
    function GetSelectedComponentClass: TRegisteredComponent;
    Procedure NudgeControl(DiffX, DiffY: Integer);
    Procedure NudgeSize(DiffX, DiffY: Integer);
    procedure SelectParentOfSelection;
    function DoCopySelectionToClipboard: boolean;
    procedure DoPasteSelectionFromClipboard;

    // popup menu
    procedure BuildPopupMenu;
    procedure OnAlignPopupMenuClick(Sender: TObject);
    procedure OnMirrorHorizontalPopupMenuClick(Sender: TObject);
    procedure OnMirrorVerticalPopupMenuClick(Sender: TObject);
    procedure OnScalePopupMenuClick(Sender: TObject);
    procedure OnSizePopupMenuClick(Sender: TObject);
    procedure OnBringToFrontMenuClick(Sender: TObject);
    procedure OnSendToBackMenuClick(Sender: TObject);
    procedure OnDeleteSelectionMenuClick(Sender: TObject);
    procedure OnSnapToGridOptionMenuClick(Sender: TObject);
    procedure OnComponentEditorVerbMenuItemClick(Sender: TObject);
    procedure OnShowOptionsMenuItemClick(Sender: TObject);
    procedure OnSnapToGuideLinesOptionMenuClick(Sender: TObject);

    // hook
    function GetPropertyEditorHook: TPropertyEditorHook; override;
    function OnFormActivated: boolean;
    function OnFormCloseQuery: boolean;
  public
    ControlSelection : TControlSelection;
    DDC: TDesignerDeviceContext;
    
    constructor Create(TheDesignerForm: TCustomForm;
       AControlSelection: TControlSelection);
    procedure DeleteFormAndFree;
    destructor Destroy; override;

    procedure Modified; override;
    Procedure SelectOnlyThisComponent(AComponent:TComponent); override;
    procedure CopySelection;
    procedure CutSelection;
    function CanPaste: Boolean;
    procedure PasteSelection;
    procedure DeleteSelection;
    function InvokeComponentEditor(AComponent: TComponent;
      MenuIndex: integer): boolean;
    procedure DoProcessCommand(Sender: TObject; var Command: word;
                               var Handled: boolean);

    function NonVisualComponentLeftTop(AComponent: TComponent): TPoint;
    function NonVisualComponentAtPos(x,y: integer): TComponent;
    function WinControlAtPos(x,y: integer): TWinControl;
    function GetDesignedComponent(AComponent: TComponent): TComponent;
    function GetComponentEditorForSelection: TBaseComponentEditor;
    function GetShiftState: TShiftState; override;

    procedure AddComponentEditorMenuItems(
      AComponentEditor: TBaseComponentEditor; AParentMenuItem: TMenuItem);

    function IsDesignMsg(Sender: TControl;
       var TheMessage: TLMessage): Boolean; override;
    Procedure RemoveComponentAndChilds(AComponent: TComponent);
    procedure Notification(AComponent: TComponent;
       Operation: TOperation); override;
    procedure ValidateRename(AComponent: TComponent;
       const CurName, NewName: string); override;
    function CreateUniqueComponentName(const AClassName: string): string; override;

    procedure PaintGrid; override;
    procedure PaintClientGrid(AWinControl: TWinControl;
       aDDC: TDesignerDeviceContext);
    procedure DrawNonVisualComponents(aDDC: TDesignerDeviceContext);

  public
    property Flags: TDesignerFlags read FFlags;
    property Form: TCustomForm read FForm;
    property GridSizeX: integer read GetGridSizeX write SetGridSizeX;
    property GridSizeY: integer read GetGridSizeY write SetGridSizeY;
    property GridColor: TColor read GetGridColor write SetGridColor;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property LookupRoot: TComponent read FLookupRoot;
    property OnActivated: TNotifyEvent read FOnActivated write FOnActivated;
    property OnCloseQuery: TNotifyEvent read FOnCloseQuery write FOnCloseQuery;
    property OnComponentAdded: TOnComponentAdded
                                 read FOnComponentAdded write FOnComponentAdded;
    property OnComponentDeleted: TOnComponentDeleted
                             read FOnComponentDeleted write FOnComponentDeleted;
    property OnGetNonVisualCompIconCanvas: TOnGetNonVisualCompIconCanvas
                                            read FOnGetNonVisualCompIconCanvas
                                            write FOnGetNonVisualCompIconCanvas;
    property OnGetSelectedComponentClass: TOnGetSelectedComponentClass
                                             read FOnGetSelectedComponentClass
                                             write FOnGetSelectedComponentClass;
    property OnProcessCommand: TOnProcessCommand
                                 read FOnProcessCommand write FOnProcessCommand;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnPasteComponent: TOnPasteComponent read FOnPasteComponent
                                                 write FOnPasteComponent;
    property OnPropertiesChanged: TNotifyEvent
                           read FOnPropertiesChanged write FOnPropertiesChanged;
    property OnRemoveComponent: TOnRemoveComponent
                               read FOnRemoveComponent write FOnRemoveComponent;
    property OnRenameComponent: TOnRenameComponent
                               read FOnRenameComponent write FOnRenameComponent;
    property OnSetDesigning: TOnSetDesigning
                                     read FOnSetDesigning write FOnSetDesigning;
    property OnUnselectComponentClass: TNotifyEvent
                                                read FOnUnselectComponentClass
                                                write FOnUnselectComponentClass;
    property OnShowOptions: TNotifyEvent
                                       read FOnShowOptions write FOnShowOptions;
    property ShowGrid: boolean read GetShowGrid write SetShowGrid;
    property ShowEditorHints: boolean
                               read GetShowEditorHints write SetShowEditorHints;
    property ShowComponentCaptionHints: boolean
                                             read GetShowComponentCaptionHints
                                             write SetShowComponentCaptionHints;
    property SnapToGrid: boolean read GetSnapToGrid write SetSnapToGrid;
    property TheFormEditor: TCustomFormEditor
                                       read FTheFormEditor write FTheFormEditor;
  end;

function GetClipBrdSelectionFormat: TClipboardFormat;

implementation


uses
  SysUtils, Math;

const
  mk_lbutton =   1;
  mk_rbutton =   2;
  mk_shift   =   4;
  mk_control =   8;
  mk_mbutton = $10;

var
  ClipBrdSelectionFormat: TClipboardFormat;

function GetClipBrdSelectionFormat: TClipboardFormat;
begin
  if ClipBrdSelectionFormat=0 then
    ClipBrdSelectionFormat:=
      RegisterClipboardFormat('application/lazarus.componentselection');
  Result:=ClipBrdSelectionFormat;
end;

constructor TDesigner.Create(TheDesignerForm: TCustomForm;
  AControlSelection: TControlSelection);
begin
  inherited Create;
  FForm := TheDesignerForm;
  if FForm is TNonControlForm then
    FLookupRoot:=TNonControlForm(FForm).LookupRoot
  else
    FLookupRoot:=FForm;
  
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
  DeletingComponents:=TList.Create;
end;

procedure TDesigner.DeleteFormAndFree;
begin
  Include(FFlags,dfDestroyingForm);
  TheFormEditor.DeleteControl(FLookupRoot,true);
  Free;
end;

destructor TDesigner.Destroy;
Begin
  if FPopupMenu<>nil then
    FPopupMenu.Free;
    
  FHintWIndow.Free;
  FHintTimer.Free;
  DDC.Free;
  DeletingComponents.Free;
  Inherited Destroy;
end;

Procedure TDesigner.NudgeControl(DiffX, DiffY : Integer);
Begin
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.NudgeControl]');
  {$ENDIF}
  if (ControlSelection.SelectionForm<>Form)
  or ControlSelection.LookupRootSelected then exit;
  ControlSelection.MoveSelection(DiffX, DiffY);
end;

Procedure TDesigner.NudgeSize(DiffX, DiffY: Integer);
Begin
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.NudgeSize]');
  {$ENDIF}
  if (ControlSelection.SelectionForm<>Form)
  or ControlSelection.LookupRootSelected then exit;
  ControlSelection.SizeSelection(DiffX, DiffY);
end;

procedure TDesigner.SelectParentOfSelection;
var
  i: Integer;
begin
  if ControlSelection.LookupRootSelected then begin
    SelectOnlyThisComponent(FLookupRoot);
    exit;
  end;
  i:=ControlSelection.Count-1;
  while (i>=0)
  and ((ControlSelection[i].ParentInSelection)
    or (not ControlSelection[i].IsTControl)
    or (TControl(ControlSelection[i].Component).Parent=nil)) do dec(i);
  if i>=0 then
    SelectOnlyThisComponent(TControl(ControlSelection[i].Component).Parent);
end;

function TDesigner.DoCopySelectionToClipboard: boolean;

  function UnselectDistinctControls: boolean;
  var
    i: Integer;
    AParent, CurParent: TWinControl;
  begin
    Result:=false;
    AParent:=nil;
    i:=0;
    while i<ControlSelection.Count do begin
      if ControlSelection[i].IsTControl then begin
        // unselect controls from which the parent is selected too
        if ControlSelection[i].ParentInSelection then begin
          ControlSelection.Delete(i);
          continue;
        end;

        // check if not the top level component is selected
        CurParent:=TControl(ControlSelection[i].Component).Parent;
        if CurParent=nil then begin
          MessageDlg('Can not copy top level component.',
            'Copying a whole form is not implemented.',
            mtError,[mbCancel],0);
          exit;
        end;

        // unselect all controls, that do not have the same parent
        if (AParent=nil) then
          AParent:=CurParent
        else if (AParent<>CurParent) then begin
          ControlSelection.Delete(i);
          continue;
        end;
      end;
      inc(i);
    end;
    Result:=true;
  end;

  function CopySelectionToStream(AllComponentsStream: TStream): boolean;
  var
    i: Integer;
    BinCompStream: TMemoryStream;
    TxtCompStream: TMemoryStream;
    CurComponent: TComponent;
    Driver: TBinaryObjectWriter;
    Writer: TWriter;
  begin
    Result:=false;
    for i:=0 to ControlSelection.Count-1 do begin
      BinCompStream:=TMemoryStream.Create;
      TxtCompStream:=TMemoryStream.Create;
      try
        // write component binary stream
        try
          CurComponent:=ControlSelection[i].Component;

          Driver := TBinaryObjectWriter.Create(BinCompStream, 4096);
          Try
            Writer := TWriter.Create(Driver);
            Try
              Writer.Root:=FLookupRoot;
              Writer.WriteComponent(CurComponent);
            Finally
              Writer.Destroy;
            end;
          Finally
            Driver.Free;
          end;

          //BinCompStream.WriteComponent(CurComponent);
        except
          on E: Exception do begin
            MessageDlg('Unable to stream selected components',
              'There was an error during writing the selected component '
              +CurComponent.Name+':'+CurComponent.ClassName+':'#13
              +E.Message,
              mtError,[mbCancel],0);
            exit;
          end;
        end;
        BinCompStream.Position:=0;
        // convert binary to text stream
        try
          ObjectBinaryToText(BinCompStream,TxtCompStream);
        except
          on E: Exception do begin
            MessageDlg('Unable convert binary stream to text',
              'There was an error while converting the binary stream of the '
              +'selected component '
              +CurComponent.Name+':'+CurComponent.ClassName+':'#13
              +E.Message,
              mtError,[mbCancel],0);
            exit;
          end;
        end;
        // add text stream to the all stream
        TxtCompStream.Position:=0;
        AllComponentsStream.CopyFrom(TxtCompStream,TxtCompStream.Size);
      finally
        BinCompStream.Free;
        TxtCompStream.Free;
      end;
    end;
    Result:=true;
  end;
  
var
  AllComponentsStream: TMemoryStream;
  {$IFDEF VerboseDesigner}
  s: string;
  {$ENDIF}
begin
  Result:=false;
  if ControlSelection.Count=0 then exit;

  // Because controls will be pasted on a single parent,
  // unselect all controls, that do not have the same parent
  if not UnselectDistinctControls then exit;

  AllComponentsStream:=TMemoryStream.Create;
  try
    // copy components to stream
    if not CopySelectionToStream(AllComponentsStream) then exit;
    {$IFDEF VerboseDesigner}
    SetLength(s,AllComponentsStream.Size);
    if s<>'' then begin
      AllComponentsStream.Position:=0;
      AllComponentsStream.Read(s[1],length(s));
    end;
    writeln('TDesigner.DoCopySelectionToClipboard==============================');
    writeln(s);
    writeln('TDesigner.DoCopySelectionToClipboard==============================');
    {$ENDIF}

    // copy to clipboard
    try
      AllComponentsStream.Position:=0;
      ClipBoard.SetFormat(GetClipBrdSelectionFormat,AllComponentsStream);
    except
      on E: Exception do begin
        MessageDlg('Unable copy components to clipboard',
          'There was an error while copying the component stream to clipboard:'#13
          +E.Message,
          mtError,[mbCancel],0);
        exit;
      end;
    end;
  finally
    AllComponentsStream.Free;
  end;
  Result:=true;
end;

procedure TDesigner.DoPasteSelectionFromClipboard;
var
  AllComponentsStream: TMemoryStream;
  AllComponentText: string;
  StartPos: Integer;
  EndPos: Integer;
  CurTextCompStream: TStream;
  PasteParent: TWinControl;
  
  procedure GetPasteParent;
  var
    i: Integer;
  begin
    if PasteParent<>nil then exit;
    
    for i:=0 to ControlSelection.Count-1 do begin
      if (ControlSelection[i].Component is TWinControl)
      and (csAcceptsControls in
           TWinControl(ControlSelection[i].Component).ControlStyle)
      and (not ControlSelection[i].ParentInSelection) then begin
        PasteParent:=TWinControl(ControlSelection[i].Component);
        break;
      end;
    end;
    if (PasteParent=nil)
    and (FLookupRoot is TWinControl) then
      PasteParent:=TWinControl(FLookupRoot);
  end;
  
  procedure FindUniquePosition(AComponent: TComponent);
  var
    OverlappedComponent: TComponent;
    P: TPoint;
    AControl: TControl;
    AParent: TWinControl;
    i: Integer;
    OverlappedControl: TControl;
  begin
    if AComponent is TControl then begin
      AControl:=TControl(AComponent);
      AParent:=AControl.Parent;
      if AParent=nil then exit;
      P:=Point(AControl.Left,AControl.Top);
      i:=AParent.ControlCount-1;
      while i>=0 do begin
        OverlappedControl:=AParent.Controls[i];
        if (OverlappedControl<>AComponent)
        and (OverlappedControl.Left=P.X)
        and (OverlappedControl.Top=P.Y) then begin
          inc(P.X,NonVisualCompWidth);
          inc(P.Y,NonVisualCompWidth);
          if (P.X>AParent.ClientWidth-AControl.Width)
          or (P.Y>AParent.ClientHeight-AControl.Height) then
            break;
          i:=AParent.ControlCount-1;
        end else
          dec(i);
      end;
      P.x:=Max(0,Min(P.x,AParent.ClientWidth-AControl.Width));
      P.y:=Max(0,Min(P.y,AParent.ClientHeight-AControl.Height));
      AControl.SetBounds(P.x,P.y,AControl.Width,AControl.Height);
    end else begin
      P:=GetParentFormRelativeTopLeft(AComponent);
      repeat
        OverlappedComponent:=NonVisualComponentAtPos(P.x,P.y);
        if (OverlappedComponent=nil) then break;
        inc(P.X,NonVisualCompWidth);
        inc(P.Y,NonVisualCompWidth);
        if (P.X+NonVisualCompWidth>Form.ClientWidth)
        or (P.Y+NonVisualCompWidth>Form.ClientHeight) then
          break;
      until false;
      LongRec(AComponent.DesignInfo).Lo:=
        Max(0,Min(P.x,Form.ClientWidth-NonVisualCompWidth));
      LongRec(AComponent.DesignInfo).Hi:=
        Max(0,Min(P.y,Form.ClientHeight-NonVisualCompWidth));
    end;
  end;
  
  function PasteComponent(TextCompStream: TStream): boolean;
  var
    NewComponent: TComponent;
  begin
    Result:=false;
    TextCompStream.Position:=0;
    if Assigned(FOnPasteComponent) then begin
      NewComponent:=nil;
      FOnPasteComponent(Self,FLookupRoot,TextCompStream,
                        PasteParent,NewComponent);
      if NewComponent=nil then exit;
      FindUniquePosition(NewComponent);
    end;

    Result:=true;
  end;
  
begin
  if not CanPaste then exit;

  PasteParent:=nil;
  GetPasteParent;

  AllComponentsStream:=TMemoryStream.Create;
  try
    // read component stream from clipboard
    ClipBoard.GetFormat(GetClipBrdSelectionFormat,AllComponentsStream);
    if AllComponentsStream.Size=0 then exit;
    
    SetLength(AllComponentText,AllComponentsStream.Size);
    if AllComponentText<>'' then begin
      AllComponentsStream.Position:=0;
      AllComponentsStream.Read(AllComponentText[1],length(AllComponentText));
    end;

    AllComponentsStream.Position:=0;
    
    StartPos:=1;
    EndPos:=StartPos;
    // read till 'end'
    while EndPos<=length(AllComponentText) do begin
      if (AllComponentText[EndPos] in ['e','E'])
      and (EndPos>1)
      and (AllComponentText[EndPos-1] in [#10,#13])
      and (AnsiCompareText(copy(AllComponentText,EndPos,3),'END')=0)
      and ((EndPos+3>length(AllComponentText))
           or (AllComponentText[EndPos+3] in [#10,#13]))
      then begin
        inc(EndPos,4);
        while (EndPos<=length(AllComponentText))
        and (AllComponentText[EndPos] in [' ',#10,#13])
        do
          inc(EndPos);
        // extract text for the current component
        writeln('TDesigner.DoPasteSelectionFromClipboard==============================');
        writeln(copy(AllComponentText,StartPos,EndPos-StartPos));
        writeln('TDesigner.DoPasteSelectionFromClipboard==============================');
        
        CurTextCompStream:=TMemoryStream.Create;
        try
          CurTextCompStream.Write(AllComponentText[StartPos],EndPos-StartPos);
          CurTextCompStream.Position:=0;
          // create component from stream
          if not PasteComponent(CurTextCompStream) then exit;

        finally
          CurTextCompStream.Free;
        end;
        
        StartPos:=EndPos;
      end else begin
        inc(EndPos);
      end;
    end;

  finally
    AllComponentsStream.Free;
  end;
end;

procedure TDesigner.SelectOnlyThisComponent(AComponent:TComponent);
begin
  ControlSelection.AssignComponent(AComponent);
end;

procedure TDesigner.CopySelection;
begin
  DoCopySelectionToClipboard;
end;

procedure TDesigner.CutSelection;
begin
  if DoCopySelectionToClipboard then
    DoDeleteSelectedComponents;
end;

function TDesigner.CanPaste: Boolean;
begin
  Result:=(Form<>nil)
      and (FLookupRoot<>nil)
      and (not (csDestroying in FLookupRoot.ComponentState));
end;

procedure TDesigner.PasteSelection;
begin
  DoPasteSelectionFromClipboard;
end;

procedure TDesigner.DeleteSelection;
begin
  DoDeleteSelectedComponents;
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

procedure TDesigner.DoProcessCommand(Sender: TObject; var Command: word;
  var Handled: boolean);
begin
  if Assigned(OnProcessCommand) and (Command<>ecNone) then begin
    OnProcessCommand(Self,Command,Handled);
    Handled:=Handled or (Command=ecNone);
  end;

  if not Handled then begin
    Handled:=true;
    case Command of

    ecSelectParentComponent:
      SelectParentOfSelection;
      
    ecCopyComponents:
      CopySelection;
      
    ecCutComponents:
      begin
        CopySelection;
        DeleteSelection;
      end;
      
    ecPasteComponents:
      PasteSelection;

    else
      Handled:=false;
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
    FForm.Invalidate;
  end;
end;

procedure TDesigner.SetGridColor(const AValue: TColor);
begin
  if GridColor=AValue then exit;
  EnvironmentOptions.GridColor:=AValue;
  Form.Invalidate;
end;

procedure TDesigner.SetShowComponentCaptionHints(const AValue: boolean);
begin
  if AValue=ShowComponentCaptionHints then exit;
  Include(FFlags,dfShowComponentCaptionHints);
end;

function TDesigner.PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
var
  OldDuringPaintControl, InternalPaint: boolean;
begin
  Result:=true;
  
  //writeln('***  TDesigner.PaintControl A ',Sender.Name,':',Sender.ClassName);
  // Set flag
  OldDuringPaintControl:=dfDuringPaintControl in FFlags;
  Include(FFlags,dfDuringPaintControl);
  
  // send the Paint message to the control, so that it paints itself
  //writeln('TDesigner.PaintControl B ',Sender.Name);
  Sender.Dispatch(TheMessage);
  //writeln('TDesigner.PaintControl C ',Sender.Name,' DC=',HexStr(Cardinal(TheMessage.DC),8));

  // paint the Designer stuff
  if TheMessage.DC<>0 then begin
    InternalPaint:=(TheMessage.Msg=LM_INTERNALPAINT);
    DDC.SetDC(Form,TheMessage.DC);
    {$IFDEF VerboseDesignerDraw}
    writeln('TDesigner.PaintControl D ',Sender.Name,':',Sender.ClassName,
      ' DC=',HexStr(DDC.DC,8),
      ' FormOrigin=',DDC.FormOrigin.X,',',DDC.FormOrigin.Y,
      ' DCOrigin=',DDC.DCOrigin.X,',',DDC.DCOrigin.Y,
      ' FormClientOrigin=',DDC.FormClientOrigin.X,',',DDC.FormClientOrigin.Y,
      ' Internal=',InternalPaint
      );
    {$ENDIF}
    if LastPaintSender=Sender then begin
      writeln('NOTE: TDesigner.PaintControl E control painted twice: ',Sender.Name,':',Sender.ClassName);
      //RaiseException('');
    end;
    LastPaintSender:=Sender;
    // client grid
    if (not InternalPaint) and (Sender is TWinControl)
    and (csAcceptsControls in Sender.ControlStyle) then begin
      PaintClientGrid(TWinControl(Sender),DDC);
    end;
    // marker
    if (ControlSelection.SelectionForm=Form)
    and (ControlSelection.IsSelected(Sender)) then begin
      ControlSelection.DrawMarker(Sender,DDC);
    end;
    // non visual component icons
    DrawNonVisualComponents(DDC);
    // guidelines and grabbers
    if (ControlSelection.SelectionForm=Form) then begin
      ControlSelection.DrawGuideLines(DDC);
      ControlSelection.DrawGrabbers(DDC);
    end;
    // rubberband
    if ControlSelection.RubberBandActive
    and ((ControlSelection.SelectionForm=Form)
    or (ControlSelection.SelectionForm=nil)) then begin
      ControlSelection.DrawRubberBand(DDC);
    end;
    // clean up
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
  if ControlSelection.SelectionForm=Form then begin
    if not ControlSelection.IsResizing then begin
      {writeln('###  TDesigner.SizeControl ',Sender.Name,':',Sender.ClassName,
        ' ',Sender.Width,',',Sender.Height,
        ' Type=',TheMessage.SizeType
        ,' ',TheMessage.Width,',',TheMessage.Height,' Pos=',Sender.Left,',',Sender.Top);}
      ControlSelection.UpdateBounds;
      if Assigned(FOnPropertiesChanged) then
        FOnPropertiesChanged(Self);
    end;
    ControlSelection.InvalidateGuideLinesCache;
  end;
end;

function TDesigner.MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
begin
  Result:=true;
  Sender.Dispatch(TheMessage);
  //writeln('***  TDesigner.MoveControl A ',Sender.Name,':',Sender.ClassName,' ',ControlSelection.SelectionForm=Form,' ',not ControlSelection.IsResizing,' ',ControlSelection.IsSelected(Sender));
  if ControlSelection.SelectionForm=Form then begin
    if not ControlSelection.IsResizing then begin
      //writeln('***  TDesigner.MoveControl ',Sender.Name,':',Sender.ClassName,' ',Assigned(FOnPropertiesChanged));
      ControlSelection.UpdateBounds;
      if Assigned(FOnPropertiesChanged) then
        FOnPropertiesChanged(Self);
    end;
    ControlSelection.InvalidateGuideLinesCache;
  end;
end;

procedure TDesigner.MouseDownOnControl(Sender: TControl;
  var TheMessage: TLMMouse);
var
  CompIndex:integer;
  SelectedCompClass: TRegisteredComponent;
  NonVisualComp: TComponent;
  ParentForm: TCustomForm;
  Shift: TShiftState;
Begin
  FHintTimer.Enabled := False;
  Exclude(FFLags,dfHasSized);
  SetCaptureControl(nil);
  ParentForm:=GetParentForm(Sender);
  if (ParentForm=nil) then exit;

  MouseDownPos:=GetFormRelativeMousePosition(Form);
  LastMouseMovePos:=MouseDownPos;

  MouseDownComponent:=nil;
  MouseDownSender:=nil;
  
  NonVisualComp:=NonVisualComponentAtPos(MouseDownPos.X,MouseDownPos.Y);
  if NonVisualComp<>nil then MouseDownComponent:=NonVisualComp;

  if MouseDownComponent=nil then begin
    MouseDownComponent:=GetDesignedComponent(Sender);
    if MouseDownComponent=nil then exit;
  end;
  MouseDownSender:=Sender;

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

  Shift := [];
  if (TheMessage.keys and MK_Shift) = MK_Shift then
    Include(Shift,ssShift);
  if (TheMessage.keys and MK_Control) = MK_Control then
    Include(Shift,ssCtrl);


  {$IFDEF VerboseDesigner}
  writeln('************************************************************');
  write('MouseDownOnControl');
  write(' ',Sender.Name,':',Sender.ClassName);
  //write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
  //write(' Mouse=',MouseDownPos.X,',',MouseDownPos.Y);
  //writeln('');

  if (TheMessage.Keys and MK_Shift) = MK_Shift then
    Write(' Shift down')
  else
    Write(' No Shift down');

  if (TheMessage.Keys and MK_Control) = MK_Control then
    Writeln(', CTRL down')
  else
    Writeln(', No CTRL down');
  {$ENDIF}

  SelectedCompClass:=GetSelectedComponentClass;


  if (TheMessage.Keys and MK_LButton) > 0 then begin
    // left button
    // -> check if a grabber was activated
    ControlSelection.ActiveGrabber:=
      ControlSelection.GrabberAtPos(MouseDownPos.X,MouseDownPos.Y);
      
    if SelectedCompClass = nil then begin
      // selection mode
      if ControlSelection.ActiveGrabber=nil then begin
        // no grabber resizing

        CompIndex:=ControlSelection.IndexOf(MouseDownComponent);
        if ssCtrl in Shift then begin
          // child selection
        end else begin
          if (ssShift in Shift) then begin
          // shift key pressed (multiselection)

            if CompIndex<0 then begin
              // not selected
              // add component to selection
              if (ControlSelection.SelectionForm<>nil)
              and (ControlSelection.SelectionForm<>Form)
              then begin
                MessageDlg('Invalid multiselection',
                  'Multiselected components must be of a single form.',
                  mtInformation,[mbOk],0);
              end else begin
                ControlSelection.Add(MouseDownComponent);
              end;
            end else begin
              // remove from multiselection
              ControlSelection.Delete(CompIndex);
            end;
          end else begin
            // no shift key (single selection or kept multiselection)

            if (CompIndex<0) then begin
              // select only this component
              ControlSelection.AssignComponent(MouseDownComponent);
            end else
              // sync with the interface
              ControlSelection.UpdateBounds;
          end;
        end;
      end else begin
        // mouse down on grabber -> begin sizing
        // grabber is already activated
        // the sizing is handled in mousemove and mouseup
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
  var TheMessage:TLMMouse);
var
  ParentCI, NewCI: TComponentInterface;
  NewLeft, NewTop, NewWidth, NewHeight: Integer;
  Shift: TShiftState;
  SenderParentForm: TCustomForm;
  RubberBandWasActive: boolean;
  ParentClientOrigin: TPoint;
  SelectedCompClass: TRegisteredComponent;
  SelectionChanged, NewRubberbandSelection: boolean;
  
  procedure GetShift;
  begin
    Shift := [];
    if (TheMessage.keys and MK_Shift) = MK_Shift then
      Include(Shift,ssShift);
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
  var
    NewParent: TComponent;
    NewParentControl: TWinControl;
  begin
    if MouseDownComponent=nil then exit;

    // add a new component
    ControlSelection.RubberbandActive:=false;
    ControlSelection.Clear;

    // find a parent for the new component
writeln('AddComponent A ',FLookupRoot is TCustomForm);
    if FLookupRoot is TCustomForm then begin
      if MouseDownComponent is TWinControl then
        NewParentControl:=TWinControl(MouseDownComponent)
      else
        NewParentControl:=WinControlAtPos(MouseDownPos.X,MouseUpPos.X);
      while (NewParentControl<>nil)
      and ((not (csAcceptsControls in NewParentControl.ControlStyle))
        or ((NewParentControl.Owner<>FLookupRoot)
             and (NewParentControl<>FLookupRoot)))
      do begin
        NewParentControl:=NewParentControl.Parent;
      end;
      NewParent:=NewParentControl;
    end else begin
      NewParent:=FLookupRoot;
    end;
    ParentCI:=TComponentInterface(TheFormEditor.FindComponent(NewParent));
    if not Assigned(ParentCI) then exit;
    
    if not PropertyEditorHook.BeforeAddComponent(Self,
                                     SelectedCompClass.ComponentClass,NewParent)
    then begin
      writeln('TDesigner.AddComponent ',
              SelectedCompClass.ComponentClass.ClassName,' not possible');
      exit;
    end;

    // calculate initial bounds
    ParentClientOrigin:=GetParentFormRelativeClientOrigin(NewParent);
    NewLeft:=Min(MouseDownPos.X,MouseUpPos.X);
    NewTop:=Min(MouseDownPos.Y,MouseUpPos.Y);
    if SelectedCompClass.ComponentClass.InheritsFrom(TControl) then begin
      // adjust left,top to parent origin
      dec(NewLeft,ParentClientOrigin.X);
      dec(NewTop,ParentClientOrigin.Y);
    end;
    NewWidth:=Abs(MouseUpPos.X-MouseDownPos.X);
    NewHeight:=Abs(MouseUpPos.Y-MouseDownPos.Y);
    if Abs(NewWidth+NewHeight)<7 then begin
      // this very small component is probably only a wag, take default size
      NewWidth:=0;
      NewHeight:=0;
    end;

    // create component and component interface
    NewCI := TComponentInterface(TheFormEditor.CreateComponent(
       ParentCI,SelectedCompClass.ComponentClass
      ,NewLeft,NewTop,NewWidth,NewHeight));

    // set initial properties
    if NewCI.Component is TControl then
      TControl(NewCI.Component).Visible:=true;
    if Assigned(FOnSetDesigning) then
      FOnSetDesigning(Self,NewCI.Component,True);
      
    // tell IDE about the new component (e.g. add it to the source)
    try
      if Assigned(FOnComponentAdded) then
        FOnComponentAdded(Self,NewCI.Component,SelectedCompClass);
    except
      on E: Exception do
        MessageDlg('Error:',E.Message,mtError,[mbOk],0);
    end;

    // creation completed
    // -> select new component
    SelectOnlyThisComponent(TComponent(NewCI.Component));
    if not (ssShift in Shift) then
      if Assigned(FOnUnselectComponentClass) then
        // this resets the component palette to the selection tool
        FOnUnselectComponentClass(Self);
        
    //Form.Invalidate;
    {$IFDEF VerboseDesigner}
    writeln('NEW COMPONENT ADDED: Form.ComponentCount=',Form.ComponentCount,
       '  NewCI.Control.Owner.Name=',NewCI.Component.Owner.Name);
    {$ENDIF}
  end;
  
  procedure RubberbandSelect;
  var
    MaxParentControl: TControl;
  begin
    if (ssShift in Shift)
    and (ControlSelection.SelectionForm<>nil)
    and (ControlSelection.SelectionForm<>Form)
    then begin
      MessageDlg(fdInvalidMutliselectionCap,
        'Multiselected components must be of a single form.',
        mtInformation,[mbOk],0);
      exit;
    end;

    ControlSelection.BeginUpdate;
    // check if start new selection or add/remove:
    NewRubberbandSelection:= (not (ssShift in Shift))
      or (ControlSelection.SelectionForm<>Form);
    // if user press the Control key, then component candidates are only
    // childs of the control, where the mouse started
    if (ssCtrl in shift) and (MouseDownComponent is TControl) then
      MaxParentControl:=TControl(MouseDownComponent)
    else
      MaxParentControl:=Form;
    SelectionChanged:=false;
    ControlSelection.SelectWithRubberBand(
      FLookupRoot,NewRubberbandSelection,ssShift in Shift,SelectionChanged,
      MaxParentControl);
    if ControlSelection.Count=0 then begin
      ControlSelection.Add(FLookupRoot);
      SelectionChanged:=true;
    end;
    ControlSelection.RubberbandActive:=false;
    ControlSelection.EndUpdate;
    {$IFDEF VerboseDesigner}
    with ControlSelection.Grabbers[0] do
      writeln('RubberbandSelect ',Left,',',Top,',',Width,',',Height);
    {$ENDIF}
    Form.Invalidate;
  end;
  
  procedure PointSelect;
  begin
    if (not (ssShift in Shift)) then begin
      // select only the mouse down component
      ControlSelection.AssignComponent(MouseDownComponent);
      if (MouseDownClickCount=2)
      and (ControlSelection.SelectionForm=Form) then begin
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
    end;
  end;
  
Begin
  FHintTimer.Enabled := False;
  SetCaptureControl(nil);
  
  // check if the message is for the designed form
  // and there was a mouse down before
  SenderParentForm:=GetParentForm(Sender);
  if (MouseDownComponent=nil) or (SenderParentForm=nil)
  or (SenderParentForm<>Form)
  or ((ControlSelection.SelectionForm<>nil)
    and (ControlSelection.SelectionForm<>Form)) then
  begin
    MouseDownComponent:=nil;
    MouseDownSender:=nil;
    exit;
  end;
  
  ControlSelection.ActiveGrabber:=nil;
  RubberBandWasActive:=ControlSelection.RubberBandActive;
  SelectedCompClass:=GetSelectedComponentClass;

  GetShift;
  MouseUpPos:=GetFormRelativeMousePosition(Form);

  {$IFDEF VerboseDesigner}
  writeln('************************************************************');
  write('MouseUpOnControl');
  write(' ',Sender.Name,':',Sender.ClassName);
  //write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
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
  SelectedCompClass: TRegisteredComponent;
  CurSnappedMousePos, OldSnappedMousePos: TPoint;
begin
  SetCaptureControl(nil);
  if [dfShowEditorHints,dfShowComponentCaptionHints]*FFlags<>[] then begin
    FHintTimer.Enabled := False;

    // hide hint
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

  if ControlSelection.SelectionForm=Form then
    Grabber:=ControlSelection.GrabberAtPos(
                         LastMouseMovePos.X, LastMouseMovePos.Y)
  else
    Grabber:=nil;
                         
  if MouseDownComponent=nil then begin
    if Grabber = nil then
      ACursor:= crDefault
    else begin
      ACursor:= Grabber.Cursor;
    end;
    if ACursor<>LastFormCursor then begin
      LastFormCursor:=ACursor;
      CNSendMessage(LM_SETCURSOR, Form, Pointer(Integer(ACursor)));
    end;
    
    exit;
  end;

  Shift := [];
  if (TheMessage.keys and MK_Shift) = MK_Shift then
    Include(Shift,ssShift);
  if (TheMessage.keys and MK_Control) = MK_Control then
    Include(Shift,ssCtrl);

  if (ControlSelection.SelectionForm=nil)
  or (ControlSelection.SelectionForm=Form)
  then begin
    if (TheMessage.keys and MK_LButton) = MK_LButton then begin
      // left button pressed
      if (ControlSelection.ActiveGrabber<>nil) then begin
        // grabber moving -> size selection
        if not (dfHasSized in FFlags) then begin
          ControlSelection.SaveBounds;
          Include(FFlags,dfHasSized);
        end;
        OldSnappedMousePos:=
          ControlSelection.SnapGrabberMousePos(OldMouseMovePos);
        CurSnappedMousePos:=
          ControlSelection.SnapGrabberMousePos(LastMouseMovePos);
        ControlSelection.SizeSelection(
          CurSnappedMousePos.X-OldSnappedMousePos.X,
          CurSnappedMousePos.Y-OldSnappedMousePos.Y);
        if Assigned(OnModified) then OnModified(Self);
      end else begin
        // no grabber active
        SelectedCompClass:=GetSelectedComponentClass;
        if (not ControlSelection.RubberBandActive)
        and (SelectedCompClass=nil)
        and (Shift=[])
        and (ControlSelection.Count>=1)
        and (not ControlSelection.LookupRootSelected)
        then begin
          // move selection
          if not (dfHasSized in FFlags) then begin
            ControlSelection.SaveBounds;
            Include(FFlags,dfHasSized);
          end;
          if ControlSelection.MoveSelectionWithSnapping(
            LastMouseMovePos.X-MouseDownPos.X,LastMouseMovePos.Y-MouseDownPos.Y)
          then begin
            if Assigned(OnModified) then OnModified(Self);
          end;
        end
        else
        begin
          // rubberband sizing (selection or creation)
          ControlSelection.RubberBandBounds:=Rect(MouseDownPos.X,MouseDownPos.Y,
                                                  LastMouseMovePos.X,
                                                  LastMouseMovePos.Y);
          if SelectedCompClass=nil then
            ControlSelection.RubberbandType:=rbtSelection
          else
            ControlSelection.RubberbandType:=rbtCreating;
          ControlSelection.RubberBandActive:=true;
        end;
      end;
    end
    else begin
      ControlSelection.ActiveGrabber:=nil;
    end;
  end;
end;


{
-----------------------------K E Y D O W N -------------------------------
}
{
  Handles the keydown messages.  DEL deletes the selected controls, CTRL-ARROR
  moves the selection up one, SHIFT-ARROW resizes, etc.
}
Procedure TDesigner.KeyDown(Sender : TControl; var TheMessage:TLMKEY);
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
  DoProcessCommand(Self,Command,Handled);

  if not Handled then begin
    Handled:=true;
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
        
    else
      Handled:=false;
    end;
  end;
  
  if Handled then begin
    TheMessage.CharCode:=0;
  end;
end;


{-----------------------------------------K E Y U P --------------------------------}
Procedure TDesigner.KeyUp(Sender : TControl; var TheMessage:TLMKEY);
Begin
  {$IFDEF VerboseDesigner}
  //Writeln('TDesigner.KEYUP ',TheMessage.CharCode,' ',TheMessage.KeyData);
  {$ENDIF}
end;

procedure TDesigner.DoDeleteSelectedComponents;
var
  i: integer;
begin
  if (ControlSelection.Count=0) or (ControlSelection.SelectionForm<>Form) then
    exit;
  if (ControlSelection.LookupRootSelected) then begin
    if ControlSelection.Count>1 then
      MessageDlg('Invalid delete',
       'The root component can not be deleted.',mtInformation,
       [mbOk],0);
    exit;
  end;
  // mark selected components for deletion
  for i:=0 to ControlSelection.Count-1 do
    MarkComponentForDeletion(ControlSelection[i].Component);
  // clear selection by selecting the LookupRoot
  SelectOnlythisComponent(FLookupRoot);
  // delete marked components
  while DeletingComponents.Count>0 do
    RemoveComponentAndChilds(
      TComponent(DeletingComponents[DeletingComponents.Count-1]));
end;

procedure TDesigner.DoDeleteComponent(AComponent: TComponent;
  FreeComponent: boolean);
var
  Hook: TPropertyEditorHook;
begin
  PopupMenuComponentEditor:=nil;
  if TheFormEditor.FindComponent(AComponent)<>nil then begin
    // unselect component
    ControlSelection.Remove(AComponent);
    // call RemoveComponent handler
    if Assigned(FOnRemoveComponent) then
      FOnRemoveComponent(Self,AComponent);
    // call component deleting handlers
    Hook:=GetPropertyEditorHook;
    if Hook<>nil then
      Hook.ComponentDeleting(AComponent);
    // delete component
    TheFormEditor.DeleteControl(AComponent,FreeComponent);
    // unmark component
    DeletingComponents.Remove(AComponent);
    // call ComponentDeleted handler
    if Assigned(FOnComponentDeleted) then
      FOnComponentDeleted(Self,AComponent);
  end;
end;

procedure TDesigner.MarkComponentForDeletion(AComponent: TComponent);
begin
  if not ComponentIsMarkedForDeletion(AComponent) then
    DeletingComponents.Add(AComponent);
end;

function TDesigner.ComponentIsMarkedForDeletion(AComponent: TComponent
  ): boolean;
begin
  Result:=(DeletingComponents.IndexOf(AComponent)>=0);
end;

function TDesigner.GetSelectedComponentClass: TRegisteredComponent;
begin
  Result:=nil;
  if Assigned(FOnGetSelectedComponentClass) then
    FOnGetSelectedComponentClass(Self,Result);
end;

function TDesigner.IsDesignMsg(Sender: TControl;
  var TheMessage: TLMessage): Boolean;
Begin
  Result := false;
  if csDesigning in Sender.ComponentState then begin
    Result:=true;
    case TheMessage.Msg of
      LM_PAINT,
      LM_INTERNALPAINT:
                      Result:=PaintControl(Sender,TLMPaint(TheMessage));
      LM_KEYDOWN:     KeyDown(Sender,TLMKey(TheMessage));
      LM_KEYUP:       KeyUP(Sender,TLMKey(TheMessage));
      LM_LBUTTONDOWN,
      LM_RBUTTONDOWN,
      LM_LBUTTONDBLCLK: MouseDownOnControl(Sender,TLMMouse(TheMessage));
      LM_LBUTTONUP,
      LM_RBUTTONUP:   MouseUpOnControl(Sender,TLMMouse(TheMessage));
      LM_MOUSEMOVE:   MouseMoveOnControl(Sender, TLMMouse(TheMessage));
      LM_SIZE:        Result:=SizeControl(Sender,TLMSize(TheMessage));
      LM_MOVE:        Result:=MoveControl(Sender,TLMMove(TheMessage));
      LM_ACTIVATE:    Result:=OnFormActivated;
      LM_CLOSEQUERY:  Result:=OnFormCloseQuery;
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

Procedure TDesigner.RemoveComponentAndChilds(AComponent :TComponent);
var
  i: integer;
  AWinControl: TWinControl;
  ChildControl: TControl;
Begin
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.RemoveComponentAndChilds] ',AComponent.Name,':',AComponent.ClassName);
  {$ENDIF}
  if (AComponent=FLookupRoot) or (AComponent=Form) then exit;
  // remove all child controls owned by the LookupRoot
  if (AComponent is TWinControl) then begin
    AWinControl:=TWinControl(AComponent);
    i:=AWinControl.ControlCount-1;
    while (i>=0) do begin
      ChildControl:=AWinControl.Controls[i];
      if ChildControl.Owner=FLookupRoot then begin
        RemoveComponentAndChilds(ChildControl);
        // the component list of the form has changed
        // -> restart the search
        i:=AWinControl.ControlCount-1;
      end else
        dec(i);
    end;
  end;
  // remove component
  {$IFDEF VerboseDesigner}
  Writeln('[TDesigner.RemoveComponentAndChilds] C ',AComponent.Name,':',AComponent.ClassName);
  {$ENDIF}
  DoDeleteComponent(AComponent,true);
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
    DoDeleteComponent(AComponent,false);
  end;
end;

procedure TDesigner.PaintGrid;
begin
  // This is done in PaintControls
  if FLookupRoot<>FForm then begin
    // this is a special designer form -> lets draw itself
    FForm.Paint;
  end;
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
        if (Visible or ((csDesigning in ComponentState)
          and not (csNoDesignVisible in ControlStyle)))
        and (csOpaque in ControlStyle)
        then begin
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
  // check if component is initialized
  if (CurName='') or (NewName='')
  or ((AComponent<>nil) and (csDestroying in AComponent.ComponentState)) then
    exit;
  // check if component is the LookupRoot
  if AComponent=nil then AComponent:=FLookupRoot;
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
  Result:=TheFormEditor.CreateUniqueComponentName(AClassName,FLookupRoot);
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

procedure TDesigner.OnSnapToGridOptionMenuClick(Sender: TObject);
begin
  EnvironmentOptions.SnapToGrid:=not EnvironmentOptions.SnapToGrid;
end;

procedure TDesigner.OnShowOptionsMenuItemClick(Sender: TObject);
begin
  if Assigned(OnShowOptions) then OnShowOptions(Self);
end;

procedure TDesigner.OnSnapToGuideLinesOptionMenuClick(Sender: TObject);
begin
  EnvironmentOptions.SnapToGuideLines:=not EnvironmentOptions.SnapToGuideLines;
end;

function TDesigner.GetGridColor: TColor;
begin
  Result:=EnvironmentOptions.GridColor;
end;

function TDesigner.GetShowComponentCaptionHints: boolean;
begin
  Result:=dfShowComponentCaptionHints in FFlags;
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

function TDesigner.GetShowEditorHints: boolean;
begin
  Result:=dfShowEditorHints in FFlags;
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

procedure TDesigner.SetShowEditorHints(const AValue: boolean);
begin
  if AValue=ShowEditorHints then exit;
  Include(FFlags,dfShowEditorHints);
end;

procedure TDesigner.DrawNonVisualComponents(aDDC: TDesignerDeviceContext);
var
  i, j, ItemLeft, ItemTop, ItemRight, ItemBottom,
  IconWidth, IconHeight: integer;
  Diff, ItemLeftTop: TPoint;
  IconRect: TRect;
  IconCanvas: TCanvas;
  AComponent: TComponent;
begin
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if (not (AComponent is TControl))
    and (not ComponentIsInvisible(AComponent)) then begin
      Diff:=aDDC.FormOrigin;
      // non-visual component
      ItemLeftTop:=NonVisualComponentLeftTop(AComponent);
      ItemLeft:=ItemLeftTop.X-Diff.X;
      ItemTop:=ItemLeftTop.Y-Diff.Y;
      ItemRight:=ItemLeft+NonVisualCompWidth;
      ItemBottom:=ItemTop+NonVisualCompWidth;
      if not aDDC.RectVisible(ItemLeft,ItemTop,ItemRight,ItemBottom) then
        continue;
      aDDC.Save;
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
        FOnGetNonVisualCompIconCanvas(Self,AComponent
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
      and (ControlSelection.IsSelected(AComponent)) then
        ControlSelection.DrawMarkerAt(aDDC,
          ItemLeft,ItemTop,NonVisualCompWidth,NonVisualCompWidth);
    end;
  end;
end;

function TDesigner.GetDesignedComponent(AComponent: TComponent): TComponent;
begin
  Result:=AComponent;
  if AComponent=Form then begin
    Result:=FLookupRoot;
  end else begin
    while (Result<>nil)
    and (Result<>FLookupRoot)
    and (Result.Owner<>FLookupRoot)
    and (Result is TControl) do
      Result:=TControl(Result).Parent;
  end;
end;

function TDesigner.GetComponentEditorForSelection: TBaseComponentEditor;
begin
  Result:=nil;
  if (ControlSelection.Count<>1)
  or (ControlSelection.SelectionForm<>Form) then exit;
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
  for i:=FLookupRoot.ComponentCount-1 downto 0 do begin
    Result:=FLookupRoot.Components[i];
    if (not (Result is TControl))
    and (not ComponentIsInvisible(Result)) then begin
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

function TDesigner.WinControlAtPos(x, y: integer): TWinControl;
var i: integer;
  WinControlBounds: TRect;
begin
  for i:=FLookupRoot.ComponentCount-1 downto 0 do begin
    Result:=TWinControl(FLookupRoot.Components[i]);
    if (Result is TWinControl) then begin
      with Result do begin
        WinControlBounds:=GetParentFormRelativeBounds(Result);
        if (WinControlBounds.Left<=x) and (WinControlBounds.Top<=y)
        and (WinControlBounds.Right>x)
        and (WinControlBounds.Bottom>y) then
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
  LookupRootIsSelected,
  OnlyNonVisualCompsAreSelected,
  CompsAreSelected: boolean;
begin
  if FPopupMenu<>nil then FPopupMenu.Free;

  ControlSelIsNotEmpty:=(ControlSelection.Count>0)
                        and (ControlSelection.SelectionForm=Form);
  LookupRootIsSelected:=ControlSelection.LookupRootSelected;
  OnlyNonVisualCompsAreSelected:=
                               ControlSelection.OnlyNonVisualComponentsSelected;
  CompsAreSelected:=ControlSelIsNotEmpty and not LookupRootIsSelected;

  FPopupMenu:=TPopupMenu.Create(nil);

  AddComponentEditorMenuItems(PopupMenuComponentEditor,FPopupMenu.Items);

  // menuitem: align, mirror horizontal, mirror vertical, scale, size
  FAlignMenuItem := TMenuItem.Create(FPopupMenu);
  with FAlignMenuItem do begin
    Caption := fdmAlignWord;
    OnClick := @OnAlignPopupMenuClick;
    Enabled := CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FAlignMenuItem);

  FMirrorHorizontalMenuItem := TMenuItem.Create(FPopupMenu);
  with FMirrorHorizontalMenuItem do begin
    Caption := fdmMirrorHorizontal;
    OnClick := @OnMirrorHorizontalPopupMenuClick;
    Enabled := CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FMirrorHorizontalMenuItem);

  FMirrorVerticalMenuItem := TMenuItem.Create(FPopupMenu);
  with FMirrorVerticalMenuItem do begin
    Caption := fdmMirrorVertical;
    OnClick := @OnMirrorVerticalPopupMenuClick;
    Enabled := CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FMirrorVerticalMenuItem);

  FScaleMenuItem := TMenuItem.Create(FPopupMenu);
  with FScaleMenuItem do begin
    Caption := fdmScaleWord;
    OnClick := @OnScalePopupMenuClick;
    Enabled := CompsAreSelected and not OnlyNonVisualCompsAreSelected;
  end;
  FPopupMenu.Items.Add(FScaleMenuItem);

  FSizeMenuItem := TMenuItem.Create(FPopupMenu);
  with FSizeMenuItem do begin
    Caption := fdmSizeWord;
    OnClick := @OnSizePopupMenuClick;
    Enabled := CompsAreSelected and not OnlyNonVisualCompsAreSelected;
  end;
  FPopupMenu.Items.Add(FSizeMenuItem);
  
  AddSeparator;
  
  // menuitem: BringToFront, SendToBack
  FBringToFrontMenuItem := TMenuItem.Create(FPopupMenu);
  with FBringToFrontMenuItem do begin
    Caption:= fdmBringTofront;
    OnClick:= @OnBringToFrontMenuClick;
    Enabled:= CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FBringToFrontMenuItem);
  
  FSendToBackMenuItem:= TMenuItem.Create(FPopupMenu);
  with FSendToBackMenuItem do begin
    Caption:= fdmSendtoback;
    OnClick:= @OnSendToBackMenuClick;
    Enabled:= CompsAreSelected;
  end;
  FPopupMenu.Items.Add(FSendToBackMenuItem);
  
  AddSeparator;
  
  // menuitem: delete selection
  FDeleteSelectionMenuItem:=TMenuItem.Create(FPopupMenu);
  with FDeleteSelectionMenuItem do begin
    Caption:= fdmDeleteSelection;
    OnClick:=@OnDeleteSelectionMenuClick;
    Enabled:= ControlSelIsNotEmpty and (not LookupRootIsSelected);
  end;
  FPopupMenu.Items.Add(FDeleteSelectionMenuItem);

  AddSeparator;

  FSnapToGridOptionMenuItem:=TMenuItem.Create(FPopupMenu);
  with FSnapToGridOptionMenuItem do begin
    Caption:= fdmSnapToGridOption;
    OnClick:=@OnSnapToGridOptionMenuClick;
    Checked:=EnvironmentOptions.SnapToGrid;
  end;
  FPopupMenu.Items.Add(FSnapToGridOptionMenuItem);

  FSnapToGuideLinesOptionMenuItem:=TMenuItem.Create(FPopupMenu);
  with FSnapToGuideLinesOptionMenuItem do begin
    Caption:= fdmSnapToGuideLinesOption;
    OnClick:=@OnSnapToGuideLinesOptionMenuClick;
    Checked:=EnvironmentOptions.SnapToGuideLines;
  end;
  FPopupMenu.Items.Add(FSnapToGuideLinesOptionMenuItem);

  FShowOptionsMenuItem:=TMenuItem.Create(FPopupMenu);
  with FShowOptionsMenuItem do begin
    Caption:= fdmShowOptions;
    OnClick:=@OnShowOptionsMenuItemClick;
  end;
  FPopupMenu.Items.Add(FShowOptionsMenuItem);
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

Procedure TDesigner.HintTimer(Sender: TObject);
var
  Rect : TRect;
  AHint : String;
  AControl : TControl;
  Position, ClientPos : TPoint;
  AWinControl: TWinControl;
  AComponent: TComponent;
begin
  FHintTimer.Enabled := False;
  if [dfShowEditorHints,dfShowComponentCaptionHints]*FFlags=[] then exit;

  Position := Mouse.CursorPos;
  AWinControl := FindLCLWindow(Position);
  if not (Assigned(AWinControl)) then Exit;
  if GetDesignerForm(AWinControl)<>Form then exit;

  // first search a non visual component at the position
  ClientPos:=Form.ScreenToClient(Position);
  AComponent:=NonVisualComponentAtPos(ClientPos.X,ClientPos.Y);
  if AComponent=nil then begin
    // then search a control at the position
    ClientPos := AWinControl.ScreenToClient(Position);

    AComponent := AWinControl.ControlAtPos(ClientPos,True);
    if not Assigned(AComponent) then
      AComponent := AWinControl;
  end;
  
  // create a nice hint:
  
  // component name and classname
  if (dfShowComponentCaptionHints in FFlags) then
    AHint := AComponent.Name+' : '+AComponent.ClassName
  else
    AHint:='';
  // component position
  if (dfShowEditorHints in FFlags) then begin
    if AHint<>'' then AHint:=AHint+#10;
    if AComponent is TControl then begin
      AControl:=TControl(AComponent);
      AHint := AHint + 'Left : '+IntToStr(AControl.Left)
                     + '  Top : '+IntToStr(AControl.Top)
                + #10+ 'Width : '+IntToStr(AControl.Width)
                     + '  Height : '+IntToStr(AControl.Height);
    end else begin
      AHint := AHint + 'Left : '+IntToStr(GetComponentLeft(AComponent))
                     + '  Top : '+IntToStr(GetComponentTop(AComponent));
    end;
  end;

  Rect := FHintWindow.CalcHintRect(0,AHint,nil);  //no maxwidth
  Rect.Left := Position.X+10;
  Rect.Top := Position.Y+5;
  Rect.Right := Rect.Left + Rect.Right;
  Rect.Bottom := Rect.Top + Rect.Bottom;

  FHintWindow.ActivateHint(Rect,AHint);
end;

procedure TDesigner.SetSnapToGrid(const AValue: boolean);
begin
  if SnapToGrid=AValue then exit;
  EnvironmentOptions.SnapToGrid:=AValue;
end;

function TDesigner.OnFormActivated: boolean;
begin
  //the form was activated.
  if Assigned(FOnActivated) then FOnActivated(Self);
  Result:=true;
end;

function TDesigner.OnFormCloseQuery: boolean;
begin
  if Assigned(FOnCloseQuery) then FOnCloseQuery(Self);
  Result:=true;
end;

function TDesigner.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result:=TheFormEditor.PropertyEditorHook;
end;

initialization
  ClipBrdSelectionFormat:=0;

end.

