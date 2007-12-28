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

{off $DEFINE VerboseDesigner}
{off $DEFINE VerboseDesignerDraw}

uses
  Classes, SysUtils, Math, LCLProc, LCLType, LResources, LCLIntf, LMessages,
  Forms, Controls, GraphType, Graphics, Dialogs, ExtCtrls, Menus, ClipBrd,
  PropEdits, ComponentEditors, MenuIntf, IDEImagesIntf,
  LazarusIDEStrConsts, EnvironmentOpts, IDECommands, ComponentReg,
  NonControlDesigner, AlignCompsDlg, SizeCompsDlg, ScaleCompsDlg, TabOrderDlg,
  DesignerProcs, CustomFormEditor,
  ControlSelection, ChangeClassDialog, EditorOptions;

type
  TDesigner = class;

  TOnGetSelectedComponentClass = procedure(Sender: TObject;
    var RegisteredComponent: TRegisteredComponent) of object;
  TOnSetDesigning = procedure(Sender: TObject; Component: TComponent;
    Value: boolean) of object;
  TOnPasteComponent = procedure(Sender: TObject; LookupRoot: TComponent;
    TxtCompStream: TStream; Parent: TWinControl;
    var NewComponent: TComponent) of object;
  TOnPersistentDeleted = procedure(Sender: TObject; APersistent: TPersistent)
    of object;
  TOnGetNonVisualCompIcon = procedure(Sender: TObject;
    AComponent: TComponent; var Icon: TBitmap) of object;
  TOnRenameComponent = procedure(Designer: TDesigner; AComponent: TComponent;
    const NewName: string) of object;
  TOnProcessCommand = procedure(Sender: TObject; Command: word;
    var Handled: boolean) of object;

  TDesignerFlag = (
    dfHasSized,
    dfDuringPaintControl,
    dfShowEditorHints,
    dfShowComponentCaptionHints,
    dfDestroyingForm,
    dfDeleting,
    dfNeedPainting
    );
  TDesignerFlags = set of TDesignerFlag;

  { TDesigner }

  TDesigner = class(TComponentEditorDesigner)
  private
    DesignerPopupMenu: TPopupMenu;
    FDefaultFormBounds: TRect;
    FLastFormBounds: TRect;
    FDefaultFormBoundsValid: boolean;
    FFlags: TDesignerFlags;
    FGridColor: TColor;
    FLookupRoot: TComponent;
    FOnActivated: TNotifyEvent;
    FOnCloseQuery: TNotifyEvent;
    FOnPersistentDeleted: TOnPersistentDeleted;
    FOnGetNonVisualCompIcon: TOnGetNonVisualCompIcon;
    FOnGetSelectedComponentClass: TOnGetSelectedComponentClass;
    FOnModified: TNotifyEvent;
    FOnPasteComponent: TOnPasteComponent;
    FOnProcessCommand: TOnProcessCommand;
    FOnPropertiesChanged: TNotifyEvent;
    FOnRenameComponent: TOnRenameComponent;
    FOnSaveAsXML: TNotifyEvent;
    FOnSetDesigning: TOnSetDesigning;
    FOnShowOptions: TNotifyEvent;
    FOnUnselectComponentClass: TNotifyEvent;
    FOnViewLFM: TNotifyEvent;
    FShiftState: TShiftState;
    FTheFormEditor: TCustomFormEditor;

    //hint stuff
    FHintTimer: TTimer;
    FHintWIndow: THintWindow;

    function GetGridColor: TColor;
    function GetGridSizeX: integer;
    function GetGridSizeY: integer;
    function GetIsControl: Boolean;
    function GetShowBorderSpacing: boolean;
    function GetShowComponentCaptionHints: boolean;
    function GetShowEditorHints: boolean;
    function GetShowGrid: boolean;
    function GetSnapToGrid: boolean;
    Procedure HintTimer(sender : TObject);
    procedure InvalidateWithParent(AComponent: TComponent);
    procedure SetDefaultFormBounds(const AValue: TRect);
    procedure SetGridColor(const AValue: TColor);
    procedure SetGridSizeX(const AValue: integer);
    procedure SetGridSizeY(const AValue: integer);
    procedure SetIsControl(Value: Boolean);
    procedure SetShowBorderSpacing(const AValue: boolean);
    procedure SetShowComponentCaptionHints(const AValue: boolean);
    procedure SetShowEditorHints(const AValue: boolean);
    procedure SetShowGrid(const AValue: boolean);
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
    DeletingPersistent: TList;
    IgnoreDeletingPersistent: TList;

    LastPaintSender: TControl;

    // event handlers for designed components
    function PaintControl(Sender: TControl; TheMessage: TLMPaint):boolean;
    function SizeControl(Sender: TControl; TheMessage: TLMSize):boolean;
    function MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
    procedure MouseDownOnControl(Sender: TControl; var TheMessage : TLMMouse);
    procedure MouseMoveOnControl(Sender: TControl; var TheMessage: TLMMouse);
    procedure MouseUpOnControl(Sender: TControl; var TheMessage:TLMMouse);
    procedure KeyDown(Sender: TControl; var TheMessage:TLMKEY);
    procedure KeyUp(Sender: TControl; var TheMessage:TLMKEY);
    function  HandleSetCursor(var TheMessage: TLMessage): boolean;

    // procedures for working with components and persistents
    function GetDesignControl(AControl: TControl): TControl;
    function DoDeleteSelectedPersistents: boolean;
    procedure DoDeletePersistent(APersistent: TPersistent; FreeIt: boolean);
    procedure MarkPersistentForDeletion(APersistent: TPersistent);
    function PersistentIsMarkedForDeletion(APersistent: TPersistent): boolean;
    function GetSelectedComponentClass: TRegisteredComponent;
    procedure NudgePosition(DiffX, DiffY: Integer);
    procedure NudgeSize(DiffX, DiffY: Integer);
    procedure SelectParentOfSelection;
    function DoCopySelectionToClipboard: boolean;
    function GetPasteParent: TWinControl;
    function DoPasteSelectionFromClipboard(PasteFlags: TComponentPasteSelectionFlags
                                           ): boolean;
    function DoInsertFromStream(s: TStream; PasteParent: TWinControl;
                                PasteFlags: TComponentPasteSelectionFlags): Boolean;
    procedure DoShowTabOrderEditor;
    procedure DoShowChangeClassDialog;
    procedure DoOrderMoveSelectionToFront;
    procedure DoOrderMoveSelectionToBack;
    procedure DoOrderForwardSelectionOne;
    procedure DoOrderBackSelectionOne;

    procedure GiveComponentsNames;
    procedure NotifyPersistentAdded(APersistent: TPersistent);
    function  ComponentClassAtPos(const AClass: TComponentClass;
                                  const APos: TPoint; const UseRootAsDefault,
                                  IgnoreHidden: boolean): TComponent;

    // popup menu
    procedure BuildPopupMenu;
    procedure DesignerPopupMenuPopup(Sender: TObject);
    procedure OnComponentEditorVerbMenuItemClick(Sender: TObject);
    procedure OnAlignPopupMenuClick(Sender: TObject);
    procedure OnMirrorHorizontalPopupMenuClick(Sender: TObject);
    procedure OnMirrorVerticalPopupMenuClick(Sender: TObject);
    procedure OnScalePopupMenuClick(Sender: TObject);
    procedure OnSizePopupMenuClick(Sender: TObject);
    procedure OnTabOrderMenuClick(Sender: TObject);
    procedure OnOrderMoveToFrontMenuClick(Sender: TObject);
    procedure OnOrderMoveToBackMenuClick(Sender: TObject);
    procedure OnOrderForwardOneMenuClick(Sender: TObject);
    procedure OnOrderBackOneMenuClick(Sender: TObject);
    procedure OnCopyMenuClick(Sender: TObject);
    procedure OnCutMenuClick(Sender: TObject);
    procedure OnPasteMenuClick(Sender: TObject);
    procedure OnDeleteSelectionMenuClick(Sender: TObject);
    procedure OnChangeClassMenuClick(Sender: TObject);
    procedure OnChangeParentMenuClick(Sender: TObject);
    procedure OnSnapToGridOptionMenuClick(Sender: TObject);
    procedure OnShowOptionsMenuItemClick(Sender: TObject);
    procedure OnSnapToGuideLinesOptionMenuClick(Sender: TObject);
    procedure OnViewLFMMenuClick(Sender: TObject);
    procedure OnSaveAsXMLMenuClick(Sender: TObject);

    // hook
    function GetPropertyEditorHook: TPropertyEditorHook; override;
    function OnFormActivated: boolean;
    function OnFormCloseQuery: boolean;
  public
    ControlSelection : TControlSelection;
    DDC: TDesignerDeviceContext;

    constructor Create(TheDesignerForm: TCustomForm;
       AControlSelection: TControlSelection);
    procedure FreeDesigner(FreeComponent: boolean);
    destructor Destroy; override;

    procedure Modified; override;
    procedure SelectOnlyThisComponent(AComponent:TComponent); override;
    function CopySelection: boolean; override;
    function CutSelection: boolean; override;
    function CanPaste: Boolean; override;
    function PasteSelection(PasteFlags: TComponentPasteSelectionFlags): boolean; override;
    function DeleteSelection: boolean; override;
    function CopySelectionToStream(AllComponentsStream: TStream): boolean; override;
    function InsertFromStream(s: TStream; Parent: TWinControl;
                              PasteFlags: TComponentPasteSelectionFlags): Boolean; override;
    function InvokeComponentEditor(AComponent: TComponent;
                                   MenuIndex: integer): boolean; override;
    procedure DoProcessCommand(Sender: TObject; var Command: word;
                               var Handled: boolean);

    function NonVisualComponentLeftTop(AComponent: TComponent): TPoint;
    function NonVisualComponentAtPos(x,y: integer): TComponent;
    procedure MoveNonVisualComponentIntoForm(AComponent: TComponent);
    procedure MoveNonVisualComponentsIntoForm;
    function WinControlAtPos(x,y: integer; UseRootAsDefault,
                             IgnoreHidden: boolean): TWinControl;
    function ControlAtPos(x,y: integer; UseRootAsDefault,
                          IgnoreHidden: boolean): TControl;
    function ComponentAtPos(x,y: integer; UseRootAsDefault,
                            IgnoreHidden: boolean): TComponent;
    function GetDesignedComponent(AComponent: TComponent): TComponent;
    function GetComponentEditorForSelection: TBaseComponentEditor;
    function GetShiftState: TShiftState; override;

    procedure AddComponentEditorMenuItems(AComponentEditor: TBaseComponentEditor;
                                          ClearOldOnes: boolean);

    function IsDesignMsg(Sender: TControl;
                                  var TheMessage: TLMessage): Boolean; override;
    function UniqueName(const BaseName: string): string; override;
    Procedure RemovePersistentAndChilds(APersistent: TPersistent);
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure ValidateRename(AComponent: TComponent;
       const CurName, NewName: string); override;
    function CreateUniqueComponentName(const AClassName: string): string; override;

    procedure PaintGrid; override;
    procedure PaintClientGrid(AWinControl: TWinControl;
       aDDC: TDesignerDeviceContext);
    procedure DrawNonVisualComponents(aDDC: TDesignerDeviceContext);
    procedure DrawDesignerItems(OnlyIfNeeded: boolean); override;
    procedure CheckFormBounds;
    procedure DoPaintDesignerItems;
  public
    property Flags: TDesignerFlags read FFlags;
    property GridSizeX: integer read GetGridSizeX write SetGridSizeX;
    property GridSizeY: integer read GetGridSizeY write SetGridSizeY;
    property GridColor: TColor read GetGridColor write SetGridColor;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property LookupRoot: TComponent read FLookupRoot;
    property OnActivated: TNotifyEvent read FOnActivated write FOnActivated;
    property OnCloseQuery: TNotifyEvent read FOnCloseQuery write FOnCloseQuery;
    property OnPersistentDeleted: TOnPersistentDeleted
                             read FOnPersistentDeleted write FOnPersistentDeleted;
    property OnGetNonVisualCompIcon: TOnGetNonVisualCompIcon
                      read FOnGetNonVisualCompIcon write FOnGetNonVisualCompIcon;
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
    property OnRenameComponent: TOnRenameComponent
                               read FOnRenameComponent write FOnRenameComponent;
    property OnSetDesigning: TOnSetDesigning
                                     read FOnSetDesigning write FOnSetDesigning;
    property OnUnselectComponentClass: TNotifyEvent
                                                read FOnUnselectComponentClass
                                                write FOnUnselectComponentClass;
    property OnShowOptions: TNotifyEvent
                                       read FOnShowOptions write FOnShowOptions;
    property OnViewLFM: TNotifyEvent read FOnViewLFM write FOnViewLFM;
    property OnSaveAsXML: TNotifyEvent read FOnSaveAsXML write FOnSaveAsXML;
    property ShowGrid: boolean read GetShowGrid write SetShowGrid;
    property ShowBorderSpacing: boolean read GetShowBorderSpacing write SetShowBorderSpacing;
    property ShowEditorHints: boolean
                               read GetShowEditorHints write SetShowEditorHints;
    property ShowComponentCaptionHints: boolean
                                             read GetShowComponentCaptionHints
                                             write SetShowComponentCaptionHints;
    property SnapToGrid: boolean read GetSnapToGrid write SetSnapToGrid;
    property TheFormEditor: TCustomFormEditor
                                       read FTheFormEditor write FTheFormEditor;
    property DefaultFormBounds: TRect read FDefaultFormBounds write SetDefaultFormBounds;
    property DefaultFormBoundsValid: boolean read FDefaultFormBoundsValid
                                             write FDefaultFormBoundsValid;
  end;

const
  DesignerMenuRootName = 'Designer';
var
  DesignerMenuAlign: TIDEMenuCommand;
  DesignerMenuMirrorHorizontal: TIDEMenuCommand;
  DesignerMenuMirrorVertical: TIDEMenuCommand;
  DesignerMenuScale: TIDEMenuCommand;
  DesignerMenuSize: TIDEMenuCommand;
  
  DesignerMenuTabOrder: TIDEMenuCommand;
    DesignerMenuOrderMoveToFront: TIDEMenuCommand;
    DesignerMenuOrderMoveToBack: TIDEMenuCommand;
    DesignerMenuOrderForwardOne: TIDEMenuCommand;
    DesignerMenuOrderBackOne: TIDEMenuCommand;

  DesignerMenuCut: TIDEMenuCommand;
  DesignerMenuCopy: TIDEMenuCommand;
  DesignerMenuPaste: TIDEMenuCommand;
  DesignerMenuDeleteSelection: TIDEMenuCommand;

  DesignerMenuChangeClass: TIDEMenuCommand;
  DesignerMenuChangeParent: TIDEMenuSection;
  DesignerMenuViewLFM: TIDEMenuCommand;
  DesignerMenuSaveAsXML: TIDEMenuCommand;

  DesignerMenuSnapToGridOption: TIDEMenuCommand;
  DesignerMenuSnapToGuideLinesOption: TIDEMenuCommand;
  DesignerMenuShowOptions: TIDEMenuCommand;


procedure RegisterStandardDesignerMenuItems;


implementation


const
  mk_lbutton =   1;
  mk_rbutton =   2;
  mk_shift   =   4;
  mk_control =   8;
  mk_mbutton = $10;

procedure RegisterStandardDesignerMenuItems;
begin
  DesignerMenuRoot:=RegisterIDEMenuRoot(DesignerMenuRootName);

  // register the dynamic section for the component editor
  DesignerMenuSectionComponentEditor:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                    'Component editor section');

  // register the custom dynamic section
  DesignerMenuSectionCustomDynamic:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                      'Custom dynamic section');

  // register align section
  DesignerMenuSectionAlign:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                               'Align section');
    DesignerMenuAlign:=RegisterIDEMenuCommand(DesignerMenuSectionAlign,
                                         'Align',fdmAlignWord);
    DesignerMenuMirrorHorizontal:=RegisterIDEMenuCommand(DesignerMenuSectionAlign,
                                       'Mirror horizontal',fdmMirrorHorizontal);
    DesignerMenuMirrorVertical:=RegisterIDEMenuCommand(DesignerMenuSectionAlign,
                                       'Mirror vertical',fdmMirrorVertical);
    DesignerMenuScale:=RegisterIDEMenuCommand(DesignerMenuSectionAlign,
                                       'Scale',fdmScaleWord);
    DesignerMenuSize:=RegisterIDEMenuCommand(DesignerMenuSectionAlign,
                                       'Size',fdmSizeWord);

  // register tab and order section
  DesignerMenuSectionOrder:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                               'Order section');
    DesignerMenuTabOrder:=RegisterIDEMenuCommand(DesignerMenuSectionOrder,
                                       'Tab order',fdmTabOrder);
    DesignerMenuSectionZOrder:=RegisterIDESubMenu(DesignerMenuSectionOrder,
                                                  'ZOrder section','');
      DesignerMenuOrderMoveToFront:=RegisterIDEMenuCommand(DesignerMenuSectionZOrder,
                                   'Move to z order front',fdmOrderMoveTofront);
      DesignerMenuOrderMoveToBack:=RegisterIDEMenuCommand(DesignerMenuSectionZOrder,
                                   'Move to z order back',fdmOrderMoveToBack);
      DesignerMenuOrderForwardOne:=RegisterIDEMenuCommand(DesignerMenuSectionZOrder,
                                 'Move z order forward one',fdmOrderForwardOne);
      DesignerMenuOrderBackOne:=RegisterIDEMenuCommand(DesignerMenuSectionZOrder,
                                  'Move z order backwards one',fdmOrderBackOne);

  // register clipboard section
  DesignerMenuSectionClipboard:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                           'Clipboard section');
    DesignerMenuCut:=RegisterIDEMenuCommand(DesignerMenuSectionClipboard,
                                            'Cut',lisMenuCut, nil, nil, nil, 'menu_edit_cut');
    DesignerMenuCopy:=RegisterIDEMenuCommand(DesignerMenuSectionClipboard,
                                            'Copy',lisMenuCopy, nil, nil, nil, 'menu_edit_copy');
    DesignerMenuPaste:=RegisterIDEMenuCommand(DesignerMenuSectionClipboard,
                                            'Paste',lisMenuPaste, nil, nil, nil, 'menu_edit_paste');
    DesignerMenuDeleteSelection:=RegisterIDEMenuCommand(DesignerMenuSectionClipboard,
                                         'Delete selection',fdmDeleteSelection);

  // register miscellaneous section
  DesignerMenuSectionMisc:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                       'Miscellaneous section');
    DesignerMenuChangeClass:=RegisterIDEMenuCommand(DesignerMenuSectionMisc,
                                                 'Change class',lisChangeClass);
    DesignerMenuChangeParent:=RegisterIDEMenuSection(DesignerMenuSectionMisc,
                                                 'Change parent');
    DesignerMenuChangeParent.ChildsAsSubMenu:=true;
    DesignerMenuChangeParent.Caption:=lisChangeParent;
    DesignerMenuViewLFM:=RegisterIDEMenuCommand(DesignerMenuSectionMisc,
                                                'View LFM',lisViewSourceLfm);
    DesignerMenuSaveAsXML:=RegisterIDEMenuCommand(DesignerMenuSectionMisc,
                                                'Save as XML',fdmSaveFormAsXML);

  // register options section
  DesignerMenuSectionOptions:=RegisterIDEMenuSection(DesignerMenuRoot,
                                                             'Options section');
    DesignerMenuSnapToGridOption:=RegisterIDEMenuCommand(DesignerMenuSectionOptions,
                                            'Snap to grid',fdmSnapToGridOption);
    DesignerMenuSnapToGuideLinesOption:=RegisterIDEMenuCommand(DesignerMenuSectionOptions,
                               'Snap to guide lines',fdmSnapToGuideLinesOption);
    DesignerMenuShowOptions:=RegisterIDEMenuCommand(DesignerMenuSectionOptions,
                                                 'Show options',fdmShowOptions, nil, nil, nil, 'menu_environment_options');
end;

constructor TDesigner.Create(TheDesignerForm: TCustomForm;
  AControlSelection: TControlSelection);
begin
  inherited Create;
  FForm := TheDesignerForm;
  if FForm is TNonControlDesignerForm then
    FLookupRoot:=TNonControlDesignerForm(FForm).LookupRoot
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
  DeletingPersistent:=TList.Create;
  IgnoreDeletingPersistent:=TList.Create;
end;

procedure TDesigner.FreeDesigner(FreeComponent: boolean);
var
  i: Integer;
begin
  Include(FFlags,dfDestroyingForm);
  if FLookupRoot is TComponent then begin
    // unselect
    for i:=FLookupRoot.ComponentCount-1 downto 0 do
      TheControlSelection.Remove(LookupRoot.Components[i]);
    TheControlSelection.Remove(LookupRoot);
    if GlobalDesignHook.LookupRoot=FLookupRoot then
      GlobalDesignHook.LookupRoot:=nil;
    if FreeComponent then begin
      // tell hooks about deleting
      for i:=FLookupRoot.ComponentCount-1 downto 0 do
        GlobalDesignHook.PersistentDeleting(FLookupRoot.Components[i]);
      GlobalDesignHook.PersistentDeleting(FLookupRoot);
    end;
    // delete
    if Form<>nil then
      Form.Designer:=nil;
    TheFormEditor.DeleteComponent(FLookupRoot,FreeComponent);
  end;
  Free;
end;

destructor TDesigner.Destroy;
Begin
  FreeAndNil(DesignerPopupMenu);
  FreeAndNil(FHintWIndow);
  FreeAndNil(FHintTimer);
  FreeAndNil(DDC);
  FreeAndNil(DeletingPersistent);
  FreeAndNil(IgnoreDeletingPersistent);
  Inherited Destroy;
end;

procedure TDesigner.NudgePosition(DiffX, DiffY : Integer);
begin
  {$IFDEF VerboseDesigner}
  DebugLn('[TDesigner.NudgePosition]');
  {$ENDIF}
  if (ControlSelection.SelectionForm<>Form)
  or ControlSelection.LookupRootSelected then exit;
  ControlSelection.MoveSelection(DiffX, DiffY);
  Modified;
end;

procedure TDesigner.NudgeSize(DiffX, DiffY: Integer);
begin
  {$IFDEF VerboseDesigner}
  DebugLn('[TDesigner.NudgeSize]');
  {$ENDIF}
  if (ControlSelection.SelectionForm<>Form)
  or ControlSelection.LookupRootSelected then exit;
  ControlSelection.SizeSelection(DiffX, DiffY);
  Modified;
end;

procedure TDesigner.SelectParentOfSelection;
var
  i: Integer;
begin
  if ControlSelection.OnlyInvisiblePersistensSelected then exit;

  if ControlSelection.LookupRootSelected then begin
    SelectOnlyThisComponent(FLookupRoot);
    exit;
  end;
  i:=ControlSelection.Count-1;
  while (i>=0)
  and (   (ControlSelection[i].ParentInSelection)
       or (not ControlSelection[i].IsTControl)
       or (TControl(ControlSelection[i].Persistent).Parent=nil)) do dec(i);
  if i>=0 then
    SelectOnlyThisComponent(TControl(ControlSelection[i].Persistent).Parent);
end;

function TDesigner.CopySelectionToStream(AllComponentsStream: TStream): boolean;

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
        CurParent:=TControl(ControlSelection[i].Persistent).Parent;
        if CurParent=nil then begin
          MessageDlg(lisCanNotCopyTopLevelComponent,
            lisCopyingAWholeFormIsNotImplemented,
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

var
  i: Integer;
  BinCompStream: TMemoryStream;
  TxtCompStream: TMemoryStream;
  CurComponent: TComponent;
  DestroyDriver: Boolean;
  Writer: TWriter;
begin
  Result:=false;
  if (ControlSelection.Count=0) then exit;

  // Because controls will be pasted on a single parent,
  // unselect all controls, that do not have the same parent
  if not UnselectDistinctControls then exit;

  for i:=0 to ControlSelection.Count-1 do begin
    if not ControlSelection[i].IsTComponent then continue;

    BinCompStream:=TMemoryStream.Create;
    TxtCompStream:=TMemoryStream.Create;
    try
      // write component binary stream
      try
        CurComponent:=TComponent(ControlSelection[i].Persistent);

        DestroyDriver:=false;
        Writer := CreateLRSWriter(BinCompStream,DestroyDriver);
        Try
          Writer.Root:=FLookupRoot;
          Writer.WriteComponent(CurComponent);
        Finally
          if DestroyDriver then Writer.Driver.Free;
          Writer.Destroy;
        end;
      except
        on E: Exception do begin
          MessageDlg(lisUnableToStreamSelectedComponents,
            Format(lisThereWasAnErrorDuringWritingTheSelectedComponent, [
              CurComponent.Name, CurComponent.ClassName, #13, E.Message]),
            mtError,[mbCancel],0);
          exit;
        end;
      end;
      BinCompStream.Position:=0;
      // convert binary to text stream
      try
        LRSObjectBinaryToText(BinCompStream,TxtCompStream);
      except
        on E: Exception do begin
          MessageDlg(lisUnableConvertBinaryStreamToText,
            Format(lisThereWasAnErrorWhileConvertingTheBinaryStreamOfThe, [
              CurComponent.Name, CurComponent.ClassName, #13, E.Message]),
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

function TDesigner.InsertFromStream(s: TStream; Parent: TWinControl;
  PasteFlags: TComponentPasteSelectionFlags): Boolean;
begin
  Result:=DoInsertFromStream(s,Parent,PasteFlags);
end;

function TDesigner.DoCopySelectionToClipboard: boolean;
var
  AllComponentsStream: TMemoryStream;
  AllComponentText: string;
begin
  Result := false;
  if ControlSelection.Count = 0 then exit;
  if ControlSelection.OnlyInvisiblePersistensSelected then exit;

  AllComponentsStream:=TMemoryStream.Create;
  try
    // copy components to stream
    if not CopySelectionToStream(AllComponentsStream) then exit;
    SetLength(AllComponentText,AllComponentsStream.Size);
    if AllComponentText<>'' then begin
      AllComponentsStream.Position:=0;
      AllComponentsStream.Read(AllComponentText[1],length(AllComponentText));
    end;

    // copy to clipboard
    try
      ClipBoard.AsText:=AllComponentText;
    except
      on E: Exception do begin
        MessageDlg(lisUnableCopyComponentsToClipboard,
          Format(lisThereWasAnErrorWhileCopyingTheComponentStreamToCli, [#13,
            E.Message]),
          mtError,[mbCancel],0);
        exit;
      end;
    end;
  finally
    AllComponentsStream.Free;
  end;
  Result:=true;
end;

function TDesigner.GetPasteParent: TWinControl;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to ControlSelection.Count-1 do begin
    if (ControlSelection[i].IsTWinControl)
    and (csAcceptsControls in
         TWinControl(ControlSelection[i].Persistent).ControlStyle)
    and (not ControlSelection[i].ParentInSelection) then begin
      Result:=TWinControl(ControlSelection[i].Persistent);
      break;
    end;
  end;
  if (Result=nil)
  and (FLookupRoot is TWinControl) then
    Result:=TWinControl(FLookupRoot);
end;

function TDesigner.DoPasteSelectionFromClipboard(
  PasteFlags: TComponentPasteSelectionFlags): boolean;
var
  AllComponentText: string;
  CurTextCompStream: TMemoryStream;
begin
  Result:=false;
  if not CanPaste then exit;
  // read component stream from clipboard
  AllComponentText:=ClipBoard.AsText;
  if AllComponentText='' then exit;
  CurTextCompStream:=TMemoryStream.Create;
  try
    CurTextCompStream.Write(AllComponentText[1],length(AllComponentText));
    CurTextCompStream.Position:=0;
    if not DoInsertFromStream(CurTextCompStream,nil,PasteFlags) then
      exit;
  finally
    CurTextCompStream.Free;
  end;
  Result:=true;
end;

function TDesigner.DoInsertFromStream(s: TStream;
  PasteParent: TWinControl; PasteFlags: TComponentPasteSelectionFlags): Boolean;
var
  AllComponentText: string;
  StartPos: Integer;
  EndPos: Integer;
  CurTextCompStream: TStream;
  NewSelection: TControlSelection;
  l: Integer;

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
        word(Max(0,Min(P.x,Form.ClientWidth-NonVisualCompWidth)));
      LongRec(AComponent.DesignInfo).Hi:=
        word(Max(0,Min(P.y,Form.ClientHeight-NonVisualCompWidth)));
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
      // create component and add to LookupRoot
      FOnPasteComponent(Self,FLookupRoot,TextCompStream,
                        PasteParent,NewComponent);
      if NewComponent=nil then exit;
      // add new component to new selection
      NewSelection.Add(NewComponent);
      // set new nice bounds
      if cpsfFindUniquePositions in PasteFlags then
        FindUniquePosition(NewComponent);
      // finish adding component
      NotifyPersistentAdded(NewComponent);
      Modified;
    end;

    Result:=true;
  end;

begin
  Result:=false;
  //debugln('TDesigner.DoInsertFromStream A');
  if (cpsfReplace in PasteFlags) and (not DeleteSelection) then exit;

  //debugln('TDesigner.DoInsertFromStream B s.Size=',dbgs(s.Size),' S.Position=',dbgs(S.Position));
  if PasteParent=nil then PasteParent:=GetPasteParent;
  NewSelection:=TControlSelection.Create;
  try

    // read component stream from clipboard
    if (s.Size<=S.Position) then begin
      debugln('TDesigner.DoInsertFromStream Stream Empty s.Size=',dbgs(s.Size),' S.Position=',dbgs(S.Position));
      exit;
    end;
    l:=s.Size-s.Position;
    SetLength(AllComponentText,l);
    s.Read(AllComponentText[1],length(AllComponentText));

    StartPos:=1;
    EndPos:=StartPos;
    // read till 'end'
    while EndPos<=length(AllComponentText) do begin
      //debugln('TDesigner.DoInsertFromStream C');
      if (AllComponentText[EndPos] in ['e','E'])
      and (EndPos>1)
      and (AllComponentText[EndPos-1] in [#10,#13])
      and (CompareText(copy(AllComponentText,EndPos,3),'END')=0)
      and ((EndPos+3>length(AllComponentText))
           or (AllComponentText[EndPos+3] in [#10,#13]))
      then begin
        inc(EndPos,4);
        while (EndPos<=length(AllComponentText))
        and (AllComponentText[EndPos] in [' ',#10,#13])
        do
          inc(EndPos);
        // extract text for the current component
        {$IFDEF VerboseDesigner}
        DebugLn('TDesigner.DoInsertFromStream==============================');
        DebugLn(copy(AllComponentText,StartPos,EndPos-StartPos));
        DebugLn('TDesigner.DoInsertFromStream==============================');
        {$ENDIF}

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
    if NewSelection.Count>0 then
      ControlSelection.Assign(NewSelection);
    NewSelection.Free;
  end;
  Result:=true;
end;

procedure TDesigner.DoShowTabOrderEditor;
begin
  if ShowTabOrderDialog(FLookupRoot)=mrOk then
    Modified;
end;

procedure TDesigner.DoShowChangeClassDialog;
begin
  if (ControlSelection.Count=1) and (not ControlSelection.LookupRootSelected)
  then
    ShowChangeClassDialog(Self,ControlSelection[0].Persistent);
end;

procedure TDesigner.DoOrderMoveSelectionToFront;
begin
  if ControlSelection.Count <> 1 then Exit;
  if not ControlSelection[0].IsTControl then Exit;

  TControl(ControlSelection[0].Persistent).BringToFront;
  Modified;
end;

procedure TDesigner.DoOrderMoveSelectionToBack;
begin
  if ControlSelection.Count <> 1 then Exit;
  if not ControlSelection[0].IsTControl then Exit;

  TControl(ControlSelection[0].Persistent).SendToBack;
  Modified;
end;

procedure TDesigner.DoOrderForwardSelectionOne;
var
  Control: TControl;
  Parent: TWinControl;
begin
  if ControlSelection.Count <> 1 then Exit;
  if not ControlSelection[0].IsTControl then Exit;

  Control := TControl(ControlSelection[0].Persistent);
  Parent := Control.Parent;
  if Parent = nil then Exit;

  Parent.SetControlIndex(Control, Parent.GetControlIndex(Control) + 1);

  Modified;
end;

procedure TDesigner.DoOrderBackSelectionOne;
var
  Control: TControl;
  Parent: TWinControl;
begin
  if ControlSelection.Count <> 1 then Exit;
  if not ControlSelection[0].IsTControl then Exit;

  Control := TControl(ControlSelection[0].Persistent);
  Parent := Control.Parent;
  if Parent = nil then Exit;

  Parent.SetControlIndex(Control, Parent.GetControlIndex(Control) - 1);

  Modified;
end;

procedure TDesigner.GiveComponentsNames;
var
  i: Integer;
  CurComponent: TComponent;
begin
  if LookupRoot=nil then exit;
  for i:=0 to LookupRoot.ComponentCount-1 do begin
    CurComponent:=LookupRoot.Components[i];
    if CurComponent.Name='' then
      CurComponent.Name:=UniqueName(CurComponent.ClassName);
  end;
end;

procedure TDesigner.NotifyPersistentAdded(APersistent: TPersistent);
begin
  try
    GiveComponentsNames;
    GlobalDesignHook.PersistentAdded(APersistent,false);
  except
    on E: Exception do
      MessageDlg('Error:',E.Message,mtError,[mbOk],0);
  end;
end;

procedure TDesigner.SelectOnlyThisComponent(AComponent:TComponent);
begin
  ControlSelection.AssignPersistent(AComponent);
end;

function TDesigner.CopySelection: boolean;
begin
  Result := DoCopySelectionToClipboard;
end;

function TDesigner.CutSelection: boolean;
begin
  Result := DoCopySelectionToClipboard and DoDeleteSelectedPersistents;
end;

function TDesigner.CanPaste: Boolean;
begin
  Result:=(Form<>nil)
      and (FLookupRoot<>nil)
      and (not (csDestroying in FLookupRoot.ComponentState));
end;

function TDesigner.PasteSelection(
  PasteFlags: TComponentPasteSelectionFlags): boolean;
begin
  Result:=DoPasteSelectionFromClipboard(PasteFlags);
end;

function TDesigner.DeleteSelection: boolean;
begin
  Result:=DoDeleteSelectedPersistents;
end;

function TDesigner.InvokeComponentEditor(AComponent: TComponent;
  MenuIndex: integer): boolean;
var
  CompEditor: TBaseComponentEditor;
begin
  Result:=false;
  DebugLn('TDesigner.InvokeComponentEditor A ',AComponent.Name,':',AComponent.ClassName);
  CompEditor:=TheFormEditor.GetComponentEditor(AComponent);
  if CompEditor=nil then begin
    DebugLn('TDesigner.InvokeComponentEditor',
      ' WARNING: no component editor found for ',
        AComponent.Name,':',AComponent.ClassName);
    exit;
  end;
  DebugLn('TDesigner.InvokeComponentEditor B ',CompEditor.ClassName);
  try
    CompEditor.Edit;
    Result:=true;
  except
    on E: Exception do begin
      DebugLn('TDesigner.InvokeComponentEditor ERROR: ',E.Message);
      MessageDlg(Format(lisErrorIn, [CompEditor.ClassName]),
        Format(lisTheComponentEditorOfClassHasCreatedTheError, ['"',
          CompEditor.ClassName, '"', #13, '"', E.Message, '"']),
        mtError,[mbOk],0);
    end;
  end;
end;

procedure TDesigner.DoProcessCommand(Sender: TObject; var Command: word;
  var Handled: boolean);
begin
  if Assigned(OnProcessCommand) and (Command <> ecNone)
  then begin
    OnProcessCommand(Self,Command,Handled);
    Handled := Handled or (Command = ecNone);
  end;

  if Handled then Exit;

  case Command of
    ecDesignerSelectParent : SelectParentOfSelection;
    ecDesignerCopy         : CopySelection;
    ecDesignerCut          : CutSelection;
    ecDesignerPaste        : PasteSelection([cpsfFindUniquePositions]);
    ecDesignerMoveToFront  : DoOrderMoveSelectionToFront;
    ecDesignerMoveToBack   : DoOrderMoveSelectionToBack;
    ecDesignerForwardOne   : DoOrderForwardSelectionOne;
    ecDesignerBackOne      : DoOrderBackSelectionOne;
  else
    Exit;
  end;
  
  Handled := True;
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
  DebugLn('TDesigner.INVALIDATEWITHPARENT ',AComponent.Name,':',AComponent.ClassName);
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

procedure TDesigner.SetDefaultFormBounds(const AValue: TRect);
begin
  FDefaultFormBounds:=AValue;
end;

procedure TDesigner.SetGridColor(const AValue: TColor);
begin
  if GridColor=AValue then exit;
  EnvironmentOptions.GridColor:=AValue;
  Form.Invalidate;
end;

procedure TDesigner.SetShowBorderSpacing(const AValue: boolean);
begin
  if ShowBorderSpacing=AValue then exit;
  EnvironmentOptions.ShowBorderSpacing:=AValue;
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

  {$IFDEF VerboseDsgnPaintMsg}
  writeln('***  TDesigner.PaintControl A ',Sender.Name,':',Sender.ClassName,
          ' DC=',DbgS(TheMessage.DC));
  {$ENDIF}
  // Set flag
  OldDuringPaintControl:=dfDuringPaintControl in FFlags;
  Include(FFlags,dfDuringPaintControl);

  // send the Paint message to the control, so that it paints itself
  //writeln('TDesigner.PaintControl B ',Sender.Name);
  Sender.Dispatch(TheMessage);
  //writeln('TDesigner.PaintControl C ',Sender.Name,' DC=',DbgS(TheMessage.DC));

  // paint the Designer stuff
  if TheMessage.DC <> 0 then begin
    Include(FFlags,dfNeedPainting);

    InternalPaint:=(TheMessage.Msg=LM_INTERNALPAINT);
    DDC.SetDC(Form, TheMessage.DC);
    {$IFDEF VerboseDesignerDraw}
    writeln('TDesigner.PaintControl D ',Sender.Name,':',Sender.ClassName,
      ' DC=',DbgS(DDC.DC,8),
     {' FormOrigin=',DDC.FormOrigin.X,',',DDC.FormOrigin.Y,}
      ' DCOrigin=',DDC.DCOrigin.X,',',DDC.DCOrigin.Y,
      ' FormClientOrigin=',DDC.FormClientOrigin.X,',',DDC.FormClientOrigin.Y,
      ' Internal=',InternalPaint
      );
    {$ENDIF}
    if LastPaintSender=Sender then begin
      //writeln('NOTE: TDesigner.PaintControl E control painted twice: ',
      //  Sender.Name,':',Sender.ClassName,' DC=',DbgS(TheMessage.DC));
      //RaiseException('');
    end;
    LastPaintSender:=Sender;

    // client grid
    if (not InternalPaint) and (Sender is TWinControl)
    and (csAcceptsControls in Sender.ControlStyle) then begin
      PaintClientGrid(TWinControl(Sender),DDC);
    end;
    
    if not EnvironmentOptions.DesignerPaintLazy then
      DoPaintDesignerItems;

    // clean up
    DDC.Clear;
  end;
  //writeln('TDesigner.PaintControl END ',Sender.Name);

  if not OldDuringPaintControl then
    Exclude(FFlags,dfDuringPaintControl);
end;

function TDesigner.HandleSetCursor(var TheMessage: TLMessage): boolean;
begin
  Result := Lo(TheMessage.LParam) = HTCLIENT;
  if Result then
  begin
    Form.SetTempCursor(LastFormCursor);
    TheMessage.Result := 1;
  end;
end;

function TDesigner.GetDesignControl(AControl: TControl): TControl;
// checks if AControl is designable.
// if not check Owner.
// AControl can be a TNonControlDesignerForm
var
  OwnerControl: TControl;
  AComponent: TComponent;
begin
  Result:=AControl;
  if (Result=nil) or (Result=LookupRoot) or (Result.Owner=LookupRoot) then exit;
  if Result=Form then exit;
  if (Result.Owner is TControl) then begin
    OwnerControl:=TControl(Result.Owner);
    if csOwnedChildsSelectable in OwnerControl.ControlStyle then
      exit;
    Result:=GetDesignControl(OwnerControl);
  end else begin
    AComponent:=GetDesignedComponent(AControl);
    if AComponent is TControl then
      Result:=TControl(AComponent)
    else
      Result:=nil;
  end;
end;

function TDesigner.SizeControl(Sender: TControl; TheMessage: TLMSize):boolean;
begin
  Result:=true;
  Sender.Dispatch(TheMessage);
  if ControlSelection.SelectionForm=Form then begin
    ControlSelection.CheckForLCLChanges(true);
  end;
end;

function TDesigner.MoveControl(Sender: TControl; TheMessage: TLMMove):boolean;
begin
  Result:=true;
  Sender.Dispatch(TheMessage);
  //debugln('***  TDesigner.MoveControl A ',Sender.Name,':',Sender.ClassName,' ',ControlSelection.SelectionForm=Form,' ',not ControlSelection.IsResizing,' ',ControlSelection.IsSelected(Sender));
  if ControlSelection.SelectionForm=Form then begin
    ControlSelection.CheckForLCLChanges(true);
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
  DesignSender: TControl;
begin
  FHintTimer.Enabled := False;
  Exclude(FFLags,dfHasSized);
  SetCaptureControl(nil);
  DesignSender:=GetDesignControl(Sender);
  ParentForm:=GetParentForm(DesignSender);
  //DebugLn(['TDesigner.MouseDownOnControl DesignSender=',dbgsName(DesignSender),' ParentForm=',dbgsName(ParentForm)]);
  if (ParentForm=nil) then exit;
  
  MouseDownPos:=GetFormRelativeMousePosition(Form);
  LastMouseMovePos:=MouseDownPos;

  MouseDownComponent:=nil;
  MouseDownSender:=nil;

  NonVisualComp:=NonVisualComponentAtPos(MouseDownPos.X,MouseDownPos.Y);
  if NonVisualComp<>nil then begin
    MouseDownComponent:=NonVisualComp;
    MoveNonVisualComponentIntoForm(NonVisualComp);
  end;

  if (MouseDownComponent=nil) then begin
    MouseDownComponent:=ComponentAtPos(MouseDownPos.X,MouseDownPos.Y,true,true);
    if (MouseDownComponent=nil) then exit;
  end;
  MouseDownSender:=DesignSender;

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
  DebugLn('************************************************************');
  DbgOut('MouseDownOnControl');
  DbgOut(' Sender=',dbgsName(Sender),' DesignSender=',dbgsName(DesignSender));
  //write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
  //write(' Mouse=',MouseDownPos.X,',',MouseDownPos.Y);
  //writeln('');

  if (TheMessage.Keys and MK_Shift) = MK_Shift then
    DbgOut(' Shift down')
  else
    DbgOut(' No Shift down');

  if (TheMessage.Keys and MK_Control) = MK_Control then
    DebugLn(', CTRL down')
  else
    DebugLn(', No CTRL down');
  {$ENDIF}

  SelectedCompClass:=GetSelectedComponentClass;


  if (TheMessage.Keys and MK_LButton) > 0 then begin
    // left button
    // -> check if a grabber was activated
    ControlSelection.ActiveGrabber:=
      ControlSelection.GrabberAtPos(MouseDownPos.X,MouseDownPos.Y);
    SetCaptureControl(ParentForm);

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
                MessageDlg(lisInvalidMultiselection,
                  fdInvalidMultiselectionText,
                  mtInformation,[mbOk],0);
              end else begin
                ControlSelection.Add(MouseDownComponent);
              end;
            end else begin
              // remove from multiselection
              ControlSelection.Delete(CompIndex);
            end;
          end else begin
            // no shift key (single selection or keeping multiselection)

            if (CompIndex<0) then begin
              // select only this component
              ControlSelection.AssignPersistent(MouseDownComponent);
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
  DebugLn('[TDesigner.MouseDownOnControl] END');
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
  ParentClientOrigin, PopupPos: TPoint;
  SelectedCompClass: TRegisteredComponent;
  SelectionChanged, NewRubberbandSelection: boolean;
  DesignSender: TControl;

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
    NewComponent: TComponent;
  begin
    if MouseDownComponent=nil then exit;

    // add a new component
    ControlSelection.RubberbandActive:=false;
    ControlSelection.Clear;

    // find a parent for the new component
    if FLookupRoot is TCustomForm then begin
      if MouseDownComponent is TWinControl then
        NewParentControl:=TWinControl(MouseDownComponent)
      else
        NewParentControl:=WinControlAtPos(MouseDownPos.X,MouseUpPos.X,true,true);
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

    if not PropertyEditorHook.BeforeAddPersistent(Self,
                                     SelectedCompClass.ComponentClass,NewParent)
    then begin
      DebugLn('TDesigner.AddComponent ',
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
       ParentCI,SelectedCompClass.ComponentClass,'',
       NewLeft,NewTop,NewWidth,NewHeight));
    if NewCI=nil then exit;
    NewComponent:=NewCI.Component;

    // set initial properties
    if NewComponent is TControl then
      TControl(NewComponent).Visible:=true;
    if Assigned(FOnSetDesigning) then
      FOnSetDesigning(Self,NewComponent,True);

    // tell IDE about the new component (e.g. add it to the source)
    NotifyPersistentAdded(NewComponent);

    // creation completed
    // -> select new component
    SelectOnlyThisComponent(NewComponent);
    if not (ssShift in Shift) then
      if Assigned(FOnUnselectComponentClass) then
        // this resets the component palette to the selection tool
        FOnUnselectComponentClass(Self);

    {$IFDEF VerboseDesigner}
    DebugLn('NEW COMPONENT ADDED: Form.ComponentCount=',DbgS(Form.ComponentCount),
       '  NewComponent.Owner.Name=',NewComponent.Owner.Name);
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
      MessageDlg(lisInvalidMultiselection,
        fdInvalidMultiselectionText,
        mtInformation,[mbOk],0);
      exit;
    end;

    ControlSelection.BeginUpdate;
    // check if start new selection or add/remove:
    NewRubberbandSelection:= (not (ssShift in Shift))
      or (ControlSelection.SelectionForm<>Form);
    // update non visual components
    MoveNonVisualComponentsIntoForm;
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
    DebugLn('RubberbandSelect ',DbgS(ControlSelection.Grabbers[0]));
    {$ENDIF}
    Form.Invalidate;
  end;

  procedure PointSelect;
  begin
    if (not (ssShift in Shift)) then begin
      // select only the mouse down component
      ControlSelection.AssignPersistent(MouseDownComponent);
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
  DesignSender:=GetDesignControl(Sender);
  SenderParentForm:=GetParentForm(DesignSender);
  //DebugLn(['TDesigner.MouseUpOnControl DesignSender=',dbgsName(DesignSender),' SenderParentForm=',dbgsName(SenderParentForm),' ',TheMessage.XPos,',',TheMessage.YPos]);
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
  DebugLn('************************************************************');
  DbgOut('MouseUpOnControl');
  DbgOut(' Sender=',dbgsName(Sender),' DesignSender=',dbgsName(DesignSender));
  //write(' Msg=',TheMessage.Pos.X,',',TheMessage.Pos.Y);
  DebugLn('');
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
    if EnvironmentOptions.RightClickSelects
    and (not ControlSelection.IsSelected(MouseDownComponent))
    and (Shift=[]) then
      PointSelect;
    PopupMenuComponentEditor:=GetComponentEditorForSelection;
    BuildPopupMenu;
    PopupPos := Form.ClientToScreen(MouseUpPos);
    DesignerPopupMenu.Popup(PopupPos.X,PopupPos.Y);
  end;

  DisableRubberBand;

  LastMouseMovePos.X:=-1;
  Exclude(FFlags,dfHasSized);

  MouseDownComponent:=nil;
  MouseDownSender:=nil;
  {$IFDEF VerboseDesigner}
  DebugLn('[TDesigner.MouseLeftUpOnControl] END');
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
  DesignSender: TControl;
begin
  if [dfShowEditorHints,dfShowComponentCaptionHints]*FFlags<>[] then begin
    FHintTimer.Enabled := False;

    // hide hint
    FHintTimer.Enabled :=
            (TheMessage.keys or (MK_LButton and MK_RButton and MK_MButton) = 0);
    if FHintWindow.Visible then
      FHintWindow.Visible := False;
  end;

  DesignSender:=GetDesignControl(Sender);
  //DebugLn('TDesigner.MouseMoveOnControl Sender=',dbgsName(Sender),' ',dbgsName(DesignSender));
  SenderParentForm:=GetParentForm(DesignSender);
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
      Form.SetTempCursor(ACursor);
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
          //debugln('TDesigner.MouseMoveOnControl Move MouseDownComponent=',dbgsName(MouseDownComponent),' OldMouseMovePos=',dbgs(OldMouseMovePos),' MouseMovePos',dbgs(LastMouseMovePos),' MouseDownPos=',dbgs(MouseDownPos));
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
  DebugLn(['TDesigner.KEYDOWN ',TheMessage.CharCode,' ',TheMessage.KeyData]);
  {$ENDIF}

  Shift := KeyDataToShiftState(TheMessage.KeyData);

  Handled:=false;
  Command:=FTheFormEditor.TranslateKeyToDesignerCommand(
                                                     TheMessage.CharCode,Shift);
  //DebugLn('TDesigner.KEYDOWN Command=',dbgs(Command));
  DoProcessCommand(Self,Command,Handled);
  //DebugLn(['TDesigner.KeyDown Command=',Command,' Handled=',Handled,' TheMessage.CharCode=',TheMessage.CharCode]);

  if not Handled then begin
    Handled:=true;
    case TheMessage.CharCode of
    VK_DELETE:
      if not ControlSelection.OnlyInvisiblePersistensSelected then
        DoDeleteSelectedPersistents;

    VK_UP:
      if (ssCtrl in Shift) then
        NudgePosition(0,-1)
      else if (ssShift in Shift) then
        NudgeSize(0,-1);

    VK_DOWN:
      if (ssCtrl in Shift) then
        NudgePosition(0,1)
      else if (ssShift in Shift) then
        NudgeSize(0,1);

    VK_RIGHT:
      if (ssCtrl in Shift) then
        NudgePosition(1,0)
      else if (ssShift in Shift) then
        NudgeSize(1,0);

    VK_LEFT:
      if (ssCtrl in Shift) then
        NudgePosition(-1,0)
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


{------------------------------------K E Y U P --------------------------------}
Procedure TDesigner.KeyUp(Sender : TControl; var TheMessage:TLMKEY);
Begin
  {$IFDEF VerboseDesigner}
  //Writeln('TDesigner.KEYUP ',TheMessage.CharCode,' ',TheMessage.KeyData);
  {$ENDIF}
end;

function TDesigner.DoDeleteSelectedPersistents: boolean;
var
  i: integer;
  APersistent: TPersistent;
  AncestorRoot: TComponent;
begin
  Result:=true;
  if (ControlSelection.Count=0) or (ControlSelection.SelectionForm<>Form) then
    exit;
  Result:=false;
  // check if a component is the lookup root (can not be deleted)
  if (ControlSelection.LookupRootSelected) then begin
    if ControlSelection.Count>1 then
      MessageDlg(lisInvalidDelete,
       lisTheRootComponentCanNotBeDeleted, mtInformation,
       [mbCancel],0);
    exit;
  end;
  // check if a selected component is inherited (can not be deleted)
  for i:=0 to ControlSelection.Count-1 do begin
    if not ControlSelection[i].IsTComponent then continue;
    AncestorRoot:=TheFormEditor.GetAncestorLookupRoot(
                                    TComponent(ControlSelection[i].Persistent));
    if AncestorRoot<>nil then begin
      MessageDlg(lisInvalidDelete,
       'The component '+dbgsName(ControlSelection[i].Persistent)
       +' is inherited from '+dbgsName(AncestorRoot)+'.'#13
       +'To delete an inherited component open the ancestor and delete it there.',
       mtInformation, [mbCancel],0);
      exit;
    end;
  end;
  // mark selected components for deletion
  for i:=0 to ControlSelection.Count-1 do
    MarkPersistentForDeletion(ControlSelection[i].Persistent);
  // clear selection by selecting the LookupRoot
  SelectOnlyThisComponent(FLookupRoot);
  // delete marked components
  Include(FFlags,dfDeleting);
  try
    if DeletingPersistent.Count=0 then exit;
    while DeletingPersistent.Count>0 do begin
      APersistent:=TPersistent(DeletingPersistent[DeletingPersistent.Count-1]);
      //writeln('TDesigner.DoDeleteSelectedComponents A ',AComponent.Name,':',AComponent.ClassName,' ',DbgS(AComponent));
      RemovePersistentAndChilds(APersistent);
      //writeln('TDesigner.DoDeleteSelectedComponents B ',DeletingPersistent.IndexOf(AComponent));
    end;
    IgnoreDeletingPersistent.Clear;
  finally
    Exclude(FFlags,dfDeleting);
    Modified;
  end;
  Result:=true;
end;

procedure TDesigner.DoDeletePersistent(APersistent: TPersistent;
  FreeIt: boolean);
var
  Hook: TPropertyEditorHook;
begin
  if APersistent=nil then exit;
  //writeln('TDesigner.DoDeleteComponent A ',AComponent.Name,':',AComponent.ClassName,' ',DbgS(AComponent));
  PopupMenuComponentEditor:=nil;
  // unselect component
  ControlSelection.Remove(APersistent);
  if (APersistent is TComponent)
  and (TheFormEditor.FindComponent(TComponent(APersistent))=nil) then begin
    // this component is currently in the process of deletion or the component
    // was not properly created
    // -> do not call handlers and simply get rid of the rubbish
    //writeln('TDesigner.DoDeleteComponent UNKNOWN ',AComponent.Name,':',AComponent.ClassName,' ',DbgS(AComponent));
    if FreeIt then
      APersistent.Free;
    // unmark component
    DeletingPersistent.Remove(APersistent);
    IgnoreDeletingPersistent.Remove(APersistent);
    exit;
  end;
  // call component deleting handlers
  Hook:=GetPropertyEditorHook;
  if Hook<>nil then
    Hook.PersistentDeleting(APersistent);
  // delete component
  if APersistent is TComponent then
    TheFormEditor.DeleteComponent(TComponent(APersistent),FreeIt)
  else if FreeIt then
    APersistent.Free;
  // unmark component
  DeletingPersistent.Remove(APersistent);
  IgnoreDeletingPersistent.Remove(APersistent);
  // call ComponentDeleted handler
  if Assigned(FOnPersistentDeleted) then
    FOnPersistentDeleted(Self,APersistent);
end;

procedure TDesigner.MarkPersistentForDeletion(APersistent: TPersistent);
begin
  if (not PersistentIsMarkedForDeletion(APersistent)) then
    DeletingPersistent.Add(APersistent);
end;

function TDesigner.PersistentIsMarkedForDeletion(APersistent: TPersistent
  ): boolean;
begin
  Result:=(DeletingPersistent.IndexOf(APersistent)>=0);
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
      CN_KEYDOWN:     KeyDown(Sender,TLMKey(TheMessage));
      CN_KEYUP:       KeyUP(Sender,TLMKey(TheMessage));
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
      LM_SETCURSOR:   Result:=HandleSetCursor(TheMessage);
    else
      Result:=false;
    end;
  end else begin
    if (TheMessage.Msg=LM_PAINT)
    or (TheMessage.Msg=LM_INTERNALPAINT)
    or (TheMessage.Msg=CN_KEYDOWN)
    or (TheMessage.Msg=CN_KEYUP)
    or (TheMessage.Msg=LM_LBUTTONDOWN)
    or (TheMessage.Msg=LM_RBUTTONDOWN)
    or (TheMessage.Msg=LM_LBUTTONDBLCLK)
    or (TheMessage.Msg=LM_LBUTTONUP)
    or (TheMessage.Msg=LM_RBUTTONUP)
    or (TheMessage.Msg=LM_MOUSEMOVE)
    or (TheMessage.Msg=LM_SIZE)
    or (TheMessage.Msg=LM_MOVE)
    or (TheMessage.Msg=LM_ACTIVATE)
    or (TheMessage.Msg=LM_CLOSEQUERY)
    or (TheMessage.Msg=LM_SETCURSOR)
    then
      DebugLn(['TDesigner.IsDesignMsg NOT DESIGNING? ',dbgsName(Sender),' TheMessage.Msg=',GetMessageName(TheMessage.Msg)]);
  end;
end;

function TDesigner.UniqueName(const BaseName: string): string;
begin
  Result:=TheFormEditor.CreateUniqueComponentName(BaseName,LookupRoot);
end;

procedure TDesigner.Modified;
Begin
  ControlSelection.SaveBounds;
  if Assigned(FOnModified) then FOnModified(Self);
end;

Procedure TDesigner.RemovePersistentAndChilds(APersistent: TPersistent);
var
  i: integer;
  AWinControl: TWinControl;
  ChildControl: TControl;
Begin
  if APersistent=nil then exit;
  {$IFDEF VerboseDesigner}
  DebugLn('[TDesigner.RemovePersistentAndChilds] ',dbgsName(APersistent),' ',DbgS(APersistent));
  {$ENDIF}
  if (APersistent=FLookupRoot) or (APersistent=Form)
  or (IgnoreDeletingPersistent.IndexOf(APersistent)>=0)
  then exit;
  // remove all child controls owned by the LookupRoot
  if (APersistent is TWinControl) then begin
    AWinControl:=TWinControl(APersistent);
    i:=AWinControl.ControlCount-1;
    while (i>=0) do begin
      ChildControl:=AWinControl.Controls[i];
      if (ChildControl.Owner=FLookupRoot)
      and (IgnoreDeletingPersistent.IndexOf(ChildControl)<0) then begin
        //Writeln('[TDesigner.RemoveComponentAndChilds] B ',AComponent.Name,':',AComponent.ClassName,' ',DbgS(AComponent),' Child=',ChildControl.Name,':',ChildControl.ClassName,' i=',i);
        RemovePersistentAndChilds(ChildControl);
        // the component list of the form has changed
        // -> restart the search
        i:=AWinControl.ControlCount-1;
      end else
        dec(i);
    end;
  end;
  // remove component
  {$IFDEF VerboseDesigner}
  DebugLn('[TDesigner.RemovePersistentAndChilds] C ',dbgsName(APersistent));
  {$ENDIF}
  DoDeletePersistent(APersistent,true);
end;

procedure TDesigner.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  if Operation = opInsert then begin
    {$IFDEF VerboseDesigner}
    DebugLn('opInsert ',AComponent.Name,':',AComponent.ClassName,' ',DbgS(AComponent));
    {$ENDIF}
    if dfDeleting in FFlags then begin
      // a component has auto created a new component during deletion
      // -> ignore the new component
      IgnoreDeletingPersistent.Add(AComponent);
    end;
  end
  else
  if Operation = opRemove then begin
    {$IFDEF VerboseDesigner}
    DebugLn('[TDesigner.Notification] opRemove ',
            AComponent.Name,':',AComponent.ClassName);
    {$ENDIF}
    DoDeletePersistent(AComponent,false);
  end;
end;

procedure TDesigner.PaintGrid;
begin
  // This is normally done in PaintControls
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
  CurControl: TControl;
begin
  if (AWinControl=nil)
  or (not (csAcceptsControls in AWinControl.ControlStyle))
  or ((not ShowGrid) and (not ShowBorderSpacing)) then exit;

  aDDC.Save;
  try
    // exclude all child control areas
    Count:=AWinControl.ControlCount;
    for i := 0 to Count - 1 do begin
      with AWinControl.Controls[I] do begin
        if (Visible or ((csDesigning in ComponentState)
          and not (csNoDesignVisible in ControlStyle)))
        then begin
          Clip := ExcludeClipRect(aDDC.DC, Left, Top, Left + Width, Top + Height);
          if Clip = NullRegion then exit;
        end;
      end;
    end;

    // paint points
    if ShowGrid then begin
      StepX:=GridSizeX;
      StepY:=GridSizeY;
      MaxX:=AWinControl.ClientWidth;
      MaxY:=AWinControl.ClientHeight;
      x := 0;
      while x <= MaxX do begin
        y := 0;
        while y <= MaxY do begin
          aDDC.Canvas.Pixels[x, y] := GridColor;
          Inc(y, StepY);
        end;
        Inc(x, StepX);
      end;
    end;
    
    if ShowBorderSpacing then begin
      aDDC.Canvas.Brush.Color:=clRed;
      for i:=0 to Count-1 do begin
        CurControl:=AWinControl.Controls[i];
        aDDC.Canvas.FrameRect(
          CurControl.Left-CurControl.BorderSpacing.GetSpace(akLeft),
          CurControl.Top-CurControl.BorderSpacing.GetSpace(akTop),
          CurControl.Left+CurControl.Width+CurControl.BorderSpacing.GetSpace(akRight)-1,
          CurControl.Top+CurControl.Height+CurControl.BorderSpacing.GetSpace(akBottom)-1
          );
      end;
    end;
  finally
    aDDC.Restore;
  end;
end;

procedure TDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  // check if component is initialized
  if (CurName='') or (NewName='')
  or ((AComponent<>nil) and (csDestroying in AComponent.ComponentState)) then
    exit;

  // check if component is the LookupRoot
  if AComponent=nil then AComponent:=FLookupRoot;

  // consistency check
  if CurName<>AComponent.Name then
    DebugLn('WARNING: TDesigner.ValidateRename: OldComponentName="',CurName,'" <> AComponent=',dbgsName(AComponent));
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
  //DebugLn(['TDesigner.OnComponentEditorVerbMenuItemClick Sender=',dbgsName(Sender)]);
  if Sender is TMenuItem then
    AMenuItem:=TMenuItem(Sender)
  else if Sender is TIDEMenuCommand then
    AMenuItem:=TIDEMenuCommand(Sender).MenuItem
  else
    exit;
  Verb:=PopupMenuComponentEditor.GetVerbCount-1;
  VerbCaption:=AMenuItem.Caption;
  while (Verb>=0) and (VerbCaption<>PopupMenuComponentEditor.GetVerb(Verb)) do
    dec(Verb);
  if Verb<0 then exit;
  try
    PopupMenuComponentEditor.ExecuteVerb(Verb);
  except
    on E: Exception do begin
      DebugLn('TDesigner.OnComponentEditorVerbMenuItemClick ERROR: ',E.Message);
      MessageDlg(Format(lisErrorIn, [PopupMenuComponentEditor.ClassName]),
        Format(lisTheComponentEditorOfClassInvokedWithVerbHasCreated, ['"',
          PopupMenuComponentEditor.ClassName, '"', #13, IntToStr(Verb), '"',
          VerbCaption, '"', #13, #13, '"', E.Message, '"']),
        mtError,[mbOk],0);
    end;
  end;
end;

procedure TDesigner.OnDeleteSelectionMenuClick(Sender: TObject);
begin
  DoDeleteSelectedPersistents;
end;

procedure TDesigner.OnChangeClassMenuClick(Sender: TObject);
begin
  DoShowChangeClassDialog;
end;

procedure TDesigner.OnChangeParentMenuClick(Sender: TObject);
var
  Item: TIDEMenuCommand;
  NewParentName: String;
  i: Integer;
  CurControl: TControl;
  NewParent: TWinControl;
begin
  if not (Sender is TIDEMenuCommand) then exit;
  Item:=TIDEMenuCommand(Sender);
  NewParentName:=Item.Caption;
  NewParent:=TWinControl(LookupRoot.FindComponent(NewParentName));
  if (NewParent=nil) or (not (NewParent is TWinControl)) then exit;
  Form.DisableAlign;
  try
    i:=ControlSelection.Count-1;
    while (i>=0) do begin
      if i<ControlSelection.Count then begin
        if ControlSelection[i].IsTControl then begin
          CurControl:=TControl(ControlSelection[i].Persistent);
          CurControl.Parent:=NewParent;
        end;
      end;
      dec(i);
    end;
  finally
    if Form<>nil then
      Form.EnableAlign;
  end;
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

procedure TDesigner.OnViewLFMMenuClick(Sender: TObject);
begin
  if Assigned(OnViewLFM) then OnViewLFM(Self);
end;

procedure TDesigner.OnSaveAsXMLMenuClick(Sender: TObject);
begin
  if Assigned(OnSaveAsXML) then OnSaveAsXML(Self);
end;

procedure TDesigner.OnCopyMenuClick(Sender: TObject);
begin
  CopySelection;
end;

procedure TDesigner.OnCutMenuClick(Sender: TObject);
begin
  CutSelection;
end;

procedure TDesigner.OnPasteMenuClick(Sender: TObject);
begin
  PasteSelection([cpsfFindUniquePositions]);
end;

procedure TDesigner.OnTabOrderMenuClick(Sender: TObject);
begin
  DoShowTabOrderEditor;
end;

function TDesigner.GetGridColor: TColor;
begin
  Result:=EnvironmentOptions.GridColor;
end;

function TDesigner.GetShowBorderSpacing: boolean;
begin
  Result:=EnvironmentOptions.ShowBorderSpacing;
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
  i, j, ItemLeft, ItemTop, ItemRight, ItemBottom: integer;
  Diff, ItemLeftTop: TPoint;
  IconRect: TRect;
  Icon: TBitmap;
  AComponent: TComponent;
begin
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if ComponentIsNonVisual(AComponent) then begin
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
        Pen.Width:=1;
        Pen.Color:=clWhite;
        for j:=0 to NonVisualCompBorder-1 do begin
          MoveTo(ItemLeft+j,ItemBottom-j);
          LineTo(ItemLeft+j,ItemTop+j);
          LineTo(ItemRight-j,ItemTop+j);
        end;
        Pen.Color:=clBlack;
        for j:=0 to NonVisualCompBorder-1 do begin
          MoveTo(ItemLeft+j,ItemBottom-j);
          LineTo(ItemRight-j,ItemBottom-j);
          MoveTo(ItemRight-j,ItemTop+j);
          LineTo(ItemRight-j,ItemBottom-j+1);
        end;
        IconRect:=Rect(ItemLeft+NonVisualCompBorder,ItemTop+NonVisualCompBorder,
             ItemRight-NonVisualCompBorder,ItemBottom-NonVisualCompBorder);
        Brush.Color:=clBtnFace;
        //writeln('TDesigner.DrawNonVisualComponents A ',IconRect.Left,',',IconRect.Top,',',IconRect.Right,',',IconRect.Bottom);
        FillRect(Rect(IconRect.Left,IconRect.Top,
           IconRect.Right+1,IconRect.Bottom+1));
      end;
      if Assigned(FOnGetNonVisualCompIcon) then begin
        Icon:=nil;
        FOnGetNonVisualCompIcon(Self,AComponent,Icon);
        if Icon<>nil then begin
          inc(IconRect.Left,(NonVisualCompIconWidth-Icon.Width) div 2);
          inc(IconRect.Top,(NonVisualCompIconWidth-Icon.Height) div 2);
          IconRect.Right:=IconRect.Left+Icon.Width;
          IconRect.Bottom:=IconRect.Top+Icon.Height;
          StretchMaskBlt(aDDC.Canvas.Handle, IconRect.Left, IconRect.Top,
            IconRect.Right-IconRect.Left, IconRect.Bottom-IconRect.Top, 
            Icon.Canvas.Handle, 0, 0, Icon.Width, Icon.Height,
            Icon.MaskHandle, 0, 0, SRCCOPY);
        end;
      end;
      if (ControlSelection.Count>1)
      and (ControlSelection.IsSelected(AComponent)) then
        ControlSelection.DrawMarkerAt(aDDC,
          ItemLeft,ItemTop,NonVisualCompWidth,NonVisualCompWidth);
    end;
  end;
end;

procedure TDesigner.DrawDesignerItems(OnlyIfNeeded: boolean);
var
  DesignerDC: HDC;
begin
  if OnlyIfNeeded and (not (dfNeedPainting in FFlags)) then exit;
  Exclude(FFlags,dfNeedPainting);

  if (Form=nil) or (not Form.HandleAllocated) then exit;

  //writeln('TDesigner.DrawDesignerItems B painting');
  DesignerDC:=GetDesignerDC(Form.Handle);
  DDC.SetDC(Form,DesignerDC);
  DoPaintDesignerItems;
  DDC.Clear;
  ReleaseDesignerDC(Form.Handle,DesignerDC);
end;

procedure TDesigner.CheckFormBounds;
// check if the Form was moved or resized
// Note: During form loading the window manager can resize and position
//       the Form. Such initial changes are ignored, by waiting and comparing
//       not before the IDE becomes idle. When the IDE becomes the first time
//       idle, the form bounds are stored and used as default.
//       After that any change of the Form Bounds is treated as a user move
//       and thus calls Modified.
var
  NewFormBounds: TRect;
begin
  NewFormBounds:=Form.BoundsRect;
  if FDefaultFormBoundsValid then begin
    if (not CompareRect(@NewFormBounds,@FLastFormBounds))
    and (not CompareRect(@NewFormBounds,@FDefaultFormBounds)) then begin
      //debugln('TDesigner.CheckFormBounds');
      Modified;
      if ControlSelection.SelectionForm=Form then begin
        ControlSelection.CheckForLCLChanges(true);
      end;
    end;
  end else begin
    FDefaultFormBoundsValid:=true;
    FDefaultFormBounds:=NewFormBounds;
  end;
  FLastFormBounds:=NewFormBounds;
end;

procedure TDesigner.DoPaintDesignerItems;
begin
  // marker (multi selection markers)
  if (ControlSelection.SelectionForm=Form)
  and (ControlSelection.Count>1) then begin
    ControlSelection.DrawMarkers(DDC);
  end;
  // non visual component icons
  DrawNonVisualComponents(DDC);
  // guidelines and grabbers
  if (ControlSelection.SelectionForm=Form) then begin
    if EnvironmentOptions.ShowGuideLines then
      ControlSelection.DrawGuideLines(DDC);
    ControlSelection.DrawGrabbers(DDC);
  end;
  // rubberband
  if ControlSelection.RubberBandActive
  and ((ControlSelection.SelectionForm=Form)
  or (ControlSelection.SelectionForm=nil)) then begin
    ControlSelection.DrawRubberBand(DDC);
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
  or (ControlSelection.SelectionForm<>Form)
  or (not ControlSelection[0].IsTComponent) then exit;
  Result:=
   TheFormEditor.GetComponentEditor(TComponent(ControlSelection[0].Persistent));
end;

procedure TDesigner.AddComponentEditorMenuItems(
  AComponentEditor: TBaseComponentEditor; ClearOldOnes: boolean);
var
  VerbCount, i: integer;
  NewMenuCmd: TIDEMenuCommand;
begin
  if (AComponentEditor=nil) or (DesignerMenuSectionComponentEditor=nil) then
    exit;
  if ClearOldOnes then
    DesignerMenuSectionComponentEditor.Clear;
  VerbCount:=AComponentEditor.GetVerbCount;
  for i:=0 to VerbCount-1 do begin
    NewMenuCmd:=RegisterIDEMenuCommand(DesignerMenuSectionComponentEditor,
      'ComponentEditorVerMenuItem'+IntToStr(i),
      AComponentEditor.GetVerb(i),
      @OnComponentEditorVerbMenuItemClick);
    if NewMenuCmd.MenuItem<>nil then
      AComponentEditor.PrepareItem(i,NewMenuCmd.MenuItem);
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

procedure TDesigner.MoveNonVisualComponentIntoForm(AComponent: TComponent);
var
  p: TPoint;
begin
  p:=NonVisualComponentLeftTop(AComponent);
  LongRec(AComponent.DesignInfo).Lo:=p.x;
  LongRec(AComponent.DesignInfo).Hi:=p.y;
end;

procedure TDesigner.MoveNonVisualComponentsIntoForm;
var
  i: Integer;
  AComponent: TComponent;
begin
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if (not (AComponent is TControl))
    and (not ComponentIsInvisible(AComponent)) then begin
      MoveNonVisualComponentIntoForm(AComponent);
    end;
  end;
end;

function TDesigner.ComponentClassAtPos(const AClass: TComponentClass;
  const APos: TPoint; const UseRootAsDefault, IgnoreHidden: boolean): TComponent;

  function DoComponent: TComponent;
  var
    i: integer;
    Bounds: TRect;
  begin
    for i := FLookupRoot.ComponentCount - 1 downto 0 do
    begin
      Result := FLookupRoot.Components[i]; // bit tricky, but we set it to nil anyhow
      if not Result.InheritsFrom(AClass) then Continue;
      if Result is TControl then begin
        if IgnoreHidden and (not ControlIsInDesignerVisible(TControl(Result)))
        then
          Continue;
        if csNoDesignSelectable in TControl(Result).ControlStyle then
          continue;
      end;
      Bounds := GetParentFormRelativeBounds(Result);
      if PtInRect(Bounds, APos) then Exit;
    end;
    Result := nil;
  end;

  function DoWinControl: TComponent;
  var
    i: integer;
    Bounds: TRect;
    Control: TControl;
    WinControl: TWinControl;
  begin
    Result := nil;
    if not (FLookupRoot is TWinControl) then exit;
    WinControl := TWinControl(FLookupRoot);
    i := WinControl.ControlCount;
    while i > 0 do
    begin
      Dec(i);
      Control := WinControl.Controls[i];
      if IgnoreHidden and (csNoDesignVisible in Control.ControlStyle) then
        Continue;
      if csNoDesignSelectable in Control.ControlStyle then continue;
      Bounds := GetParentFormRelativeBounds(Control);
      if not PtInRect(Bounds, APos) then Continue;

      if Control.InheritsFrom(AClass)
      then Result := Control; // at least this is a match, now look if a child matches

      if Control is TWinControl
      then begin
        Wincontrol := TWinControl(Control);
        i := WinControl.ControlCount;
        Continue; // next loop
      end;
      
      // Control has no children and a result found, no need to look further
      if Result <> nil then Exit;
    end;
  end;

begin
  // If LookupRoot is TWincontol, use the control list. It is ordered by zorder
  // We cannot use the components in that case since they are at place order

  if FLookupRoot is TWinControl
  then Result := DoWinControl
  else Result := DoComponent;
  
  if (Result = nil) and UseRootAsDefault and (FLookupRoot.InheritsFrom(AClass))
  then
    Result := LookupRoot;
end;

function TDesigner.WinControlAtPos(x, y: integer; UseRootAsDefault,
  IgnoreHidden: boolean): TWinControl;
begin
  Result := TWinControl(ComponentClassAtPos(TWinControl, Point(x,y),
                                            UseRootAsDefault, IgnoreHidden));
end;

function TDesigner.ControlAtPos(x, y: integer; UseRootAsDefault,
  IgnoreHidden: boolean): TControl;
begin
  Result := TControl(ComponentClassAtPos(TControl, Point(x,y), UseRootAsDefault,
                                IgnoreHidden));
end;

function TDesigner.ComponentAtPos(x, y: integer; UseRootAsDefault,
  IgnoreHidden: boolean): TComponent;
begin
  Result := ComponentClassAtPos(TComponent, Point(x,y), UseRootAsDefault,
                                IgnoreHidden);
end;

procedure TDesigner.BuildPopupMenu;
begin
  if DesignerPopupMenu<>nil then exit;
  
  DesignerPopupMenu:=TPopupMenu.Create(nil);
  with DesignerPopupMenu do 
  begin
    Name := 'DesignerPopupmenu';
    OnPopup := @DesignerPopupMenuPopup;
    Images := IDEImages.Images_16;
  end;

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  {$IFDEF VerboseMenuIntf}
  DesignerPopupMenu.Items.WriteDebugReport('TSourceNotebook.BuildPopupMenu ');
  DesignerMenuRoot.ConsistencyCheck;
  {$ENDIF}
  DesignerMenuRoot.MenuItem:=DesignerPopupMenu.Items;

  DesignerMenuAlign.OnClick := @OnAlignPopupMenuClick;
  DesignerMenuMirrorHorizontal.OnClick := @OnMirrorHorizontalPopupMenuClick;
  DesignerMenuMirrorVertical.OnClick := @OnMirrorVerticalPopupMenuClick;
  DesignerMenuScale.OnClick := @OnScalePopupMenuClick;
  DesignerMenuSize.OnClick := @OnSizePopupMenuClick;

  DesignerMenuTabOrder.OnClick:=@OnTabOrderMenuClick;
    DesignerMenuOrderMoveToFront.OnClick := @OnOrderMoveToFrontMenuClick;
    DesignerMenuOrderMoveToFront.MenuItem.ShortCut :=
                     EditorOpts.KeyMap.CommandToShortCut(ecDesignerMoveToFront);
    DesignerMenuOrderMoveToBack.OnClick := @OnOrderMoveToBackMenuClick;
    DesignerMenuOrderMoveToBack.MenuItem.ShortCut :=
                     EditorOpts.KeyMap.CommandToShortCut(ecDesignerMoveToBack);
    DesignerMenuOrderForwardOne.OnClick := @OnOrderForwardOneMenuClick;
    DesignerMenuOrderForwardOne.MenuItem.ShortCut :=
                     EditorOpts.KeyMap.CommandToShortCut(ecDesignerForwardOne);
    DesignerMenuOrderBackOne.OnClick := @OnOrderBackOneMenuClick;
    DesignerMenuOrderBackOne.MenuItem.ShortCut :=
                     EditorOpts.KeyMap.CommandToShortCut(ecDesignerBackOne);

  DesignerMenuCut.OnClick:=@OnCutMenuClick;
  DesignerMenuCopy.OnClick:=@OnCopyMenuClick;
  DesignerMenuPaste.OnClick:=@OnPasteMenuClick;
  DesignerMenuDeleteSelection.OnClick:=@OnDeleteSelectionMenuClick;

  DesignerMenuChangeClass.OnClick:=@OnChangeClassMenuClick;
  DesignerMenuViewLFM.OnClick:=@OnViewLFMMenuClick;
  DesignerMenuSaveAsXML.OnClick:=@OnSaveAsXMLMenuClick;

  DesignerMenuSnapToGridOption.OnClick:=@OnSnapToGridOptionMenuClick;
  DesignerMenuSnapToGridOption.ShowAlwaysCheckable:=true;
  DesignerMenuSnapToGuideLinesOption.OnClick:=@OnSnapToGuideLinesOptionMenuClick;
  DesignerMenuSnapToGuideLinesOption.ShowAlwaysCheckable:=true;
  DesignerMenuShowOptions.OnClick:=@OnShowOptionsMenuItemClick;
end;

procedure TDesigner.DesignerPopupMenuPopup(Sender: TObject);
var
  ControlSelIsNotEmpty,
  LookupRootIsSelected,
  OnlyNonVisualsAreSelected,
  CompsAreSelected: boolean;
  MultiCompsAreSelected: boolean;
  OneControlSelected: Boolean;
  SelectionVisible: Boolean;
  
  procedure UpdateChangeParentMenu;
  var
    Candidates: TFPList;
    i: Integer;
    Candidate: TWinControl;
    j: Integer;
    CurSelected: TSelectedControl;
    Item: TIDEMenuItem;
  begin
    Candidates:=TFPList.Create;
    if ControlSelIsNotEmpty
    and (not LookupRootIsSelected)
    and (LookupRoot is TWinControl) then begin
      for i:=0 to LookupRoot.ComponentCount-1 do begin
        if not (LookupRoot.Components[i] is TWinControl) then continue;

        Candidate:=TWinControl(LookupRoot.Components[i]);
        if not (csAcceptsControls in Candidate.ControlStyle) then continue;
        j:=ControlSelection.Count-1;
        while j>=0 do begin
          CurSelected:=ControlSelection[j];
          if CurSelected.IsTControl then begin
            if CurSelected.Persistent=Candidate then break;
            if CurSelected.IsTWinControl
            and TWinControl(CurSelected.Persistent).IsParentOf(Candidate)
            then
              break;
          end;
          dec(j);
        end;
        if j<0 then
          Candidates.Add(Candidate);
      end;
      
    end;
    
    DesignerMenuChangeParent.Visible:=Candidates.Count>0;
    DesignerMenuChangeParent.Clear;
    for i:=0 to Candidates.Count-1 do begin
      Item:=TIDEMenuCommand.Create(DesignerMenuChangeParent.Name+'_'+IntToStr(i));
      DesignerMenuChangeParent.AddLast(Item);
      Item.Caption:=TWinControl(Candidates[i]).Name;
      Item.OnClick:=@OnChangeParentMenuClick;
    end;
    Candidates.Free;
  end;
  
begin
  ControlSelIsNotEmpty:=(ControlSelection.Count>0)
                        and (ControlSelection.SelectionForm=Form);
  LookupRootIsSelected:=ControlSelection.LookupRootSelected;
  OnlyNonVisualsAreSelected := ControlSelection.OnlyNonVisualPersistentsSelected;
  SelectionVisible:=not ControlSelection.OnlyInvisiblePersistensSelected;
  CompsAreSelected:=ControlSelIsNotEmpty and SelectionVisible
                    and not LookupRootIsSelected;
  OneControlSelected := ControlSelIsNotEmpty and ControlSelection[0].IsTControl;
  MultiCompsAreSelected := CompsAreSelected and (ControlSelection.Count>1);

  AddComponentEditorMenuItems(PopupMenuComponentEditor,true);

  DesignerMenuAlign.Enabled := MultiCompsAreSelected;
  DesignerMenuMirrorHorizontal.Enabled := MultiCompsAreSelected;
  DesignerMenuMirrorVertical.Enabled := MultiCompsAreSelected;
  DesignerMenuScale.Enabled := CompsAreSelected and not OnlyNonVisualsAreSelected;
  DesignerMenuSize.Enabled := CompsAreSelected and not OnlyNonVisualsAreSelected;

  DesignerMenuTabOrder.Enabled:= (FLookupRoot is TWinControl)
              and (TWinControl(FLookupRoot).ControlCount>0);
  DesignerMenuSectionZOrder.Enabled := CompsAreSelected;
    DesignerMenuOrderMoveToFront.Enabled := OneControlSelected;
    DesignerMenuOrderMoveToBack.Enabled := OneControlSelected;
    DesignerMenuOrderForwardOne.Enabled := OneControlSelected;
    DesignerMenuOrderBackOne.Enabled := OneControlSelected;

  DesignerMenuCut.Enabled:= CompsAreSelected;
  DesignerMenuCopy.Enabled:= CompsAreSelected;
  DesignerMenuPaste.Enabled:= CanPaste;
  DesignerMenuDeleteSelection.Enabled:= CompsAreSelected;
  
  DesignerMenuChangeClass.Enabled:= CompsAreSelected and (ControlSelection.Count=1);
  UpdateChangeParentMenu;

  DesignerMenuSnapToGridOption.Checked:=EnvironmentOptions.SnapToGrid;
  DesignerMenuSnapToGuideLinesOption.Checked:=EnvironmentOptions.SnapToGuideLines;
end;

procedure TDesigner.OnAlignPopupMenuClick(Sender: TObject);
var
  HorizAlignment, VertAlignment: TComponentAlignment;
  HorizAlignID, VertAlignID: integer;
begin
  if ShowAlignComponentsDialog(HorizAlignID,VertAlignID)=mrOk then begin
    case HorizAlignID of
     0: HorizAlignment:=csaNone;
     1: HorizAlignment:=csaSides1;
     2: HorizAlignment:=csaCenters;
     3: HorizAlignment:=csaSides2;
     4: HorizAlignment:=csaCenterInWindow;
     5: HorizAlignment:=csaSpaceEqually;
     6: HorizAlignment:=csaSide1SpaceEqually;
     7: HorizAlignment:=csaSide2SpaceEqually;
    end;
    case VertAlignID of
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
var
  ScaleInPercent: integer;
begin
  if ShowScaleComponentsDialog(ScaleInPercent)=mrOk then begin
    ControlSelection.ScaleComponents(ScaleInPercent);
  end;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnSizePopupMenuClick(Sender: TObject);
var
  HorizSizing, VertSizing: TComponentSizing;
  HorizSizingID, VertSizingID: integer;
  AWidth, AHeight: integer;
begin
  if ShowSizeComponentsDialog(HorizSizingID,AWidth,VertSizingID,AHeight)=mrOk
  then begin
    case HorizSizingID of
     0: HorizSizing:=cssNone;
     1: HorizSizing:=cssShrinkToSmallest;
     2: HorizSizing:=cssGrowToLargest;
     3: HorizSizing:=cssFixed;
    end;
    case VertSizingID of
     0: VertSizing:=cssNone;
     1: VertSizing:=cssShrinkToSmallest;
     2: VertSizing:=cssGrowToLargest;
     3: VertSizing:=cssFixed;
    end;
    ControlSelection.SizeComponents(HorizSizing,AWidth,VertSizing,AHeight);
  end;
  ControlSelection.SaveBounds;
end;

procedure TDesigner.OnOrderMoveToFrontMenuClick(Sender: TObject);
begin
  DoOrderMoveSelectionToFront;
end;

procedure TDesigner.OnOrderMoveToBackMenuClick(Sender: TObject);
begin
  DoOrderMoveSelectionToBack;
end;

procedure TDesigner.OnOrderForwardOneMenuClick(Sender: TObject);
begin
  DoOrderForwardSelectionOne;
end;

procedure TDesigner.OnOrderBackOneMenuClick(Sender: TObject);
begin
  DoOrderBackSelectionOne;
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
    AComponent := ComponentAtPos(ClientPos.X,ClientPos.Y,true,true);
    if not Assigned(AComponent) then
      AComponent := AWinControl;
  end;
  AComponent:=GetDesignedComponent(AComponent);
  if AComponent=nil then exit;

  // create a nice hint:

  // component name and classname
  if (dfShowComponentCaptionHints in FFlags) then
    AHint := AComponent.Name+': '+AComponent.ClassName
  else
    AHint:='';
  // component position
  if (dfShowEditorHints in FFlags) then begin
    if AHint<>'' then AHint:=AHint+#10;
    if AComponent is TControl then begin
      AControl:=TControl(AComponent);
      AHint := AHint + 'Left: '+IntToStr(AControl.Left)
                     + '  Top: '+IntToStr(AControl.Top)
                + #10+ 'Width: '+IntToStr(AControl.Width)
                     + '  Height: '+IntToStr(AControl.Height);
    end else begin
      AHint := AHint + 'Left: '+IntToStr(GetComponentLeft(AComponent))
                     + '  Top: '+IntToStr(GetComponentTop(AComponent));
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

end.

