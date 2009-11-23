{
 /***************************************************************************
                                   ActnList.pas
                                   ------------


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ActnList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLStrConsts, LCLType, LCLProc, LCLIntf, ImgList, LCLClasses, LMessages;
  
type

  { TContainedAction }

  TCustomActionList = class;

  TContainedAction = class(TBasicAction)
  private
    FCategory: string;
    FActionList: TCustomActionList;
    function GetIndex: Integer;
    procedure SetCategory(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetActionList(NewActionList: TCustomActionList);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
  public
    destructor Destroy; override;
    function Execute: Boolean; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    function Update: Boolean; override;
    property ActionList: TCustomActionList read FActionList write SetActionList;
    property Index: Integer read GetIndex write SetIndex stored False;
  published
    property Category: string
      read FCategory write SetCategory;
  end;

  TContainedActionClass = class of TContainedAction;

  { TActionListEnumerator }

  TActionListEnumerator = class
  private
    FList: TCustomActionList;
    FPosition: Integer;
    function GetCurrent: TContainedAction;
  public
    constructor Create(AList: TCustomActionList);
    function MoveNext: Boolean;
    property Current: TContainedAction read GetCurrent;
  end;


  { TCustomActionList }

  TActionEvent = procedure (AAction: TBasicAction; var Handled: Boolean) of object;
  TActionListState = (asNormal, asSuspended, asSuspendedEnabled);

  TCustomActionList = class(TLCLComponent)
  private
    FActions: TFPList;// list of TContainedAction
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FOnChange: TNotifyEvent;
    FOnExecute: TActionEvent;
    FOnUpdate: TActionEvent;
    FState: TActionListState;
    function GetAction(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    procedure ImageListChange(Sender: TObject);
    procedure SetAction(Index: Integer; Value: TContainedAction);
    procedure SetState(const Value: TActionListState);
  protected
    procedure AddAction(Action: TContainedAction); virtual;
    procedure RemoveAction(Action: TContainedAction); virtual;
    procedure Change; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetImages(Value: TCustomImageList); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnExecute: TActionEvent read FOnExecute write FOnExecute;
    property OnUpdate: TActionEvent read FOnUpdate write FOnUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ActionByName(const ActionName: string): TContainedAction;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function GetEnumerator: TActionListEnumerator;
    function IndexOfName(const ActionName: string): integer;
    function IsShortCut(var Message: TLMKey): Boolean;
    function UpdateAction(Action: TBasicAction): Boolean; override;

    property Actions[Index: Integer]: TContainedAction read GetAction write SetAction; default;
    property ActionCount: Integer read GetActionCount;
    property Images: TCustomImageList read FImages write SetImages;
    property State: TActionListState read FState write SetState default asNormal;
  end;


  { TActionList }

  TActionList = class(TCustomActionList)
  published
    property Images;
    property State;
    property OnChange;
    property OnExecute;
    property OnUpdate;
  end;


  { TShortCutList
    List of shortcut and texts. The TShortCut values are stored in the Objects. }

  TShortCutList = class(TStringList)
  private
    function GetShortCuts(Index: Integer): TShortCut;
  public
    function Add(const S: String): Integer; override;
    function IndexOfShortCut(const Shortcut: TShortCut): Integer;
    property ShortCuts[Index: Integer]: TShortCut read GetShortCuts;
  end;


  { TCustomAction
    FClients is a list of TActionLink }

  THintEvent = procedure (var HintStr: string; var CanShow: Boolean) of object;

  TCustomAction = class(TContainedAction)
  private
    FAutoCheck: Boolean;
    FCaption: TTranslateString;
    FChecked: Boolean;
    FChecking: Boolean;
    FDisableIfNoHandler: Boolean;
    FEnabled: Boolean;
    FGroupIndex: Integer;
    FHelpContext: THelpContext;
    FHelpKeyword: string;
    FHelpType: THelpType;
    FHint: TTranslateString;
    FImageIndex: TImageIndex;
    FOnHint: THintEvent;
    FSavedEnabledState: Boolean;
    FSecondaryShortCuts: TShortCutList;// nil as default
    FShortCut: TShortCut;
    FVisible: Boolean;
    procedure SetAutoCheck(Value: Boolean);
    procedure SetCaption(const Value: TTranslateString);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetHelpContext(Value: THelpContext); virtual;
    procedure SetHelpKeyword(const Value: string); virtual;
    procedure SetHelpType(Value: THelpType);
    procedure SetHint(const Value: TTranslateString);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetShortCut(Value: TShortCut);
    procedure SetVisible(Value: Boolean);
    function GetSecondaryShortCuts: TShortCutList;
    procedure SetSecondaryShortCuts(const Value: TShortCutList);
    function IsSecondaryShortCutsStored: Boolean;
  protected
    FImage: TObject;
    FMask: TObject;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetName(const Value: TComponentName); override;
    function HandleShortCut: Boolean; virtual;
    property SavedEnabledState: Boolean
      read FSavedEnabledState write FSavedEnabledState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DoHint(var HintStr: string): Boolean; virtual;
    function Execute: Boolean; override;
  public
    property AutoCheck: Boolean
                              read FAutoCheck write  SetAutoCheck default False;
    property Caption: TTranslateString read FCaption write SetCaption;
    property Checked: Boolean read FChecked write SetChecked default False;
    property DisableIfNoHandler: Boolean read FDisableIfNoHandler
                                        write FDisableIfNoHandler default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HelpContext: THelpContext
                               read FHelpContext write SetHelpContext default 0;
    property HelpKeyword: string read FHelpKeyword write SetHelpKeyword;
    property HelpType: THelpType
                             read FHelpType write SetHelpType default htContext;
    property Hint: TTranslateString read FHint write SetHint;
    property ImageIndex: TImageIndex
                                read FImageIndex write SetImageIndex default -1;
    property OnHint: THintEvent read FOnHint write FOnHint;
    property SecondaryShortCuts: TShortCutList read GetSecondaryShortCuts
                  write SetSecondaryShortCuts stored IsSecondaryShortCutsStored;
    property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;


  { TAction }

  TAction = class(TCustomAction)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property DisableIfNoHandler default True;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property OnExecute;
    property OnHint;
    property OnUpdate;
    property SecondaryShortCuts;
    property ShortCut;
    property Visible;
  end;


  { TActionLink }

  TActionLink = class(TBasicActionLink)
  protected
    procedure SetAutoCheck(Value: Boolean); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetGroupIndex(Value: Integer); virtual;
    procedure SetHelpContext(Value: THelpContext); virtual;
    procedure SetHelpKeyword(const Value: string); virtual;
    procedure SetHelpType(Value: THelpType); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetImageIndex(Value: Integer); virtual;
    procedure SetShortCut(Value: TShortCut); virtual;
    procedure SetVisible(Value: Boolean); virtual;
  public
    function IsCaptionLinked: Boolean; virtual;
    function IsCheckedLinked: Boolean; virtual;
    function IsEnabledLinked: Boolean; virtual;
    function IsGroupIndexLinked: Boolean; virtual;
    function IsHelpContextLinked: Boolean; virtual;
    function IsHelpLinked: Boolean; virtual;
    function IsHintLinked: Boolean; virtual;
    function IsImageIndexLinked: Boolean; virtual;
    function IsShortCutLinked: Boolean; virtual;
    function IsVisibleLinked: Boolean; virtual;
  end;

  TActionLinkClass = class of TActionLink;


type
  TEnumActionProc = procedure (const Category: string;
    ActionClass: TBasicActionClass; Info: Pointer) of object;

procedure RegisterActions(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
procedure UnRegisterActions(const AClasses: array of TBasicActionClass);
procedure EnumRegisteredActions(Proc: TEnumActionProc; Info: Pointer);
function CreateAction(TheOwner: TComponent;
  ActionClass: TBasicActionClass): TBasicAction;

const
  RegisterActionsProc: procedure (const CategoryName: string;
                                  const AClasses: array of TBasicActionClass;
                                  Resource: TComponentClass)= nil;
  UnRegisterActionsProc: procedure(const AClasses: array of TBasicActionClass
                                   ) = nil;
  EnumRegisteredActionsProc: procedure(Proc: TEnumActionProc;
                                       Info: Pointer) = nil;
  CreateActionProc: function(TheOwner: TComponent;
                            ActionClass: TBasicActionClass): TBasicAction = nil;

var
  ApplicationActionComponent: TComponent = nil;


procedure Register;

implementation

procedure RegisterActions(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
begin
  if Assigned(RegisterActionsProc) then
    RegisterActionsProc(CategoryName, AClasses, Resource)
  else
    raise Exception.Create(SInvalidActionRegistration);
end;

procedure UnRegisterActions(const AClasses: array of TBasicActionClass);
begin
  if Assigned(UnRegisterActionsProc) then
    UnRegisterActionsProc(AClasses)
  else
    raise Exception.Create(SInvalidActionUnregistration);
end;

procedure EnumRegisteredActions(Proc: TEnumActionProc; Info: Pointer);
begin
  if Assigned(EnumRegisteredActionsProc) then
    EnumRegisteredActionsProc(Proc, Info)
  else
    raise Exception.Create(SInvalidActionEnumeration);
end;

function CreateAction(TheOwner: TComponent;
  ActionClass: TBasicActionClass): TBasicAction;
begin
  if Assigned(CreateActionProc) then
    Result := CreateActionProc(TheOwner, ActionClass)
  else
    raise Exception.Create(SInvalidActionCreation);
end;

{$I containedaction.inc}
{$I customactionlist.inc}
{$I actionlink.inc}
{$I shortcutlist.inc}
{$I customaction.inc}
{$I lclaction.inc}

{ TActionListEnumerator }

function TActionListEnumerator.GetCurrent: TContainedAction;
begin
  Result := FList.Actions[FPosition];
end;

constructor TActionListEnumerator.Create(AList: TCustomActionList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TActionListEnumerator.MoveNext: Boolean;
begin
  inc(FPosition);
  Result := FPosition < FList.ActionCount;
end;

procedure Register;
begin
  RegisterComponents('Standard',[TActionList]);
  RegisterNoIcon([TAction]);
end;

initialization
  ApplicationActionComponent:=nil;

end.
