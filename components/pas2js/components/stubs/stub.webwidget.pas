{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set stubs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit stub.webwidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stub.web;

Const

  SElementData = 'wwElement';
  STopElementData = SElementData+'Top';
  SContentElementData = SElementData+'Content';
  SElementClass = 'wwClass';

  sEventAbort = 'abort';
  SEventAnimationCancel = 'animationcancel';
  SEventAnimationEnd = 'animationend';
  SEventAnimationIteration = 'animationiteration';
  SEventAnimationStart = 'animationstart';
  sEventAuxClick = 'auxclick';
  sEventBlur = 'blur';
  SEventCancel = 'cancel';
  SEventCanPlay = 'canplay';
  SEventCanPlayThrough = 'canplaythrough';
  SEventChange = 'change';
  sEventClick = 'click';
  sEventCompositionEnd = 'compositionend';
  sEventCompositionStart = 'compositionstart';
  sEventCompositionUpdate = 'compositionupdate';
  sEventContextMenu = 'contextmenu';
  sEventCopy = 'copy';
  sEventCut = 'cut';
  sEventCueChange = 'cuechange';
  sEventDblClick = 'dblclick';
  sEventDurationChange = 'durationchange';
  sEventEnded  = 'ended';
  sEventError  = 'error';
  sEventFocus = 'focus';
  sEventFocusIn  = 'focusin';
  sEventFocusOut  = 'focusout';
  SEventGotPointerCapture = 'gotpointercapture';
  SEventInput = 'input';
  SEventInvalid = 'invalid';
  sEventKeyDown = 'keydown';
  sEventKeyPress = 'keypress';
  sEventKeyUp = 'keyup';
  sEventLoad = 'load';
  sEventLoadedData = 'loadeddata';
  sEventLoadedMetaData = 'loadedmetadata';
  sEventLoadend = 'loadend';
  sEventLoadStart = 'loadstart';
  SEventLostPointerCapture = 'lostpointercapture';
  sEventMouseDown = 'mousedown';
  sEventMouseEnter = 'mouseenter';
  sEventMouseLeave = 'mouseleave';
  sEventMouseMove = 'mousemove';
  sEventMouseOut = 'mouseout';
  sEventMouseUp = 'mouseup';
  sEventOverFlow = 'overflow';
  sEventPaste = 'paste';
  sEventPause = 'pause';
  sEventPlay = 'play';
  SEventPointerCancel = 'pointercancel';
  SEventPointerDown = 'pointerdown';
  SEventPointerEnter = 'pointerenter';
  SEventPointerLeave = 'pointerleave';
  SEventPointerMove = 'pointermove';
  SEventPointerOut = 'pointerout';
  SEventPointerOver = 'pointerover';
  SEventPointerUp = 'pointerup';
  sEventReset = 'reset';
  sEventResize = 'resize';
  sEventScroll = 'scroll';
  sEventSelect = 'select';
  sEventSubmit = 'submit';
  sEventTouchStart = 'touchstart';
  SEventTransitionCancel = 'transitioncancel';
  SEventTransitionEnd = 'transitionend';
  SEventTransitionRun = 'transitionrun';
  SEventTransitionStart = 'transitionstart';
  SEventWheel = 'wheel';


Type
  EWidgets = Class(Exception);
  TCustomWebWidget = Class;

  THTMLNotifyEvent = Procedure (Sender : TObject; Event : TJSEvent) of object;

  TEventDispatch = Record
    MsgStr : String;
    HTMLEvent : TJSEvent;
    EventHandler : THTMLNotifyEvent;
  end;

  { TStyleItem }
  TStylePriority = (spNone,spImportant);

  TStyleItem = Class(TCollectionItem)
  private
    FPriority: TStylePriority;
    FName: String;
    FValue: String;
    FImported : Boolean;
    procedure SetPriority(AValue: TStylePriority);
    procedure SetValue(AValue: String);
    procedure SetName(AValue: String);
  Protected
    procedure MarkDirty;
    Function GetDisplayName: string; override;
  Public
    Property Imported : Boolean read FImported;
    Procedure Assign(Source : TPersistent) ; override;
  Published
    Property Name : String Read FName Write SetName;
    Property Value : String Read FValue Write SetValue;
    Property Priority : TStylePriority Read FPriority Write SetPriority;
  end;

  { TWebWidgetStyles }

  TWebWidgetStyles = Class(TOwnedCollection)
  private
    function Add(const aName: String; const aValue: String): TStyleItem;
    Function GetStyleItem(aIndex : Integer): TStyleItem;
    procedure SetStyleItem(aIndex : Integer; AValue: TStyleItem);
  Protected
    function EnsureStyle(const aName: String; const aValue: String = ''): TStyleItem;
    Function IndexOfStyle(const aName : String) : Integer;
    Function FindStyle(aName : String) : TStyleItem;
  Public
    Property Styles[aIndex : Integer] : TStyleItem Read GetStyleItem Write SetStyleItem; default;
  end;


  TStyleRefresh = (srOnElementID, // Only refresh styles if ElementID was set and we bind to existing element.
                   srAlways,      // Always refresh styles
                   srNever);      // Never refresh
  TStyleRefreshes = Set of TStyleRefresh;

  { TReferenceItem }
  TJSHTMLElementArray = Array of TJSHTMLElement;

  TReferenceItem = Class(TCollectionItem)
  private
    FName: String;
    FSelector: String;
  Protected
    Function GetDisplayName: string; override;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Selector : String Read FSelector Write FSelector;
    Property Name : String Read FName Write FName;
  end;

  { TWebWidgetReferences }

  TWebWidgetReferences = Class(TOwnedCollection)
  Private
    function GetReferenceItem(aIndex : Integer): TReferenceItem;
    procedure SetReferenceItem(aIndex : Integer; AValue: TReferenceItem);
  Protected
  Public
    Property Items[aIndex : Integer] : TReferenceItem Read GetReferenceItem Write SetReferenceItem;
  end;

  { TCustomWebWidget }

  TCustomWebWidget = Class(TComponent)
  Private
    const MaxEvents = 66;
  private
    FElementID,
    FParentID,
    FClasses : String;
    FAfterRenderHTML: TNotifyEvent;
    FAfterUnRenderHTML: TNotifyEvent;
    FBeforeRenderHTML: TNotifyEvent;
    FBeforeUnRenderHTML: TNotifyEvent;
    FStyleRefresh: TStyleRefresh;
    FStyles: TWebWidgetStyles;
    FReferences : TWebWidgetReferences;
    FMyEvents : Array[0..MaxEvents] of THTMLNotifyEvent;
    FVisible : Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetClasses: String;
    function GetHaveReferences: Boolean;
    function GetHTMLEvent(AIndex: Integer): THTMLNotifyEvent;
    function GetParentID: String;
    function GetElementID: String;
    function GetReferences: TWebWidgetReferences;
    procedure SetClasses(AValue: String);
    procedure SetElementID(AValue: String);
    procedure SetHTMLEvent(AIndex: Integer; AValue: THTMLNotifyEvent);
    procedure SetParentID(AValue: String);
    procedure SetReferences(AValue: TWebWidgetReferences);
    procedure SetStyles(AValue: TWebWidgetStyles);
    // This protected section is not meant to be made public
  Protected
    function CreateStyles: TWebWidgetStyles; virtual;
    Function CreateReferences: TWebWidgetReferences; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  // This protected section can be published in descendents
  Protected
    // Parent or Element ID: Will be used when determining the HTML element when rendering.
    // Only one of the Parent or Element ID can be set.
    Property ParentID : String Read GetParentID Write SetParentID;
    Property ElementID : String Read GetElementID Write SetElementID;
    // When reading, returns the actual classes if rendered.
    // When rendering, these classes are added to any existing ones if the element exists.
    Property Classes : String Read GetClasses Write SetClasses;
    // Apply these styles when rendering. Depending on StyleRefresh, styles are imported from actual element.
    Property Styles: TWebWidgetStyles Read FStyles Write SetStyles;
    // When rendering, should we refresh styles ?
    Property StyleRefresh : TStyleRefresh Read FStyleRefresh Write FStyleRefresh;
    // Possible references to sub widgets, based on CSS selectors
    Property References : TWebWidgetReferences Read GetReferences write SetReferences;
    // Events of TWebWidget
    Property BeforeRenderHTML : TNotifyEvent Read FBeforeRenderHTML Write FBeforeRenderHTML;
    Property AfterRenderHTML : TNotifyEvent Read FAfterRenderHTML Write FAfterRenderHTML;
    Property BeforeUnRenderHTML : TNotifyEvent Read FBeforeUnRenderHTML Write FBeforeUnRenderHTML;
    Property AfterUnRenderHTML : TNotifyEvent Read FAfterUnRenderHTML Write FAfterUnRenderHTML;
    // HTML DOM events
    Property OnAbort: THTMLNotifyEvent Index 0 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationCancel: THTMLNotifyEvent Index 1 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationEnd: THTMLNotifyEvent Index 2 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationIteration: THTMLNotifyEvent Index 3 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAnimationStart: THTMLNotifyEvent Index 4 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnAuxClick : THTMLNotifyEvent Index 5 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnBlur : THTMLNotifyEvent Index 6 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCancel : THTMLNotifyEvent Index 7 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCanPlay : THTMLNotifyEvent Index 8 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCanPlayThrough : THTMLNotifyEvent Index 9 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnChange : THTMLNotifyEvent Index 10 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnClick : THTMLNotifyEvent Index 11 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCompositionEnd : THTMLNotifyEvent Index 12 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCompositionStart : THTMLNotifyEvent Index 13 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCompositionUpdate : THTMLNotifyEvent Index 14 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnContextMenu : THTMLNotifyEvent Index 15 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCopy : THTMLNotifyEvent Index 16 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCut : THTMLNotifyEvent Index 17 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnCueChange : THTMLNotifyEvent Index 18 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnDblClick : THTMLNotifyEvent Index 19 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnDurationChange : THTMLNotifyEvent Index 20 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnEnded  : THTMLNotifyEvent Index 21 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnError  : THTMLNotifyEvent Index 22 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnFocus : THTMLNotifyEvent Index 23 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnFocusIn  : THTMLNotifyEvent Index 24 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnFocusOut  : THTMLNotifyEvent Index 25 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnGotPointerCapture : THTMLNotifyEvent Index 26 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnInput : THTMLNotifyEvent Index 27 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnInvalid : THTMLNotifyEvent Index 28 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnKeyDown : THTMLNotifyEvent Index 29 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnKeyPress : THTMLNotifyEvent Index 30 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnKeyUp : THTMLNotifyEvent Index 31 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoad : THTMLNotifyEvent Index 32 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadedData : THTMLNotifyEvent Index 33 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadedMetaData : THTMLNotifyEvent Index 34 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadend : THTMLNotifyEvent Index 35 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLoadStart : THTMLNotifyEvent Index 36 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnLostPointerCapture : THTMLNotifyEvent Index 37 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseDown : THTMLNotifyEvent Index 38 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseEnter : THTMLNotifyEvent Index 39 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseLeave : THTMLNotifyEvent Index 40 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseMove : THTMLNotifyEvent Index 41 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseOut : THTMLNotifyEvent Index 42 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnMouseUp : THTMLNotifyEvent Index 43 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnOverFlow : THTMLNotifyEvent Index 44 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPaste : THTMLNotifyEvent Index 45 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPause : THTMLNotifyEvent Index 46 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPlay : THTMLNotifyEvent Index 47 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerCancel : THTMLNotifyEvent Index 48 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerDown : THTMLNotifyEvent Index 49 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerEnter : THTMLNotifyEvent Index 50 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerLeave : THTMLNotifyEvent Index 51 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerMove : THTMLNotifyEvent Index 52 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerOut : THTMLNotifyEvent Index 53 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerOver : THTMLNotifyEvent Index 54 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnPointerUp : THTMLNotifyEvent Index 55 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnReset : THTMLNotifyEvent Index 56 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnResize : THTMLNotifyEvent Index 57 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnScroll : THTMLNotifyEvent Index 58 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnSelect : THTMLNotifyEvent Index 59 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnSubmit : THTMLNotifyEvent Index 60 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTouchStart : THTMLNotifyEvent Index 61 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionCancel : THTMLNotifyEvent Index 62 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionEnd : THTMLNotifyEvent Index 63 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionRun : THTMLNotifyEvent Index 64 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnTransitionStart : THTMLNotifyEvent Index 65 Read GetHTMLEvent Write SetHTMLEvent;
    Property OnWheel : THTMLNotifyEvent Index 66 Read GetHTMLEvent Write SetHTMLEvent;
  end;
  TCustomWebWidgetClass = Class of TCustomWebWidget;

  { TWebWidget }

  TWebWidget = Class(TCustomWebWidget)
  Published
    // Properties
    Property ParentID;
    Property ElementID;
    Property Classes;
    Property Styles;
    Property StyleRefresh;
    // Events
    Property BeforeRenderHTML;
    Property AfterRenderHTML;
    Property OnAbort;
    Property OnAnimationCancel;
    Property OnAnimationEnd;
    Property OnAnimationIteration;
    Property OnAnimationStart;
    Property OnAuxClick;
    Property OnBlur;
    Property OnCancel;
    Property OnCanPlay;
    Property OnCanPlayThrough;
    Property OnChange;
    Property OnClick;
    Property OnCompositionEnd;
    Property OnCompositionStart;
    Property OnCompositionUpdate;
    Property OnContextMenu;
    Property OnCopy;
    Property OnCut;
    Property OnCueChange;
    Property OnDblClick;
    Property OnDurationChange;
    Property OnEnded ;
    Property OnError ;
    Property OnFocus;
    Property OnFocusIn ;
    Property OnFocusOut ;
    Property OnGotPointerCapture;
    Property OnInput;
    Property OnInvalid;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoad;
    Property OnLoadedData;
    Property OnLoadedMetaData;
    Property OnLoadend;
    Property OnLoadStart;
    Property OnLostPointerCapture;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseOut;
    Property OnMouseUp;
    Property OnOverFlow;
    Property OnPaste;
    Property OnPause;
    Property OnPlay;
    Property OnPointerCancel;
    Property OnPointerDown;
    Property OnPointerEnter;
    Property OnPointerLeave;
    Property OnPointerMove;
    Property OnPointerOut;
    Property OnPointerOver;
    Property OnPointerUp;
    Property OnReset;
    Property OnResize;
    Property OnScroll;
    Property OnSelect;
    Property OnSubmit;
    Property OnTouchStart;
    Property OnTransitionCancel;
    Property OnTransitionEnd;
    Property OnTransitionRun;
    Property OnTransitionStart;
    Property OnWheel;
  end;
  TWebWidgetClass = Class of TWebWidget;

  { TContainerWidget }

  TContainerWidget = Class(TWebWidget)
  private
    const KnownStyleCount = 6;
    Const KnownStyles : Array[0..KnownStyleCount] of string = ('min-width','max-width','min-height','max-height','display','width','height');

    function GetKnownStyle(AIndex: Integer): String;
    procedure SetKnownStyle(AIndex: Integer; AValue: String);
  Public
    constructor Create(aOwner: TComponent);  override;
    Property MinWidth : String Index 0 Read GetKnownStyle Write SetKnownStyle;
    Property MaxWidth : String Index 1 Read GetKnownStyle Write SetKnownStyle;
    Property MinHeight : String Index 2 Read GetKnownStyle Write SetKnownStyle;
    Property MaxHeight : String Index 3 Read GetKnownStyle Write SetKnownStyle;
    Property Display : String Index 4 Read GetKnownStyle Write SetKnownStyle;
    Property Width : String Index 5 Read GetKnownStyle Write SetKnownStyle;
    Property Height : String Index 6 Read GetKnownStyle Write SetKnownStyle;
  end;

  { TCustomTemplateWidget }

  TCustomTemplateWidget = Class(TWebWidget)
  private
    FContainerTag: String;
    procedure SetContainerTag(AValue: String);
  Protected
    // When set, a tag will be created and the template will be rendered below this tag.
    Property ContainerTag : String Read FContainerTag Write SetContainerTag;
  Public
  end;

  { TSimpleTemplateWidget }

  TSimpleTemplateWidget = Class(TCustomTemplateWidget)
  Private
    FTemplate: String;
    procedure SetTemplate(AValue: String);
  Public
    Property References;
  Published
    // The template.
    Property Template : String Read FTemplate Write SetTemplate;
    Property ContainerTag;
  end;


  TTemplateWidget = Class(TCustomTemplateWidget)
  Private
    FTemplate: TStrings;
    procedure SetTemplate(AValue: TStrings);
  Protected
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property References;
  Published
    // The template.
    Property Template : TStrings Read FTemplate Write SetTemplate;
    Property ContainerTag;
  end;

  { TCustomLoopTemplateWidget }

  { TLoopTemplateValue }

  TLoopTemplateValue = Class
  Public
    Index : Integer;
    Name : String;
    Value : String;
  end;
  TGetLoopTemplateValueEvent = Procedure (Sender : TObject; aValue : TLoopTemplateValue) of object;

  { TLoopTemplateGroup }

  TLoopTemplateGroup = class(TCollectionItem)
  private
    FFooterTemplate: String;
    FHeaderTemplate: String;
    FName: string;
    procedure SetFooterTemplate(AValue: String);
    procedure SetHeaderTemplate(AValue: String);
    procedure SetName(AValue: string);
  Public
    function GetDisplayName : string; override;
    Procedure Assign(Source: TPersistent); override;
  Published
    Property Name : string Read FName Write SetName;
    Property HeaderTemplate : String Read FHeaderTemplate Write SetHeaderTemplate;
    Property FooterTemplate : String Read FFooterTemplate Write SetFooterTemplate;
  end;

  { TLoopTemplateGroupList }

  TLoopTemplateGroupList = class(TOwnedCollection)
  private
    function GetG(aIndex : Integer): TLoopTemplateGroup;
    procedure SetG(aIndex : Integer; AValue: TLoopTemplateGroup);
  Protected
    procedure Update(Item: TCollectionItem); override;
  Public
    Function IndexOfGroup(const aName : string) : Integer;
    Function FindGroup(const aName : string) : TLoopTemplateGroup;
    Function GetGroup(const aName : string) : TLoopTemplateGroup;
    Function AddGroup(Const aName,aHeader,aFooter : String) : TLoopTemplateGroup;
    Property Groups [aIndex : Integer] : TLoopTemplateGroup Read GetG Write SetG; default;
  end;

  TCustomLoopTemplateWidget = Class(TCustomTemplateWidget)
  Private
    FFooter: String;
    FGroups: TLoopTemplateGroupList;
    FHeader: String;
    FOnGetValue: TGetLoopTemplateValueEvent;
    FTemplate: String;
    procedure SetFooter(AValue: String);
    procedure SetGroups(AValue: TLoopTemplateGroupList);
    procedure SetHeader(AValue: String);
    procedure SetTemplate(AValue: String);
  Protected
  Protected
    // Template support
    Class Function CreateGroups(aOwner : TComponent) : TLoopTemplateGroupList; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  Protected
    // The templates.
    Property Groups : TLoopTemplateGroupList Read FGroups Write SetGroups;
    Property HeaderTemplate : String Read FHeader Write SetHeader;
    Property ItemTemplate : String Read FTemplate Write SetTemplate;
    Property FooterTemplate : String Read FFooter Write SetFooter;
    Property OnGetValue : TGetLoopTemplateValueEvent Read FOnGetValue Write FOnGetValue;
  end;

  { TSimpleLoopTemplateWidget }

  { TSimpleLoopTemplateGroup }

  TSimpleLoopTemplateGroup = Class(TLoopTemplateGroup)
  private
    FGroupValueTemplate: String;
    procedure SetGroupValueTemplate(AValue: String);
  Published
    Property GroupValueTemplate : String Read FGroupValueTemplate Write SetGroupValueTemplate;
  end;

  TSimpleLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  private
    FItemCount: Integer;
    FOnGetGroupValue: TGetLoopTemplateValueEvent;
    procedure SetItemCount(AValue: Integer);
  Published
    Property ItemCount : Integer Read FItemCount Write SetItemCount;
    Property Groups;
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
    Property OnGetGroupValue : TGetLoopTemplateValueEvent Read FOnGetGroupValue Write FOnGetGroupValue;
    Property References;
    Property ContainerTag;
  end;


  { TListLoopTemplateWidget }
Type
  TListKind = (lkCollection,lkFPList,lkList,lkObjectArray,lkJSArray);
  TValueMode = (vmRTTI,vmProperty);

  TListLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  Published
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
  end;

implementation

uses TypInfo;

ResourceString
   SErrUnknownTemplateGroup = 'Unknown template group item: "%s"';
   SErrDuplicateTemplateGroup = 'Duplicate template group item: "%s"';

{ TSimpleLoopTemplateGroup }

procedure TSimpleLoopTemplateGroup.SetGroupValueTemplate(AValue: String);
begin
  if FGroupValueTemplate=AValue then Exit;
  FGroupValueTemplate:=AValue;
  Changed(False);
end;

{ TLoopTemplateGroupList }

function TLoopTemplateGroupList.GetG(aIndex : Integer): TLoopTemplateGroup;
begin
  Result:=TLoopTemplateGroup(Items[aIndex])
end;

procedure TLoopTemplateGroupList.SetG(aIndex : Integer; AValue: TLoopTemplateGroup);
begin
  Items[aIndex]:=aValue;
end;

procedure TLoopTemplateGroupList.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

function TLoopTemplateGroupList.IndexOfGroup(const aName: string): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetG(Result).Name,aName) do
    Dec(Result);
end;

function TLoopTemplateGroupList.FindGroup(const aName: string): TLoopTemplateGroup;

Var
  Idx: Integer;

begin
  Idx:=IndexOfGroup(aName);
  if (Idx=-1) then
    Result:=Nil
  else
    Result:=GetG(Idx);
end;

function TLoopTemplateGroupList.GetGroup(const aName: string): TLoopTemplateGroup;
begin
  Result:=FindGroup(aName);
  if (Result=Nil) then
    raise EWidgets.CreateFmt(SErrUnknownTemplateGroup, [aName]);
end;

function TLoopTemplateGroupList.AddGroup(const aName, aHeader, aFooter: String): TLoopTemplateGroup;
begin
  if IndexOfGroup(aName)<>-1 then
    raise EWidgets.CreateFmt(SErrDuplicateTemplateGroup, [aName]);
  Result:=add as TLoopTemplateGroup;
  Result.Name:=aName;
  Result.FFooterTemplate:=aFooter;
  Result.HeaderTemplate:=aHeader;
end;

{ TLoopTemplateGroup }

procedure TLoopTemplateGroup.SetFooterTemplate(AValue: String);
begin
  if FFooterTemplate=AValue then Exit;
  FFooterTemplate:=AValue;
  Changed(False);
end;

procedure TLoopTemplateGroup.SetHeaderTemplate(AValue: String);
begin
  if FHeaderTemplate=AValue then Exit;
  FHeaderTemplate:=AValue;
  Changed(False);
end;

procedure TLoopTemplateGroup.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  if Assigned(Collection) then
    if TLoopTemplateGroupList(Collection).IndexOfGroup(aValue)<>-1 then
      raise EWidgets.CreateFmt(SErrDuplicateTemplateGroup, [aValue]);
  FName:=AValue;
end;

function TLoopTemplateGroup.GetDisplayName: string;
begin
  Result:=Name;
  if Result='' then
    Result:=Inherited GetDisplayName;
end;

procedure TLoopTemplateGroup.Assign(Source: TPersistent);

Var
  G : TLoopTemplateGroup;

begin
  if Source is TLoopTemplateGroup then
    begin
    G:=Source as TLoopTemplateGroup;
    FName:=G.Name;
    FHeaderTemplate:=G.HeaderTemplate;
    FFooterTemplate:=G.FooterTemplate;
    end
  else
    inherited Assign(Source);
end;




{ TSimpleLoopTemplateWidget }

procedure TSimpleLoopTemplateWidget.SetItemCount(AValue: Integer);
begin
  if FItemCount=AValue then Exit;
  FItemCount:=AValue;
end;


{ TCustomLoopTemplateWidget }

procedure TCustomLoopTemplateWidget.SetTemplate(AValue: String);
begin
  if FTemplate=aValue then exit;
  FTemplate:=aValue;
end;

class function TCustomLoopTemplateWidget.CreateGroups(aOwner: TComponent
  ): TLoopTemplateGroupList;
begin
  Result:=TLoopTemplateGroupList.Create(aOwner,TLoopTemplateGroup);
end;


procedure TCustomLoopTemplateWidget.SetFooter(AValue: String);
begin
  if FFooter=AValue then Exit;
  FFooter:=AValue;
end;

procedure TCustomLoopTemplateWidget.SetGroups(AValue: TLoopTemplateGroupList);
begin
  if FGroups=AValue then Exit;
  FGroups.Assign(AValue);
end;

procedure TCustomLoopTemplateWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
end;


constructor TCustomLoopTemplateWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FGroups:=CreateGroups(Self);
end;

destructor TCustomLoopTemplateWidget.Destroy;
begin
  FreeAndNil(FGroups);
  inherited Destroy;
end;


{ TSimpleTemplateWidget }

procedure TSimpleTemplateWidget.SetTemplate(AValue: String);

begin
  FTemplate:=AValue;
end;


{ TWebWidgetReferences }

function TWebWidgetReferences.GetReferenceItem(aIndex : Integer): TReferenceItem;
begin
  Result:=TReferenceItem(Inherited Items[aIndex])
end;

procedure TWebWidgetReferences.SetReferenceItem(aIndex : Integer; AValue: TReferenceItem);
begin
  Items[aIndex]:=aValue;
end;


{ TReferenceItem }

function TReferenceItem.GetDisplayName: string;
begin
  Result:=Name;
  if Result='' then
    Result:=inherited GetDisplayName;
end;

procedure TReferenceItem.Assign(Source: TPersistent);

Var
  RI : TReferenceItem absolute source;

begin
  if Source is TReferenceItem then
    begin
    FName:=RI.Name;
    FSelector:=RI.Selector;
    end;
end;


{ TCustomTemplateWidget }


procedure TCustomTemplateWidget.SetContainerTag(AValue: String);
begin
  if FContainerTag=AValue then Exit;
  FContainerTag:=AValue;
end;


{ TTemplateWidget }

procedure TTemplateWidget.SetTemplate(AValue: TStrings);
begin
  if FTemplate=AValue then Exit;
  FTemplate.Assign(AValue);
end;


constructor TTemplateWidget.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FTemplate:=TStringList.Create;
  FContainerTag:='';
end;

destructor TTemplateWidget.Destroy;

begin
  FreeAndNil(FTemplate);
  inherited Destroy;
end;

{ TContainerWidget }

function TContainerWidget.GetKnownStyle(AIndex: Integer): String;

var
  S : TStyleItem;
begin
  S:=Styles.FindStyle(KnownStyles[aIndex]);
  if Assigned(S) then
    Result:=S.Value;
end;

procedure TContainerWidget.SetKnownStyle(AIndex: Integer; AValue: String);

begin
  Styles.EnsureStyle(KnownStyles[aIndex]).Value:=aValue;
end;


constructor TContainerWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  MinWidth:='32px';
  MinHeight:='12px';
  Width:='50%';
  Height:='50%';
end;



{ TWebWidgetStyles }

function TWebWidgetStyles.GetStyleItem(aIndex: Integer): TStyleItem;
begin
  Result:=TStyleItem(Items[Aindex]);
end;

procedure TWebWidgetStyles.SetStyleItem(aIndex: Integer; AValue: TStyleItem);
begin
  Items[aIndex]:=aValue;
end;


function TWebWidgetStyles.Add(const aName: String; const aValue: String): TStyleItem;
begin
  Result:=(Inherited Add) as TStyleItem;
  // Don't use Name
  Result.FName:=aName;
  if aValue<>'' then
    Result.Value:=aValue; // will trigger markdirty
end;

function TWebWidgetStyles.EnsureStyle(const aName: String; const aValue: String): TStyleItem;
begin
  Result:=FindStyle(aName);
  if Result=Nil then
    Result:=Add(aName,aValue)
  else if AValue<>'' then
    Result.Value:=aValue
end;


function TWebWidgetStyles.IndexOfStyle(const aName: String): integer;

begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aName,GetStyleItem(Result).Name) do
    Dec(Result);
end;

function TWebWidgetStyles.FindStyle(aName: String): TStyleItem;

Var
  Idx : integer;

begin
  Idx:=IndexOfStyle(aName);
  If Idx=-1 then
    Result:=Nil
  else
    Result:=GetStyleItem(Idx)
end;


{ TStyleItem }

procedure TStyleItem.MarkDirty;

begin
end;

function TStyleItem.GetDisplayName: string;
begin
  Result:=Name;
  if Result='' then
    Result:=inherited GetDisplayName;
end;

procedure TStyleItem.SetValue(AValue: String);
begin
  if FValue=AValue then Exit;
  FValue:=aValue;
  MarkDirty;
end;

procedure TStyleItem.SetPriority(AValue: TStylePriority);
begin
  if FPriority=AValue then Exit;
  FPriority:=AValue;
  MarkDirty;
end;


procedure TStyleItem.SetName(AValue: String);
begin
  if aValue=FName then Exit;
  FName:=AValue;
  MarkDirty;
end;

procedure TStyleItem.Assign(Source: TPersistent);

Var
  SI : TStyleItem absolute source;

begin
  if Source is TStyleItem then
    begin
    FName:=SI.FName;
    FValue:=SI.FValue;
    FImported:=SI.FImported;
    end
  else
    inherited Assign(Source);
end;


{ TCustomWebWidget }



function TCustomWebWidget.GetHaveReferences: Boolean;

begin
  Result:=Assigned(FReferences);
end;

function TCustomWebWidget.GetHTMLEvent(AIndex: Integer): THTMLNotifyEvent;

begin
  Result:=nil;
  if (aIndex>=0) and (aIndex<=MaxEvents) then
    Result:=FMyEvents[aindex];
end;


function TCustomWebWidget.GetClasses: String;
begin
  Result:=FClasses;
end;



function TCustomWebWidget.GetParentID: String;


begin
  Result:=FParentID;
end;

function TCustomWebWidget.GetElementID: String;

begin
    Result:=FElementID;
end;


function TCustomWebWidget.GetReferences: TWebWidgetReferences;
begin
  Result:=FReferences;
end;


procedure TCustomWebWidget.SetClasses(AValue: String);
begin
  FClasses:=aValue;
end;


procedure TCustomWebWidget.SetElementID(AValue: String);
begin
  if (FElementID=AValue) then Exit;
  FElementID:=AValue;
end;

procedure TCustomWebWidget.SetHTMLEvent(AIndex: Integer; AValue: THTMLNotifyEvent);

begin
  if (aIndex<0) or (aIndex>MaxEvents) then
    exit;
  FMyEvents[aIndex]:=aValue;
end;


procedure TCustomWebWidget.SetParentID(AValue: String);

begin
  if (FParentID=AValue) then exit;
  FParentID:=aValue;
end;


procedure TCustomWebWidget.SetReferences(AValue: TWebWidgetReferences);
begin
  if (aValue=FReferences) then exit;
  References.Assign(aValue);
end;

procedure TCustomWebWidget.SetStyles(AValue: TWebWidgetStyles);
begin
  if FStyles=AValue then Exit;
  FStyles.Assign(AValue);
end;

procedure TCustomWebWidget.SetVisible(AValue: Boolean);

begin
  if aValue=FVisible then
    Exit;
  FVisible:=aValue;
end;


function TCustomWebWidget.CreateStyles: TWebWidgetStyles;
begin
  Result:=TWebWidgetStyles.Create(Self,TStyleItem);
end;

function TCustomWebWidget.CreateReferences: TWebWidgetReferences;
begin
  Result:=TWebWidgetReferences.Create(Self,TReferenceItem);
end;


constructor TCustomWebWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FReferences:=CreateReferences;
  FStyles:=CreateStyles;
  FVisible:=True;
end;

destructor TCustomWebWidget.Destroy;

begin
  FreeAndNil(FStyles);
  FreeAndNil(FReferences);
  inherited Destroy;
end;



end.

