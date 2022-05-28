unit stub.bulmawidgets;

{$mode objfpc}
{$h+}
{ $modeswitch externalclass}

Interface

uses
  Stub.web,
  SysUtils,
  Classes,
  stub.templateloader,
  stub.webwidget;

Const
  DefaultClose = 'DefaultCloseClicks';

Type

  { TBulmaModal }
  TOnModalHideEvent = Procedure (Sender : TObject; CloseEl : TJSHTMLElement; Values : TStrings) of object;

  TModalItemKind = (mikValue,mikClose);

  { TModalReferenceItem }

  TModalReferenceItem = Class(TReferenceItem)
  private
    FInitialValue: String;
    FKind: TModalItemKind;
  Protected
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Kind : TModalItemKind Read FKind Write FKind;
    Property InitialValue : String Read FInitialValue Write FInitialValue;
  end;

  { TModalReferences }

  TModalReferences = Class(TWebWidgetReferences)
  private
    function GetMR(aIndex : Integer): TModalReferenceItem;
    procedure SetMR(aIndex : Integer; AValue: TModalReferenceItem);
  Public
    Property ModalRefs[aIndex : Integer] : TModalReferenceItem Read GetMR Write SetMR; default;
  end;

  TBulmaModal = Class(TCustomWebWidget)
  private
    FBackDrop: Boolean;
    FFocus: Boolean;
    FKeyBoard: Boolean;
    FOnHide: TOnModalHideEvent;
    FShowOnRender: Boolean;
    FTemplate: String;
    FTemplateLoader: TCustomTemplateLoader;
    FTemplateName: String;
    FRemoveOnHide: Boolean;
    FOKButtonName: String;
    FCancelButtonName: String;
    FCloseButtonName: String;
    FOnRender: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetModalReferences: TModalReferences;
    procedure SetModalReferences(AValue: TModalReferences);
    procedure SetTemplateLoader(AValue: TCustomTemplateLoader);
  protected
    function CreateReferences: TWebWidgetReferences; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    Property ParentID;
    Property ElementID;
    Property Styles;
    Property StyleRefresh;
    Property References : TModalReferences Read GetModalReferences Write SetModalReferences;
    Property ShowOnRender: Boolean Read FShowOnRender Write FShowOnrender default False;
    Property RemoveOnHide : Boolean Read FRemoveOnHide Write FRemoveOnHide default True;
    Property BackDrop : Boolean Read FBackDrop Write FBackDrop default false;
    Property KeyBoard : Boolean Read FKeyBoard Write FKeyBoard default false;
    Property Focus : Boolean Read FFocus Write FFocus default false;
    Property OKButtonName : String Read FOKButtonName write FOKButtonName;
    Property CancelButtonName : String Read FCancelButtonName Write FCancelButtonName;
    Property CloseButtonName : String Read FCloseButtonName Write FCloseButtonName;
    // Template gets precedence over templatename;
    Property Template : String Read FTemplate Write FTemplate;
    Property TemplateName : String Read FTemplateName Write FTemplateName;
    Property TemplateLoader : TCustomTemplateLoader Read FTemplateLoader Write SetTemplateLoader;
    Property OnHide : TOnModalHideEvent Read FOnHide Write FOnHide;
    Property OnRender : TNotifyEvent Read FOnRender Write FOnRender;
    Property OnShow : TNotifyEvent Read FOnShow Write FOnShow;
  end;

  { TSimpleToastWidget }
  TContextual = (cNone,
                 cPrimary,cLink,cInfo,cSuccess,cWarning,cDanger,
                 cWhite,cLight,cDark,cBlack,cText,cGhost);
  TToastPosition = (tpDefault,
               tpTopRight,tpTopLeft,tpTopCenter,
               tpBottomRight,tpBottomLeft,tpBottomCenter,
               tpCenter);

  // Single toast message

  TBaseBulmaToastWidget = Class(TCustomWebWidget)
  private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FBody: String;
    FBoolean: Boolean;
    FContextual: TContextual;
    FHeader: String;
    FHeaderImage: String;
    FHideDelay: Integer;
    FMinWidth: Integer;
    FSingle: Boolean;
    FPosition: TToastPosition;
    procedure SetAnimate(AValue: Boolean);
    procedure SetAutoHide(AValue: Boolean);
    procedure SetBody(AValue: String);
    procedure SetBoolean(AValue: Boolean);
    procedure SetContextual(AValue: TContextual);
    procedure SetHeader(AValue: String);
    procedure SetHeaderImage(AValue: String);
    procedure SetHideDelay(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
  Protected
    FElement : TJSHTMLElement;
    function BodyHTML: String; virtual;
    function HeaderHTML: String; virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Refresh;
    procedure Hide;
  Protected
    Property Header : String Read FHeader Write SetHeader;
    Property Body : String Read FBody Write SetBody;
    Property HeaderImage : String Read FHeaderImage Write SetHeaderImage;
    Property CloseButton : Boolean Read FBoolean Write SetBoolean;
    Property Contextual : TContextual Read FContextual write SetContextual;
    Property HideDelay : Integer Read FHideDelay Write SetHideDelay default 2000;
    Property AutoHide : Boolean Read FAutoHide Write SetAutoHide default True;
    Property Animate : Boolean Read FAnimate Write SetAnimate default False;
    Property MinWidth : Integer Read FMinWidth Write SetMinWidth default 200;
    Property Single : Boolean Read FSingle Write FSingle;
    Property Position : TToastPosition Read FPosition Write FPosition default tpTopRight;
  end;
  
  TBulmaToastWidget = class(TBaseBulmaToastWidget)
    Property ParentID;
    Property ElementID;
    Property Header; 
    Property Body;
    Property HeaderImage;
    Property CloseButton;
    Property Contextual;
    Property HideDelay;
    Property AutoHide;
    Property Animate;
    Property MinWidth;
    Property Single;
    Property Position;
  end;

  // Encapsulates the global tag where the toasts are shown.

  { TToastManager }

  TToastManager = Class(TComponent)
  Private
    class var
      _instance : TToastManager;
      _ToastID : NativeInt;
  Private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FHideDelay: Integer;
    FMinheight: Integer;
    FMinWidth: Integer;
    FMultiToast: Boolean;
    FToastIcon: String;
    FParentID: String;
    FToastPosition: TToastPosition;
    procedure SetMinHeight(AValue: Integer);
    procedure SetMultiToast(AValue: Boolean);
    procedure SetParentID(const Value: String);
    class function CreateElement(aTag : String; aID: String = ''): TJSHTMLElement; static;
  Public
    Constructor Create(aOwner : TComponent); override;
    class function Instance : TToastManager;
    Class Function getToastID : String;
    Procedure clear;
    function ShowToast(const aHeader, aBody: String; aContext: TContextual=cNone; Closable: Boolean=True; aDelay : Integer = 0): TBaseBulmaToastWidget;
  Published
    Property ParentID : String Read FParentID Write SetParentID;
    Property MultiToast : Boolean Read FMultiToast Write SetMultiToast;
    Property MinHeight : Integer Read FMinheight Write SetMinHeight default 250;
    Property ToastHideDelay : Integer Read FHideDelay Write FHideDelay default 2000;
    Property ToastAutoHide : Boolean Read FAutoHide Write FAutoHide default True;
    Property ToastAnimate : Boolean Read FAnimate Write FAnimate default False;
    Property ToastMinWidth : Integer Read FMinWidth Write FMinWidth default 200;
    Property ToastIcon : String Read FToastIcon Write FToastIcon;
    Property ToastPosition : TToastPosition Read FToastPosition Write FToastPosition;
  end;

Const
  ContextualNames : Array[TContextual] of string
                  = ('','primary','link','info','success','warning','danger',
                     'white','light','dark','black','text','ghost');
  ToastPositionNames : Array[TToastPosition] of string =
               ('top-right','top-right','top-left','top-center',
                'bottom-right','bottom-left','bottom-center',
                'center');

Function Toasts : TToastManager;

Implementation

{ TModalReferenceItem }

procedure TModalReferenceItem.Assign(Source: TPersistent);

Var
  MRI : TModalReferenceItem absolute Source;

begin
  if Source is TModalReferenceItem then
    begin
    Self.Kind:=MRI.Kind;
    Self.InitialValue:=MRI.InitialValue;
    end;
  inherited Assign(Source);
end;


{ TModalReferences }

function TModalReferences.GetMR(aIndex : Integer): TModalReferenceItem;
begin
  Result:=TModalReferenceItem(Items[aIndex])
end;

procedure TModalReferences.SetMR(aIndex : Integer; AValue: TModalReferenceItem);
begin
  Items[aIndex]:=aValue;
end;


{ TBulmaModal }


procedure TBulmaModal.SetModalReferences(AValue: TModalReferences);
begin
  References.Assign(aValue);
end;

procedure TBulmaModal.SetTemplateLoader(AValue: TCustomTemplateLoader);
begin
  if FTemplateLoader=AValue then Exit;
  if assigned(FTemplateLoader) then
    FTemplateLoader.RemoveFreeNotification(Self);
  FTemplateLoader:=AValue;
  if assigned(FTemplateLoader) then
    FTemplateLoader.FreeNotification(Self);
end;


function TBulmaModal.CreateReferences: TWebWidgetReferences;
begin
  Result:=TModalReferences.Create(Self,TModalReferenceItem);
end;

procedure TBulmaModal.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=TemplateLoader) then
    FTemplateLoader:=Nil;
end;


destructor TBulmaModal.Destroy;
begin
  inherited;
end;


constructor TBulmaModal.Create(aOwner: TComponent);
begin
  inherited;
  FRemoveOnHide:=True;
end;


function TBulmaModal.GetModalReferences: TModalReferences;
begin
  Result:=TModalReferences(Inherited References);
end;


{ TBulmaToast }

function Toasts: TToastManager;
begin
  Result:=TToastManager.Instance;
end;

{ TToastManager }

class function TToastManager.Instance: TToastManager;
begin
  if _instance=Nil then
   _instance:=TToastManager.Create(Nil);
  Result:=_instance;
end;


procedure TToastManager.SetMinHeight(AValue: Integer);
begin
  if FMinheight=AValue then Exit;
  FMinheight:=AValue;
end;

procedure TToastManager.SetMultiToast(AValue: Boolean);
begin
  if FMultiToast=AValue then Exit;
  FMultiToast:=AValue;
end;

procedure TToastManager.SetParentID(const Value: String);
begin
  FParentID:=Value;
end;



class function TToastManager.getToastID: String;
begin
  Inc(_ToastID);
  Result:='toast-'+intToStr(_ToastID);
end;

Class Function TToastManager.CreateElement(aTag : String; aID : String = '') : TJSHTMLElement;

begin
  Result:=nil;
  if aTag='' then;
  if aID='' then;
end;


procedure TToastManager.clear;
begin
end;

constructor TToastManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinHeight:=250;
  FMinWidth:=200;
  FMultiToast:=True;
  FHideDelay:=2000;
  FAutoHide:=True;
  FAnimate:=False;
end;

function TToastManager.ShowToast(const aHeader, aBody: String; aContext : TContextual = cNone; Closable: Boolean = True; aDelay : Integer = 0): TBaseBulmaToastWidget;


begin
  if aHeader=aBody then;
  if aContext<>cnone then;
  if closable then;
  if aDelay=0 then;
  Result:=Nil;
end;

{ TBaseBulmaToastWidget }


procedure TBaseBulmaToastWidget.SetBody(AValue: String);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;
end;

procedure TBaseBulmaToastWidget.SetAnimate(AValue: Boolean);
begin
  if FAnimate=AValue then Exit;
  FAnimate:=AValue;
end;

procedure TBaseBulmaToastWidget.SetAutoHide(AValue: Boolean);
begin
  if FAutoHide=AValue then Exit;
  FAutoHide:=AValue;
end;

procedure TBaseBulmaToastWidget.SetBoolean(AValue: Boolean);
begin
  if FBoolean=AValue then Exit;
  FBoolean:=AValue;
end;

procedure TBaseBulmaToastWidget.SetContextual(AValue: TContextual);
begin
  if FContextual=AValue then Exit;
  FContextual:=AValue;
end;

procedure TBaseBulmaToastWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
end;

procedure TBaseBulmaToastWidget.SetHeaderImage(AValue: String);
begin
  if FHeaderImage=AValue then Exit;
  FHeaderImage:=AValue;
end;

procedure TBaseBulmaToastWidget.SetHideDelay(AValue: Integer);
begin
  if FHideDelay=AValue then Exit;
  FHideDelay:=AValue;
end;

procedure TBaseBulmaToastWidget.SetMinWidth(AValue: Integer);
begin
  if FMinWidth=AValue then Exit;
  FMinWidth:=AValue;
end;


function TBaseBulmaToastWidget.HeaderHTML: String;

Var
  S : String;

begin
  Result:='';
  if (Header='') and (HeaderImage='') then
    exit;
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:=' is-'+S;
  Result:='<p class="title is-6 is-small">';
  if HeaderImage<>'' then
    Result:=Result+'<img src="'+HeaderImage+'" class="icon is-small">';
  Result:=Result+Header;
  Result:=Result+'</p>'
end;


function TBaseBulmaToastWidget.BodyHTML: String;

begin
  Result:='<div class="message-body is-light">';
  Result:=Result+HeaderHTML;
  Result:=Result+'<p>'+Body+'</p>';
  Result:=Result+'</div>';
end;


constructor TBaseBulmaToastWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinWidth:=200;
  FAutoHide:=True;
  FHideDelay:=2000;
  FPosition:=tpTopRight;
end;

procedure TBaseBulmaToastWidget.Hide;
begin

end;


procedure TBaseBulmaToastWidget.Refresh;

Begin
end;

end.
