{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    extra Bootstrap widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit stub.bootstrapwidgets;

{$mode objfpc}
{$h+}

interface

uses
  Classes, SysUtils, stub.web, stub.webwidget, stub.TemplateLoader;

Type

  { TSimpleToastWidget }
  TContextual = (cNone,cPrimary,cSecondary,cSuccess,cDanger,cWarning,cInfo,cLight,cDark);

  TSimpleToastWidget = Class(TCustomTemplateWidget)
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
    FOnHide: TNotifyEvent;
    FSmallHeader: String;
    FUnrenderOnHide: Boolean;
    procedure SetAnimate(AValue: Boolean);
    procedure SetAutoHide(AValue: Boolean);
    procedure SetBody(AValue: String);
    procedure SetBoolean(AValue: Boolean);
    procedure SetContextual(AValue: TContextual);
    procedure SetHeader(AValue: String);
    procedure SetHeaderImage(AValue: String);
    procedure SetHideDelay(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetSmallHeader(AValue: String);
    procedure SetUnrenderOnHide(AValue: Boolean);
  Protected
{    function BodyHTML: String; virtual;
    function CloseButtonHTML: String; virtual;
    function HeaderHTML: String; virtual;
    Function GetTemplateHTML: String; override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;}
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Hide;
  Published
    Property Header : String Read FHeader Write SetHeader;
    Property SmallHeader : String Read FSmallHeader Write SetSmallHeader;
    Property Body : String Read FBody Write SetBody;
    Property HeaderImage : String Read FHeaderImage Write SetHeaderImage;
    Property CloseButton : Boolean Read FBoolean Write SetBoolean;
    Property Contextual : TContextual Read FContextual write SetContextual;
    Property HideDelay : Integer Read FHideDelay Write SetHideDelay default 2000;
    Property AutoHide : Boolean Read FAutoHide Write SetAutoHide default True;
    Property Animate : Boolean Read FAnimate Write SetAnimate default False;
    Property MinWidth : Integer Read FMinWidth Write SetMinWidth default 200;
    Property UnrenderOnHide : Boolean Read FUnrenderOnHide Write SetUnrenderOnHide;
    Property OnHide : TNotifyEvent Read FOnHide Write FOnHide;
  end;

  TBootstrapToastWidget = Class(TSimpleToastWidget);

  { TBootstrapModal }
  TOnModalHideEvent = Procedure (Sender : TObject; El : TJSHTMLElement; Values : TStrings) of object;

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

  TBootstrapModal = Class(TCustomTemplateWidget)
  private
    FBackDrop: Boolean;
    FFocus: Boolean;
    FKeyBoard: Boolean;
    FOnHide: TOnModalHideEvent;
    FShowOnRender: Boolean;
    FTemplate: String;
    FTemplateLoader: TCustomTemplateLoader;
    FTemplateName: String;
    function GetModalReferences: TModalReferences;
    procedure SetModalReferences(AValue: TModalReferences);
    procedure SetTemplateLoader(AValue: TCustomTemplateLoader);
    procedure SetTemplateName(AValue: String);
    procedure SetTemplate(AValue: String);
  protected
    Function CreateReferences: TWebWidgetReferences; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
  Published
    Property ShowOnRender: Boolean Read FShowOnRender Write FShowOnrender;
    Property BackDrop : Boolean Read FBackDrop Write FBackDrop;
    Property KeyBoard : Boolean Read FKeyBoard Write FKeyBoard;
    Property Focus : Boolean Read FFocus Write FFocus;
    Property Template : String Read FTemplate Write SetTemplate;
    Property TemplateName : String Read FTemplateName Write SetTemplateName;
    Property TemplateLoader : TCustomTemplateLoader Read FTemplateLoader Write SetTemplateLoader;
    Property OnHide : TOnModalHideEvent Read FOnHide Write FOnHide;
    Property References : TModalReferences Read GetModalReferences Write SetModalReferences;
  end;


Const
  ContextualNames : Array[TContextual] of string = ('','primary','secondary','success','danger','warning','info','light','dark');


Implementation

Resourcestring
  SErrCannotUnrenderFixedElementID = 'Cannot unrender when ElementID (%s) is set';



{ TSimpleToastWidget }


procedure TSimpleToastWidget.SetBody(AValue: String);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;

end;

procedure TSimpleToastWidget.SetAnimate(AValue: Boolean);
begin
  if FAnimate=AValue then Exit;
  FAnimate:=AValue;

end;

procedure TSimpleToastWidget.SetAutoHide(AValue: Boolean);
begin
  if FAutoHide=AValue then Exit;
  FAutoHide:=AValue;

end;

procedure TSimpleToastWidget.SetBoolean(AValue: Boolean);
begin
  if FBoolean=AValue then Exit;
  FBoolean:=AValue;

end;

procedure TSimpleToastWidget.SetContextual(AValue: TContextual);
begin
  if FContextual=AValue then Exit;
  FContextual:=AValue;

end;

procedure TSimpleToastWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;

end;

procedure TSimpleToastWidget.SetHeaderImage(AValue: String);
begin
  if FHeaderImage=AValue then Exit;
  FHeaderImage:=AValue;
end;

procedure TSimpleToastWidget.SetHideDelay(AValue: Integer);
begin
  if FHideDelay=AValue then Exit;
  FHideDelay:=AValue;
end;

procedure TSimpleToastWidget.SetMinWidth(AValue: Integer);
begin
  if FMinWidth=AValue then Exit;
  FMinWidth:=AValue;
end;

procedure TSimpleToastWidget.SetSmallHeader(AValue: String);
begin
  if FSmallHeader=AValue then Exit;
  FSmallHeader:=AValue;
end;

procedure TSimpleToastWidget.SetUnrenderOnHide(AValue: Boolean);
begin
  if FUnrenderOnHide=AValue then Exit;
  if (ElementID<>'') and aValue then
    Raise EWidgets.CreateFmt(SErrCannotUnrenderFixedElementID,[ElementID]);
  FUnrenderOnHide:=AValue;
end;


constructor TSimpleToastWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinWidth:=200;
  FAutoHide:=True;
  FHideDelay:=2000;
end;

procedure TSimpleToastWidget.Hide;
begin

end;

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

{ TBootstrapModal }


function TBootstrapModal.GetModalReferences: TModalReferences;
begin
  Result:=TModalReferences(Inherited References);
end;

procedure TBootstrapModal.SetModalReferences(AValue: TModalReferences);
begin
  References.Assign(aValue);
end;

procedure TBootstrapModal.SetTemplateLoader(AValue: TCustomTemplateLoader);
begin
  if FTemplateLoader=AValue then Exit;
  if Assigned(FTemplateLoader) then
    FTemplateLoader.RemoveFreeNotification(self);
  FTemplateLoader:=AValue;
  if Assigned(FTemplateLoader) then
    FTemplateLoader.FreeNotification(self);
end;

procedure TBootstrapModal.SetTemplateName(AValue: String);
begin
  FTemplateName:=AValue;
end;

procedure TBootstrapModal.SetTemplate(AValue: String);
begin
  FTemplate:=AValue;
end;


function TBootstrapModal.CreateReferences: TWebWidgetReferences;
begin
  Result:=TModalReferences.Create(Self,TModalReferenceItem);
end;

procedure TBootstrapModal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FTemplateLoader) then
    FTemplateLoader:=nil;
end;



end.
