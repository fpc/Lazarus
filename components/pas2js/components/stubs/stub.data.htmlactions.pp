unit Stub.Data.HTMLActions;

{$mode ObjFPC}
{$H+}

interface

uses sysutils, classes, Stub.HTMLActions, db;

Type
  TDBCustomHTMLInputElementAction = Class;
  TDBCustomHTMLButtonElementAction = Class;

  { THTMLActionDataLink }

  THTMLActionDataLink = class(TDataLink)
  private
    FFieldName: string;
    FAction : TDBCustomHTMLInputElementAction;
    procedure SetFieldName(AValue: string);
  public
    constructor Create(aAction : TDBCustomHTMLInputElementAction);
    property FieldName: string read FFieldName write SetFieldName;
  end;


  { TDBCustomHTMLInputElementAction }

  TDBCustomHTMLInputElementAction = class(THTMLCustomElementAction)
  Private
    FLink : THTMLActionDataLink;
    FOnEndEditing: TNotifyEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnStartEditing: TNotifyEvent;
    function GetDataSource: TDatasource;
    function GetFieldName: String;
    procedure SetDatasource(AValue: TDatasource);
    procedure SetFieldName(AValue: String);
  Protected
    Property Link : THTMLActionDataLink Read FLink;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property Datasource : TDatasource Read GetDataSource Write SetDatasource;
    Property FieldName : String Read GetFieldName Write SetFieldName;
    Property OnStartEditing : TNotifyEvent Read FOnStartEditing Write FOnStartEditing;
    Property OnEndEditing : TNotifyEvent Read FOnEndEditing Write FOnEndEditing;
    Property OnLayoutChanged : TNotifyEvent Read FOnLayoutChanged Write FOnLayoutChanged;
  end;
  
  TDBHTMLInputElementAction = class(TDBCustomHTMLInputElementAction)
  Published
    Property Events;
    Property CustomEvents;
    Property ElementID;
    Property PreventDefault;
    Property StopPropagation;
    Property OnExecute;
    Property BeforeBind;
    Property AfterBind;
    Property Datasource;
    Property FieldName;
    Property OnStartEditing;
    Property OnEndEditing;
    Property OnLayoutChanged;
  end;

  { TButtonActionDataLink }

  TButtonActionDataLink = class(TDataLink)
  private
    FAction: TDBCustomHTMLButtonElementAction;
  protected
    Property Action : TDBCustomHTMLButtonElementAction Read Faction;
  public
    constructor Create(aAction: TDBCustomHTMLButtonElementAction);
  end;


  { TDBCustomHTMLButtonElementAction }
  TDBButtonAction = (baFirst,baPrior,baNext,baLast,baEdit,baAppend,baInsert,baPost,baCancel,baApplyUpdates);
  TDBButtonActions = Set of TDBButtonAction;

  TDBCustomHTMLButtonElementAction = class(THTMLCustomElementAction)
  private
    FAction: TDBButtonAction;
    FLink: TButtonActionDataLink;
    FOnDisableControl: TNotifyEvent;
    FOnEnableControl: TNotifyEvent;
    function GetDataSource: TDatasource;
    procedure SetDatasource(AValue: TDatasource);
  Protected
    Property Link : TButtonActionDataLink Read FLink;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property ButtonAction : TDBButtonAction Read FAction Write FAction default baPost;
    Property Datasource : TDatasource Read GetDataSource Write SetDatasource;
    Property OnEnableControl : TNotifyEvent Read FOnEnableControl Write FOnEnableControl;
    Property OnDisableControl : TNotifyEvent Read FOnDisableControl Write FOnDisableControl;
  end;

  TDBHTMLButtonElementAction = class(TDBCustomHTMLButtonElementAction)
  Published
    Property Events;
    Property CustomEvents;
    Property ElementID;
    Property CSSSelector;
    Property PreventDefault;
    Property StopPropagation;
    Property OnExecute;
    Property BeforeBind;
    Property AfterBind;
    Property Datasource;
    Property ButtonAction;
    Property OnEnableControl;
    Property OnDisableControl;
  end;

Implementation

{ TButtonActionDataLink }


constructor TButtonActionDataLink.Create(
  aAction: TDBCustomHTMLButtonElementAction);
begin
  Inherited Create;
  Faction:=aAction;
end;

{ TDBCustomHTMLButtonElementAction }

function TDBCustomHTMLButtonElementAction.GetDataSource: TDatasource;
begin
  Result:=Link.DataSource;
end;

procedure TDBCustomHTMLButtonElementAction.SetDatasource(AValue: TDatasource);
begin
  Link.DataSource:=aValue;
end;


constructor TDBCustomHTMLButtonElementAction.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLink:=TButtonActionDataLink.Create(Self);
  FAction:=baPost;
end;

destructor TDBCustomHTMLButtonElementAction.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;



{ TDBCustomHTMLInputElementAction }

function TDBCustomHTMLInputElementAction.GetDataSource: TDatasource;
begin
  Result:=Link.DataSource;
end;


function TDBCustomHTMLInputElementAction.GetFieldName: String;
begin
  Result:=Link.FieldName;
end;

procedure TDBCustomHTMLInputElementAction.SetDatasource(AValue: TDatasource);
begin
  if aValue=Link.DataSource then exit;
  Link.Datasource:=aValue;
end;

procedure TDBCustomHTMLInputElementAction.SetFieldName(AValue: String);
begin
  Link.FieldName:=aValue;
end;



constructor TDBCustomHTMLInputElementAction.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLink:=THTMLActionDataLink.Create(Self);
end;

destructor TDBCustomHTMLInputElementAction.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;


{ THTMLActionDataLink }

procedure THTMLActionDataLink.SetFieldName(AValue: string);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
end;



constructor THTMLActionDataLink.Create(aAction: TDBCustomHTMLInputElementAction);
begin
  Inherited Create;
  FAction:=aAction;
end;


end.
