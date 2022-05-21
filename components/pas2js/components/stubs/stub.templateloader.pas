{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt

    This unit implements a HTML template loader.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Stub.TemplateLoader;

{$mode objfpc}
{$h+}

interface

uses
  Classes, SysUtils;

Type

  TTemplateNotifyEvent = Procedure(Sender : TObject; Const aTemplate : String) of object;
  TTemplateErrorNotifyEvent = Procedure(Sender : TObject; Const aTemplate,aError : String; aErrorcode : Integer) of object;


  { TCustomTemplateLoader }

  { TPreLoadTemplateItem }

  TPreLoadTemplateItem = Class(TCollectionItem)
  private
    FHTMLFile: String;
    FName: string;
    function GetHTMLFile: String;
  protected
    function GetDisplayName: string; override;
  Public
    Procedure Assign(aSource : TPersistent); override;
  Published
    Property Name: string Read FName Write FName;
    Property HTMLFile : String Read GetHTMLFile Write FHTMLFile;
  end;

  { TPreLoadTemplateItemList }

  TPreLoadTemplateItemList = Class(TCollection)
  private
    function GetT(aIndex : Integer): TPreLoadTemplateItem;
  Public
    Property Template[aIndex : Integer] : TPreLoadTemplateItem  Read GetT;default;
  end;

  TCustomTemplateLoader = Class(TComponent)
  Private
    FBaseURL: String;
    FCheckResources: Boolean;
    FOnLoad: TTemplateNotifyEvent;
    FOnLoadFail: TTemplateErrorNotifyEvent;
    FPreload: TPreLoadTemplateItemList;
    procedure SetPreload(AValue: TPreLoadTemplateItemList);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // URLs will be relative to this. Take care that you add a / at the end if needed !
    Property BaseURL : String Read FBaseURL Write FBaseURl;
    // Check resources for templates when accessing Templates.
    Property CheckResources : Boolean Read FCheckResources Write FCheckResources;
    // Called when a template was loaded.
    Property OnLoad : TTemplateNotifyEvent Read FOnLoad Write FOnLoad;
    // Called when a template failed to load.
    Property OnLoadFail : TTemplateErrorNotifyEvent Read FOnLoadFail Write FOnLoadFail;
    // templates to load when the component is loaded.
    Property PreloadTemplates : TPreLoadTemplateItemList Read FPreload Write SetPreload;
  end;

  TTemplateLoader = Class(TCustomTemplateLoader)
  Published
    Property BaseURL;
    Property CheckResources;
    Property PreloadTemplates;
    Property OnLoad;
    Property OnLoadFail;
  end;

// Global instance, for ease of use.
Function GlobalTemplates : TCustomTemplateLoader;

implementation


Function GlobalTemplates : TCustomTemplateLoader;

begin
  Result:=Nil;
end;

{ TPreLoadTemplateItemList }

function TPreLoadTemplateItemList.GetT(aIndex : Integer): TPreLoadTemplateItem;
begin
  Result:=Items[aIndex] as TPreLoadTemplateItem;
end;

{ TCustomTemplateLoader }

procedure TCustomTemplateLoader.SetPreload(AValue: TPreLoadTemplateItemList);
begin
  if FPreload=AValue then Exit;
  FPreload.Assign(AValue);
end;

constructor TCustomTemplateLoader.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPreload:=TPreLoadTemplateItemList.Create(TPreLoadTemplateItem);
end;

destructor TCustomTemplateLoader.Destroy;
begin
  FreeAndNil(FPreload);
  inherited Destroy;
end;

{ TPreLoadTemplateItem }

function TPreLoadTemplateItem.GetHTMLFile: String;
begin
  Result:=FHTMLFile;
  If (Result='') and (Name<>'') then
    Result:=LowerCase(Name)+'.html';
end;

function TPreLoadTemplateItem.GetDisplayName: string;
begin
  Result:=Name;
  if Result='' then
    Result:=inherited GetDisplayName;
end;

procedure TPreLoadTemplateItem.Assign(aSource: TPersistent);

Var
  PLI : TPreLoadTemplateItem absolute aSource;

begin
  if aSource is TPreLoadTemplateItem then
    begin
    FName:=PLI.Name;
    FHTMLFile:=PLI.FHTMLFile;
    end
  else
    inherited Assign(aSource);
end;



end.

