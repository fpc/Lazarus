{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    IDE interface to the items in the new dialog.
}
unit NewItemIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  // Flags/Options for the items
  TNewIDEItemFlag = (
    niifCopy,
    niifInherited,
    niifUse
    );
  TNewIDEItemFlags = set of TNewIDEItemFlag;

  TNewIDEItemTemplate = class;


  { TNewIDEItemCategory }

  TNewIDEItemCategory = class
  private
    FVisibleInNewDialog: boolean;
  protected
    FName: string;
    function GetCount: integer; virtual; abstract;
    function GetItems(Index: integer): TNewIDEItemTemplate; virtual; abstract;
  public
    constructor Create(const AName: string); virtual;
    procedure Clear; virtual; abstract;
    procedure Add(ATemplate: TNewIDEItemTemplate); virtual; abstract;
    function LocalizedName: string;  virtual; abstract;
    function Description: string;  virtual; abstract;
    function IndexOfCategory(const CategoryName: string): integer; virtual; abstract;
    function FindCategoryByName(const CategoryName: string): TNewIDEItemCategory; virtual; abstract;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TNewIDEItemTemplate read GetItems; default;
    property Name: string read FName;
    property VisibleInNewDialog: boolean read FVisibleInNewDialog write FVisibleInNewDialog;
  end;


  { TNewIDEItemCategories }

  TNewIDEItemCategories = class
  protected
    function GetItems(Index: integer): TNewIDEItemCategory; virtual; abstract;
    procedure SetItems(Index: integer; const AValue: TNewIDEItemCategory); virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    procedure Add(ACategory: TNewIDEItemCategory); virtual; abstract;
    Procedure Add(ACategoryName : String); virtual; abstract; 
    function Count: integer; virtual; abstract;
    function IndexOf(const CategoryName: string): integer; virtual; abstract;
    function FindByName(const CategoryName: string): TNewIDEItemCategory; virtual; abstract;
    procedure RegisterItem(const Paths: string; NewItem: TNewIDEItemTemplate); virtual; abstract;
    procedure UnregisterItem(NewItem: TNewIDEItemTemplate); virtual; abstract;
    function FindCategoryByPath(const Path: string;
                                ErrorOnNotFound: boolean): TNewIDEItemCategory; virtual; abstract;
  public
    property Items[Index: integer]: TNewIDEItemCategory
                                          read GetItems write SetItems; default;
  end;


var
  NewIDEItems: TNewIDEItemCategories;// will be set by the IDE

type
  { TNewIDEItemTemplate }

  TNewIDEItemTemplate = class(TPersistent)
  private
    fCategory: TNewIDEItemCategory;
    FVisibleInNewDialog: boolean;
  protected
    FAllowedFlags: TNewIDEItemFlags;
    FDefaultFlag: TNewIDEItemFlag;
    FName: string;
  public
    constructor Create(const AName: string; ADefaultFlag: TNewIDEItemFlag;
                       TheAllowedFlags: TNewIDEItemFlags);
    function LocalizedName: string; virtual;
    function Description: string; virtual;
    function CreateCopy: TNewIDEItemTemplate; virtual;
    procedure Assign(Source: TPersistent); override;
  public
    property DefaultFlag: TNewIDEItemFlag read FDefaultFlag;
    property AllowedFlags: TNewIDEItemFlags read FAllowedFlags;
    property Name: string read FName;
    property Category: TNewIDEItemCategory read fCategory write fCategory; // main category
    property VisibleInNewDialog: boolean read FVisibleInNewDialog write FVisibleInNewDialog;
  end;
  TNewIDEItemTemplateClass = class of TNewIDEItemTemplate;
  

procedure RegisterNewDialogItem(const Paths: string;
  NewItem: TNewIDEItemTemplate);
procedure UnregisterNewDialogItem(NewItem: TNewIDEItemTemplate);

procedure RegisterNewItemCategory(const ACategory: String);


implementation


procedure RegisterNewItemCategory(const ACategory: String);
begin
  NewIdeItems.Add(ACategory);
end;

procedure RegisterNewDialogItem(const Paths: string;
  NewItem: TNewIDEItemTemplate);
begin
  if NewIDEItems=nil then
    raise Exception.Create('RegisterNewDialogItem NewIDEItems=nil');
  NewIDEItems.RegisterItem(Paths,NewItem);
end;

procedure UnregisterNewDialogItem(NewItem: TNewIDEItemTemplate);
begin
  if NewIDEItems=nil then
    raise Exception.Create('RegisterNewDialogItem NewIDEItems=nil');
  NewIDEItems.UnregisterItem(NewItem);
end;

{ TNewIDEItemCategory }

constructor TNewIDEItemCategory.Create(const AName: string);
begin
  FVisibleInNewDialog:=true;
end;

{ TNewIDEItemTemplate }

constructor TNewIDEItemTemplate.Create(const AName: string;
  ADefaultFlag: TNewIDEItemFlag; TheAllowedFlags: TNewIDEItemFlags);
begin
  FName:=AName;
  FDefaultFlag:=ADefaultFlag;
  FAllowedFlags:=TheAllowedFlags;
  FVisibleInNewDialog:=true;
  Include(FAllowedFlags,FDefaultFlag);
end;

function TNewIDEItemTemplate.LocalizedName: string;
begin
  Result:=Name;
end;

function TNewIDEItemTemplate.Description: string;
begin
  Result:='<Description not set>';
end;

function TNewIDEItemTemplate.CreateCopy: TNewIDEItemTemplate;
begin
  Result:=TNewIDEItemTemplateClass(ClassType).Create(
                                                 Name,DefaultFlag,AllowedFlags);
  Result.Assign(Self);
end;

procedure TNewIDEItemTemplate.Assign(Source: TPersistent);
var
  Src: TNewIDEItemTemplate;
begin
  if Source is TNewIDEItemTemplate then begin
    Src:=TNewIDEItemTemplate(Source);
    FName:=Src.Name;
    FDefaultFlag:=Src.DefaultFlag;
    FAllowedFlags:=Src.AllowedFlags;
  end else
    inherited Assign(Source);
end;

end.

