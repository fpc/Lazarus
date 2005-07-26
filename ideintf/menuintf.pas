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
    Interface to the IDE menus.
}
unit MenuIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, ImgList, Graphics, IDECommands;
  
type
  TIDEMenuSection = class;

  { TIDEMenuItem
    A menu item in one of the IDE's menus.
    This is only the base class for TIDEMenuSection and TIDEMenuCommand }
    
  TIDEMenuItem = class(TPersistent)
  private
    FBitmap: TBitmap;
    FCaption: string;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FMenuItem: TMenuItem;
    FName: string;
    FOnClick: TNotifyEvent;
    FSection: TIDEMenuSection;
    FVisible: Boolean;
    FHint: string;
    procedure SetEnabled(const AValue: Boolean);
  protected
    function GetBitmap: TBitmap; virtual;
    function GetCaption: string; virtual;
    function GetHint: String; virtual;
    procedure SetBitmap(const AValue: TBitmap); virtual;
    procedure SetCaption(const AValue: string); virtual;
    procedure SetHint(const AValue: String); virtual;
    procedure SetImageIndex(const AValue: Integer); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetSection(const AValue: TIDEMenuSection); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
  public
    constructor Create(const TheName: string); virtual;
    destructor Destroy; override;
    function HasBitmap: Boolean;
  public
    property Name: string read FName write SetName;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Hint: String read GetHint write SetHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Visible: Boolean read FVisible write SetVisible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: string read GetCaption write SetCaption;
    property Section: TIDEMenuSection read FSection write SetSection;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
  end;
  TIDEMenuItemClass = class of TIDEMenuItem;
  
  { TIDEMenuSection
     }
  
  TIDEMenuSection = class(TIDEMenuItem)
  private
    FChildsAsSubMenu: boolean;
    FSubMenuImages: TCustomImageList;
  protected
    procedure SetChildsAsSubMenu(const AValue: boolean); virtual;
    procedure SetSubMenuImages(const AValue: TCustomImageList); virtual;
  public
    constructor Create(const TheName: string); override;
  public
    property ChildsAsSubMenu: boolean read FChildsAsSubMenu
                                          write SetChildsAsSubMenu default true;
    property SubMenuImages: TCustomImageList read FSubMenuImages
                                             write SetSubMenuImages;
  end;
  TIDEMenuSectionClass = class of TIDEMenuSection;

  { TIDEMenuCommand
    A leaf menu item. No childs.
    An IDE command can be assigned, which defines the shortcut.
  }

  TIDEMenuCommand = class(TIDEMenuItem)
  private
    FAutoCheck: boolean;
    FChecked: Boolean;
    FCommand: TIDECommandKeys;
    FDefault: Boolean;
    FGroupIndex: Byte;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShowAlwaysCheckable: boolean;
  protected
    procedure SetAutoCheck(const AValue: boolean); virtual;
    procedure SetChecked(const AValue: Boolean); virtual;
    procedure SetDefault(const AValue: Boolean); virtual;
    procedure SetGroupIndex(const AValue: Byte); virtual;
    procedure SetRadioItem(const AValue: Boolean); virtual;
    procedure SetRightJustify(const AValue: boolean); virtual;
    procedure SetShowAlwaysCheckable(const AValue: boolean); virtual;
    procedure SetCommand(const AValue: TIDECommandKeys); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); override;
  public
    property Command: TIDECommandKeys read FCommand write SetCommand;
    property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
    property RadioItem: Boolean read FRadioItem write SetRadioItem;
    property RightJustify: boolean read FRightJustify write SetRightJustify;
    property ShowAlwaysCheckable: boolean read FShowAlwaysCheckable
                                          write SetShowAlwaysCheckable;
  end;
  TIDEMenuCommandClass = class of TIDEMenuCommand;

implementation

{ TIDEMenuItem }

procedure TIDEMenuItem.SetEnabled(const AValue: Boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
end;

function TIDEMenuItem.GetBitmap: TBitmap;
begin
  if FBitmap=nil then
    FBitmap:=TBitmap.Create;
  FBitmap.Transparent:=True;
  Result:=FBitmap;
end;

function TIDEMenuItem.GetCaption: string;
begin
  if FCaption<>'' then
    Result:=FCaption
  else
    Result:=FName;
end;

function TIDEMenuItem.GetHint: String;
begin
  Result:=FHint;
end;

procedure TIDEMenuItem.SetBitmap(const AValue: TBitmap);
begin
  if FBitmap=AValue then exit;
  if AValue<>nil then
    Bitmap.Assign(AValue)
  else
    FBitmap.Free;
  if MenuItem<>nil then
    MenuItem.Bitmap:=FBitmap;
end;

procedure TIDEMenuItem.SetCaption(const AValue: string);
begin
  FCaption:=AValue;
  if MenuItem<>nil then
    MenuItem.Caption:=Caption;
end;

procedure TIDEMenuItem.SetHint(const AValue: String);
begin
  FHint:=AValue;
  if MenuItem<>nil then
    MenuItem.Hint:=Hint;
end;

procedure TIDEMenuItem.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if MenuItem<>nil then
    MenuItem.ImageIndex:=ImageIndex;
end;

procedure TIDEMenuItem.SetMenuItem(const AValue: TMenuItem);
begin
  if FMenuItem=AValue then exit;
  FMenuItem:=AValue;
  if MenuItem<>nil then begin
    MenuItem.Caption:=Caption;
    MenuItem.Bitmap:=FBitmap;
    MenuItem.Hint:=Hint;
    MenuItem.ImageIndex:=ImageIndex;
    MenuItem.Visible:=Visible;
  end;
end;

procedure TIDEMenuItem.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

procedure TIDEMenuItem.SetSection(const AValue: TIDEMenuSection);
begin
  if FSection=AValue then exit;
  FSection:=AValue;
end;

procedure TIDEMenuItem.SetVisible(const AValue: Boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if MenuItem<>nil then
    MenuItem.Visible:=Visible;
end;

constructor TIDEMenuItem.Create(const TheName: string);
begin
  inherited Create;
  FName:=TheName;
  FEnabled:=true;
end;

destructor TIDEMenuItem.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TIDEMenuItem.HasBitmap: Boolean;
begin
  Result:=FBitmap<>nil;
end;

{ TIDEMenuSection }

procedure TIDEMenuSection.SetSubMenuImages(const AValue: TCustomImageList);
begin
  if FSubMenuImages=AValue then exit;
  FSubMenuImages:=AValue;
end;

constructor TIDEMenuSection.Create(const TheName: string);
begin
  inherited Create(TheName);
  FChildsAsSubMenu:=true;
end;

procedure TIDEMenuSection.SetChildsAsSubMenu(const AValue: boolean);
begin
  if FChildsAsSubMenu=AValue then exit;
  FChildsAsSubMenu:=AValue;
end;

{ TIDEMenuCommand }

procedure TIDEMenuCommand.SetAutoCheck(const AValue: boolean);
begin
  if FAutoCheck=AValue then exit;
  FAutoCheck:=AValue;
  if MenuItem<>nil then MenuItem.AutoCheck:=AutoCheck;
end;

procedure TIDEMenuCommand.SetChecked(const AValue: Boolean);
begin
  if FChecked=AValue then exit;
  FChecked:=AValue;
  if MenuItem<>nil then MenuItem.Checked:=Checked;
end;

procedure TIDEMenuCommand.SetDefault(const AValue: Boolean);
begin
  if FDefault=AValue then exit;
  FDefault:=AValue;
  if MenuItem<>nil then MenuItem.Default:=Default;
end;

procedure TIDEMenuCommand.SetGroupIndex(const AValue: Byte);
begin
  if FGroupIndex=AValue then exit;
  FGroupIndex:=AValue;
  if MenuItem<>nil then
    MenuItem.GroupIndex:=GroupIndex;
end;

procedure TIDEMenuCommand.SetRadioItem(const AValue: Boolean);
begin
  if FRadioItem=AValue then exit;
  FRadioItem:=AValue;
  if MenuItem<>nil then
    MenuItem.RadioItem:=RadioItem;
end;

procedure TIDEMenuCommand.SetRightJustify(const AValue: boolean);
begin
  if FRightJustify=AValue then exit;
  FRightJustify:=AValue;
  if MenuItem<>nil then
    MenuItem.RightJustify:=RightJustify;
end;

procedure TIDEMenuCommand.SetShowAlwaysCheckable(const AValue: boolean);
begin
  if FShowAlwaysCheckable=AValue then exit;
  FShowAlwaysCheckable:=AValue;
  if MenuItem<>nil then
    MenuItem.ShowAlwaysCheckable:=ShowAlwaysCheckable;
end;

procedure TIDEMenuCommand.SetCommand(const AValue: TIDECommandKeys);
begin
  if FCommand=AValue then exit;
  FCommand:=AValue;
  if MenuItem<>nil then
    MenuItem.ShortCut:=IDEShortCutToMenuShortCut(AValue.KeyA);
end;

procedure TIDEMenuCommand.SetMenuItem(const AValue: TMenuItem);
begin
  inherited SetMenuItem(AValue);
  if MenuItem<>nil then begin
    MenuItem.AutoCheck:=AutoCheck;
    MenuItem.Checked:=Checked;
    MenuItem.Default:=Default;
    MenuItem.RadioItem:=RadioItem;
    MenuItem.RightJustify:=RightJustify;
    MenuItem.ShowAlwaysCheckable:=ShowAlwaysCheckable;
    if Command<>nil then
      MenuItem.ShortCut:=IDEShortCutToMenuShortCut(Command.KeyA)
    else
      MenuItem.ShortCut:=ShortCut(0,[]);
    MenuItem.GroupIndex:=GroupIndex;
  end;
end;

end.

