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
    Extension for the Object Inspector.
    - Favourites properties
}
unit ObjInspExt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectInspector, Forms, Controls, Buttons, StdCtrls,
  ExtCtrls, Dialogs, LCLProc,
  FileUtil, LazConf, ConfigStorage, LazarusIDEStrConsts;
  
type
  { TOIAddRemoveFavouriteDlg }

  TOIAddRemoveFavouriteDlg = class(TForm)
    NoteLabel: TLabel;
    ClassCombobox: TComboBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
  private
    FAddMode: Boolean;
    FObjectInspector: TObjectInspector;
    FPropertyName: string;
    procedure SetAddMode(const AValue: Boolean);
    procedure SetObjectInspector(const AValue: TObjectInspector);
    procedure UpdateLabel;
    procedure UpdateComboBox;
    procedure UpdateMode;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property ObjectInspector: TObjectInspector read FObjectInspector
                                               write SetObjectInspector;
    property PropertyName: string read FPropertyName;
    property AddMode: Boolean read FAddMode write SetAddMode;
  end;

const
  DefaultOIFavouriteConfigFilename = 'objectinspectorfavourites.xml';

var
  DefaultOIFavouriteProperties: TOIFavouriteProperties;

function ShowAddRemoveFavouriteDialog(ObjInspector: TObjectInspector;
  Add: Boolean): TModalResult;
function CreateDefaultOIFavouriteProperties: TOIFavouriteProperties;
function LoadOIFavouriteProperties: TOIFavouriteProperties;
procedure SaveOIFavouriteProperties(Favourites: TOIFavouriteProperties);
function GetOIFavouriteConfigFilename: string;

implementation

function CreateDefaultOIFavouriteProperties: TOIFavouriteProperties;

  procedure Add(ABaseClass: TPersistentClass; const APropertyName: string);
  begin
    Result.Add(TOIFavouriteProperty.Create(ABaseClass,APropertyName,true));
  end;

begin
  Result:=TOIFavouriteProperties.Create;
  // TControl
  Add(TComponent,'Name');
  Add(TControl,'Anchors');
  Add(TControl,'Caption');
  Add(TControl,'OnClick');
  // miscellaneous
  Add(TCustomGroupBox,'Align');
  Add(TCustomImage,'Align');
  Add(TCustomButton,'ModalResult');
  Add(TCustomLabel,'WordWrap');
  Add(TCustomEdit,'Text');
  Add(TCustomMemo,'Lines');
  Add(TCustomCheckBox,'Checked');
  Add(TCustomRadioGroup,'Items');
  Add(TCustomRadioGroup,'ItemIndex');
  Result.DeleteDoubles;
end;

function ShowAddRemoveFavouriteDialog(ObjInspector: TObjectInspector;
  Add: Boolean): TModalResult;
var
  OIAddRemoveFavouriteDlg: TOIAddRemoveFavouriteDlg;
begin
  OIAddRemoveFavouriteDlg:=TOIAddRemoveFavouriteDlg.Create(nil);
  OIAddRemoveFavouriteDlg.ObjectInspector:=ObjInspector;
  OIAddRemoveFavouriteDlg.AddMode:=Add;
  Result:=OIAddRemoveFavouriteDlg.ShowModal;
  OIAddRemoveFavouriteDlg.Free;
end;

function LoadOIFavouriteProperties: TOIFavouriteProperties;
var
  ConfigStore: TConfigStorage;
begin
  Result:=DefaultOIFavouriteProperties.CreateCopy;
  {$IFDEF DebugFavouriteroperties}
  debugln('LoadOIFavouriteProperties A FileExists(GetOIFavouriteConfigFilename)=',dbgs(FileExists(GetOIFavouriteConfigFilename)));
  Result.WriteDebugReport;
  {$ENDIF}
  if not FileExists(GetOIFavouriteConfigFilename) then exit;
  try
    ConfigStore:=DefaultConfigClass.Create(GetOIFavouriteConfigFilename,true);
    try
      Result.MergeConfig(ConfigStore,'ObjectInspector/Favourites/');
      Result.Modified:=false;
    finally
      ConfigStore.Free;
    end;
  except
    on E: Exception do begin
      debugln('Error: LoadOIFavouriteProperties: unable to read ',
              GetOIFavouriteConfigFilename);
    end;
  end;
end;

procedure SaveOIFavouriteProperties(Favourites: TOIFavouriteProperties);
var
  ConfigStore: TConfigStorage;
  DefaultFavourites: TOIFavouriteProperties;
begin
  {$IFDEF DebugFavouriteroperties}
  debugln('SaveOIFavouriteProperties Favourites.Modified=',dbgs(Favourites.Modified),
    ' FileExists(GetOIFavouriteConfigFilename)=',dbgs(FileExists(GetOIFavouriteConfigFilename)));
  {$ENDIF}
  if (not Favourites.Modified) and FileExists(GetOIFavouriteConfigFilename)
  then
    exit;
  DefaultFavourites:=CreateDefaulTOIFavouriteProperties;
  try
    if DefaultFavourites.IsEqual(Favourites) then exit;
    {$IFDEF DebugFavouriteroperties}
    debugln('SaveOIFavouriteProperties is not default');
    DefaultFavourites.WriteDebugReport;
    Favourites.WriteDebugReport;
    {$ENDIF}
    try
      ConfigStore:=DefaultConfigClass.Create(GetOIFavouriteConfigFilename,false);
      try
        Favourites.SaveNewItemsToConfig(ConfigStore,'ObjectInspector/Favourites/',
                                        DefaultFavourites);
        ConfigStore.WriteToDisk;
        Favourites.Modified:=false;
      finally
        ConfigStore.Free;
      end;
    except
      on E: Exception do begin
        debugln('Error: LoadOIFavouriteProperties: unable to write ',
                GetOIFavouriteConfigFilename);
      end;
    end;
  finally
    DefaultFavourites.Free;
  end;
end;

function GetOIFavouriteConfigFilename: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+DefaultOIFavouriteConfigFilename;
end;

{ TOIAddRemoveFavouriteDlg }

procedure TOIAddRemoveFavouriteDlg.OkButtonClick(Sender: TObject);
var
  NewClassName: String;
  CurClass: TClass;
  NewFavourite: TOIFavouriteProperty;
begin
  NewClassName:=ClassCombobox.Text;
  if (ObjectInspector<>nil) and (ObjectInspector.Selection<>nil)
  and (ObjectInspector.Selection.Count>0) then begin
    CurClass:=ObjectInspector.Selection[0].ClassType;
    while CurClass.InheritsFrom(TPersistent) do begin
      if CompareText(NewClassName,CurClass.ClassName)=0 then begin
        NewFavourite:=TOIFavouriteProperty.Create(TPersistentClass(CurClass),
                                                  PropertyName,AddMode);
        ObjectInspector.Favourites.DeleteConstraints(NewFavourite);
        ObjectInspector.Favourites.Add(NewFavourite);
        ObjectInspector.FavouriteGrid.BuildPropertyList;
        ModalResult:=mrOk;
        exit;
      end;
      CurClass:=CurClass.ClassParent;
    end;
  end;
  MessageDlg(lisClassNotFound, Format(lisOIFClassNotFound, ['"', NewClassName,
    '"']), mtError,
             [mbOk],0);
end;

procedure TOIAddRemoveFavouriteDlg.SetObjectInspector(const AValue: TObjectInspector
  );
var
  CurRow: TOIPropertyGridRow;
begin
  if FObjectInspector=AValue then exit;
  FObjectInspector:=AValue;
  CurRow:=ObjectInspector.GetActivePropertyRow;
  if (CurRow<>nil) and (CurRow.Editor<>nil) then
    FPropertyName:=CurRow.Editor.GetName;
  UpdateLabel;
  UpdateComboBox;
end;

procedure TOIAddRemoveFavouriteDlg.SetAddMode(const AValue: Boolean);
begin
  if FAddMode=AValue then exit;
  FAddMode:=AValue;
  UpdateMode;
end;

procedure TOIAddRemoveFavouriteDlg.UpdateLabel;
begin
  NoteLabel.Caption:=Format(lisOIFChooseABaseClassForTheFavouriteProperty, [
    '"', PropertyName, '"']);
end;

procedure TOIAddRemoveFavouriteDlg.UpdateComboBox;
var
  CurClass: TClass;
  NewItems: TStringList;
begin
  NewItems:=TStringList.Create;
  if (ObjectInspector<>nil) and (ObjectInspector.Selection<>nil)
  and (ObjectInspector.Selection.Count>0) then begin
    CurClass:=ObjectInspector.Selection[0].ClassType;
    // add only classes, that are TPersistent and have a registered class
    while CurClass.InheritsFrom(TPersistent) do begin
      // add only registered classes
      if GetClass(CurClass.ClassName)<>nil then
        NewItems.Add(CurClass.ClassName);
      CurClass:=CurClass.ClassParent;
    end;
  end;
  ClassCombobox.Items.Assign(NewItems);
  if ClassCombobox.Items.Count>0 then
    ClassCombobox.ItemIndex:=0;
  NewItems.Free;
end;

procedure TOIAddRemoveFavouriteDlg.UpdateMode;
begin
  if AddMode then begin
    Caption:=lisOIFAddToFavouriteProperties;
    OkButton.Caption:=lisCodeTemplAdd;
  end else begin
    Caption:=lisOIFRemoveFromFavouriteProperties;
    OkButton.Caption:=lisExtToolRemove;
  end;
end;

constructor TOIAddRemoveFavouriteDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  Name:='OIAddToFavouriteDlg';
  Width:=300;
  Height:=150;
  Position:=poDesktopCenter;
  
  NoteLabel:=TLabel.Create(Self);
  with NoteLabel do begin
    Name:='NoteLabel';
    SetBounds(5,5,Self.ClientWidth-10,50);
    WordWrap:=true;
    Parent:=Self;
  end;
  
  ClassCombobox:=TComboBox.Create(Self);
  with ClassCombobox do begin
    Name:='ClassCombobox';
    SetBounds(5,60,200,Height);
    Parent:=Self;
  end;
  
  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='AddButton';
    SetBounds(5,100,80,25);
    Caption:=lisCodeTemplAdd;
    Parent:=Self;
    OnClick:=@OkButtonClick;
  end;
  DefaultControl:=OkButton;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    SetBounds(120,100,80,25);
    Caption:=dlgCancel;
    Parent:=Self;
    ModalResult:=mrCancel;
  end;
  CancelControl:=CancelButton;
  
  UpdateMode;
end;

initialization
  DefaultOIFavouriteProperties:=CreateDefaultOIFavouriteProperties;

end.

