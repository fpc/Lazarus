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
  ExtCtrls, Dialogs,
  ConfigStorage, LazarusIDEStrConsts;
  
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

function CreateDefaultOIFavouriteProperties: TOIFavouriteProperties;
function ShowAddRemoveFavouriteDialog(ObjInspector: TObjectInspector;
  Add: Boolean): TModalResult;
function LoadOIFavouriteProperties: TOIFavouriteProperties;
procedure SaveOIFavouriteProperties(ObjInspector: TObjectInspector);

implementation

function CreateDefaultOIFavouriteProperties: TOIFavouriteProperties;

  procedure Add(ABaseClass: TPersistentClass; const APropertyName: string);
  begin
    Result.Add(TOIFavouriteProperty.Create(ABaseClass,APropertyName,true));
  end;

begin
  Result:=TOIFavouriteProperties.Create;
  // TControl
  Add(TControl,'Name');
  Add(TControl,'Anchors');
  Add(TControl,'Caption');
  Add(TControl,'OnClick');
  // miscellaneous
  Add(TGroupBox,'Align');
  Add(TImage,'Align');
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
begin
  Result:=CreateDefaultOIFavouriteProperties;
  // TODO: load and merge
end;

procedure SaveOIFavouriteProperties(ObjInspector: TObjectInspector);
begin
  // TODO save only changes
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
  MessageDlg('Class not found','Class "'+NewClassName+'" not found.',mtError,
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
  NoteLabel.Caption:='Choose a base class for the favourite '
    +'property "'+PropertyName+'".';
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
    Caption:='Add to favourite properties';
    OkButton.Caption:='Add';
  end else begin
    Caption:='Remove from favourite properties';
    OkButton.Caption:='Remove';
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
    Caption:='Add';
    Parent:=Self;
    OnClick:=@OkButtonClick;
  end;
  DefaultControl:=OkButton;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    SetBounds(120,100,80,25);
    Caption:='Cancel';
    Parent:=Self;
    ModalResult:=mrCancel;
  end;
  CancelControl:=CancelButton;
  
  UpdateMode;
end;

end.

