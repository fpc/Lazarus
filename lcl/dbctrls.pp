{ $Id$}
{
 /***************************************************************************
                               DbCtrls.pp
                             -------------------
                     An interface to DB aware Controls
                     Initial Revision : Sun Sep 14 2003


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{
@abstract(common db aware controls, as in Delphi)
@author(Andrew Johnson <acjgenius@@earthlink.net>)
@created(Sun Sep 14 2003)
@lastmod($Date$)
}
unit DbCtrls;

{$mode objfpc}
{$H+}

interface          

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  MaskEdit, LMessages;

Type
  { TFieldDataLink }
  TFieldDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: string;

    FControl: TComponent;

    // Curent State of Affairs
    FEditing: Boolean;
    IsModified: Boolean;

    // Callbacks
    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;

    function FieldCanModify: Boolean;
    function GetCanModify: Boolean;

    // set current field
    procedure SetFieldName(const Value: string);

    // make sure the field/fieldname is valid before we do stuff with it
    Function ValidateField : Boolean;
  protected
    // Testing Events
    procedure ActiveChanged; override;
    Procedure DataSetChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(aField: TField); override;
    procedure UpdateData; override;
  public
    constructor Create;
    destructor Destroy; override;

    // for control intitiating db changes etc
    function Edit: Boolean;

    procedure Modified;
    procedure Reset;

    // Attatched control
    property Control: TComponent read FControl write FControl;


    // Basic DB interfaces
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;

    // Curent State of DB
    property CanModify: Boolean read GetCanModify;
    property Editing: Boolean read FEditing;

    // Our Callbacks
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

 { TDBEdit }
  TDBEdit = class(TCustomMaskEdit)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);

    procedure SetDataField(Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure KeyPress(var Key: Char); override;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    function EditCanModify: Boolean; override;

    procedure Change; override;
    procedure Reset; override;
    procedure SetFocus; override;

    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  { TDBText }
  TDBText = class(TLabel)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    procedure SetDataField(Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  { TDBListBox }
  TDBListBox = class(TCustomListBox)
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    Procedure SetItems(Values : TStrings);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);

    procedure SetDataField(Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure KeyPress(var Key: Char); override;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure Click; override;

    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    // we need to overrride the write method for db aware.
    // the Read isn't an issue since the list will be updated
    // on data change anyway
    property Items write SetItems;

    //same as dbedit need to match the datalink status
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

{TFieldDataLink  Private Methods}

{hack around broken Field method by using this instead}
function TFieldDataLink.FieldCanModify: Boolean;
begin
  Result:=Not FField.ReadOnly;
  If Result then
    begin
    Result:=Assigned(FField.DataSet);
    If Result then
      Result:=FField.DataSet.CanModify;
    end;
end;

{
  If the field exists and can be modified, then
  we CanModify as long as this hasn't been set
  ReadOnly somewhere else. Do we need any extra tests here?
}
function TFieldDataLink.GetCanModify: Boolean;
begin
  if Assigned(FField) and (FieldCanModify) then
     Result := not ReadOnly
  else
    Result := False;
end;

{
  Set the FieldName and then update the field to match,

  If we are changing the field from a previously valid field
  we need to make sure the editing state is updated, and that the
  DataChanged method is called, easiest way I think is to set the field
  to nil then call EditingChanged and Reset before we actually set the
  proper field. This way if this turns out to be an invalid fieldname we
  are already nil anyway and all the changes to state have been made.

  Next we look up the FieldByName on the attatched DataSource.. which we have
  to make sure exists first. assuming this worked properly, then we will have a
  valid Field again, so now we need to update the Editing state again
  and call the DataChanged again, so Reset. And then we are done... I think.

  If I am missing anything or am doing this all wrong please fix :)
}
procedure TFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName :=  Value;
    If Assigned(FField) then begin
      FField := nil;
      EditingChanged;
      Reset;
    end;

    If Assigned(DataSource) and Assigned(DataSource.DataSet) then
      FField := DataSource.DataSet.FieldByName(FFieldName);

    If Assigned(FField) then begin
      EditingChanged;
      Reset;
    end;
  end;
end;

{
  Several functions seem to need to test the validity of
  the field/fieldname, so lets put it into its own function.
  I do not really know what all to put here so just check for a
  non empty FieldName and a non-nil Field, and if a valid name, but
  not a valid Field, try and re-call SetFieldName.
}
Function TFieldDataLink.ValidateField : Boolean;
var
  RealFieldName : String;
begin
  RealFieldName := FFieldName;
  If (RealFieldName <> '') and not Assigned(FField) then begin
    FFieldName := '';
    SetFieldName(RealFieldName);
  end;

  result := (RealFieldName <> '') and Assigned(FField);
end;


{TFieldDataLink  Protected Methods}

{ Delphi Help ->
    Changes to the Active property trigger the ActiveChanged method.
    If an OnActiveChange event handler is assigned, ActiveChanged calls
    this event handler. If ActiveChanged is triggered by a transition into
    an active state, then before calling the event handler, ActiveChanged makes
    sure that the Field for this TFieldDataLink is still valid.
  <-- Delphi Help

   So... just call event if exists? unles Active then we test validity of
   field. does this simply mean not nil? or is more involved?
   call mock routine for now
}
procedure TFieldDataLink.ActiveChanged;
begin
  if Active and not ValidateField
  then
    exit;

  if Assigned(FOnActiveChange) then
    FOnActiveChange(Self);
end;

{ not in the delphi version(well not int the help anyway)
  but the db version is calling RecordChange with nil,
  which is invalid if we have a real value, so just call reset
}
Procedure TFieldDataLink.DataSetChanged;
begin
  reset;
end;


{ Delphi Help ->
    Changing the field binding can change the validity of the CanModify
    property, since individual field components can disallow edits. If
    TFieldDataLink is in an editing state when the Field property is changed,
    EditingChanged checks the CanModify property. If CanModify is False, it
    changes back out of the editing state.

    Note: This differs significantly from the inherited EditingChanged method
    of TDataLink. The functionality of the inherited method is replaced in
    TFieldDataLink by the OnEditingChange event handler.
  <-- Delphi Help

  ok so another event... but this time we simply change modified state
  if Editing and not CanModify? or do we also change to match if
  if not Editing and CanModify? i.e If Editing <> CanModify??  Will assume
  the latter just in case. easy to change back if I am wrong.

  Also based on this we replace parent routine, so do we need to keep track
  of Editing state ourself? I hope this is right. Anyone know for sure?

  OK .. based on the Modified routine we need to turn off
  our IsModified routine when succesfull right? so for now just turn
  it off as per my example.
}
procedure TFieldDataLink.EditingChanged;
var
  RealEditState : Boolean;
begin
  RealEditState := (CanModify and Inherited Editing);

  if (FEditing <> RealEditState) then
  begin
    FEditing := RealEditState;
    IsModified := False;
    if Assigned(FOnEditingChange) then
      FOnEditingChange(Self);
  end;
end;

{ Delphi Help ->
    LayoutChanged is called after changes in the layout of one of the
    containers of the Control for this TFieldDataLink that might change the
    validity of its field binding. For example, if the Control is embedded
    within a TCustomDBGrid, and one of the columns is deleted, the Field
    property for the Control might become invalid.
  <-- Delphi Help

  So... just another field validity check? call our mock routine...
}
procedure TFieldDataLink.LayoutChanged;
begin
  ValidateField;
end;

{ Delphi Help ->
    Applications can not call this protected method. It is triggered
    automatically when the contents of the current record change.
    RecordChanged calls the OnDataChange event handler if there is one.
  <-- Delphi Help

  Ok so just a simple Event Handler.. what. no extra tests? we gotta
  have at least one.. :)

  yeah lets go ahead and make sure the field matches the
  internal one. can it ever not? and if not what about nil? do we
  need to do something special? maybe another test is needed later....

  does this only get called after Modified? assume so till I know otherwise
  and turn off IsModified.

  hah. same thing as Reset but with a test so lets just call Reset and let
  it do the work
}
procedure TFieldDataLink.RecordChanged(aField: TField);
begin
  if (aField = FField) then
     Reset;
end;

{ Delphi Help ->
    UpdateData overrides the default UpdateData method to call the
    OnUpdateData event handler where the data-aware control can write any
    pending edits to the record in the dataset.
  <-- Delphi Help

  where..can write pending events. So I guess when we have already
  called Modified? Aka if not IsModified exit otherwise call event?
  works for me.
}
procedure TFieldDataLink.UpdateData;
begin
  if not IsModified then
    exit;

  IsModified := False;

  if Assigned(FOnUpdateData) then
    FOnUpdateData(Self);
end;


{TFieldDataLink  Public Methods}

constructor TFieldDataLink.Create;
begin
  inherited Create;
  VisualControl := True;
  FField := nil;
  FFieldname := '';
end;

destructor TFieldDataLink.Destroy;
begin
  inherited Destroy;
end;

{ Delphi Help ->
    Use Edit to try to ensure that the contents of the field can be modified.
    A return value of True indicates that the field was already in an editing
    state, or that the DataSource was successfully changed to allow editing.
    A return value of False indicates that the DataSource could not be changed
    to allow editing. For example, if the CanModify property is False, Edit
    fails, and returns False.
  <-- Delphi Help

  ok so the way I see it, since the inherited function calls EditingChanged,
  which we have already overriden to modify our own Editing state if its invalid,
  I should just be calling the inherited routine here, but only if CanModify,
  since there is no point otherwise. But since we _are_ keeping track of editing
  state ourselves we return our own state, not the inherited. If anyone know
  better please fix.
}
function TFieldDataLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;

  Result := FEditing;
end;

{ Delphi Help ->
    Call Modified when the Control for this TFieldDataLink begins processing
    edits.
  <-- Delphi Help

  ok so. well _that's_ helpfull. for the moment going to keep track
  by adding an IsModified... based on the other functions thus far
  we need to know whether we are in state, so I am assuming it goes

  Call Modified ->
    IsModified:=True;//Waiting for modifications

  Call SomeFunction->
    If IsModified then begin
      (do something)
      IsModified := False;//All modifications complete
    end
    else
     (do something else? exit?);
}
procedure TFieldDataLink.Modified;
begin
  IsModified := True;
end;

{ Delphi Help ->
    The Control that owns a TFieldDataLink object calls its Reset method to
    process a UI action that cancels edits to the field. Reset calls the
    OnDataChange event handler without writing any pending changes to the
    record in the dataset.
  <-- Delphi Help

  hrmm. just call to the OnDataChange Event, and turn off IsModified I guess
  better ideas anyone?

  oh huh. same thing as RecordChanged but without the test so I can just
  have it call this instead :)
}
procedure TFieldDataLink.Reset;
begin
  if Assigned(FOnDataChange) then
    FOnDataChange(Self);

  IsModified := False;
end;

{$Include dbedit.inc}
{$Include dbtext.inc}
{$Include dblistbox.inc}

end.

{ =============================================================================

  $Log$
  Revision 1.2  2003/09/15 01:56:48  ajgenius
  Added TDBListBox. needs more work for ReadOnly

  Revision 1.1  2003/09/14 18:40:55  ajgenius
  add initial TFieldDataLink, TDBEdit and TDBText


}
