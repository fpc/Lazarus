{
TDBDateTimePicker control for Lazarus
- - - - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

This unit is part of DateTimeCtrls package for Lazarus.
TDBDateTimePicker is data-aware version of TDateTimePicker control.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see the file COPYING.modifiedLGPL.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope this control will be useful.
}
unit DBDateTimePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateTimePicker, db, DBCtrls, LMessages;

type

  { TDBDateTimePicker }

  TDBDateTimePicker = class(TCustomDateTimePicker)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FReadOnly: Boolean;
    FDataChangeCount: Integer;
    FChangingCount: Integer;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure SetReadOnly(const AValue: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    function GetField: TField;
    procedure CheckField;
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure ConfirmChanges; override;
    procedure UndoChanges; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
    property CalendarWrapperClass;
    property DroppedDown;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;

    property ArrowShape;
    property ShowCheckBox;
    property Checked;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property AutoSize;
    property Font;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property BorderStyle;
    property BorderSpacing;
    property Enabled;
    property Color;
    property ParentColor;
    property DateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property LeadingZeros;
    property ShowHint;
    property ParentShowHint;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
    property NullInputAllowed;
    property Kind;
    property TimeSeparator;
    property TimeFormat;
    property TimeDisplay;
    property DateMode;
    property UseDefaultSeparators;
    property Cascade;
    property AutoButtonSize;
    property AutoAdvance;
    property HideDateTimeParts;
    property BiDiMode;
    property ParentBiDiMode;
    property MonthNames;
    property ShowMonthNames;
    property CalAlignment;
  //events:
    property OnChange;
    property OnCheckBoxChange;
    property OnDropDown;
    property OnCloseUp;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnUTF8KeyPress;
  end;

implementation

{ TDBDateTimePicker }

function TDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBDateTimePicker.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
  CheckField;
end;

procedure TDBDateTimePicker.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, AValue);
  CheckField;
end;

procedure TDBDateTimePicker.DataChange(Sender: TObject);
begin
  if (FChangingCount = 0) then begin
    Inc(FDataChangeCount);
    try
      if Assigned(FDataLink.Field) and not FDataLink.Field.IsNull then begin
        // Using the SetTheDateJumpMinMax procedure, instead of property
        SetDateTimeJumpMinMax(FDataLink.Field.AsDateTime); // assignment allows
            // this control to display dates from database whose value falls
            // outside of MinDate and MaxDate interval.
            // Note that user still cannot enter such values in the control.
      end else
        DateTime := NullDate;

    finally
      Dec(FDataChangeCount);
    end;
  end;
end;

procedure TDBDateTimePicker.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly <> AValue then begin
    FReadOnly := AValue;
    CheckField;
  end;
end;

procedure TDBDateTimePicker.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then begin
    if DateIsNull then
      FDataLink.Field.AsVariant := Null
    else
      FDataLink.Field.AsDateTime := DateTime;
  end;
end;

procedure TDBDateTimePicker.ActiveChange(Sender: TObject);
begin
  CheckField;
end;

function TDBDateTimePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBDateTimePicker.CheckField;
begin
  if (FDataLink.Active) and Assigned(FDataLink.Field) then
    inherited ReadOnly := FReadOnly or (not FDataLink.CanModify)
  else begin
    inherited ReadOnly := True;
    DateTime := NullDate;
  end;
end;

procedure TDBDateTimePicker.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBDateTimePicker.Change;
begin
  if (FDataChangeCount <= 0) and Assigned(FDataLink) then begin
    Inc(FChangingCount);
    try
      if FDataLink.Edit then begin
        FDataLink.Modified;
        inherited Change; // calls OnChange event handler
      end else
        FDataLink.Reset; // reverts user changes
    finally
      Dec(FChangingCount);
    end;
  end;
end;

procedure TDBDateTimePicker.ConfirmChanges;
begin
  inherited ConfirmChanges;

  if Assigned(FDataLink) then
    try
      FDataLink.UpdateRecord;
    except
      SetFocus;
      raise;
    end;

end;

procedure TDBDateTimePicker.UndoChanges;
begin
  FDataLink.Reset;

  inherited UndoChanges;
end;

constructor TDBDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataChangeCount := 0;
  FChangingCount := 0;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  DateTime := NullDate;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;

  CheckField;
end;

destructor TDBDateTimePicker.Destroy;
begin
  FDataLink.OnUpdateData := nil;
  FDataLink.OnDataChange := nil;
  FDataLink.OnActiveChange := nil;
  FreeAndNil(FDataLink);

  inherited Destroy;
end;

end.
