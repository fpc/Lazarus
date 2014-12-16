{
 /***************************************************************************
                               DbExtCtrls.pp
                               -------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit DBExtCtrls;

{$mode objfpc}
{$H+}

interface          

uses
  Classes, SysUtils, LCLType, LMessages,
  DB, DBCtrls, EditBtn;

type

  { TDBDateEdit }

  TDBDateEdit = class(TDateEdit)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    function GetField: TField;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function EditCanModify: Boolean; override;
    procedure EditEnter; override;
    procedure EditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EditKeyPress(var Key: Char); override;
    procedure ButtonClick; override;
    procedure EditChange; override;
    procedure EditExit; override;
    procedure Reset; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;

  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBDateEdit]);
end;

{$Include dbdateedit.inc}

end.
