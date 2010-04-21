{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the EducationLaz package                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Michael Kuhardt

  Abstract:
    Frame to setup Properties and events
}
unit EduPropsEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, StdCtrls, ExtCtrls, LazConfigStorage, IDEOptionsIntf, EduOptions,
  ObjectInspector;

type

  { TEduPropsEventsOptions }

  TEduPropsEventsOptions = class(TEduOptionsNode)
  private

    FPropsMinimal: boolean;
    FPropsExt: boolean;
    FPropsFull: boolean;

    FEventsMinimal: boolean;
    FEventsExt: boolean;
    FEventsFull: boolean;

  public
    constructor Create; override;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    procedure Apply(Enable: boolean); override;

    property PropsMinimal: boolean read FPropsMinimal write FPropsMinimal;
    property PropsExt: boolean read FPropsExt write FPropsExt;
    property PropsFull: boolean read FPropsFull write FPropsFull;

    property EventsMinimal: boolean read FEventsMinimal write FEventsMinimal;
    property EventsExt: boolean read FEventsExt write FEventsExt;
    property EventsFull: boolean read FEventsFull write FEventsFull;

  end;

  { TEduPropsEventsFrame }

  TEduPropsEventsFrame = class(TAbstractIDEOptionsEditor)
    grpBoxEvents: TGroupBox;
    grpBoxProps: TGroupBox;
    OptsPanel: TPanel;
    PropsMinRadioBtn: TRadioButton;
    PropsExtRadioBtn: TRadioButton;
    rdGrpProps: TRadioGroup;
    PropsFullRadioBtn: TRadioButton;
    EventsMinRadioBtn: TRadioButton;
    EventsExtRadioBtn: TRadioButton;
    EventsFullRadioBtn: TRadioButton;
    rdGrpEvents: TRadioGroup;
    stTextEvents: TStaticText;
    stTextProps2: TStaticText;
    stTextProps: TStaticText;

    procedure EventsExtRadioBtnClick(Sender: TObject);
    procedure EventsFullRadioBtnClick(Sender: TObject);
    procedure EventsMinRadioBtnClick(Sender: TObject);
    procedure PropsExtRadioBtnClick(Sender: TObject);
    procedure PropsFullRadioBtnClick(Sender: TObject);
    procedure PropsMinRadioBtnClick(Sender: TObject);
    procedure rdGrpPropsClick(Sender: TObject);

  private
  public
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

var
  EduPropsEventsOptions: TEduPropsEventsOptions = nil;

procedure Register;

implementation

procedure Register;
begin
  EduPropsEventsOptions:=TEduPropsEventsOptions.Create;
  EducationOptions.Root.Add(EduPropsEventsOptions);
  EduPropsEventsOptionsID:=RegisterIDEOptionsEditor(EduOptionID,
                           TEduPropsEventsFrame,EduPropsEventsOptionsID)^.Index;
end;

{ TEduPropsEventsOptions }

constructor TEduPropsEventsOptions.Create;

begin
  inherited Create;
  Name:='PropsEvents';

  PropsMinimal:=true;
  PropsExt:=false;
  PropsFull:=false;

  EventsMinimal:=true;
  EventsExt:=false;
  EventsFull:=false;

end;

destructor TEduPropsEventsOptions.Destroy;
begin
  inherited Destroy;
end;

function TEduPropsEventsOptions.Load(Config: TConfigStorage): TModalResult;
begin

  FPropsMinimal:=Config.GetValue('PropsMinimal',true);
  FPropsExt:=Config.GetValue('PropsExt',true);
  FPropsFull:=Config.GetValue('PropsFull',true);

  FEventsMinimal:=Config.GetValue('EventsMinimal',true);
  FEventsExt:=Config.GetValue('EventsExt',true);
  FEventsFull:=Config.GetValue('EventsFull',true);


  Result:=inherited Load(Config);
end;

function TEduPropsEventsOptions.Save(Config: TConfigStorage): TModalResult;
begin

  Config.SetValue('PropsMinimal',FPropsMinimal);
  Config.SetValue('PropsExt',FPropsExt);
  Config.SetValue('PropsFull',FPropsFull);

  Config.SetValue('EventsMinimal',FEventsMinimal);
  Config.SetValue('EventsExt',FEventsExt);
  Config.SetValue('EventsFull',FEventsFull);

  Result:=inherited Save(Config);
end;

procedure TEduPropsEventsOptions.Apply(Enable: boolean);
begin
  inherited Apply(Enable);
end;

{ TEduPropsEventsFrame }


procedure TEduPropsEventsFrame.PropsMinRadioBtnClick(Sender: TObject);
begin
  grpBoxProps.Caption:=ersGrpBoxPropsMin;
  stTextProps.Caption:=ersStTextPropsMin;
  stTextProps2.Visible:=false;
end;

procedure TEduPropsEventsFrame.rdGrpPropsClick(Sender: TObject);
begin

end;

procedure TEduPropsEventsFrame.PropsExtRadioBtnClick(Sender: TObject);
begin
  grpBoxProps.Caption:=ersGrpBoxPropsExt;
  stTextProps.Caption:=ersStTextPropsMin;
  stTextProps2.Visible:=true;
end;

procedure TEduPropsEventsFrame.EventsMinRadioBtnClick(Sender: TObject);
begin
  grpBoxEvents.Caption:=ersGrpBoxEventsMin;
  stTextEvents.Caption:=ersStTextEventsMin;
end;

procedure TEduPropsEventsFrame.EventsExtRadioBtnClick(Sender: TObject);
begin
   grpBoxEvents.Caption:=ersGrpBoxEventsExt;
   stTextEvents.Caption:=ersStTextEventsExt;
end;

procedure TEduPropsEventsFrame.EventsFullRadioBtnClick(Sender: TObject);
begin
  grpBoxEvents.Caption:=ersGrpBoxEventsFull;
  stTextEvents.Caption:=ersStTextEventsFull;
end;

procedure TEduPropsEventsFrame.PropsFullRadioBtnClick(Sender: TObject);
begin
  grpBoxProps.Caption:=ersGrpBoxPropsFull;
  stTextProps.Caption:=ersStTextPropsFull;
    stTextProps2.Visible:=false;
end;



function TEduPropsEventsFrame.GetTitle: String;
begin
  Result:=ersEduPropsEventsTitle;
end;

procedure TEduPropsEventsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    PropsMinRadioBtn.Checked:=EduPropsEventsOptions.PropsMinimal;
    PropsExtRadioBtn.Checked:=EduPropsEventsOptions.PropsExt;
    PropsFullRadioBtn.Checked:=EduPropsEventsOptions.PropsFull;

    EventsMinRadioBtn.Checked:=EduPropsEventsOptions.EventsMinimal;
    EventsExtRadioBtn.Checked:=EduPropsEventsOptions.EventsExt;
    EventsFullRadioBtn.Checked:=EduPropsEventsOptions.EventsFull;
  end;
end;

procedure TEduPropsEventsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

  rdGrpProps.Caption:= ersRdGrpPropsCaption;
  rdGrpEvents.Caption:= ersRdGrpEventsCaption;

  PropsMinRadioBtn.Caption:= ersShowMinimal;
  PropsExtRadioBtn.Caption:= ersShowExtended;
  PropsFullRadioBtn.Caption:= ersRdBtnFull;

  EventsMinRadioBtn.Caption:= ersShowMinimal;
  EventsExtRadioBtn.Caption:= ersShowExtended;
  EventsFullRadioBtn.Caption:= ersRdBtnFull;

  stTextProps2.Caption:=ersStTextPropsExt;
  stTextProps2.Visible:=false;

  if (EduPropsEventsOptions.PropsMinimal)
   then begin
    grpBoxProps.Caption:=ersGrpBoxPropsMin;
    stTextProps.Caption:=ersStTextPropsMin
   end else if (EduPropsEventsOptions.PropsExt)
      then begin
        grpBoxProps.Caption:=ersGrpBoxPropsExt;
        stTextProps.Caption:=ersStTextPropsMin;
        stTextProps2.Visible:=true
      end
   else begin
     grpBoxProps.Caption:=ersGrpBoxPropsFull;
     stTextProps.Caption:=ersStTextPropsFull

   end;
   if (EduPropsEventsOptions.EventsMinimal)
   then begin
    grpBoxEvents.Caption:=ersGrpBoxEventsMin;
    stTextEvents.Caption:=ersStTextEventsMin
   end
   else if (EduPropsEventsOptions.EventsExt)
      then begin
        grpBoxEvents.Caption:=ersGrpBoxEventsExt;
        stTextEvents.Caption:=ersStTextEventsExt;
      end
   else begin
     grpBoxEvents.Caption:=ersGrpBoxEventsFull;
     stTextEvents.Caption:=ersStTextEventsFull;
   end;
end;

class function TEduPropsEventsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=EducationIDEOptionsClass;
end;

procedure TEduPropsEventsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    EduPropsEventsOptions.PropsMinimal:=PropsMinRadioBtn.Checked;
    EduPropsEventsOptions.PropsExt:=PropsExtRadioBtn.Checked;
    EduPropsEventsOptions.PropsFull:=PropsFullRadioBtn.Checked;

    EduPropsEventsOptions.EventsMinimal:=EventsMinRadioBtn.Checked;
    EduPropsEventsOptions.EventsExt:=EventsExtRadioBtn.Checked;
    EduPropsEventsOptions.EventsFull:=EventsFullRadioBtn.Checked;
  end;
end;

{$R *.lfm}

end.
