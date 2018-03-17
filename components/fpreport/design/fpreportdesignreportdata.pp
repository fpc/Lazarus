{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Base classes to manage a collection of report data loops.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpreportdesignreportdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, forms, fpjson, fpreport, fpreportData;

Type
  EDesignReportData = Class(EReportDataError);

  { TDesignReportDataHandler }

  { TReportDataConfigFrame }

  TReportDataConfigFrame = Class(TFrame)
  Public
    Procedure GetConfig(aConfig : TJSONObject); virtual; abstract;
    Procedure SetConfig(aConfig : TJSONObject); virtual; abstract;
    Function SaveNotOKMessage : String; virtual;
  end;

  { TDesignReportDataManager }

  TDesignReportDataManager = class(TFPCustomReportDataManager)
  private
  Public
    class function HasDesignTypeHandler(aTypeName: String) : Boolean;
    class function CreateConfigFrame(aTypeName: String; AOwner: TComponent): TReportDataConfigFrame;
    Property DataDefinitions;
  end;

implementation

Resourcestring
  SErrNotDesignData = 'The handler for data type %s is registered, but cannot handle visual configuration';

{ TReportDataConfigFrame }

function TReportDataConfigFrame.SaveNotOKMessage: String;
begin
  Result:='';
end;

{ TDesignReportDataCollection }


class function TDesignReportDataManager.HasDesignTypeHandler(aTypeName: String): Boolean;

Var
  C: TComponentClass;

begin
  C:=GetConfigFrameClass(aTypeName);
  Result:=C.InheritsFrom(TReportDataConfigFrame);
end;


Class function TDesignReportDataManager.CreateConfigFrame(aTypeName : String; AOwner: TComponent): TReportDataConfigFrame;

Var
  C: TComponentClass;

begin
  C:=GetConfigFrameClass(aTypeName);
  if not C.InheritsFrom(TReportDataConfigFrame) then
    Raise EDesignReportData.CreateFmt(SErrNotDesignData,[aTypeName]);
  Result:=TReportDataConfigFrame(C.Create(aOwner));
end;

end.

