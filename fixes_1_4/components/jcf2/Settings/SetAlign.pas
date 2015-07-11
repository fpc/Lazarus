{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetAlign.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit SetAlign;

{ settings to do with statement alignment
  AFS 29 Dec 1999
}

{$I JcfGlobal.inc}

interface

uses JcfSetBase, SettingsStream;

type

  TSetAlign = class(TSetBase)
  private

    fbAlignAssign: boolean;
    fbAlignConst: boolean;
    fbAlignTypedef: boolean;
    fbAlignVar: boolean;
    fbAlignComment: boolean;
    fbAlignField: boolean;

    fiMinColumn, fiMaxColumn: integer;
    fiMaxVariance, fiMaxVarianceInterface: integer;
    fiMaxUnalignedStatements: integer;
    fbInterfaceOnly: boolean;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property AlignAssign: boolean Read fbAlignAssign Write fbAlignAssign;
    property AlignConst: boolean Read fbAlignConst Write fbAlignConst;
    property AlignTypeDef: boolean Read fbAlignTypeDef Write fbAlignTypeDef;
    property AlignVar: boolean Read fbAlignVar Write fbAlignVar;

    property AlignComment: boolean Read fbAlignComment Write fbAlignComment;
    property AlignField: boolean read fbAlignField write fbAlignField;

    property MinColumn: integer Read fiMinColumn Write fiMinColumn;
    property MaxColumn: integer Read fiMaxColumn Write fiMaxColumn;
    property MaxVariance: integer Read fiMaxVariance Write fiMaxVariance;
    property MaxVarianceInterface: integer Read fiMaxVarianceInterface Write fiMaxVarianceInterface;

    property MaxUnalignedStatements: integer
      Read fiMaxUnalignedStatements Write fiMaxUnalignedStatements;

    property InterfaceOnly: boolean Read fbInterfaceOnly Write fbInterfaceOnly;
  end;


implementation

const
  REG_ALIGN_ASSIGN  = 'AlignAssign';
  REG_ALIGN_CONST   = 'AlignConst';
  REG_ALIGN_TYPEDEF = 'AlignTypedef';
  REG_ALIGN_VAR     = 'AlignVars';
  REG_ALIGN_COMMENT = 'AlignComment';
  REG_ALIGN_FIELDS = 'AlignFields';

  REG_MIN_COLUMN   = 'MinColumn';
  REG_MAX_COLUMN   = 'MaxColumn';
  REG_MAX_VARIANCE = 'MaxVariance';
  REG_MAX_VARIANCE_INTERFACE = 'MaxVarianceInterface';
  REG_MAX_UNALIGNED_STATEMENTS = 'MaxUnalignedStatements';

  REG_INTERFACE_ONLY = 'InterfaceOnly';

  { TSetAlign }

constructor TSetAlign.Create;
begin
  inherited;
  SetSection('Align');
end;

procedure TSetAlign.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbAlignAssign  := pcStream.Read(REG_ALIGN_ASSIGN, False);
  fbAlignConst   := pcStream.Read(REG_ALIGN_CONST, False);
  fbAlignTypedef := pcStream.Read(REG_ALIGN_TYPEDEF, False);
  fbAlignVar     := pcStream.Read(REG_ALIGN_VAR, False);
  fbAlignComment := pcStream.Read(REG_ALIGN_COMMENT, False);
  fbAlignField   := pcStream.Read(REG_ALIGN_FIELDS, False);

  fbInterfaceOnly := pcStream.Read(REG_INTERFACE_ONLY, False);

  fiMinColumn   := pcStream.Read(REG_MIN_COLUMN, 2);
  fiMaxColumn   := pcStream.Read(REG_MAX_COLUMN, 60);
  fiMaxVariance := pcStream.Read(REG_MAX_VARIANCE, 10);
  fiMaxVarianceInterface := pcStream.Read(REG_MAX_VARIANCE_INTERFACE, 10);
  fiMaxUnalignedStatements := pcStream.Read(REG_MAX_UNALIGNED_STATEMENTS, 0);
end;

procedure TSetAlign.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ALIGN_ASSIGN, fbAlignAssign);
  pcOut.Write(REG_ALIGN_CONST, fbAlignConst);
  pcOut.Write(REG_ALIGN_TYPEDEF, fbAlignTypedef);
  pcOut.Write(REG_ALIGN_VAR, fbAlignVar);
  pcOut.Write(REG_ALIGN_COMMENT, fbAlignComment);
  pcOut.Write(REG_ALIGN_FIELDS, fbAlignField);

  pcOut.Write(REG_INTERFACE_ONLY, fbInterfaceOnly);

  pcOut.Write(REG_MIN_COLUMN, fiMinColumn);
  pcOut.Write(REG_MAX_COLUMN, fiMaxColumn);
  pcOut.Write(REG_MAX_VARIANCE, fiMaxVariance);
  pcOut.Write(REG_MAX_VARIANCE_INTERFACE, fiMaxVarianceInterface);
  pcOut.Write(REG_MAX_UNALIGNED_STATEMENTS, fiMaxUnalignedStatements);
end;

end.
