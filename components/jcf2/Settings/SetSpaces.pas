{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetSpaces.pas, released February 2001.
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

{ mostly spacing and line breaking +options }

unit SetSpaces;

{$I JcfGlobal.inc}

interface

uses JcfSetBase, SettingsStream, SettingsTypes;

type

  TSetSpaces = class(TSetBase)
  private
    fbTabsToSpaces: boolean;
    fbSpacesToTabs: boolean;
    fiSpacesForTab: integer;
    fiSpacesPerTab: integer;

    fbFixSpacing: boolean;
    fbSpaceBeforeClassHeritage: boolean;

    fiSpacesBeforeColonVar: integer;
    fiSpacesBeforeColonConst: integer;
    fiSpacesBeforeColonParam: integer;
    fiSpacesBeforeColonFn: integer;
    fiSpacesBeforeColonClassVar: integer;
    fiSpacesBeforeColonRecordField: integer;
    fiSpacesBeforeColonCaseLabel: integer;
    fiSpacesBeforeColonLabel: integer;
    fiSpacesBeforeColonInGeneric: integer;

    fiMaxSpacesInCode: integer;
    fbUseMaxSpacesInCode: boolean;

    // add spaces
    fbSpaceBeforeOpenBracketsInFunctionDeclaration: boolean;
    fbSpaceBeforeOpenBracketsInFunctionCall: boolean;
    fbSpaceBeforeOpenSquareBracketsInExpression: boolean;

    fbSpaceAfterOpenBrackets: boolean;
    fbSpaceBeforeCloseBrackets: boolean;
    fbMoveSpaceToBeforeColon: boolean;

    feSpaceForOperator: TTriOptionStyle;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property TabsToSpaces: boolean read fbTabsToSpaces write fbTabsToSpaces;
    property SpacesToTabs: boolean read fbSpacesToTabs write fbSpacesToTabs;

    property SpacesPerTab: integer read fiSpacesPerTab write fiSpacesPerTab;
    property SpacesForTab: integer read fiSpacesForTab write fiSpacesForTab;

    property FixSpacing: boolean read fbFixSpacing write fbFixSpacing;

    property SpaceBeforeClassHeritage: boolean
      read fbSpaceBeforeClassHeritage write fbSpaceBeforeClassHeritage;

    property SpacesBeforeColonVar: integer read fiSpacesBeforeColonVar
      write fiSpacesBeforeColonVar;
    property SpacesBeforeColonConst: integer
      read fiSpacesBeforeColonConst write fiSpacesBeforeColonConst;
    property SpacesBeforeColonParam: integer
      read fiSpacesBeforeColonParam write fiSpacesBeforeColonParam;
    property SpacesBeforeColonFn: integer read fiSpacesBeforeColonFn
      write fiSpacesBeforeColonFn;
    property SpacesBeforeColonClassVar: integer
      read fiSpacesBeforeColonClassVar write fiSpacesBeforeColonClassVar;
    property SpacesBeforeColonRecordField: integer
      read fiSpacesBeforeColonRecordField write fiSpacesBeforeColonRecordField;
    property SpacesBeforeColonCaseLabel: integer
      read fiSpacesBeforeColonCaseLabel write fiSpacesBeforeColonCaseLabel;
    property SpacesBeforeColonLabel: integer
      read fiSpacesBeforeColonLabel write fiSpacesBeforeColonLabel;
    property SpacesBeforeColonInGeneric: integer
      read fiSpacesBeforeColonInGeneric write fiSpacesBeforeColonInGeneric;

    property MaxSpacesInCode: integer read fiMaxSpacesInCode write fiMaxSpacesInCode;
    property UseMaxSpacesInCode: boolean read fbUseMaxSpacesInCode write fbUseMaxSpacesInCode;

    property SpaceForOperator: TTriOptionStyle read feSpaceForOperator write feSpaceForOperator;

    property SpaceBeforeOpenBracketsInFunctionDeclaration: boolean
      read fbSpaceBeforeOpenBracketsInFunctionDeclaration write fbSpaceBeforeOpenBracketsInFunctionDeclaration;
    property SpaceBeforeOpenBracketsInFunctionCall: boolean read fbSpaceBeforeOpenBracketsInFunctionCall write fbSpaceBeforeOpenBracketsInFunctionCall;
    property SpaceBeforeOpenSquareBracketsInExpression: boolean
      read fbSpaceBeforeOpenSquareBracketsInExpression write fbSpaceBeforeOpenSquareBracketsInExpression;

    property SpaceAfterOpenBrackets: boolean read fbSpaceAfterOpenBrackets write fbSpaceAfterOpenBrackets;
    property SpaceBeforeCloseBrackets: boolean read fbSpaceBeforeCloseBrackets write fbSpaceBeforeCloseBrackets;
    property MoveSpaceToBeforeColon: boolean read fbMoveSpaceToBeforeColon write fbMoveSpaceToBeforeColon;

  end;

implementation

const
  SET_TABS_TO_SPACES = 'TabsToSpaces';
  SET_SPACES_TO_TABS = 'SpacesToTabs';
  SET_SPACES_PER_TAB = 'SpacesPerTab';
  SET_SPACES_FOR_TAB = 'SpacesForTab';

  SET_FIX_SPACING = 'FixSpacing';

  SET_SPACE_BEFORE_CLASS_HERITAGE = 'SpaceBeforeClassHeritage';

  SET_SPACES_BEFORE_COLON_VAR   = 'SpacesBeforeColonVar';
  SET_SPACES_BEFORE_COLON_CONST = 'SpacesBeforeColonConst';
  SET_SPACES_BEFORE_COLON_PARAM = 'SpacesBeforeColonParam';
  SET_SPACES_BEFORE_COLON_FN    = 'SpacesBeforeColonFn';
  SET_SPACES_BEFORE_COLON_CLASS_VAR = 'SpacesBeforeColonClassVar';
  SET_SPACES_BEFORE_COLON_RECORD_FIELD = 'SpacesBeforeColonRecordField';
  SET_SPACES_BEFORE_COLON_CASE_LABEL = 'SpacesBeforeColonCaseLabel';
  SET_SPACES_BEFORE_COLON_LABEL = 'SpacesBeforeColonLabel';
  SET_SPACES_BEFORE_COLON_IN_GENERIC = 'SpacesBeforeColonInGeneric';

  SET_MAX_SPACES_IN_CODE     = 'MaxSpacesInCode';
  SET_USE_MAX_SPACES_IN_CODE = 'UseMaxSpacesInCode';

  SET_SPACE_FOR_OPERATOR = 'SpaceForOperator';

  SET_SPACE_BEFORE_OPEN_BRACKETS_IN_FUNCTION_DECLARATION = 'SpaceBeforeOpenBracketsInFunctionDeclaration';
  SET_SPACE_BEFORE_OPEN_BRACKETS_IN_FUNCTION_CALL = 'SpaceBeforeOpenBracketsInFunctionCall';
  SET_SPACE_BEFORE_OPEN_SQUARE_BRACKETS_IN_EXPRESSION = 'SpaceBeforeOpenSquareBracketsInExpression';

  SET_SPACE_AFTER_OPEN_BRACKETS = 'SpaceAfterOpenBrackets';
  SET_SPACE_BEFORE_CLOSE_BRACKETS = 'SpaceBeforeCloseBrackets';
  SET_MOVE_SPACE_TO_BEFORE_COLON = 'MoveSpaceToBeforeColon';


constructor TSetSpaces.Create;
begin
  inherited;
  SetSection('Spaces');
end;

procedure TSetSpaces.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbTabsToSpaces := pcStream.Read(SET_TABS_TO_SPACES, True);
  fbSpacesToTabs := pcStream.Read(SET_SPACES_TO_TABS, False);

  fiSpacesPerTab := pcStream.Read(SET_SPACES_PER_TAB, 2);
  fiSpacesForTab := pcStream.Read(SET_SPACES_FOR_TAB, 2);

  fbFixSpacing := pcStream.Read(SET_FIX_SPACING, True);

  fbSpaceBeforeClassHeritage := pcStream.Read(SET_SPACE_BEFORE_CLASS_HERITAGE, False);

  fiSpacesBeforeColonVar   := pcStream.Read(SET_SPACES_BEFORE_COLON_VAR, 0);
  fiSpacesBeforeColonConst := pcStream.Read(SET_SPACES_BEFORE_COLON_CONST, 0);
  fiSpacesBeforeColonParam := pcStream.Read(SET_SPACES_BEFORE_COLON_PARAM, 0);
  fiSpacesBeforeColonFn    := pcStream.Read(SET_SPACES_BEFORE_COLON_FN, 0);
  fiSpacesBeforeColonClassVar := pcStream.Read(SET_SPACES_BEFORE_COLON_CLASS_VAR, 0);
  fiSpacesBeforeColonRecordField := pcStream.Read(SET_SPACES_BEFORE_COLON_RECORD_FIELD, 0);

  fiSpacesBeforeColonCaseLabel := pcStream.Read(SET_SPACES_BEFORE_COLON_CASE_LABEL, 0);
  fiSpacesBeforeColonLabel     := pcStream.Read(SET_SPACES_BEFORE_COLON_LABEL, 0);
  fiSpacesBeforeColonInGeneric := pcStream.Read(SET_SPACES_BEFORE_COLON_IN_GENERIC, 0);

  fiMaxSpacesInCode    := pcStream.Read(SET_MAX_SPACES_IN_CODE, 2);
  fbUseMaxSpacesInCode := pcStream.Read(SET_USE_MAX_SPACES_IN_CODE, False);

  feSpaceForOperator := TTriOptionStyle(pcStream.Read(SET_SPACE_FOR_OPERATOR, Ord(eAlways)));

  fbSpaceBeforeOpenBracketsInFunctionDeclaration := pcStream.Read(SET_SPACE_BEFORE_OPEN_BRACKETS_IN_FUNCTION_DECLARATION, False);
  fbSpaceBeforeOpenBracketsInFunctionCall := pcStream.Read(SET_SPACE_BEFORE_OPEN_BRACKETS_IN_FUNCTION_CALL, False);
  fbSpaceBeforeOpenSquareBracketsInExpression := pcStream.Read(SET_SPACE_BEFORE_OPEN_SQUARE_BRACKETS_IN_EXPRESSION, False);

  fbSpaceAfterOpenBrackets := pcStream.Read(SET_SPACE_AFTER_OPEN_BRACKETS, False);
  fbSpaceBeforeCloseBrackets := pcStream.Read(SET_SPACE_BEFORE_CLOSE_BRACKETS, False);
  fbMoveSpaceToBeforeColon := pcStream.Read(SET_MOVE_SPACE_TO_BEFORE_COLON, False);
end;

procedure TSetSpaces.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(SET_TABS_TO_SPACES, fbTabsToSpaces);
  pcOut.Write(SET_SPACES_TO_TABS, fbSpacesToTabs);
  pcOut.Write(SET_SPACES_PER_TAB, fiSpacesPerTab);
  pcOut.Write(SET_SPACES_FOR_TAB, fiSpacesForTab);

  pcOut.Write(SET_FIX_SPACING, fbFixSpacing);
  pcOut.Write(SET_SPACE_BEFORE_CLASS_HERITAGE, fbSpaceBeforeClassHeritage);

  pcOut.Write(SET_SPACES_BEFORE_COLON_VAR, fiSpacesBeforeColonVar);
  pcOut.Write(SET_SPACES_BEFORE_COLON_CONST, fiSpacesBeforeColonConst);
  pcOut.Write(SET_SPACES_BEFORE_COLON_PARAM, fiSpacesBeforeColonParam);
  pcOut.Write(SET_SPACES_BEFORE_COLON_FN, fiSpacesBeforeColonFn);
  pcOut.Write(SET_SPACES_BEFORE_COLON_CLASS_VAR, fiSpacesBeforeColonClassVar);

  pcOut.Write(SET_SPACES_BEFORE_COLON_RECORD_FIELD, fiSpacesBeforeColonRecordField);

  pcOut.Write(SET_SPACES_BEFORE_COLON_CASE_LABEL, fiSpacesBeforeColonCaseLabel);
  pcOut.Write(SET_SPACES_BEFORE_COLON_LABEL, fiSpacesBeforeColonLabel);
  pcOut.Write(SET_SPACES_BEFORE_COLON_IN_GENERIC, fiSpacesBeforeColonInGeneric);

  pcOut.Write(SET_MAX_SPACES_IN_CODE, fiMaxSpacesInCode);
  pcOut.Write(SET_USE_MAX_SPACES_IN_CODE, fbUseMaxSpacesInCode);

  pcOut.Write(SET_SPACE_FOR_OPERATOR, ord(feSpaceForOperator));

  pcOut.Write(SET_SPACE_BEFORE_OPEN_BRACKETS_IN_FUNCTION_DECLARATION, fbSpaceBeforeOpenBracketsInFunctionDeclaration);
  pcOut.Write(SET_SPACE_BEFORE_OPEN_BRACKETS_IN_FUNCTION_CALL, fbSpaceBeforeOpenBracketsInFunctionCall);
  pcOut.Write(SET_SPACE_BEFORE_OPEN_SQUARE_BRACKETS_IN_EXPRESSION, fbSpaceBeforeOpenSquareBracketsInExpression);

  pcOut.Write(SET_SPACE_AFTER_OPEN_BRACKETS, fbSpaceAfterOpenBrackets);
  pcOut.Write(SET_SPACE_BEFORE_CLOSE_BRACKETS, fbSpaceBeforeCloseBrackets);

  pcOut.Write(SET_MOVE_SPACE_TO_BEFORE_COLON, fbMoveSpaceToBeforeColon);
end;


end.
