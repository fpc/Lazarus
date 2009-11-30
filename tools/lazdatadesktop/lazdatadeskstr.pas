{ Lazarus Database Desktop Resource Strings

  Copyright (C) 2009 Marcelo B Paula

  Estes fontes são software livre; você pode redistribuir e/ou modificá-los
  sob os termos da GNU Library General Public License como publicada pela Free
  Software Foundation; ou a versão 2 da Licença, ou (a sua escolha) qualquer
  versão posterior.

  Este código é distribuído na esperança de que seja útil, mas SEM
  QUALQUER GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE ou
  ADEQUAÇÃO A UMA FINALIDADE PARTICULAR. Veja a licença GNU General Public
  License para maiores detalhes.

  Você deve ter recebido uma cópia da licença GNU Library General Public
  License juntamente com esta biblioteca; senão, escreva a Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit lazdatadeskstr;

{$mode objfpc}{$H+}

interface

resourcestring

  // Connection Editor
  SNodeTables         = 'Tables';
  SNodeFields         = 'Fields';
  SNodeIndexes        = 'Indexes';
  SNewDictionary      = 'New database';
  SNodeDataBase       = 'Database';
  SNodeTableData      = 'Table Data';
  SNodeIndexOptions   = 'Index options: ';
  SNodeIndexFields    = 'Index fields: ';
  SParameter          = 'Parameter';
  SValue              = 'Value';
  SEngineType         = 'Engine';
  SDescription        = 'Description';
  SColName            = 'Name';
  SColType            = 'Type';
  SColSize            = 'Size';
  SColFields          = 'Fields';
  SColOptions         = 'Options';
  SQuery              = 'Run query';
  SSelectedObject     = 'Selected object';

  // Dict Editor
  SNodeDataDictionary = 'Datadictionary';
  SNodeDomains        = 'Domains';
  SNodeSequences      = 'Sequences';
  SNodeForeignkeys    = 'Foreign keys';
  STable = 'Table';
  SField = 'Field';
  SIndex = 'Index';
  SSequence = 'Sequence';
  SForeignKey = 'Foreign key';
  SDomain = 'Domain';
  SNew = 'New %s';
  SErrUnknownType = 'Unknown object type: %d';
  SNewObject = 'Create new %s';
  SNameFor = 'Enter a name for the new %s';
  SDeleteObject = 'Delete this %s';
  SObject = 'Object';

  // Generate SQL statements form
  SErrSelectTable  = 'No table selected. Please select a table';
  SErrSelectFields = 'No fields selected. Please select some fields';
  //
  sld_Generatesqlstatements = 'Generate SQL statements';
  sld_Tableandfields = 'Tables and &Fields';
  sld_Select         = '&Select';
  sld_Insert         = '&Insert';
  sld_Update         = '&Update';
  sld_Delete         = '&Delete';
  sld_Createtable    = 'Create table';
  sld_Table          = 'Ta&ble';
  sld_Keyfields      = '&Key fields';
  sld_Selectupdateinsertfields = 'Select/Update/Insert fields';
  sld_Options        = 'Options';
  sld_Indent         = 'I&ndent';
  sld_Linelength     = 'Line Length';
  sld_Createfulltablecreationsql = 'Create full table creation SQL';
  sld_Generatesql    = 'Generate SQL';
  sld_Ok             = '&OK';
  sld_Cancel         = '&Cancel';
  //
  eoLineFeedAfterField = 'Linefeed after each field';
  eoUseOldInWhereParams = 'Use OLD prefix in where parameters';
  eoAndTermsInBrackets = 'Put brackets around AND Terms';
  eoQuoteFieldNames = 'Quote field names';
  eoLineFeedAfterAndTerm = 'Linefeed after AND terms';
  eoAddTerminator = 'Add terminator';
  eoSkipForeignKeys = 'Skip foreign keys';

  // Import/Update datadictionary form
  sld_Importupdatedatadictionary = 'Import/Update datadictionary';
  sld_Selectall      = 'Select &all';
  sld_Selectnone     = 'Select &none';
  sld_Updateexistingtables = 'Update existing tables';

  // Select a conection type form
  sld_Selectaconnectiontype = 'Select a conection type';

  // Connect to a database form
  sld_Connecttoadatabase = 'Connect to a database';
  sld_Host           = 'Host';
  sld_Database       = 'Database';
  sld_Username       = 'Username';
  sld_Password       = 'Password';
  sld_Charset        = 'Charset';

  // Query panel
  SSQLFilters = 'SQL files|*.sql|All files|*.*';
  SRowsAffected = 'Query executed succesfully: %d rows affected.';
  SErrNoEngine = 'No database engine !';
  SExecute = 'Execute SQL';
  SHintExecute = 'Execute SQL statement';
  SPrevious = 'Previous';
  SHintPrevious = 'Previous SQL statement';
  SNext = 'Next';
  SHintNext = 'Next SQL statement';
  SLoad = 'Load SQL';
  SHintLoad = 'Load SQL statement from file';
  SSave = 'Save SQL';
  SHintSave = 'Save SQL statement to file';
  SClose = 'Close result';
  SHintClose = 'Close query result data panel';
  SExport = 'Export data';
  SHintExport = 'Export this data';
  SCreateCode = 'Create code';
  SHintCreateCode = 'Create pascal code for this data';

  // Main form
  SSaveData     = 'Save changes';
  SDontSave     = 'Discard changes';
  SDontClose    = 'Do not close editor';
  SConfirmClose = 'Confirm close';
  SDDModified   = 'Data dictionary "%s" has changed.'#13#10+
                  'What do you want to do with the changes?';
  SImportDictInto = 'Import datadictionary';
  SWhichCurrentDictToUse = 'A data dictionary is active.'+
                           'Would you like to import into this data dictionary ?';
  SUseCurrentDict = 'Yes, use the active dictionary';
  SUseNewDict     = 'No, import in a new dictionary';
  SNewTable       = 'Create new table';
  SNewTableName   = 'Enter a name for the new table:';
  SNewSequence    = 'Create new sequence';
  SNewSequenceName  = 'Enter a name for the new sequence:';
  SNewDomain      = 'Create new domain';
  SNewDomainName  = 'Enter a name for the new domain:';
  SNewField       = 'Create new field in table %s';
  SNewFieldName   = 'Enter a name for the new field:';
  SNewIndex       = 'Create new index on table %s';
  SNewIndexName   = 'Enter a name for the new index:';
  SNewForeignKey  = 'Create new foreign key in table %s';
  SNewForeignKeyName   = 'Enter a name for the new foreign key:';
  SSelectDBFDir   = 'Select a directory with DBF files';
  SNewConnection  = 'New connection';
  SConnectionDescription = 'Enter a descriptive name for the connection';
  SConnectionNameExists = 'There is already a connection named "%s"'#13#10+
                          'Would you like to override the connection data ?';
  SUnknownDictionary = 'Unknown data dictionary: %s';
  SUnknownConnection = 'Unknown connection: %s';
  SCreateConnection = 'Would you like to create a new connection for this database ?';
  //
  sld_Lazarusdatabasedesktop = 'Lazarus Database Desktop';
  sld_Menufile               = '&File';
  sld_Menuedit               = '&Edit';
  sld_Menudictionary         = '&Dictionary';
  sld_Menuconnections        = 'Connections';
  sld_Menudictionaryimport   = '&Import';
  //
  sld_Actionsave           = '&Save';
  sld_ActionsaveH          = 'Save Data Dictionary';
  sld_Actionnew            = '&New';
  sld_ActionnewH           = 'Create a new Data Dictionary';
  sld_Actionexit           = 'E&xit';
  sld_ActionexitH          = 'Quit this program';
  sld_Actionopen           = '&Open...';
  sld_ActionopenH          = 'Open a new Data Dictionary';
  sld_Actionclose          = '&Close';
  sld_ActioncloseH         = 'Close current Data Dictionary';
  sld_Actioncloseall       = 'Close &All';
  sld_ActioncloseallH      = 'Close all Data Dictionaries';
  sld_Actionsaveas         = 'Save &as';
  sld_ActionsaveasH        = 'Save datadictionary as';
  sld_Actionopenrecentdatadict  = 'Open';
  sld_ActionopenrecentdatadictH = 'Open selected recent datadictionary';
  sld_Actiondeleterecentdatadict= '&Delete';
  //
  sld_Actioncut            = 'Cu&t';
  sld_ActioncutH           = 'Cut';
  sld_Actioncopy           = '&Copy';
  sld_ActioncopyH          = 'Copy';
  sld_Actionpaste          = '&Paste';
  sld_ActionpasteH         = 'Paste';
  //
  sld_Actionnewtable       = 'New &table';
  sld_ActionnewtableH      = 'Create a new table';
  sld_Actionnewfield       = 'New &field';
  sld_ActionnewfieldH      = 'Create a new field in the current table';
  sld_Actiondeleteobject   = 'Delete &Object';
  sld_ActiondeleteobjectH  = 'Delete the currently selected object';
  sld_Actiongeneratesql    = '&Generate SQL';
  sld_ActiongeneratesqlH   = 'Generate SQL statements for the current table';
  sld_Actionnewindex       = 'New index';
  sld_ActionnewindexH      = 'Add new index to current table';
  sld_Actioncreatecode     = 'Create &code';
  sld_ActioncreatecodeH    = 'Create code from definition or data';
  sld_Actionaddsequence    = 'New sequence';
  sld_ActionaddsequenceH   = 'Add a sequence';
  sld_Actionaddforeignkey  = 'New Foreign Key';
  sld_ActionaddforeignkeyH = 'Add a foreign key to the table';
  sld_Actionadddomain      = 'New domain';
  sld_ActionadddomainH     = 'Add a domain to the data dictionary';
  //
  sld_Actionnewconnection     = '&New connection';
  sld_Actiondeleteconnection  = '&Delete connection';
  sld_Actioncopyconnection    = '&Copy connection';
  sld_Actionopenconnection    = '&Open connection';
  sld_ActionopenconnectionH   = 'Open selected recent connection';
  //
  sld_Separator               = '-';
  //
  sld_Dictionaries         = 'Dictionaries';
  sld_Connections          = 'Connections';
  //
  sld_Recentlv1            = 'Name';
  sld_Recentlv2            = 'Filename';
  sld_Recentlv3            = 'Last used on';
  //
  sld_Connectionlv1        = 'Name';
  sld_Connectionlv2        = 'Driver';
  sld_Connectionlv3        = 'Last used';
  sld_Connectionlv4        = 'Description';
  //
  sld_opendatadictionarytitle      = 'Open data dictionary';
  sld_opendatadictionaryfilter     = 'Data dictionary files|*.fpd|Ini files|*.ini|All files|*.*';
  sld_savefileastitle              = 'Save file as';
  sld_savefileasfilter             = 'Data dictionary files|*.fpd|Ini files|*.ini|All files|*.*';
  //
  sld_Fromconnection       = 'From connection';
  //

implementation

end.

