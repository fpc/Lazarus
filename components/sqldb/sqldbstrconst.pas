unit SqlDbStrConst;

{$mode objfpc}{$H+}

interface

resourcestring

  lrsGeneratesqlstatements = 'Edit/Generate SQL Statements';
  lrsSQLDataSetOpen = 'Dataset not open: %s';
  lrsSQLGenSelect = 'You must select fields to be updated and key fields.';

  SSQLScript     = 'SQL Script file';
  SSQLScriptDesc = 'Create a new SQL Script file';
  SSQLSource = 'Insert your SQL statements here';
  SEditSQL = 'Edit SQL ...';
  SGenerateUpdateSQL = 'Generate update SQL';
  SEditUpdateSQL = 'Edit all SQL statements';
  SErrConnectionNotAssigned = 'Database not assigned. Assign Database first.';

  SFireBirdDatabases = 'Firebird databases';
  SSQLite3Databases = 'SQLite3 databases';
  SInterbaseDatabases = 'Interbase databases';
  SSQLStringsPropertyEditorDlgTitle = 'Editing %s';

  sLibraries = 'Shared libraries';

  SResultTabCaption = 'Results';
  SSQLTabCaption    = 'SQL Code';
  SMetaTabCaption   = 'Metadata';
  SMetaTables       = 'Tables';
  SMetaColumns      = 'Columns';
  SMetaProcedures   = 'Procedures';
  SMetaPleaseSpecifyATableInTheObjectField = 'Please specify a table in the '
    +'object field.';
  SMetaSysTables    = 'SysTables';

  SLoadSQLCodeHint = 'Load SQL code ...';
  SSaveSQLCodeHint = 'Save SQL code ...';
  SRunSQLCodeHint = 'Run SQL code';
  SQuickCheckOfSQLSyntaxHint = 'Quick check of SQL syntax';

  // SQL Parser results:
  // Note: sql parser is not quite exact, so indicate it's not completely sure
  SSQLOK            = 'Quick SQL check OK';
  SQLSyntaxOK       = 'No syntax errors in SQL statement found.';
  SSQLError         = 'Probable SQL error';
  SSQLSyntaxError   = 'Probable syntax error in SQL statement:'+slineBreak+'%s';

  // GenerateSQLDlg
  SSQLGenerated = 'SQL statements successfully generated.';
  SSQLNoTable = 'No table is selected for generation of SQL statements.';
  SSQLTablesAndFields = 'Tables and Fields';
  SSQLTable = 'Table';
  SSQLShowSystemTables = 'Show system tables';
  SSQLDelimitersForFieldTableNames = 'Delimiters for field/table names';
  SSQLQuoteChar = 'Quote character';
  SSQLBrackets = 'Brackets';
  SSQLOneFieldPerLine = 'One field per line';
  SSQLUppercaseKeywords = 'Uppercase keywords';
  SSQLFullyQualifiedFields = 'Fully qualified fields';
  SSQLIndent = 'Indent';
  SSQLLineLength = 'Line length';
  SSQLGenerateSQL = 'Generate SQL';
  SSQLKeyFields = 'Key fields';
  SSQLSelectUpdateInsertFields = '"Select"/"Update"/"Insert" fields';
  SSQLNoSQLGenerated = 'No SQL statements were generated. Do you really want to close?';

implementation

end.

