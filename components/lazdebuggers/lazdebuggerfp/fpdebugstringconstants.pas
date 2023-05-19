unit FpDebugStringConstants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  drsKeyForAddress = 'Key for Address';
  drsKeyForTypename = 'Key for Typename';

  drsFunctionName = 'Function name';
  drsCallSysVarToLStr = 'Call SysVarToLStr';
  drsCallJsonForDebug = 'Call JsonForDebug';
  drsConverterNormalizeVariant = 'Convert variant to value type';
  drsRunAllThreadsWhileEval = 'Run all threads while evaluating';

  LazInternalErr                           = 'Internal error: %1:s';

  LazErrSymbolNotFound_p                   = 'Identifier not found: "%1:s"%2:s';
  LazErrIndexOutOfRange                    = 'Index out of range: %1:d';
  LazErrTypeNotIndexable                   = 'The value has no index';
  LazErrExpectedOrdinalVal_p               = 'Expected an ordinal value, but found ''%1:s''%2:s';
  LazErrCannotCastToPointer_p              = 'Can''t cast value to pointer ''%1:s''%2:s';
  LazErrCannotDeref_p                      = 'Can''t dereference expression ''%1:s''%2:s';

  LazErrPasParserEmptyExpression           = 'Empty Expression';
  LazErrPasParserUnexpectedEndOfExpression = 'Unexpected end of expression after ''%1:s''';
  LazErrPasParserUnexpectedToken_p         = 'Unexpected token ''%1:s''%2:s';
  LazErrPasParserUnknownIntrinsic_p        = 'Intrinsic function ''%1:s'' not found%2:s';

  LazErrPasParserMissingOpenBracket_p      = 'No opening bracket for ''%1:s'' found%2:s';
  LazErrPasParserWrongOpenBracket_p        = 'Mismatched opening bracket (''%3:s'' at pos %2:d) for ''%1:s'' found%4:s';
  LazErrPasParserMissingIndexExpression    = 'Expected Expression but found closing bracket: ''%1:s'' at pos %2:d';
  LazErrPasParserMissingExprAfterComma     = 'Expected Expression after Comma, but found closing bracket %1:s at pos %2:d';
  LazErrPasParserIndexError_Wrapper        = 'Index error at pos %2:d for ''%1:s''[]: %3:s';
  LazErrPasParserUnterminatedString_p      = 'String not terminated%1:s';
  LazErrPasParserExpectedNumber_p          = 'Expected Number, but found ''%1:s''%2:s';


  (* Any resourcestring endig in *_p may or may not have one of the below included.
     The value may also be an empty string instead.
  *)
  LazErrPasParser_AtStart                  = ' at start of expression';
  LazErrPasParser_PositionAfter            = ' at pos %1:d after ''%2:s''';
  LazErrPasParser_Position                 = ' at pos %1:d';

implementation

end.

