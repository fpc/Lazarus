unit jcfbaseConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  //Settings caption and error messages
  lisTheSettingsFileDoesNotExist = 'The settings file "%s" does not exist.%s'+
    'The formatter will work better if it is configured to use a valid settings file';
  lisErrorWritingSettingsFileReadOnly = 'Error writing settings file: %s is read only';
  lisErrorWritingSettingsException = 'Error writing settings file %s:%s%s';
  lisNoSettingsFound = 'No settings found';

implementation

end.

