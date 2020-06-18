{%MainUnit ../lclintf.pas}

// Locates the default browser associated in the system

function FindBrowserExecutable(const ShortFilename: String; out ABrowser: String): Boolean; inline;
begin
  ABrowser := SearchFileInPath(ShortFilename + GetExeExt, '',
                    GetEnvironmentVariableUTF8('PATH'), PathSeparator,
                    [sffDontSearchInBasePath]);
  Result := (ABrowser <> '');
end;

const
  PredefinedBrowserStrings: array[1..11] of String = (
    'xdg-open',
    'htmlview',
    'firefox',
    'mozilla',
    'google-chrome',
    'galeon',
    'konqueror',
    'safari',
    'netscape',
    'opera',
    'iexplore'
  );

function FindPredefinedBrowser(out ABrowser, AParams: String): Boolean;
var
  i: Integer;
begin
  ABrowser := '';
  AParams := '"%s"';
  for i := Low(PredefinedBrowserStrings) to High(PredefinedBrowserStrings) do
    if FindBrowserExecutable(PredefinedBrowserStrings[i], ABrowser) then Break;
  Result := (ABrowser <> '');
end;


