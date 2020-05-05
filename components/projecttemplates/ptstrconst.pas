unit ptstrconst;

{$mode objfpc}{$H+}

interface

resourcestring
  SErrNoSuchTemplate = '"%s": No such template.';
  SErrCouldNotCreateDir = 'Could not create directory "%s"';
  SErrFailedToCopyFile = 'Failed to copy file "%s" to "%s"';

  STemplateCategory = 'Template projects';
  SProjectTemplateSettings = 'Project templates options ...';
  SNewFromTemplate = 'New project from template';

  SVariable    = 'Variable';
  SValue       = 'Value';
  SDescription = 'Description';
  SNoAdditionalVars = 'This project has no additional variables.';
  //
  SNameforProject = '&Name for new project:';
  SCreateinDir    = 'Create in &directory:';
  SThisProject    = 'This project contains some additional variables. Please provide values for these variables.';

  STitle = 'Project templates settings';
  SDirect= '&Directory with templates:';

implementation

end.

