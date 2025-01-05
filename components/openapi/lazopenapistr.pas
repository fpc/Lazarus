unit lazopenapistr;

{$mode objfpc}{$H+}

interface

const
  SProjectRefreshOpenAPIName = 'PrjRefreshOpenAPI';
  SDataOpenAPIFile = 'OpenAPIFile';
  SDataOpenAPIConfig = 'OpenAPIConfig';
  SDataOpenAPIBaseFileName = 'OpenAPIBase';
  SConfigFileName = 'lazopenapi.cfg';

Resourcestring
  SCMDOpenAPIWizard = 'ShowOpenAPICodeGenerator';
  SCMDOpenAPIWizardCaption = 'OpenAPI code generator...';
  SOpenAPICodeGenerator = 'OpenAPI code generation';
  SErrFailedToGenerateAPI = 'Failed to generate OpenAPI files. Unexpected error %s with message: %s';
  SProjectOpenAPIClient = 'OpenAPI client application';
  SProjectOpenAPIClientDescription = 'A client application to consume a REST service described by an OpenAPI file.';
  SProjectOpenAPIServer = 'OpenAPI server application';
  SProjectOpenAPIServerDescription = 'A server application to offer an REST service consume a REST service described by an OpenAPI file.';
  SProjectOpenAPIClientServer = 'OpenAPI client and server applications';
  SProjectOpenAPIClientServerDescription = 'Client and server applications to consume and offer a REST service described by an OpenAPI file.';
  SErrFailedToCreateProjectDir = 'Failed to create project directory "%s"';
  SErrInvalidOpenAPIFile = 'Invalid OpenAPI description file: "%s"';
  SErrInvalidOpenAPIConfigFile = 'Invalid OpenAPI code generation config file: "%s"';
  SRegenerateOpenAPI = 'Regenerate OpenAPI units';
  SOpenAPIProjectOptionsCaption = 'Open API options';

implementation

end.

