
This is the fpWeb components directory README.
It contains 2 packages:

The LazWeb package implements IDE functionality for FCL-Web.
It should work with version 2.4.x of FPC.

The LazWebExtra package implements more IDE projects and components,
which are available only as of version 2.5.1 of the compiler.
As these items become available in a stable release, the package will be
merged in Lazweb.

Both packages registers a series of project wizards, and some components.

The following project types are registered in the lazweb package:

- CGI application
- Apache Application (an apache module)
- Custom CGI application ("bare bones" cgi, no web modules)
- FastCGI application
- Custom FastCGI application ("bare bones" fastcgi, no web modules)

For each of these projects, a Web Module is created by default. 
The following web modules can be created:
- WebDataModule (TFPWebModule)
    This is a general-purpose HTTP request handling module, 
    which can be used to handle any HTTP request for any kind of data.
- HTMLModule (TFPHTMLModule)
    This is a specific HTTP request handling module, aimed specially
    at producing a HTML response.

The following components are registered:

 THTMLDatasetContentProducer
  Creates a HTML table based on a TDataset descendent
 THTMLSelectProducer
  Creates a <SELECT> element based on a TStringList.
 THTMLDatasetSelectProducer
  Creates a <SELECT> element based on a dataset (a lookup combo) 
 THTMLEntityProducer
  Creates a HTML ENTITY document
 THTMLPageProducer
  Creates a HTML page using an event handler.
THTMLDataSetFormShowProducer
  Creates a <FORM> which allows one to show a single record of a dataset.
THTMLDataSetFormEditProducer
  Creates a <FORM> which allows one to edit a single record of a dataset.
THTMLDataSetFormGridProducer
  Creates a series of forms.

The lazwebextra package additionally registers the following web modules
and components:
- Web Data Provider module (TFPWebProviderDataModule)
   Used to convert TDataset to a variety of formats and to handle updates to the data.
   (ExtJS JSON and XML currently)
- JSON-RPC request handling module (TJSONRPCModule)
   Used to handle JSON-RPC requests.
- Ext.Direct handling of JSON-RPC requests (TExtDirectModule)
   Handles the Ext.Direct variant of JSON-RPC.

The following components are registered:

TFPWebDataProvider
  Handles CRUD (Create/Read/Update/Delete) operations on a TDataset

TSQLDBWebDataProvider
  Handles CRUD (Create/Read/Update/Delete) operations on a TSQLConnection.
  (INSERT/SELECT/UPDATE/DELETE SQL statements can be specified).

TWebdataInputAdaptor
  Transforms HTTP request to a format that TFPWebDataProvider
  understands, using event handlers.

TExtJSJSonWebdataInputAdaptor
  TWebdataInputAdaptor descendent that transforms a ExtJS Datastore
  JSON request to input usable by TFPWebDataProvider.

TExtJSJSONDataFormatter
  Outputs JSON as expected by ExtJS datastores from all operations on a 
  TFPWebDataProvider instance.

TExtJSXMLWebdataInputAdaptor
  TWebdataInputAdaptor descendent that transforms a ExtJS Datastore
  XML request to input usable by TFPWebDataProvider.

TExtJSXMLDataFormatter,  
  Outputs XML as expected by ExtJS datastores from all operations on a
  TFPWebDataProvider instance.

TJSONRPCHandler
  Component that handles a single JSON-RPC request.

TJSONRPCDispatcher
  Component that dispatches a batch of JSON-RPC requests to appropriate
  TJSONRPCHandler instances.

TSessionJSONRPCDispatcher
  Component that dispatches a batch of JSON-RPC requests to appropriate
  TJSONRPCHandler instances, and which is session-aware. (session in the
fcl-web sense of the word)

TJSONRPCContentProducer
  Component that handles input from a HTTP request, passes it on to a
  TJSONRPCDispatcher instance and formats the result.

TExtDirectDispatcher
TExtDirectContentProducer
  Descendents of TJSONRPCDispatcher and TJSONRPCContentProducer that
  understand the Ext.Direct variant of extJS.

Demo projects are available in the demo subdirectory, and additional demos
can be found in the fcl-web/examples directory of FPC.

Additional READMEs with information on the components are in the fcl-web/src/*
directories.

Author: Michael Van Canneyt & Joost van der Sluis
