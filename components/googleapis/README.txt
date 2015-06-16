This package installs the Google APIs in Free Pascal 3.1 in the lazarus
component palette.

Each Google Service API is a REST API that offers multiple resources.

See the end of this file for instructions for compiling with FPC 2.6.x

==================
Component overview
==================

A Google API breaks down in 4 parts, all of which have a base class in the
following units:

TGoogleClient - unit googleclient
---------------------------------

This is a simple component that handles the transport and authorization,
it needs a TFPWebClient descendent (such as TFPHTTPWebClient, available in
the weblaz package) and a TFPOauth2Handler descendent to communicate with 
Google servers (the component creates a default one). 

These 2 classes are part of fcl-web, a synapse-based TFPWebclient descendant
is available separately.

TGoogleAPI - unit googleservice
-------------------------------

There is a descendent of this component for each Google service API, which
handles all calls to the service. It uses a TGoogleClient component to
handle actual communication.

This class contains a method called ServiceCall which is used by all
resources in the API to execute service requests. It will use the client to
do the actual HTTP request.

Each unit google*.pp in this package contains a single component that
descends from the TGoogleAPI component.

TGoogleResource - unit googleservice
------------------------------------   

For each resource exposed by the service, a descendent of this class is generated
that has all the methods for that resource, as described in the REST service
description.

TGoogleResource uses an instance of the TGoogleAPI class to handle all calls to  
the service. 

Each API unit google*.pp in this package contains one or more
TGoogleResource    
descendents, used in the API of that unit.

TGoogleBaseObject - unit googlebase
-----------------------------------

For each data type used in the API, a descendent of this class is used: it is a
descendent of TBaseObject (unit restbase, part of fcl-web) and handles
loading from and saving to JSON.

====================
Demo programs setup.
====================
The demo programs create the necessary API components in code, it is
therefor not necessary to install the lazgoogleapis package in the IDE,
it just has to be opened and compiled.

There are 4 demo programs. Before they can be used, they must be defined in Google.

The following procedure is not needed for the discovery demo, it does not
need authentication.

You will need to:

1. Have a google account

2. Be registered as a google Developer. Just go to the google developer console at:
   https://console.developers.google.com/

3. Register a new project in the console.

4. Create a new client ID and Secret key under Credentials (APIs & Auth)
   As a redirect URI, enter:  urn:ietf:wg:oauth:2.0:oob

   The Client ID and secret key must be saved in the google.ini file in the example directory:

[Credentials]
ClientID=Your client ID
ClientSecret=Your secret key
Scope=https://www.googleapis.com/auth/calendar
 
The scope must match the API that is used. (the example files contain the
correct scope)

5. Under credentials, request credentials (under APIs) for
   Calendar API
   Drive API
   Tasks API

You can re-use the same Client ID and secret for all examples, or you
can create different client IDs and keys, or even create different projects.

========================
Compiling with FPC 2.6.4
========================

The code works with FPC 2.6.4. To work with FPC 2.6.4, you need the
following files from the FPC SVN repository (trunk):

From packages/fcl-web/src/base:

fpoauth2.pp
fphttpwebclient.pp
fpwebclient.pp
restcodegen.pp
restbase.pp
fpoauth2ini.pp
fpjwt.pp

From packages/googleapi/src
All *.pp files

There is a directory 2_6_4 to which the files can be copied.

Additionally, in the directory 2_6_4 there is a unit synapsewebclient.
This unit implements a TFPWebclient descendent that works with Synapse.
It works with version 2.6.4, and with version 3.x of Free Pascal.

You need to have compiled the laz_synapse package (distributed with synapse)
for this to work.

To compile the package and the demos, copy the needed files to directory 2_6_4 and 
add this directory to the source path of the lazgoogleapi package and all
demo programs.
