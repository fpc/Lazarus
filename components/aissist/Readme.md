# Lazarus AI assistant - AIssist

This directory contains an implementation of a Lazarus AI assistent.
There are several directories and packages.

To install, install the following packages in the correct order:

- In the components/chatcontrol directory, install the lazchatctrl package.
  This package contains the lazarus chat controls, needed in some of the
  forms and demos.

- Install the aissist package in the api dir.
  This package contains an abstract 'AI assistant' client. It relies on API
  providers to implement actual APIs

- Install the janai package in the api/janai dir
  This package contains a Jan AI API. It is compatible to the ChatGPT API,
  and should be usable for communicating with ChatGPT. (but you need a
  license key) 

- Install the laz_aissist package. 
  This package contains the actual integration of the AI client in the Lazarus IDE.
  It registers the following things:
   - a menu entry in the 'View' menu called 'AIssist chat'.
   - A settings page in the tools - options menu: the 'AI Assistant options'   page.
      Here you must set :
       - The protocol to use (currently only the JanAI/ChatGPT API is supported)
      - The URL where the AI requests must be sent
      - The model you wish to use.
      - The maximum lengt of the AI replies

The Demos directory contains a console and GUI demo of the AIClient class. 
They serve to demonstrate the API and can be used separately from the IDE.

