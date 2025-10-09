# Lazarus AI assistant - AIssist

This directory contains an implementation of a Lazarus AI assistent.
There are several directories and packages.

To install, install the following packages in the correct order:

- In the components/chatcontrol directory, install the lazchatctrl package.
  This package contains the lazarus chat controls, needed in some of the
  forms and demos.

- Install the ./api/lazllms.lpk package in the api dir.
  This package contains an abstract 'LLM client' API. It relies on API
  providers to implement actual APIs: Currently Claude, Gemini, ChatGPT and
  OLLama are supported. The former JanAI package is replaced by the ChatGPT API.

- Install the ide/laz_aissist package. 
  This package contains the actual integration of the AI client in the Lazarus IDE.
  It registers the following things:
   - a menu entry in the 'View' menu called 'AIssist chat'.
   - A settings page in the tools - options menu: the 'AI Assistant options'   page.
      Here you must set :
       - The protocol to use (currently only the JanAI/ChatGPT API is supported)
      - The URL where the AI requests must be sent
      - The model you wish to use.
      - The maximum lengt of the AI replies
   - A "source" menu entry 'AI explain selected code' which will launch the
        AI and lets the AI provide an explanation of the selected code.

The Demos directory contains a console and GUI demo of the LLMClient class
and the various LLM provider APIS.

They serve to demonstrate the API and can be used separately from the IDE.