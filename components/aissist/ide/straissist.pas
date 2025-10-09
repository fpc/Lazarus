{ Copyright (C) 2024

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt

  Abstract: Strings for AI Assistant
}
unit StrAIssist;

{$mode objfpc}{$H+}

interface

const
  SAISSistChatForm = 'AIssistChatForm';
  SCMDViewAIssistChatForm = 'ViewAIssistChatForm';
  SCMDExplainSelectedCode = 'AIExplainSelectedCode';

  SConfigFile = 'aissist.xml';
  KeyServerURL = 'ServerURL';
  KeyProtocol = 'Protocol';
  KeyDefaultModel = 'DefaultModel';
  KeyDefaultMaxLength = 'DefaultMaxLength';
  KeyAuthorizationKey = 'AuthorizationKey';

Resourcestring
  SAIssistChatMenuCaption = 'AIssist chat';
  SAIExplainSelectedCodeCaption = 'AI Explain Selected Code';
  SConfigTitle  = 'AI Assistant options';

  SErrorTitle   = 'Drat!';
  SErrorIntro   = 'An error occurred while talking to the AI!';
  SErrorInfo    = 'Here is the error we got: %s';
  SErrorContext = 'This is what we were trying to do: %s %s';
  SErrorBody    = 'And this is what we were saying:';

  SErrPleaseConfigure = 'No AI server has been configured.'+sLineBreak+
                        'Use the "Configure" button to enter IDE options and configure an AI server';
  SErrPleaseEnterPrompt = 'Please enter an AI prompt such as:'+sLineBreak+
                          '"generate a ''Hello,World!'' program in Pascal".';
  SErrAIWaiting = 'The AI engine is still answering your previous prompt.'+
                  'Please wait for it to finish.';
  SErrNoAnswer = 'No answer from AI, try refining your prompt';
  SExplainPrompt = 'Explain the following Object Pascal code:';
  SNoExplanation = 'The AI could not explain. Maybe ask a different question?';
  SAIExplanation = 'This is what the AI thinks of the selected code:';

  SendPromptCaption = 'Send AI prompt';
  EditPromptCaption = 'Edit AI prompt';

implementation

end.

