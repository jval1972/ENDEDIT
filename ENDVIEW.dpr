program ENDVIEW;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  viewermain in 'viewermain.pas' {Form1},
  ee_screen in 'ee_screen.pas',
  ee_utils in 'ee_utils.pas',
  ee_dosfont in 'ee_dosfont.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
