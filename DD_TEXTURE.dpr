program DD_TEXTURE;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  tx_gl in 'tx_gl.pas',
  dglOpenGL in 'dglOpenGL.pas',
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  tx_utils in 'tx_utils.pas',
  tx_glutils in 'tx_glutils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Texture Perpective';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
