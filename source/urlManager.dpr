program urlManager;

uses
  Vcl.Forms,
  frmUrlManager in 'frmUrlManager.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  lib.urls in 'lib.urls.pas',
  lib.urls.converter in 'lib.urls.converter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Turquoise Gray');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
