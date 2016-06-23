program urlManager;

uses
  Vcl.Forms,
  frmUrlManager in 'frmUrlManager.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  lib.kinvey.rest in 'lib.kinvey.rest.pas',
  lib.urls in 'lib.urls.pas',
  lib.options in 'lib.options.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
