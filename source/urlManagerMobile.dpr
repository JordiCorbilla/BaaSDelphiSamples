program urlManagerMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmUrlManagerMobile in 'frmUrlManagerMobile.pas' {Form2},
  lib.options in 'lib.options.pas',
  lib.urls in 'lib.urls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
