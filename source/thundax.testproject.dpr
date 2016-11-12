program thundax.testproject;

uses
  Vcl.Forms,
  uFormTest in 'uFormTest.pas' {Form3},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Blue');
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
