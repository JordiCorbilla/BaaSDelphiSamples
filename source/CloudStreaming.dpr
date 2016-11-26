program CloudStreaming;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMain in 'frmMain.pas' {main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tmain, main);
  Application.Run;
end.
