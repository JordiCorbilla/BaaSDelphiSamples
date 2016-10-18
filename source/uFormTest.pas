unit uFormTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, lib.firebase.rest;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  firebase : string;
begin
  firebase := TFirebaseRest.New.GetCollection;
  Memo1.Lines.Add(firebase);
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  jsonString : string;
begin
  jsonString := '{"param1":"'+Edit1.text+'","value":"'+Edit2.text+'"}';
  if TFirebaseRest.New.Add(jsonString) then
  begin
    showMessage('Item has been successfully added!');
  end
  else
    showMessage('There was an error adding the data to the cloud storage');
end;

end.
