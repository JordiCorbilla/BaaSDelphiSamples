// Copyright (c) 2016, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit frmUrlManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    edtUser: TEdit;
    Label1: TLabel;
    edtPassword: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtUrl: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  lib.urls, lib.kinvey.rest, lib.urls.converter, System.Contnrs, Generics.Collections;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  url : IUrls;
begin
  url := TUrls.Create(edtUser.Text, edtPassword.Text, edtUrl.Text);
  if TKinveyRest.New.Add(url.ToJsonString()) then
  begin
    showMessage('Item has been successfully added!');
    edtUser.Text := '';
    edtPassword.Text := '';
    edtUrl.Clear;
    Button2Click(sender);
  end
  else
    showMessage('There was an error adding the data to the cloud storage');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  collection : string;
  jsonConverter : TUrlConverter;
  listUrls : TList<IUrls>;
  url : IUrls;
  listItem : TListItem;
begin
  Listview1.Clear;
  collection := TKinveyRest.New.GetCollection();
  jsonConverter := TUrlConverter.Create(collection);
  listUrls := jsonConverter.GetCollection();
  for url in listUrls do
  begin
    listItem := Listview1.Items.Add;
    listItem.Caption := url.User;
    listItem.SubItems.Add(url.Password);
    listItem.SubItems.Add(url.Url);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button2Click(sender);
end;

end.
