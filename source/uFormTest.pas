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

unit uFormTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, lib.firebase.rest, DBXJSON, System.JSON, Data.DBXJSONCommon;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Button2: TButton;
    AddFile: TButton;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    GetFile: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AddFileClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure GetFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  NetEncoding, IdCoderMIME, IdHash, IdHMAC, IdHMACSHA1, IdGlobal;

{$R *.dfm}

procedure TForm3.AddFileClick(Sender: TObject);
var
  strFileStream: TFileStream;
  arr : TJSONArray;
  s : string;
  ByteArray: array of Byte;
  B: byte;
begin
  OpenDialog1.InitialDir := GetCurrentDir;

  // Only allow existing files to be selected
  OpenDialog1.Options := [ofFileMustExist];

  // Allow only .dpr and .pas files to be selected
  OpenDialog1.Filter :=
    'PDF Files|*.pdf|MS Word|*.docx';

  // Select pascal files as the starting filter type
  //OpenDialog1.FilterIndex := 2;

  // Display the open file dialog
  if OpenDialog1.Execute then
  begin
    //ShowMessage('File : '+OpenDialog1.FileName);
    strFileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    arr := TDBXJSONTools.StreamToJSON(strFileStream, 0, strFileStream.Size);

    strFileStream.Position := 0;
    SetLength(ByteArray, strFileStream.Size);
    strFileStream.Read(ByteArray[0], strFileStream.Size);
    strFileStream.Free;

    //s := String(TNetEncoding.URL.EncodeBytesToString(TIdEncoderMIME.EncodeBytes(IndyTextEncoding_UTF8.GetBytes(ByteArray))));
    s := '{"document":"'+extractfilename(OpenDialog1.FileName)+'","array":'+arr.toJSON+'}';
    TFirebaseRest.New.Add(s);

    //arr.
  end
  else
  begin
    ShowMessage('Open file was cancelled');
  end;

  // Free up the dialog
  OpenDialog1.Free;
end;

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

procedure TForm3.Button3Click(Sender: TObject);
begin
  TFirebaseRest.New.Delete();
end;

procedure TForm3.GetFileClick(Sender: TObject);
var
 arr : TJSONArray;
 firebase : string;
 LJsonArr   : TJSONArray;
 LJsonValue : TJSONValue;
 LItem : TJSONValue;
 s, s1 : string;
begin
  firebase := TFirebaseRest.New.GetCollection;


  //arr := TJSONarray.Create();
  LJsonArr := TJSONObject.ParseJSONValue(firebase, false) as TJSONArray;
    for LJsonValue in LJsonArr do
    begin
      for LItem in TJSONArray(LJsonValue) do
      begin
        if (TJSONPair(LItem).JsonString.Value = 'array') then
          s := TJSONPair(LItem).JsonValue.Value;
        if (TJSONPair(LItem).JsonString.Value = 'document') then
          s1 := TJSONPair(LItem).JsonValue.Value;
      end;
    end;
  //strFileStream := TDBXJSONTools.JSONToStream(JSONArray);
end;

end.
