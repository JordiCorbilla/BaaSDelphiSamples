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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, lib.firebase.rest, DBXJSON,
  System.JSON, Data.DBXJSONCommon, lib.document, generics.collections,
  Vcl.OleCtrls, SHDocVw;

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
    Memo2: TMemo;
    Button4: TButton;
    ListBox1: TListBox;
    WebBrowser1: TWebBrowser;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AddFileClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    function LoadDocuments(jsonString: string): TList<IDocument>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  NetEncoding, IdCoderMIME, IdHash, IdHMAC, IdHMACSHA1, IdGlobal, StrUtils;

{$R *.dfm}

procedure TForm3.AddFileClick(Sender: TObject);
var
  strFileStream: TFileStream;
  arr : TJSONArray;
  s : string;
  ByteArray: array of Byte;
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
  List : TList<IDocument>;
  i: Integer;
begin
  firebase := TFirebaseRest.New.GetCollection;
  List := LoadDocuments(firebase);
  //Memo1.Lines.Add(firebase);

  for i := 0 to list.Count-1 do
  begin
    listbox1.AddItem(list[i].FileName, TDocument(list[i]));
  end;


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

procedure TForm3.ListBox1DblClick(Sender: TObject);
var
  document : TDOcument;
begin
  if (ListBox1.ItemIndex >= 0) then
  begin
    WebBrowser1.Navigate('about:blank');
    sleep(2000);
    Application.ProcessMessages;
    document := (listBox1.Items.Objects[ListBox1.ItemIndex] as TDocument);
    document.Save;
    WebBrowser1.Navigate('c:\temp\' + document.FileName);
  end;
end;

function TForm3.LoadDocuments(jsonString : string) : TList<IDocument>;
var
  s : string;
  i : integer;
  j, k : integer;
  doc : string;
  name : string;
  list : TList<IDocument>;
begin
  list := TList<IDocument>.Create;
  s := jsonString;
  i := 1;
  while i > 0 do
  begin
    i := AnsiPos('array', s);
    j := AnsiPos('document', s);
    if ((i > 0) and (j>0)) then
    begin
      doc := copy(s, i, j-i);
      doc := doc.Replace('array":', '');
      doc := doc.Replace(',"', '');
      name := AnsiRightStr(s, length(s)-j+1);
      k := AnsiPos('}', name);
      name := copy(name, 0, k);
      name := name.Replace('document":"', '');
      name := name.Replace('"}', '');
      s := AnsiRightStr(s, length(s)-(j+k));
      list.Add(TDocument.New(name, doc));
    end;
  end;
  result := list;
end;

procedure TForm3.Button4Click(Sender: TObject);
var
  s : string;
  i : integer;
  j, k : integer;
  doc : string;
  name : string;
  jsonArray :   TJSONArray;
    fs: TFileStream;
    Stream : TStream;
    buf: TBytes;
begin
  //Parse the following string
  //{"-KUmo1JUUAVTRAiZVXKd":{"array":[37,80],"document":"TESTING FIREBASE.pdf"}}
  s := '{"-KUmo1JUUAVTRAiZVXKd":{"array":[37,80],"document":"TESTING FIREBASE.pdf"},"-KUmo1JUUAVTRAiZVXKd":{"array":[37,80],"document":"TESTING FIREBASE.pdf"}}'; //memo1.Lines.ToString;
  s := memo1.Lines.Text;

  i := 1;
  while i > 0 do
  begin
    i := AnsiPos('array', s);
    j := AnsiPos('document', s);
    if ((i > 0) and (j>0)) then
    begin
      doc := copy(s, i, j-i);
      //array":[37,80],"
      doc := doc.Replace('array":', '');
      doc := doc.Replace(',"', '');
      memo2.Lines.Add(doc);

      name := AnsiRightStr(s, length(s)-j+1);
      k := AnsiPos('}', name);
      name := copy(name, 0, k);
      name := name.Replace('document":"', '');
      name := name.Replace('"}', '');
      memo2.Lines.Add(name);
      s := AnsiRightStr(s, length(s)-(j+k));
      //document":"TESTING FIREBASE.pdf"}
    end;
  end;

   jsonArray := TJSONObject.ParseJSONValue(doc) as TJSONArray;

   fs := TFileStream.Create('c:\temp\' + name, fmCreate);
   Stream := TDBXJSONTools.JSONToStream(jsonArray);
   SetLength(buf, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(buf[0], Stream.Size);
    fs.WriteBuffer(buf[0], Stream.Size);
    Stream.Free;
    fs.Free;
end;

end.
