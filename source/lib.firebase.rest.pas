// Copyright (c) 2017, Jordi Corbilla
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

unit lib.firebase.rest;

interface

uses
  lib.urls, IdHTTP, IdIOHandler, IdIOHandlerStream,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdGlobal,
  System.SysUtils, System.Variants, System.Classes, lib.options,
  IdSSLOpenSSLHeaders_Static, System.IOUtils;

type
  IFirebaseRest = interface
    function Add(jsonString: string) : boolean;
    function GetCollection() : string;
    function Delete() : boolean;
    function RegisterDeviceToken(token: string; apiKey : string) : string;
  end;

  TFirebaseRest = class(TInterfacedObject, IFirebaseRest)
  private
    FOptions : IOptions;
  public
    function Add(jsonString: string) : boolean;
    function GetCollection() : string;
    function Delete() : boolean;
    constructor Create();
    function RegisterDeviceToken(token: string; apiKey : string) : string;
    class function New() : IFirebaseRest;
  end;

implementation

uses
  IdCoderMIME;

{ TFirebaseRest }

function TFirebaseRest.Add(jsonString: string): boolean;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  JsonToSend: TStringStream;
begin
  JsonToSend := TStringStream.Create(jsonString);
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
//      IdHTTP.ReadTimeout := 30000;
//      IdHTTP.HandleRedirects := True;
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.ContentType := 'application/json';
      response := IdHTTP.Post('https://delphitestproject.firebaseio.com/.json?auth='+FOptions.FirebaseAuth, JsonToSend);
      response := response.Replace(Char(#10), '');
      result := (response.Contains('name'));
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
    JsonToSend.Free;
  end;
end;

constructor TFirebaseRest.Create;
begin
{$IFDEF ANDROID}
  FOptions := TOptions.Create;
  FOptions.FirebaseAuth := '7NZu878Wr56kWm4dSnVIoX52nd02zIRFsoGs7O1y';
{$ELSE}
  FOptions := TOptions.New.Load;
{$ENDIF}
end;

function TFirebaseRest.Delete: boolean;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  JsonToSend: TStringStream;
begin
  JsonToSend := TStringStream.Create('');
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
//      IdHTTP.ReadTimeout := 30000;
//      IdHTTP.HandleRedirects := True;
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.ContentType := 'application/json';
      IdHTTP.Delete('https://delphitestproject.firebaseio.com/.json?auth='+FOptions.FirebaseAuth, JsonToSend);
      response := response.Replace(Char(#10), '');
      result := (response.Contains('name'));
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
    JsonToSend.Free;
  end;
end;

//Sample using curl
//curl --cacert ca-bundle.crt -X GET -H 'Content-Type: application/json' "https://delphitestproject.firebaseio.com/.json?auth=7NZu878Wr56kWm4dSnVIoX52nd02zIRFsoGs7O1y"
//curl: (6) Could not resolve host: application
//{"Process":"Windows"}

function TFirebaseRest.GetCollection: string;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
begin
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
//      IdHTTP.ReadTimeout := 30000;
//      IdHTTP.HandleRedirects := True;
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.ReadTimeout := IdTimeoutInfinite;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.ContentType := 'application/json';
      response := IdHTTP.Get('https://delphitestproject.firebaseio.com/.json?auth='+FOptions.FirebaseAuth);
      result := response;
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
  end;
end;

class function TFirebaseRest.New: IFirebaseRest;
begin
  result := Create;
end;

//Register device token example: https://developers.google.com/instance-id/reference/server
//https://iid.googleapis.com/iid/v1:batchImport
//{
//  "application": "com.google.FCMTestApp",
//  "sandbox":false,
//  "apns_tokens":[
//      "368dde283db539abc4a6419b1795b6131194703b816e4f624ffa12",
//      "76b39c2b2ceaadee8400b8868c2f45325ab9831c1998ed70859d86"
//   ]
//  }
//}
function TFirebaseRest.RegisterDeviceToken(token, apiKey: string): string;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  jsonString : string;
  JsonToSend: TStringStream;
begin
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.ReadTimeout := IdTimeoutInfinite;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.CustomHeaders.Values['Authorization: key'] := apiKey;
      IdHTTP.Request.ContentType := 'application/json';
      jsonString := '{"application": "com.google.FCMDelphiTest","sandbox":false,"apns_tokens":["'+token+'"]}';
      JsonToSend := TStringStream.Create(jsonString);
      response := IdHTTP.Post('https://iid.googleapis.com/iid/v1:batchImport', JsonToSend);
      response := response.Replace(Char(#10), '');
      result := response;
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
  end;
end;

end.
