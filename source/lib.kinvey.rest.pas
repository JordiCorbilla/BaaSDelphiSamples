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

unit lib.kinvey.rest;

interface

uses
  lib.urls, System.Contnrs, Generics.Collections, IdHTTP, IdIOHandler, IdIOHandlerStream,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdGlobal,
  System.SysUtils, System.Variants, System.Classes;

type
  TKinveyRest = class(TObject)
    class procedure Add(url : IUrls);
    class function GetUrls() : TList<IUrls>;
  end;

implementation

{ TKinveyRest }

class procedure TKinveyRest.Add(url: IUrls);
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  JsonToSend: TStringStream;
begin
  JsonToSend := TStringStream.Create('{"user":"'+url.User+'","password":"'+url.Password+'","Url":"'+url.Url+'"}');
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.CustomHeaders.Values['Authorization'] := 'Basic';
      IdHTTP.Request.CustomHeaders.Values['X-Kinvey-API-Version'] := '3';
      IdHTTP.Request.ContentType := 'application/json';
      response := IdHTTP.Post('https://baas.kinvey.com/appdata//websites/', JsonToSend);
      response := response.Replace(AnsiChar(#10), '');
      //result := (response.Contains('createdAt'));
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
    JsonToSend.Free;
  end;
end;

class function TKinveyRest.GetUrls: TList<IUrls>;
begin

end;

end.
