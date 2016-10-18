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

unit lib.firebase.rest;

interface

uses
  lib.urls, IdHTTP, IdIOHandler, IdIOHandlerStream,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdGlobal,
  System.SysUtils, System.Variants, System.Classes, lib.options;

type
  IFirebaseRest = interface
    function Add(jsonString: string) : boolean;
    function GetCollection() : string;
  end;

  TFirebaseRest = class(TInterfacedObject, IFirebaseRest)
  private
    FOptions : IOptions;
  public
    function Add(jsonString: string) : boolean;
    function GetCollection() : string;
    constructor Create();
    class function New() : IFirebaseRest;
  end;

implementation

uses
  IdCoderMIME;

{ TFirebaseRest }

function TFirebaseRest.Add(jsonString: string): boolean;
begin

end;

constructor TFirebaseRest.Create;
begin
  FOptions := TOptions.New.Load;
end;

function TFirebaseRest.GetCollection: string;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  encodedHeader : string;
begin
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

end.
