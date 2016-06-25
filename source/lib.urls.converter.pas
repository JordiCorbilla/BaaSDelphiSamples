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

unit lib.urls.converter;

interface

uses
  System.Contnrs, Generics.Collections, lib.urls;

type
  TUrlConverter = class(TObject)
  private
    FJsonString : string;
  public
    constructor Create(jsonString : string);
    function GetCollection() : TList<IUrls>;
  end;

implementation

uses
  DBXJSON, System.JSON, System.SysUtils;

{ TUrlConverter }

constructor TUrlConverter.Create(jsonString: string);
begin
  FJsonString := jsonString;
end;

function TUrlConverter.GetCollection: TList<IUrls>;
var
  LJsonArr   : TJSONArray;
  LJsonValue : TJSONValue;
  LItem : TJSONValue;
  user : string;
  password : string;
  url : string;
  collection : TList<IUrls>;
begin
  collection := TList<IUrls>.create();
  try
    LJsonArr := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(FJsonString),0) as TJSONArray;
    for LJsonValue in LJsonArr do
    begin
      for LItem in TJSONArray(LJsonValue) do
      begin
        if (TJSONPair(LItem).JsonString.Value = 'user') then
          user := TJSONPair(LItem).JsonValue.Value;
        if (TJSONPair(LItem).JsonString.Value = 'password') then
          password := TJSONPair(LItem).JsonValue.Value;
        if (TJSONPair(LItem).JsonString.Value = 'url') then
          url := TJSONPair(LItem).JsonValue.Value;
      end;
      collection.Add(TUrls.Create(user, password, url));
    end;
  finally
    result := collection;
  end;
end;

end.
