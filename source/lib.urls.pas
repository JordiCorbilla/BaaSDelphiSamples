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

unit lib.urls;

interface

type
  IUrls = interface
    procedure SetPassword(const Value: string);
    procedure SetUrl(const Value: string);
    procedure SetUser(const Value: string);
    function GetUser() : string;
    function GetPasword() : string;
    function GetUrl() : string;
    property User : string read GetUser write SetUser;
    property Password : string read GetPasword write SetPassword;
    property Url : string read GetUrl write SetUrl;
  end;

  TUrls = class(TInterfacedObject, IUrls)
  private
    FPassword: string;
    FUrl: string;
    FUser: string;
    procedure SetPassword(const Value: string);
    procedure SetUrl(const Value: string);
    procedure SetUser(const Value: string);
    function GetUser() : string;
    function GetPasword() : string;
    function GetUrl() : string;
  public
    property User : string read GetUser write SetUser;
    property Password : string read GetPasword write SetPassword;
    property Url : string read GetUrl write SetUrl;
    Constructor Create(user, password, url : string);
  end;

implementation

{ TUrls }

constructor TUrls.Create(user, password, url: string);
begin
  SetUser(user);
  SetPassword(password);
  SetUrl(url);
end;

function TUrls.GetPasword: string;
begin
  result := FPassword;
end;

function TUrls.GetUrl: string;
begin
  result := FUrl;
end;

function TUrls.GetUser: string;
begin
  result := FUser;
end;

procedure TUrls.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TUrls.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

procedure TUrls.SetUser(const Value: string);
begin
  FUser := Value;
end;

end.
