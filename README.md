# BaaS Delphi Indy Samples over https

[![Delphi version](https://img.shields.io/badge/delphi-Berlin10.1update2-red.svg?style=plastic)](https://app.box.com/s/p7hwuaic1qsm14juf3pmuojv0ko98ok5)
![License](https://img.shields.io/badge/license-MIT-green.svg?style=plastic)

![](https://3.bp.blogspot.com/-_QZgeowOPYQ/V2xA5wUUHLI/AAAAAAAAFgQ/6Ca1guejVVw2U1s3EYi56bGkshge8b02ACLcB/s1600/mbaas.png)
![](https://3.bp.blogspot.com/-R9V-4Cq47sI/WDqsB6Lbk0I/AAAAAAAAF1Q/DQP6AkGaJxstXBpLLN-6jiVgU2AkkoXYgCLcB/s1600/firebasediagram.png)

==============
Code samples for **BaaS ([Kinvey](https://www.kinvey.com/))**, **BaaS ([Firebase](https://firebase.google.com/))** and **PaaS ([Parse.com](http://parse.com/))** using Delphi 10.1 Berlin Update 2 and [Indy Library](http://www.indyproject.org/index.en.aspx)
--------------
**Related Articles:**
  - **Kinvey**:
    - [BaaS with Kinvey and Delphi 10.1 Berlin](http://thundaxsoftware.blogspot.co.uk/2016/06/baas-with-kinvey-and-delphi-101-berlin.html)
  - **Parse.com**:
    - [Sending REST API messages with Delphi to Parse.com](http://thundaxsoftware.blogspot.co.uk/2015/12/sending-rest-api-messages-with-delphi.html)
    - [Deploying Parse server to Heroku](http://thundaxsoftware.blogspot.co.uk/2016/05/deploying-parse-server-to-heroku.html)    
    - [Creating your own self-hosted Parse server](http://thundaxsoftware.blogspot.co.uk/2016/05/creating-your-own-self-hosted-parse.html)
  - **Firebase**:
    - [Firebase file streaming with Delphi](http://thundaxsoftware.blogspot.co.uk/2016/11/firebase-file-streaming-with-delphi.html)

**Win64 example app using Kinvey**

![](https://4.bp.blogspot.com/-AsqDrk0ZnAA/V26CtdQr9FI/AAAAAAAAFhc/dMZrrIs3bJoGPVx2Vsc8nxt46i4W4pqcQCLcB/s640/vclExample.png)

**Android example app using Kinvey**

![](https://3.bp.blogspot.com/-aYTpo_Q5MVc/V26ViH6L8FI/AAAAAAAAFiE/QNomSrfWic0ZEeTD8Na6IyzUOuWnzPC5gCLcB/s640/Screenshot_2016-06-25-14-47-29.png)

**Data stored in Kinvey**

![](https://1.bp.blogspot.com/-q6DpnnASkCY/V2-a06zpAfI/AAAAAAAAFiU/6KLybvIw-KsJ9ewDYQIwYy9xUvUGRpFJgCLcB/s640/Kinvey7.png)

These can be downloaded here:
  - https://github.com/JordiCorbilla/BaaSDelphiSamples/releases

*Example usage Kinvey via Https:*
```Delphi
function TKinveyRest.GetCollection: string;
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
      encodedHeader := TIdEncoderMIME.EncodeString(FOptions.AppId + ':' + FOptions.MasterSecret);
      IdHTTP.Request.CustomHeaders.Values['Authorization'] := 'Basic ' + encodedHeader;
      IdHTTP.Request.CustomHeaders.Values['X-Kinvey-API-Version'] := '3';
      IdHTTP.Request.ContentType := 'application/json';
      response := IdHTTP.Get('https://baas.kinvey.com/appdata/'+FOptions.AppId+'/'+FOptions.Collection+'/');
      result := response;
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
  end;
end;
```

*Example usage Firebase via Https:*
```Delphi
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
```

*Example usage Parse.com via Https:*
```Delphi
procedure TParseRest.GetCollection: string;
var
  response : string;
  JSONToSend: TStringStream;
begin
  JSONToSend := TStringStream.Create('{}');
  IdHTTP1.Request.Connection := 'Keep-Alive';
  IdSSLIOHandlerSocketOpenSSL1.SSLOptions.Method := sslvSSLv23;
  IdHTTP1.Request.CustomHeaders.Clear;
  IdHTTP1.Request.CustomHeaders.Values['X-Parse-Application-Id'] := 'yourAppId';
  IdHTTP1.Request.CustomHeaders.Values['X-Parse-REST-API-Key'] := 'yourRESTAPIKey';
  IdHTTP1.Request.ContentType := 'application/json';
  response := IdHttp1.Post('https://api.parse.com/1/events/AppOpened', JSONToSend);
  result := response;
end;
```

**Licence**
-------

    The MIT License (MIT)
    
    Copyright (c) 2016 Jordi Corbilla
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
