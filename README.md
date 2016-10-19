# BaaS Delphi Samples
![](https://3.bp.blogspot.com/-_QZgeowOPYQ/V2xA5wUUHLI/AAAAAAAAFgQ/6Ca1guejVVw2U1s3EYi56bGkshge8b02ACLcB/s1600/mbaas.png)
==============
Code samples for **BaaS (Kinvey)**, **BaaS (Firebase)** and **PaaS (Parse.com)** using Delphi
--------------
**Related Articles:**
  - [BaaS with Kinvey and Delphi 10.1 Berlin](http://thundaxsoftware.blogspot.co.uk/2016/06/baas-with-kinvey-and-delphi-101-berlin.html)
  - [Creating your own self-hosted Parse server](http://thundaxsoftware.blogspot.co.uk/2016/05/creating-your-own-self-hosted-parse.html)
  - [Deploying Parse server to Heroku](http://thundaxsoftware.blogspot.co.uk/2016/05/deploying-parse-server-to-heroku.html)
  - [Sending REST API messages with Delphi to Parse.com](http://thundaxsoftware.blogspot.co.uk/2015/12/sending-rest-api-messages-with-delphi.html)

**Win64 example app**

![](https://4.bp.blogspot.com/-AsqDrk0ZnAA/V26CtdQr9FI/AAAAAAAAFhc/dMZrrIs3bJoGPVx2Vsc8nxt46i4W4pqcQCLcB/s640/vclExample.png)

**Android example app**

![](https://3.bp.blogspot.com/-aYTpo_Q5MVc/V26ViH6L8FI/AAAAAAAAFiE/QNomSrfWic0ZEeTD8Na6IyzUOuWnzPC5gCLcB/s640/Screenshot_2016-06-25-14-47-29.png)

**Data stored in Kinvey**

![](https://1.bp.blogspot.com/-q6DpnnASkCY/V2-a06zpAfI/AAAAAAAAFiU/6KLybvIw-KsJ9ewDYQIwYy9xUvUGRpFJgCLcB/s640/Kinvey7.png)


These can be downloaded here:
  - https://github.com/JordiCorbilla/BaaSDelphiSamples/releases

*Example usage:*
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

**Licence**
-------

    The MIT License (MIT)
    
    Copyright (c) 2015 Jordi Corbilla
    
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
