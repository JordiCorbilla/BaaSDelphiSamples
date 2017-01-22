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

unit CloudMessaging;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ActnList, FMX.TabControl, System.Actions, System.IOUtils,
  FMX.ScrollBox, FMX.Memo, System.PushNotification, lib.firebase.rest, IdSSLOpenSSLHeaders
  {$IFDEF ANDROID}
    , FMX.PushNotification.android
  {$ENDIF};

type
  TfrmMain = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton2: TSpeedButton;
    ActionList1: TActionList;
    TitleAction: TControlAction;
    PreviousTabAction1: TPreviousTabAction;
    NextTabAction1: TNextTabAction;
    ShowToken: TAction;
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    RegisterDevice: TAction;
    procedure ShowTokenExecute(Sender: TObject);
    procedure RegisterDeviceExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    PushService: TPushService;
    ServiceConnection: TPushServiceConnection;
    DeviceId : string;
    DeviceToken : string;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  IdOpenSSLSetLibPath(TPath.GetDocumentsPath);
end;

procedure TfrmMain.RegisterDeviceExecute(Sender: TObject);
var
  response : string;
begin
  //Register the device and get the token back
  response := TFirebaseRest.New.RegisterDeviceToken(DeviceId,'xx');
  Memo1.Lines.Add(response);
end;

procedure TfrmMain.ShowTokenExecute(Sender: TObject);
begin
{$IFDEF ANDROID}
  PushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);
  PushService.AppProps[TPushService.TAppPropNames.GCMAppID] := 'FCM ID';
{$ENDIF}
  ServiceConnection := TPushServiceConnection.Create(PushService);
  ServiceConnection.Active := True;

  DeviceID := PushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceID];
  DeviceToken := PushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
  Memo1.Lines.Add('DeviceID: ' + DeviceID);
  Memo1.Lines.Add('deviceToken: ' + deviceToken);
end;

end.
