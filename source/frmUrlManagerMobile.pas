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

unit frmUrlManagerMobile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Backend.ServiceTypes, REST.Backend.MetaTypes, System.JSON,
  REST.Backend.KinveyServices, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Backend.BindSource, REST.Backend.ServiceComponents,
  REST.Backend.KinveyProvider, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, REST.Backend.Providers,
  FMX.Controls.Presentation, FMX.StdCtrls, lib.urls, Generics.Collections,
  FMX.Edit, FMX.TabControl;

type
  TForm2 = class(TForm)
    ListView1: TListView;
    btnRefresh: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    TabControl1: TTabControl;
    tabCloud: TTabItem;
    tabDetails: TTabItem;
    Panel1: TPanel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    btnAdd: TButton;
    lblUser: TLabel;
    lblPassword: TLabel;
    lblUrl: TLabel;
    procedure btnRefreshClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBackendList: TBackendObjectList<TUrls>;
    function CreateUrlsList(const AProviderID: string; const AStorage: TBackendStorageApi): TBackendObjectList<TUrls>;
    function GetCollection: TList<TUrls>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  StrUtils;

{$R *.fmx}
{$R *.LgXhdpiTb.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}

function TForm2.GetCollection() : TList<TUrls>;
var
  list: TList<TUrls>;
  url: TUrls;
begin
  try
    FBackendList := CreateUrlsList(BackendStorage1.Provider.ProviderID, BackendStorage1.Storage);
    list := TList<TUrls>.Create;
    try
      for url in FBackendList do
        list.Add(url);
    except
      list.Free;
      raise;
    end;
  finally
    result := list;
  end;
end;

procedure TForm2.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  lblUser.Text := 'User: ' + AItem.Text;
  lblPassword.Text := 'Password: ' + AnsiLeftStr(AItem.Detail, AnsiPos(',', AItem.Detail)-1);
  lblUrl.Text := 'Url: ' + AnsiRightStr(AItem.Detail, Length(AItem.Detail) - AnsiPos(',', AItem.Detail));
  Self.TabControl1.ActiveTab := TabDetails;
end;

procedure TForm2.btnRefreshClick(Sender: TObject);
var
  list : TList<TUrls>;
  listitem : TListViewItem;
  url : TUrls;
begin
  list := GetCollection();
  listview1.items.clear;
  for url in list do
  begin
    listitem := listview1.Items.Add;
    listItem.Text := url.User;
    listItem.Detail := url.Password + ',' + url.Url;
  end;
end;

procedure TForm2.btnAddClick(Sender: TObject);
var
  url : TUrls;
  LEntity: TBackendEntityValue;
begin
  url := TURls.Create(edit1.Text, edit2.Text, edit3.Text);
  try
  BackendStorage1.Storage.CreateObject<TUrls>('websites', url, LEntity);
  except
    showMessage('Error adding details');
    raise;
  end;
  showMessage('Item has been successfully added!');
  edit1.Text := '';
  edit2.Text := '';
  edit3.Text := '';
  btnRefreshClick(sender);
end;

function TForm2.CreateUrlsList(const AProviderID: string; const AStorage: TBackendStorageApi): TBackendObjectList<TUrls>;
var
  query: TArray<string>;
  urlList: TBackendObjectList<TUrls>;
begin
  urlList := TBackendObjectList<TUrls>.Create;
  try
    query := TArray<string>.Create(Format('order=%s', ['user']));
    AStorage.QueryObjects<TUrls>('websites', query, urlList);
  except
    urlList.Free;
    raise;
  end;
  Result := urlList;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  btnRefreshClick(sender);
end;

end.
